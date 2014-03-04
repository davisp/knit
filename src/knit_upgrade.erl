% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

% Initial versions based on rebar_upgrade.erl

-module(knit_upgrade).


-export([
    run/0
]).


-include_lib("sasl/src/systools.hrl").
-include("knit.hrl").


run() ->
    knit_log:info("Generating release upgrades."),
    knit_util:reset_tmp_directory(),

    case knit_vsn:to_upgrade() of
        [] ->
            knit_log:warn("No versions available for upgrades.");
        Versions ->
            run(Versions)
    end,
    
    case knit_cfg:get(remove_tmp) of
        true ->
            knit_log:info("Removing temp directory."),
            knit_file:rm_rf(knit_cfg:get(tmp_dir));
        false ->
            ok
    end.


run(Versions) ->
    lists:foreach(fun knit_release:extract/1, Versions),

    FromReleases = lists:map(fun knit_release:load/1, Versions),
    ToRelease = knit_release:load(),

    SupportedAppVsns = knit_appups:generate(FromReleases, ToRelease),

    ValidFromReleases = filter_from_releases(FromReleases, SupportedAppVsns),

    case length(ValidFromReleases) > 0 of
        true ->
            run_systools(ValidFromReleases, ToRelease);
        false ->
            twig_log:warn("No valid release upgrades found.", [])
    end,

    ok.

filter_from_releases(FromReleases, SupportedAppVsns) ->
    lists:filter(fun({_, Rel, FromApps}) ->
        try
            lists:foreach(fun({{Name, Vsn}, _App}) ->
                case dict:find(Name, SupportedAppVsns) of
                    {ok, Vsns} ->
                        case lists:member(Vsn, Vsns) of
                            true ->
                                ok;
                            false ->
                                throw({missing_vsn, Name, Vsn})
                        end;
                    error ->
                        % App was deleted. No biggy.
                        ok
                end
            end, FromApps),
            true
        catch throw:{missing_vsn, Name, Vsn} ->
            Fmt = "Upgrade for release ~s is missing appup for ~s ~s",
            knit_log:warn(Fmt, [Rel#release.vsn, Name, Vsn]),
            false
        end
    end, FromReleases).


run_systools(FromRels, ToRel) ->
    {BootRelName, BootRelVsn} = knit_vsn:boot_rel(),
    BootRel = BootRelName ++ "-" ++ BootRelVsn,

    Paths = collect_paths([ToRel | FromRels]),
    UpRels = [RelFile || {RelFile, _Rel, _Apps} <- FromRels],

    % First things first, create a tmp directory containing
    % the versioned rel file. This is mainly due to
    % systools:make_tar/2 basing all of its work off the
    % location and name of the rel file. For now I'll assume
    % that no package has a version named "upgrade".
    
    UpgradeDir = knit_release:tmp_location("upgrade"),
    case filelib:is_dir(UpgradeDir) of
        true ->
            ?IO_ERROR("Temp directory exists: ~s", [UpgradeDir]);
        false ->
            ok
    end,
    knit_file:ensure_dir_exists(UpgradeDir),
    copy_rel_file(UpgradeDir, BootRel),

    knit_file:do_in_dir(UpgradeDir, fun() ->
        make_relup(BootRel, UpRels, Paths),
        make_script(BootRel, Paths),
        make_tar(BootRel, Paths),
        update_tar(BootRelName, BootRelVsn, BootRel),
        package_upgrade()
    end),

    knit_log:info("Upgrade tarball created."),
    ok.


copy_rel_file(UpgradeDir, BootRel) ->
    Src = knit_release:rel_file() ++ ".rel",
    Dst = filename:join(UpgradeDir, BootRel ++ ".rel"),
    knit_file:copy(Src, Dst).
    

make_relup(BootRel, UpRels, Paths) ->
    case systools:make_relup(BootRel, UpRels, [], [silent, {path, Paths}]) of
        {ok, _Relup, Mod, RelupWarnings} ->
            knit_util:report_warnings(Mod, RelupWarnings);
        {error, Mod, Error} ->
            Reason = Mod:format_error(Error),
            Fmt = "Error creating relup: ~s",
            ?ABORT(Fmt, [Reason])
    end.


make_script(BootRel, Paths) ->
    case systools:make_script(BootRel, [silent, {path, Paths}]) of
        {ok, Mod, Warnings0} ->
            % Strip out source not found warnings cause they're
            % noisy and pointless.
            Warnings = lists:filter(fun(W) ->
                case W of
                    {_, {source_not_found, _}} -> false;
                    _ -> true
                end
            end, Warnings0),
            knit_util:report_warnings(Mod, Warnings);
        {error, Mod, Error} ->
            Reason = Mod:format_error(Error),
            Fmt = "Error creating boot script: ~s",
            ?ABORT(Fmt, [Reason])
    end.


make_tar(BootRel, Paths) ->
    case systools:make_tar(BootRel, [silent, {path, Paths}]) of
        {ok, Mod, Warnings0} ->
            % Strip out source not found warnings cause they're
            % noisy and pointless.
            Warnings = lists:filter(fun(W) ->
                case W of
                    {_, {source_not_found, _}} -> false;
                    _ -> true
                end
            end, Warnings0),
            knit_util:report_warnings(Mod, Warnings);
        {error, Mod, Errors} ->
            Reason = Mod:format_error(Errors),
            Fmt = "Error creating upgrade tar: ~s",
            ?ABORT(Fmt, [Reason])
    end.


update_tar(BootRelName, BootRelVsn, BootRel) ->
    % We need to tweak the upgrade tarball a bit
    % with some conveniences for running the node
    % after a release has been applied.

    TarFile = BootRel ++ ".tar.gz",
    case erl_tar:extract(TarFile, [compressed]) of
        ok ->
            ok;
        {error, Error} ->
            Reason = erl_tar:format_error(Error),
            Fmt = "Error extracting ~s: ~s",
            ?ABORT(Fmt, [filename:basename(TarFile), Reason])
    end,
    ok = file:delete(TarFile),
    
    % Copy start.boot -> BootRelName.boot for consistency with
    % normal release tarballs.
    StartBootSrc = filename:join([
            "releases",
            BootRelVsn,
            "start.boot"
        ]),
    StartBootDst = filename:join([
            "releases",
            BootRelVsn,
            BootRelName ++ ".boot"
        ]),
    knit_file:copy(StartBootSrc, StartBootDst),

    % Copy a start_clean.boot to the release for remsh or
    % other scripts.
    StartCleanBootSrc = filename:join([
            knit_cfg:get(build_dir),
            "releases",
            BootRelVsn,
            "start_clean.boot"
        ]),
    StartCleanBootDst = filename:join([
            "releases",
            BootRelVsn,
            "start_clean.boot"
        ]),
    knit_file:copy(StartCleanBootSrc, StartCleanBootDst),

    % Copy over sys.config and vm.args 
    SysConfigSrc = filename:join([
            knit_cfg:get(build_dir),
            "releases",
            BootRelVsn,
            "sys.config"
        ]),
    SysConfigDst = filename:join([
            "releases",
            BootRelVsn,
            "sys.config"
        ]),
    case filelib:is_regular(SysConfigSrc) of
        true ->
            knit_file:copy(SysConfigSrc, SysConfigDst);
        false ->
            ok
    end,
    
    VmArgsSrc = filename:join([
            knit_cfg:get(build_dir),
            "releases",
            BootRelVsn,
            "vm.args"
        ]),
    VmArgsDst = filename:join([
            "releases",
            BootRelVsn,
            "vm.args"
        ]),
    case filelib:is_regular(VmArgsSrc) of
        true ->
            knit_file:copy(VmArgsSrc, VmArgsDst);
        false ->
            ok
    end.


package_upgrade() ->
    TarFile = knit_vsn:upgrade_tarball(),
    
    Force = knit_cfg:get(force),
    case filelib:is_file(TarFile) of
        true when Force ->
            knit_log:info("Overwriting: ~s", [TarFile]),
            knit_file:rm_rf(TarFile);
        true ->
            ?IO_ERROR("Refusing to overwrite: ~s", [TarFile]);
        false ->
            ok
    end,
    
    Tar = case erl_tar:open(TarFile, [write, compressed]) of
        {ok, Tar0} ->
            Tar0;
        {error, Error} ->
            OpenFmt = "Error creating tar file ~s: ~s",
            ?IO_ERROR(OpenFmt, [TarFile, erl_tar:format_error(Error)])
    end,
    ok = erl_tar:add(Tar, "lib", []),
    ok = erl_tar:add(Tar, "releases", []),
    ok = erl_tar:close(Tar).


collect_paths(Rels) ->
    {_, BootRelVsn} = knit_vsn:boot_rel(),
    InitPaths = [
        filename:join([knit_cfg:get(build_dir), "releases", BootRelVsn])
    ],
    Paths = lists:foldl(fun({_RelFile, _Rel, Apps}, OuterAcc) ->
        lists:foldl(fun({_NameVsn, App}, InnerAcc) ->
            [App#application.dir | InnerAcc]
        end, OuterAcc, Apps)
    end, InitPaths, Rels),
    lists:reverse(lists:usort(Paths)).
