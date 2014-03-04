% Original version based on rebar_upgrade

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

    Src = knit_release:rel_file() ++ ".rel",
    Dst = filename:join(".", BootRel ++ ".rel"),
    knit_log:debug("Copying: ~s -> ~s", [Src, Dst]),
    case file:copy(Src, Dst) of
        {ok, _} ->
            ok;
        {error, Error} ->
            Reason = file:format_error(Error),
            ?IO_ERROR("Error creating ~s.rel: ~s", [BootRel, Reason])
    end,

    make_relup(BootRel, UpRels, Paths),
    make_script(BootRel, Paths),
    make_tar(BootRel, Paths),

    knit_log:info("Upgrade tarball created."),
    ok.


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
