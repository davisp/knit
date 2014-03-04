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

-module(knit_release).


-export([
    rel_file/0,
    rel_file/1,

    paths/0,
    paths/1,

    tmp_location/1,

    extract/1,
    load/0,
    load/1,

    package/0
]).


-include_lib("sasl/src/systools.hrl").
-include("knit.hrl").


rel_file() ->
    {BootRelName, BootRelVsn} = knit_vsn:boot_rel(),
    Filename = filename:absname(filename:join([
            knit_cfg:get(build_dir),
            "releases",
            BootRelVsn,
            BootRelName
        ])),
    case filelib:is_regular(Filename ++ ".rel") of
        true ->
            Filename;
        false ->
            ?ABORT("Unable to locate the current release file.", [])
    end.


rel_file(Version) ->
    {BootRelName, _} = knit_vsn:boot_rel(),
    Filename = filename:absname(filename:join([
            tmp_location(Version),
            BootRelName,
            "releases",
            Version,
            BootRelName
        ])),
    case filelib:is_regular(Filename ++ ".rel") of
        true ->
            Filename;
        false ->
            Fmt = "Release ~s is missing rel file: ~s",
            ?BAD_CONFIG(Fmt, [Version, Filename])
    end.


paths() ->
    Patterns = [
        filename:join([knit_cfg:get(build_dir), "lib", "*", "ebin"])
    ],
    paths0(Patterns).


paths(Version) ->
    {BootRelName, _} = knit_vsn:boot_rel(),
    Patterns = [
        filename:join([tmp_location(Version), BootRelName, "lib", "*", "ebin"])
    ],
    paths0(Patterns).


paths0(Patterns) ->
    Paths0 = lists:foldl(fun(P, Acc) ->
        filelib:wildcard(P) ++ Acc
    end, [], Patterns),
    lists:filter(fun filelib:is_dir/1, Paths0).


tmp_location(Version) ->
    TmpDir = knit_cfg:get(tmp_dir),
    VsnDir = filename:join(TmpDir, Version),
    filename:absname(VsnDir).


extract(Version) ->
    TarballName = knit_vsn:release_tarball(Version),
    knit_log:debug("Extracting: ~s", [TarballName]),
    RelDir = tmp_location(Version),
    knit_file:ensure_dir_exists(RelDir),
    knit_file:do_in_dir(RelDir, fun() ->
        ok = erl_tar:extract(TarballName, [compressed])
    end).


load() ->
    load(current, rel_file(), paths()).


load(Version) ->
    load(Version, rel_file(Version), paths(Version)).


load(Version, RelFile, Paths) ->
    ?TRACE("Loading ~s from:~n~p", [Version, Paths]),
    case systools_make:get_release(RelFile, Paths) of
        {ok, Rel, Apps, Warnings} ->
            knit_util:report_warnings(systools_make, Warnings),
            {RelFile, Rel, Apps};
        {error, Error} ->
            Reason = systools_make:format_error(Error),
            ?BAD_CONFIG("Error loading release ~s: ~s", [Version, Reason])
    end.


package() ->
    TarballName = knit_vsn:release_tarball(),

    knit_log:info("Creating release tarball: ~s", [TarballName]),

    Force = knit_cfg:get(force),
    case filelib:is_file(TarballName) of
        true when Force ->
            knit_log:info("Overwriting: ~s", [TarballName]),
            knit_file:rm_rf(TarballName);
        true ->
            ?IO_ERROR("Refusing to overwrite: ~s", [TarballName]);
        false ->
            ok
    end,

    TarDir = filename:dirname(knit_cfg:get(build_dir)),
    BuildDirName = filename:basename(knit_cfg:get(build_dir)),
    knit_file:do_in_dir(TarDir, fun() ->
        Tar = case erl_tar:open(TarballName, [write, compressed]) of
            {ok, Tar0} ->
                Tar0;
            {error, Reason} ->
                Fmt = "Error creating tar file ~s: ~s",
                ?IO_ERROR(Fmt, [TarballName, erl_tar:format_error(Reason)])
        end,
        ok = erl_tar:add(Tar, BuildDirName, []),
        ok = erl_tar:close(Tar)
    end).

