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

-module(knit_vsn).


-export([
    boot_rel/0,

    available/0,
    matching/0,
    to_upgrade/0,
    cmp/2,
    sort/1,
    semver/1,

    release_tarball/0,
    release_tarball/1,
    upgrade_tarball/0,
    upgrade_tarball/1
]).


-include("knit.hrl").


boot_rel() ->
    {rel, Name, Vsn, _Apps} = knit_cfg:get(boot_rel),
    {Name, Vsn}.


available() ->
    {BootRelName, BootRelVsn} = boot_rel(),
    UpgradeDir = knit_cfg:get(release_dir),
    Tarballs0 = filelib:wildcard("*.tar.gz", UpgradeDir),
    Tarballs = Tarballs0 -- [BootRelName ++ "-" ++ BootRelVsn ++ ".tar.gz"],
    Found = lists:flatmap(fun(Name) ->
        BaseName = filename:basename(Name, ".tar.gz"),
        case re:split(BaseName, "-", [{return, list}]) of
            [BootRelName, Vsn | Rest] ->
                [string:join([Vsn | Rest], "-")];
            _ ->
                []
        end
    end, Tarballs),
    Sorted = sort(Found),
    ?TRACE("Found versions:~n    ~p", [Sorted]),
    Sorted.


matching() ->
    V0 = available(),
    V1 = regexp_versions(V0),
    recent_versions(V1).


to_upgrade() ->
    case knit_cfg:get(upgrade_versions) of
        Versions when is_list(Versions) ->
            sort(Versions);
        VerFun when is_function(VerFun) ->
            VerFun(matching());
        undefined ->
            matching();
        Else ->
            ?BAD_CONFIG("Invalid upgrade_versions: ~p", Else)
    end.


cmp(Vsn1, Vsn2) when is_list(Vsn1), is_list(Vsn2) ->
    {A, B} = case knit_cfg:get(semver) of
        true -> {semver(Vsn1), semver(Vsn2)};
        false -> {Vsn1, Vsn2}
    end,
    if
        A < B -> -1;
        A > B -> 1;
        true -> 0
    end.


sort(Versions) ->
    lists:sort(fun(A, B) -> cmp(A, B) =< 0 end, Versions).


semver(Vsn) when is_list(Vsn) ->
    % A super hacky semantic version parsing that
    % hopefully doesn't fail regardless of input.
    %
    % Split the input on periods. For each component,
    % attempt to convert everything before a hyphen
    % to an integer. Fall back to the original if
    % anything happens to fail.
    [First0 | Rest] = re:split(Vsn, "\\.", [{return, list}]),
    First = case First0 of
        "v" ++ Rest -> Rest;
        _ -> First0
    end,
    TryInt = fun(P) ->
        {MaybeInt, Tail} = case re:split(Vsn, "-", [{return, list}]) of
            [MI] -> {MI, []};
            [MI | Tail0] -> {MI, Tail0}
        end,
        try
            Int = list_to_integer(MaybeInt),
            {Int, string:join(Tail, "-")}
        catch _:_ ->
            P
        end
    end,
    AsInts = lists:map(TryInt, [First | Rest]),
    list_to_tuple(AsInts);
semver(Vsn) when is_binary(Vsn) ->
    semver(binary_to_list(Vsn)).


release_tarball() ->
    {_, BootRelVsn} = boot_rel(),
    release_tarball(BootRelVsn).


release_tarball(Vsn) ->
    ReleaseDir = knit_cfg:get(release_dir),
    {BootRelName, _} = boot_rel(),
    FileName = BootRelName ++ "-" ++ Vsn ++ ".tar.gz",
    filename:absname(filename:join(ReleaseDir, FileName)).


upgrade_tarball() ->
    {_, BootRelVsn} = boot_rel(),
    upgrade_tarball(BootRelVsn).


upgrade_tarball(Vsn) ->
    UpgradeDir = knit_cfg:get(upgrade_dir),
    {BootRelName, _} = boot_rel(),
    FileName = BootRelName ++ "-" ++ Vsn ++ ".tar.gz",
    filename:absname(filename:join(UpgradeDir, FileName)).


regexp_versions(Versions) ->
    {VerRe, ReOpts} = case knit_cfg:get(version_regexp) of
        ListRe when is_list(ListRe) ->
            {ListRe, []};
        {ListRe, Opts} when is_list(ListRe) ->
            {ListRe, Opts};
        BinRe when is_binary(BinRe) ->
            {binary_to_list(BinRe), []};
        {BinRe, Opts} when is_binary(BinRe) ->
            {binary_to_list(BinRe), Opts};
        undefined ->
            {undefined, []};
        Else ->
            ?BAD_CONFIG("Invalid version_regexp: ~p", [Else])
    end,
    if VerRe == undefined -> Versions; true ->
        Matching = lists:filter(fun(V) ->
            case re:run(V, VerRe, ReOpts) of
                {match, _} -> true;
                _ -> false
            end
        end, Versions),
        ?TRACE("Versions matching regexp ~s:~n    ~p", [VerRe, Matching]),
        Matching
    end.


recent_versions(Versions) ->
    MinVersion = case knit_cfg:get(version_min) of
        ListV when is_list(ListV) ->
            ListV;
        BinV when is_binary(BinV) ->
            binary_to_list(BinV);
        undefined ->
            undefined;
        Else ->
            ?BAD_CONFIG("Invalid version_min: ~p", [Else])
    end,
    if MinVersion == undefined -> Versions; true ->
        Recent = lists:filter(fun(V) ->
            cmp(V, MinVersion) >= 0
        end, Versions),
        ?TRACE("Versions newer than ~s:~n    ~p", [MinVersion, Recent]),
        Recent
    end.

