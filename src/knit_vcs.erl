-module(knit_vcs).

-export([
    version/1,
    version/2
]).


-include("knit.hrl").


version(Vcs) ->
    case cmd(Vcs) of
        {plain, Vsn} ->
            Vsn;
        Command ->
            case knit_util:cmd(Command) of
                {ok, Out} ->
                    knit_util:strip(Out);
                {error, _Reason} ->
                    ?IO_ERROR("Error getting VCS version.", [])
            end
    end.


version(Vcs, Dir) ->
    knit_file:do_in_dir(Dir, fun() -> version(Vcs) end).


cmd(git) ->
    "git describe --always --tags";
cmd(hg) ->
    "hg identify -i";
cmd(bzr) ->
    "bzr revno";
cmd(svn) ->
    "svnversion";
cmd(fossil) ->
    "fossil info";
cmd(Version) when is_list(Version) ->
    {plain, Version};
cmd(Unknown) ->
    erlang:error({unknown_version, Unknown}).
