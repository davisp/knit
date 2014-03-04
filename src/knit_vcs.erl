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

% Initial versions based on portions of rebar_util.erl

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
