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

-module(knit_file_unix).


-export([
    rm_rf/1,
    cp_r/2,
    mv/2
]).


-include("knit.hrl").


rm_rf(Path) ->
    Cmd = knit_util:fmt("rm -rf ~s", [sh_escape(Path)]),
    {ok, []} = knit_util:cmd(Cmd),
    ok.


cp_r([], _Dst) ->
    ok;
cp_r(Srcs, Dst) ->
    Escaped = [sh_escape(Src) || Src <- Srcs],
    SrcStr = string:join(Escaped, " "),
    Cmd = knit_util:fmt("cp -R ~s \"~s\"", [SrcStr, Dst]),
    {ok, []} = knit_util:cmd(Cmd),
    ok.


mv(Src, Dst) ->
    Cmd = knit_util:fmt("mv ~s ~s", [sh_escape(Src), sh_escape(Dst)]),
    {ok, []} = knit_util:cmd(Cmd),
    ok.


sh_escape(Str) ->
    re:replace(Str, " ", "\\\\ ", [global, {return, list}]).