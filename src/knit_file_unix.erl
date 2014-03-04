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