-module(knit_util).

-export([
    abort/1,
    abort/2,
    abort/3,

    cmd/1,
    fmt/2,
    strip/1,

    reset_tmp_directory/0,

    report_warnings/2
]).


-include("knit.hrl").


abort(Code) when is_integer(Code) ->
    erlang:halt(Code).


abort(Code, Reason) ->
    knit_log:error(Reason),
    abort(Code).


abort(Code, ReasonFmt, Args) ->
    knit_log:error(ReasonFmt, Args),
    abort(Code).


cmd(Command) ->
    PortSettings = [
        exit_status,
        {line, 16384},
        use_stdio,
        stderr_to_stdout,
        hide
    ],
    Port = erlang:open_port({spawn, Command}, PortSettings),
    try
        cmd_loop(Port, [])
    after
        catch erlang:port_close(Port)
    end.


fmt(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).


strip(Val) when is_list(Val) ->
    re:replace(Val, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).


reset_tmp_directory() ->
    TmpDir = knit_cfg:get(tmp_dir),
    knit_log:debug("Resetting tmp_dir ~s", [TmpDir]),
    knit_file:rm_rf(TmpDir),
    knit_file:ensure_dir_exists(TmpDir).


report_warnings(_, []) ->
    ok;
report_warnings(Module, Warnings) ->
    Reasons = Module:format_warning(Warnings),
    lists:foreach(fun(R) -> knit_log:debug(strip(R)) end, Reasons),
    case knit_cfg:get(warnings_as_errors) of
        true ->
            ?ABORT("Treating warnings as errors.", []);
        false ->
            ok
    end.


cmd_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            cmd_loop(Port, [Line ++ "\n" | Acc]);
        {Port, {data, {noeol, Line}}} ->
            cmd_loop(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}}
    end.
