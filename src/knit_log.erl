-module(knit_log).

-export([
    error/1, error/2,
    warn/1, warn/2,
    info/1, info/2,
    debug/1, debug/2,

    trace/3, trace/4
]).


error(Msg) -> log(error, Msg, []).
error(Msg, Args) -> log(error, Msg, Args).


warn(Msg) -> log(warn, Msg, []).
warn(Msg, Args) -> log(warn, Msg, Args).


info(Msg) -> log(info, Msg, []).
info(Msg, Args) -> log(info, Msg, Args).


debug(Msg) -> log(debug, Msg, []).
debug(Msg, Args) -> log(debug, Msg, Args).


trace(Msg, File, Line) ->
    trace(Msg, [], File, Line).

trace(Msg0, Args, File, Line) ->
     Prefix = lists:flatten(io_lib:format("~s(~b) ", [File, Line])),
     log(trace, Prefix ++ Msg0, Args).


log(Level, Fmt0, Args) ->
    Devices = knit_cfg:log_devices(),
    case level(Level) =< knit_cfg:log_level() of
        true when is_list(Devices), length(Devices) > 0 ->
            Fmt = prefix(Level) ++ " " ++ Fmt0 ++ "~n",
            lists:foreach(fun(Dev) ->
                io:format(Dev, Fmt, Args)
            end, Devices);
        _ ->
            ok
    end.


level(error) -> 0;
level(warn) -> 1;
level(info) -> 2;
level(debug) -> 3;
level(trace) -> 4.


prefix(Level) ->
    "[" ++ string:to_upper(atom_to_list(Level)) ++ "]".
