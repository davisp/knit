-module(knit_util).

-export([
    abort/1,
    abort/2,
    abort/3
]).



abort(Code) when is_integer(Code) ->
    erlang:halt(Code).


abort(Code, Reason) ->
    knit_log:error(Reason),
    abort(Code).


abort(Code, ReasonFmt, Args) ->
    knit_log:error(ReasonFmt, Args),
    abort(Code).
