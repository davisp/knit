-module(knit).

-export([
    main/1
]).


steps() ->
    [
    ].


main(Args) ->
    process_flag(trap_exit, true),
    try
        ok = knit_cfg:init(Args),
        lists:foreach(fun({Mod, Fun}) ->
            erlang:apply(Mod, Fun, [])
        end, steps())
    catch T:R ->
        Stack = erlang:get_stacktrace(),
        knit_log:error("~p~n~n~p~n", [{T,R}, Stack])
    end.
