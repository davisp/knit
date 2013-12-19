-module(knit).

-export([
    main/1
]).


main(Args) ->
    {ok, Commands} = knit_cfg:init(Args),
    execute(Commands).


execute([]) ->
    ok;
execute([Command | Rest]) ->
    knit_log:debug("Executing command: ~s", [Command]),
    execute(Rest).
