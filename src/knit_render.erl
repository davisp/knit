-module(knit_render).

-export([
    resolve/1,
    run/2
]).


resolve(Ctx) ->
    resolve(dict:to_list(Ctx), Ctx).


resolve([], Ctx) ->
    Ctx;
resolve([{Key, Value0} | Rest], Ctx) when is_list(Value0) ->
    Value = run(list_to_binary(Value0), Ctx),
    resolve(Rest, dict:store(Key, Value, Ctx));
resolve([_Pair | Rest], Ctx) ->
    resolve(Rest, Ctx).


run(Tmpl0, Ctx) ->
    %% Be sure to escape any double-quotes before rendering...
    Tmpl1 = re:replace(Tmpl0, "\\\\", "\\\\\\", [global, {return, list}]),
    Tmpl2 = re:replace(Tmpl1, "\"", "\\\\\"", [global, {return, list}]),
    mustache:render(Tmpl2, Ctx).

