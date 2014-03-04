-module(knit_file_win32).


-export([
    rm_rf/1,
    cp_r/2,
    mv/2
]).


-include("knit.hrl").


rm_rf(_) ->
    ?NOT_SUPPORTED("Windows not currently supported.", []).


cp_r(_, _) ->
    ?NOT_SUPPORTED("Windows not currently supported.", []).


mv(_, _) ->
    ?NOT_SUPPORTED("Windows not currently supported.", []).