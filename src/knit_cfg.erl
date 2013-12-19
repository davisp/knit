-module(knit_cfg).

-export([
    init/1,
    
    log_level/0,
    log_devices/0
]).


-define(DEFAULT_LOG_LEVEL, 2).
-define(DEFAULT_LOG_DEVICES, [standard_io]).
-define(DEFAULT_RELTOOL_FILES, ["reltool.config", "rel/reltool.config"]).


init(Args) ->
    init_debug(Args),
    {ok, Opts, Commands} = knit_cli:parse(Args),
    knit_log:debug("Initializing config from: ~p", [Opts]),
    set_log_devices(Opts),
    set_log_level(Opts),
    set_reltool_config(Opts),
    knit_log:info("Knit Initialized [~s]", [httpd_util:rfc1123_date()]),
    {ok, Commands}.


init_debug(Args) ->
    case lists:member("-d", Args) orelse lists:member("--debug", Args) of
        true ->
            application:set_env(knit, log_level, 5);
        false ->
            ok
    end.


log_level() ->
    case application:get_env(knit, log_level) of
        {ok, Level} ->
            Level;
        _ ->
            ?DEFAULT_LOG_LEVEL
    end.
            

log_devices() ->
    case application:get_env(knit, log_devices) of
        {ok, Devices} ->
            Devices;
        _ ->
            ?DEFAULT_LOG_DEVICES
    end.


set_log_level(Opts) ->
    case lists:member(debug, Opts) of
        true ->
            application:set_env(knit, log_level, 5);
        false ->
            Level = lists:sum([V || {verbose, V} <- Opts]),
            application:set_env(knit, log_level, Level)
    end.


set_log_devices(Opts) ->
    Devices = [] ++
    case proplists:get_value(quiet, Opts, false) of
        true ->
            [];
        _ ->
            knit_log:debug("Logging to standard error."),
            [standard_error]
    end
    ++
    case proplists:get_value(log_file, Opts) of
        FileName when is_list(FileName) ->
            case file:open(FileName, [append]) of
                {ok, Fd} ->
                    knit_log:debug("Logging to ~s", [FileName]),
                    [Fd];
                {error, Error} ->
                    knit_util:abort("Error opening log file: ~p", [Error])
            end;
        undefined ->
            []
    end,
    application:set_env(knit, log_devices, Devices).


set_reltool_config(Opts) ->
    case proplists:get_value(reltool, Opts) of
        FileName when is_list(FileName) ->
            load_reltool_config(FileName);
        _ ->
            load_default_reltool_config()
    end.


load_reltool_config(FileName) ->
    case filelib:is_file(FileName) of
        false ->
            knit_util:abort("Unable to find reltool config: ~s", [FileName]);
        true ->
            ok
    end,
    case file:consult(FileName) of
        {ok, Data} ->
            application:set_env(knit, reltool_config, Data);
        {error, Error} ->
            Msg = file:format_error(Error),
            knit_log:error("Error parsing reltool config: ~s", [FileName]),
            knit_util:abort(2, Msg)
    end.


load_default_reltool_config() ->
    Found = lists:filter(fun filelib:is_file/1, ?DEFAULT_RELTOOL_FILES),
    case Found of
        [] ->
            Paths = string:join(?DEFAULT_RELTOOL_FILES, ", "),
            knit_log:warn("Error locating reltool config in: ~s", [Paths]),
            knit_util:abort(2, "No reltool config found.");
        [FileName | _] ->
            load_reltool_config(FileName)
    end.
