-module(knit_cfg).

-export([
    init/1,

    get/1,
    get_sys/1,
    get_sys/2,

    reltool_config_file/0,
    log_level/0,
    log_devices/0
]).


-include("knit.hrl").


-define(DEFAULT_LOG_LEVEL, 2).
-define(DEFAULT_LOG_DEVICES, [standard_io]).
-define(DEFAULT_RELTOOL_FILES, ["reltool.config", "rel/reltool.config"]).
-define(VAR_RE, "([^=]+)=(.*)").


init(Args) ->
    init_debug(Args),
    {ok, Opts, Vars} = knit_cli:parse(Args),
    knit_log:debug("Initializing config from: ~p", [Opts]),
    set_log_devices(Opts),
    set_log_level(Opts),
    set_reltool_config(Opts),
    set_cli_vars(Vars),
    ok.


get(force) ->
    case get_config_key(force) of
        {force, "true"} -> true;
        {force, true} -> true;
        _ -> false
    end;
get(semver) ->
    case get_config_key(semver) of
        {semver, "true"} -> true;
        {semver, true} -> true;
        _ -> false
    end;
get(warnings_as_errors) ->
    case get_config_key(warnings_as_errors) of
        {warnings_as_errors, "true"} -> true;
        {warnings_as_errors, true} -> true;
        _ -> false
    end;
get(sys) ->
    {sys, Sys} = get_config_key(sys),
    Sys;
get(boot_rel) ->
    case get_sys(boot_rel) of
        {boot_rel, Name} ->
            Sys = ?MODULE:get(sys),
            case [I || {rel, N, _, _} = I <- Sys, N == Name] of
                [Rel] ->
                    Rel;
                [] ->
                    ?BAD_CONFIG("Missing specified boot release: ~s", [Name]);
                [_|_] ->
                    ?BAD_CONFIG("Multile release definitions for: ~s", [Name])
            end;
        undefined ->
            % If boot_rel is undefined just use the first rel
            % entry in sys. Not sure if this is entirely kosher
            % or if reltool has some preference.
            case knit_cfg:get_sys(rel) of
                {rel, _, _, _} = Rel ->
                    Rel;
                _ ->
                    ?BAD_CONFIG("No valid release definition.", [])
            end
    end;
get(root_dir) ->
    case get_config_key(root_dir) of
        {root_dir, RootDir} ->
            filename:absname(RootDir);
        undefined ->
            filename:absname(code:root_dir())
    end;
get(target_dir) ->
    case get_config_key(target_dir) of
        {target_dir, TargetDir} ->
            TargetDir;
        undefined ->
            DirName = filename:dirname(reltool_config_file()),
            filename:absname(DirName)
    end;
get(build_dir) ->
    case ?MODULE:get(boot_rel) of
        {rel, Name, _Vsn, _Apps} ->
            target_subdir(Name);
        _ ->
            target_subdir("target")
    end;
get(tmp_dir) ->
    case get_config_key(tmp_dir) of
        {tmp_dir, TmpDir} ->
            TmpDir;
        undefined ->
            target_subdir(".knit_tmp")
    end;
get(release_dir) ->
    case get_config_key(release_dir) of
        {release_dir, ReleaseDir} ->
            ReleaseDir;
        undefined ->
            target_subdir("releases")
    end;
get(upgrade_dir) ->
    case get_config_key(upgrade_dir) of
        {upgrade_dir, UpgradeDir} ->
            UpgradeDir;
        undefined ->
            target_subdir("upgrades")
    end;
get(Else) ->
    case get_config_key(Else) of
        {Else, Value} ->
            Value;
        undefined ->
            undefined
    end.


get_sys(Key) ->
    get_sys(Key, undefined).


get_sys(Key, Default) ->
    {sys, Sys} = get_config_key(sys),
    case lists:keyfind(Key, 1, Sys) of
        false ->
            Default;
        Value ->
            Value
    end.


reltool_config_file() ->
    {ok, FileName} = application:get_env(knit, reltool_config_file),
    FileName.


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


init_debug(Args) ->
    case lists:member("-d", Args) orelse lists:member("--debug", Args) of
        true ->
            application:set_env(knit, log_level, 3);
        false ->
            ok
    end.


set_log_level(Opts) ->
    Base = case lists:member(debug, Opts) of
        true -> 3;
        false -> 1
    end,
    Level = Base + lists:sum([V || {verbose, V} <- Opts]),
    application:set_env(knit, log_level, Level).


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
                    ?BAD_CONFIG("Error opening log file: ~p", [Error])
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


load_reltool_config(FileName0) ->
    FileName = filename:absname(FileName0),
    case filelib:is_file(FileName) of
        false ->
            ?BAD_CONFIG("Unable to find reltool config: ~s", [FileName0]);
        true ->
            ok
    end,
    ok = file:set_cwd(filename:dirname(FileName)),
    application:set_env(knit, reltool_config_file, FileName),
    case file:consult(FileName) of
        {ok, Config} ->
            application:set_env(knit, config, set_version(Config));
        {error, Error} ->
            knit_log:error("Error parsing reltool config: ~s", [FileName]),
            ?BAD_CONFIG(file:format_error(Error), [])
    end.


load_default_reltool_config() ->
    Found = lists:filter(fun filelib:is_file/1, ?DEFAULT_RELTOOL_FILES),
    case Found of
        [] ->
            Paths = string:join(?DEFAULT_RELTOOL_FILES, ", "),
            knit_log:warn("Error locating reltool config in: ~s", [Paths]),
            ?BAD_CONFIG("No reltool config found.", []);
        [FileName | _] ->
            load_reltool_config(FileName)
    end.


set_cli_vars(Vars) ->
    lists:foreach(fun set_cli_var/1, Vars).


set_cli_var(Var) ->
    case re:run(Var, ?VAR_RE, [{capture, all_but_first, list}]) of
        {match, [Key, Val]} ->
            set_cli_var(knit_util:strip(Key), knit_util:strip(Val));
        _ ->
            ?BAD_CLI("Invalid variable declaration: ~s", [Var])
    end.


set_cli_var(Key0, Val0) ->
    Key = list_to_atom(Key0),
    Val = maybe_parse_value(Val0),
    knit_log:debug("Command line variable: ~p :: ~p", [Key, Val]),
    {ok, Config} = application:get_env(knit, config),
    NewConfig = lists:keystore(Key, 1, Config, {Key, Val}),
    application:set_env(knit, config, NewConfig).


maybe_parse_value("!!" ++ Val) ->
    ErrFmt = "Invalid command line variable value: ~s~n        ~s",
    case erl_scan:string(Val ++ ".") of
        {ok, Terms, _} ->
            case erl_parse:parse_term(Terms) of
                {ok, Term} ->
                    Term;
                {error, {_Loc, Mod, Error}} ->
                    Reason = Mod:format_error(Error),
                    ?BAD_CLI(ErrFmt, [Val, Reason])
            end;
        {error, {_Loc, Mod, Error}} ->
            Reason = Mod:format_error(Error),
            ?BAD_CLI(ErrFmt, [Val, Reason])
    end;
maybe_parse_value("\\!\\!" ++ Val) ->
    "!!" ++ Val;
maybe_parse_value(Val) ->
    Val.


set_version(Config) ->
    case lists:keyfind(sys, 1, Config) of
        {sys, Sys} ->
            NewSys = lists:map(fun set_rel_version/1, Sys),
            lists:keyreplace(sys, 1, Config, {sys, NewSys});
        false ->
            Config
    end.


set_rel_version({rel, Name, Version, Apps}) ->
    VcsVsn = knit_vcs:version(Version),
    {rel, Name, VcsVsn, Apps};
set_rel_version(Item) ->
    Item.


get_config_key(Key) ->
    get_config_key(Key, undefined).


get_config_key(Key, Default) ->
    {ok, Config} = application:get_env(knit, config),
    case lists:keyfind(Key, 1, Config) of
        false ->
            Default;
        Value ->
            Value
    end.


target_subdir(Name) ->
    DirName = ?MODULE:get(target_dir),
    filename:absname(filename:join(DirName, Name)).
