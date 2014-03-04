% Initial versions based on rebar_reltool

-module(knit_reltool).

-export([
    run/0
]).


-include("knit.hrl").


run() ->
    {BootRelName, BootRelVsn} = knit_vsn:boot_rel(),
    knit_log:info("Generating release: ~s-~s", [BootRelName, BootRelVsn]),

    Sys = knit_cfg:get(sys),
    RootDir = knit_cfg:get(root_dir),
    BuildDir = knit_cfg:get(build_dir),

    ensure_empty(BuildDir),

    knit_log:debug("Starting reltool server."),
    {ok, Server} = reltool:start_server([{sys, Sys}]),

    ensure_apps_exist(Server, Sys),

    knit_log:debug("Getting the generated target spec."),
    Spec = case reltool:get_target_spec(Server) of
        {ok, Spec0} ->
            Spec0;
        {error, Reason0} ->
            ?BAD_CONFIG("Unable to generate reltool spec: ~p", [Reason0])
    end,

    knit_log:debug("Evaluating target spec."),
    case reltool:eval_target_spec(Spec, RootDir, BuildDir) of
        ok ->
            ok;
        {error, Reason1} ->
            Fmt = "Error generating target from spec : ~w",
            ?BAD_CONFIG(Fmt, [Reason1])
    end,

    ok = create_RELEASES(BuildDir).


ensure_apps_exist(Server, SysConfig) ->
    knit_log:debug("Ensuring reltool found each app."),
    Apps = case lists:keyfind(rel, 1, SysConfig) of
        {rel, _Name, _Vsn, Apps0} ->
            lists:usort(Apps0);
        false ->
            % Is this even valid?
            [];
        Rel ->
            ?BAD_CONFIG("Invalid rel entry in reltool.config: ~p", [Rel])
    end,

    AppStr = string:join([lists:concat(["    ", A]) || A <- Apps], "\n"),
    ?TRACE("Known apps for the release:~n~s~n", [AppStr]),

    % Find any missing apps
    Missing = lists:flatmap(fun(App0) ->
        App = case App0 of
            _ when is_atom(App0) -> App0;
            _ when is_tuple(App0) -> element(1, App0)
        end,
        ?TRACE("Checking that reltool found: ~s", [App]),
        case reltool_server:get_app(Server, App) of
            {ok, _} -> [];
            _ -> [App]
        end
    end, Apps),

    % Bail if reltool is missing any applications
    if length(Missing) == 0 -> ok; true ->
        ?BAD_CONFIG("Missing reltool apps: ~p", [Missing])
    end.


ensure_empty(BuildDir) ->
    knit_log:debug("Ensuring the build directory is empty."),
    case filelib:is_dir(BuildDir) and knit_cfg:get(force) of
        true ->
            knit_log:info("Removing directory: ~s", [BuildDir]),
            knit_file:rm_rf(BuildDir);
        false ->
            ok
    end,
    case filelib:is_file(BuildDir) of
        true ->
            case file:list_dir(BuildDir) of
                {ok, []} ->
                    ok;
                {ok, _} ->
                    Fmt = "Target directory exists and is not empty:~n  ~s",
                    ?BAD_CONFIG(Fmt, [BuildDir]);
                {error, Reason} ->
                    Fmt = "Error accessing target directory: ~s",
                    ?IO_ERROR(Fmt, [file:format_error(Reason)])
            end;
        false ->
            case knit_file:ensure_dir_exists(BuildDir) of
                ok ->
                    ok;
                {error, Reason} ->
                    Fmt = "Error creating target directory ~s : ~s",
                    Args = [BuildDir, file:format_error(Reason)],
                    ?IO_ERROR(Fmt, Args)
            end
    end.


create_RELEASES(BuildDir) ->
    knit_log:debug("Generating RELEASES file."),

    RootDir = code:root_dir(),
    {BootName, BootVsn} = knit_vsn:boot_rel(),
    LibDir = filename:join(BuildDir,"lib"),
    RelDir = filename:join(BuildDir, "releases"),
    RelFile = filename:join([RelDir, BootVsn, BootName ++ ".rel"]),

    AppInfo = lists:flatmap(fun({App, Vsn}) ->
        AppDir = filename:join(LibDir, lists:concat([App, "-", Vsn])),
        case filelib:is_dir(AppDir) of
            true ->
                [{App, Vsn, LibDir}];
            false ->
                []
        end
    end, get_rel_apps(RelFile)),

    case release_handler:create_RELEASES(RootDir, RelDir, RelFile, AppInfo) of
        ok ->
            ok;
        {error, Reason} ->
            Fmt = "Failed to create the RELEASES file: ~w",
            ?IO_ERROR(Fmt, [Reason])
    end.


get_rel_apps(RelFile) ->
    Apps = case file:consult(RelFile) of
        {ok, [{release, _, _, Apps0}]} ->
            Apps0;
        {error, Reason} ->
            Fmt = "Failed to parse release file ~s : ~s",
            ?IO_ERROR(Fmt, [RelFile, file:format_error(Reason)])
    end,
    [{element(1, A), element(2, A)} || A <- Apps].
