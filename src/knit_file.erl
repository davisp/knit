-module(knit_file).


-export([
    cwd/0,
    do_in_dir/2,
    cp_r/2,
    mv/2,
    rm_rf/1,
    copy/2,
    copy_file_info/2,
    ensure_dir_exists/1
]).

-export([
    consult/1
]).


-include("knit.hrl").


cwd() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.


do_in_dir(Dir, Fun) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            erlang:error({invalid_directory, Dir})
    end,
    Cwd = knit_file:cwd(),
    try
        case file:set_cwd(Dir) of
            ok ->
                ok;
            {error, Error} ->
                erlang:error(file:format_error(Error))
        end,
        Fun()
    after
        file:set_cwd(Cwd)
    end.


cp_r(Srcs, Dst) ->
    os_invoke(cp_r, [Srcs, Dst]).


mv(Src, Dst) ->
    os_invoke(mv, [Src, Dst]).


rm_rf(Path) ->
    os_invoke(rm_rf, [Path]).


copy(Src, Dst) ->
    case file:copy(Src, Dst) of
        {ok, _} ->
            ok;
        {error, Error} ->
            Reason = file:format_error(Error),
            ?IO_ERROR("Error copynig ~s -> ~s: ~s", [Src, Dst, Reason])
    end.


copy_file_info(Src, Dst) ->
    {ok, FileInfo} = file:read_file_info(Src),
    ok = file:write_file_info(Dst, FileInfo).


ensure_dir_exists(DirName) ->
    Dummy = filename:join(DirName, "dummy"),
    filelib:ensure_dir(Dummy).


consult(File) ->
    case filename:extension(File) of
        ".script" ->
            NonScript = filename:basename(File, ".script"),
            consult_and_eval(NonScript, File);
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    consult_and_eval(File, Script);
                false ->
                    try_consult(File)
            end
    end.


os_invoke(Fun, Args) ->
    Mod = case os:type() of
        {unix, _} -> knit_file_unix;
        {win32, _} -> knit_file_win32
    end,
    erlang:apply(Mod, Fun, Args).


consult_and_eval(File, Script) ->
    knit_log:debug("Evaluating config script ~p~n", [Script]),
    ConfigData = try_consult(File),
    file:script(Script, bindings([
        {'CONFIG', ConfigData},
        {'SCRIPT', Script}
    ])).


try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            knit_log:debug("Consulting config file: ~s", [File]),
            Terms;
        {error, enoent} ->
            [];
        {error, Reason} ->
            Fmt = "Failed to consult file ~s: ~s",
            ?BAD_CONFIG(Fmt, [File, file:format_error(Reason)])
    end.


bindings(KVList) ->
    lists:foldl(fun({K,V}, Bindings) ->
        erl_eval:add_binding(K, V, Bindings)
    end, erl_eval:new_bindings(), KVList).
