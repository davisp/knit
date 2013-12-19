-module(knit_cli).


-export([parse/1]).


parse(Args) ->
    case getopt:parse(opts(), Args) of
        {ok, {Opts, Commands}} ->
            case lists:member(help, Opts) of
                true -> usage();
                false -> ok
            end,
            case lists:member(version, Opts) of
                true -> version();
                false -> ok
            end,
            case Commands of
                [] ->
                    knit_log:error("You must specify at least one command."),
                    usage();
                _ ->
                    ok
            end,
            {ok, Opts, Commands};
        {error, {Reason, Data}} ->
            io:format(standard_error, "~p : ~p~n", [Reason, Data]),
            usage()
    end.


usage() ->
    CliTail = "[command ...]",
    OptsTail = [{"command", "Commands to execute."}],
    getopt:usage(opts(), "knit", CliTail, OptsTail),
    knit_util:abort(1).


version() ->
    % TODO: Make this grab real version info
    io:format(standard_error, "knit: 0.0.0~n", []),
    knit_util:abort(0).


opts() ->
    [
        opt_help(),
        opt_version(),
        opt_verbose(),
        opt_debug(),
        opt_quiet(),
        opt_log_file(),
        opt_reltool_config()
    ].


opt_help() ->
    {help, $h, "help", undefined, "Show this help message and exit."}.


opt_version() ->
    {version, $V, "version", undefined, "Show version information and exit."}.


opt_verbose() ->
    Msg = "Set verbose output. Can be repeated for increased verbosity.",
    {verbose, $v, "verbose", integer, Msg}.


opt_debug() ->
    {debug, $d, "debug", undefined, "Set maximum verbosity logging."}.


opt_quiet() ->
    {quiet, $q, "quiet", undefined, "Disable all output to the console."}.


opt_log_file() ->
    Msg = "Log all output to the specified file. Unaffected when using quiet.",
    {log_file, $l, "log_file", string, Msg}.


opt_reltool_config() ->
    {reltool, $r, "reltool_config", string, "Path to your reltool.config"}.
