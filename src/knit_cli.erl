-module(knit_cli).


-export([parse/1]).


parse(Args) ->
    case getopt:parse(opts(), Args) of
        {ok, {Opts, Vars}} ->
            case lists:member(help, Opts) of
                true -> usage();
                false -> ok
            end,
            case lists:member(version, Opts) of
                true -> version();
                false -> ok
            end,
            {ok, Opts, Vars};
        {error, {Reason, Data}} ->
            io:format(standard_error, "~p : ~p~n", [Reason, Data]),
            usage()
    end.


usage() ->
    CliTail = "[var ...]",
    OptsTail = [{"var", "Set variables using the name=value syntax"}],
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
    {debug, $d, "debug", undefined, "Set debug verbosity logging."}.


opt_quiet() ->
    {quiet, $q, "quiet", undefined, "Disable all output to the console."}.


opt_log_file() ->
    Msg = "Log all output to the specified file. Unaffected when using quiet.",
    {log_file, $l, "log_file", string, Msg}.


opt_reltool_config() ->
    {reltool, $r, "reltool_config", string, "Path to your reltool.config"}.
