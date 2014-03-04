
-define(BAD_CLI(Msg, Args), knit_util:abort(1, Msg, Args)).
-define(BAD_CONFIG(Msg, Args), knit_util:abort(2, Msg, Args)).
-define(IO_ERROR(Msg, Args), knit_util:abort(3, Msg, Args)).
-define(NOT_SUPPORTED(Msg, Args), knit_util:abort(126, Msg, Args)).
-define(ABORT(Msg, Args), knit_util:abort(127, Msg, Args)).

-define(TRACE(Msg), knit_log:trace(Msg, ?FILE, ?LINE)).
-define(TRACE(Msg, Args), knit_log:trace(Msg, Args, ?FILE, ?LINE)).
