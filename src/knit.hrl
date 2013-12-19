
-define(TRACE(Msg), knit_log:trace(Msg, ?FILE, ?LINE)).
-define(TRACE(Msg, Args), knit_log:trace(Msg, Args, ?FILE, ?LINE)).
