
-define(LOG_SYSTEM_ID, "000000000000").

-define(LOG_START(Format),
	(fun() ->
	    LogCallId = (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(get(callid)),
	    logger:info("~s|start|~p:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()])
	end)()).
-define(LOG_START(Format, Data),
	(fun() ->
	    LogCallId = (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(get(callid)),
	    logger:info("~s|start|~p:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()] ++ Data)
	end)()).
-define(LOG_START(CallId, Format, Data),
        logger:info("~s|start|~p:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).


-define(LOG(Format),
	(fun() ->
	    LogCallId = (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(get(callid)),
	    logger:info("~s|log|~p:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()])
	end)()).
-define(LOG(Format, Data),
	(fun() ->
	    LogCallId = (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(get(callid)),
	    logger:info("~s|log|~p:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()] ++ Data)
	end)()).
-define(LOG(CallId, Format, Data),
        logger:info("~s|log|~p:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).


-define(LOG_END(Format),
	(fun() ->
	    LogCallId = (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(get(callid)),
	    logger:info("~s|end|~p:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()])
	end)()).
-define(LOG_END(Format, Data),
	(fun() ->
	    LogCallId = (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(get(callid)),
	    logger:info("~s|end|~p:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()] ++ Data)
	end)()).
-define(LOG_END(CallId, Format, Data),
        logger:info("~s|end|~p:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).


-define(LOG_SYS(Format),
	(fun() ->
	    LogCallId = (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(get(callid)),
	    logger:info("~s|sys|~p:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()])
	end)()).
-define(LOG_SYS(Format, Data),
	(fun() ->
	    LogCallId = (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(get(callid)),
	    logger:info("~s|sys|~p:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()] ++ Data)
	end)()).
-define(LOG_SYS(CallId, Format, Data),
	logger:info("~s|sys|~p:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()])).
