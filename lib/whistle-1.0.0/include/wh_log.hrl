-define(LOG_SYSTEM_ID, "000000000000").

-define(LOG_CALLID_FUN(FunCallID),
	(fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(FunCallID)).

-define(LOG_START(Format__For__Log__),
	(fun() ->
		 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
		 logger:info("~s|start|~p:~b (~w) " ++ Format__For__Log__, [LogCallId, ?MODULE, ?LINE, self()])
	 end)()).
-define(LOG_START(Format__For__Log__, Data__For__Format__),
	(fun() ->
		 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
		 logger:info("~s|start|~p:~b (~w) " ++ Format__For__Log__, [LogCallId, ?MODULE, ?LINE, self() | Data__For__Format__])
	 end)()).
-define(LOG_START(CallId, Format__For__Log__, Data__For__Format__),
        logger:info("~s|start|~p:~b (~w) " ++ Format__For__Log__, [CallId, ?MODULE, ?LINE, self() | Data__For__Format__])).


-define(LOG(Format__For__Log__),
	(fun() ->
		 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
		 logger:info("~s|log|~p:~b (~w) " ++ Format__For__Log__, [LogCallId, ?MODULE, ?LINE, self()])
	 end)()).
-define(LOG(Format__For__Log__, Data__For__Format__),
	(fun() ->
		 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
		 logger:info("~s|log|~p:~b (~w) " ++ Format__For__Log__, [LogCallId, ?MODULE, ?LINE, self() | Data__For__Format__])
	 end)()).
-define(LOG(CallId, Format__For__Log__, Data__For__Format__),
        logger:info("~s|log|~p:~b (~w) " ++ Format__For__Log__, [CallId, ?MODULE, ?LINE, self() | Data__For__Format__])).


-define(LOG_END(Format__For__Log__),
	(fun() ->
		 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
		 logger:info("~s|end|~p:~b (~w) " ++ Format__For__Log__, [LogCallId, ?MODULE, ?LINE, self()])
	 end)()).
-define(LOG_END(Format__For__Log__, Data__For__Format__),
	(fun() ->
		 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
		 logger:info("~s|end|~p:~b (~w) " ++ Format__For__Log__, [LogCallId, ?MODULE, ?LINE, self() | Data__For__Format__])
	 end)()).
-define(LOG_END(CallId, Format__For__Log__, Data__For__Format__),
        logger:info("~s|end|~p:~b (~w) " ++ Format__For__Log__, [CallId, ?MODULE, ?LINE, self() | Data__For__Format__])).


-define(LOG_SYS(Format__For__Log__),
	(fun() ->
		 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
		 logger:info("~s|sys|~p:~b (~w) " ++ Format__For__Log__, [LogCallId, ?MODULE, ?LINE, self()])
	end)()).
-define(LOG_SYS(Format__For__Log__, Data__For__Format__),
	(fun() ->
		 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
		 logger:info("~s|sys|~p:~b (~w) " ++ Format__For__Log__, [LogCallId, ?MODULE, ?LINE, self() | Data__For__Format__])
	end)()).
-define(LOG_SYS(CallId, Format__For__Log__, Data__For__Format__),
	logger:info("~s|sys|~p:~b (~w) " ++ Format__For__Log__, [CallId, ?MODULE, ?LINE, self() | Data__For__Format__])).
