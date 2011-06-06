
-define(LOG_SYSTEM_ID, "000000000000").

-define(LOG_START(Format), 
        logger:info("~s|start|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()])).
-define(LOG_START(Format, Data), 
        logger:info("~s|start|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()] ++ Data)).
-define(LOG_START(CallId, Format, Data), 
        logger:info("~s|start|~s:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).


-define(LOG(Format), 
        logger:info("~s|log|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()])).
-define(LOG(Format, Data), 
        logger:info("~s|log|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()] ++ Data)).
-define(LOG(CallId, Format, Data), 
        logger:info("~s|log|~s:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).


-define(LOG_END(Format), 
        logger:info("~s|end|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()])).
-define(LOG_END(Format, Data), 
        logger:info("~s|end|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()] ++ Data)).
-define(LOG_END(CallId, Format, Data), 
        logger:info("~s|end|~s:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).


-define(LOG_SYS(Format),
	begin
	    LogCallId = case get(callid) of
			 undefined -> ?LOG_SYSTEM_ID;
			 LogC_ -> LogC_
		     end,
	logger:info("~s|sys|~s:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()])
	end).
-define(LOG_SYS(Format, Data),
	begin
	    LogCallId = case get(callid) of
			 undefined -> ?LOG_SYSTEM_ID;
			 LogC__ -> LogC__
		     end,
	logger:info("~s|sys|~s:~b (~w) " ++ Format, [LogCallId, ?MODULE, ?LINE, self()] ++ Data)
	end).
-define(LOG_SYS(CallId, Format, Data),
	logger:info("~s|sys|~s:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()])).
