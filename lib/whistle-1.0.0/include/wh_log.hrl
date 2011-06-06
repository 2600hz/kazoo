
-define(LOG_SYSTEM_ID, "000000000000").

-define(LOG_START(Format), 
        logger:format_log(info, "~s|start|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()])).

-define(LOG_START(Format, Data), 
        logger:format_log(info, "~s|start|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()] ++ Data)).

-define(LOG_START(CallId, Format, Data), 
        logger:format_log(info, "~s|start|~s:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).

-define(LOG(Format), 
        logger:format_log(info, "~s|log|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()])).

-define(LOG(Format, Data), 
        logger:format_log(info, "~s|log|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()] ++ Data)).

-define(LOG(CallId, Format, Data), 
        logger:format_log(info, "~s|log|~s:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).

-define(LOG_END(Format), 
        logger:format_log(info, "~s|end|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()])).

-define(LOG_END(Format, Data), 
        logger:format_log(info, "~s|end|~s:~b (~w) " ++ Format, [get(callid), ?MODULE, ?LINE, self()] ++ Data)).

-define(LOG_END(CallId, Format, Data), 
        logger:format_log(info, "~s|end|~s:~b (~w) " ++ Format, [CallId, ?MODULE, ?LINE, self()] ++ Data)).
