-ifndef(WHISTLE_LOG_MACROS).

-define(LOG_SYSTEM_ID, "000000000000").
-define(LOG_PUBLISH_LEVELS, [emergency, alert, critical, error, warning, notice, info]).

-define(LOG_DEFAULT_DATA, [erlang:get(callid), ?MODULE, ?LINE, self()]).


%% ?LOG("Debug Message Goes Here") -> "debug|callid|log|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG(DebugMsg__), wh_alert:format('log', ?LOG_DEFAULT_DATA, DebugMsg__)).

%% ?LOG(Type, Msg) or ?LOG(Msg, Args)
-define(LOG(TypeOrMsg__, MsgOrArgs__), wh_alert:format('log', ?LOG_DEFAULT_DATA, TypeOrMsg__, MsgOrArgs__)).

%% ?LOG(Type, Msg, Args) or ?LOG(CallID, Msg, Args)
-define(LOG(TypeOrCallID__, DebugMsg__, DebugData__), wh_alert:format('log', ?LOG_DEFAULT_DATA, TypeOrCallID__, DebugMsg__, DebugData__)).

%% ?LOG(Type, CallID, Msg, Args)
-define(LOG(Type__, CallID__, DebugMsg__, DebugData__), wh_alert:format('log', ?LOG_DEFAULT_DATA, Type__, CallID__, DebugMsg__, DebugData__)).



%% ?LOG_START("Debug Message Goes Here") -> "debug|callid|start|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_START(DebugMsg__), wh_alert:format('start', ?LOG_DEFAULT_DATA, DebugMsg__)).

%% ?LOG_START(Type, Msg) or ?LOG_START(Msg, Args)
-define(LOG_START(TypeOrMsg__, MsgOrArgs__), wh_alert:format('start', ?LOG_DEFAULT_DATA, TypeOrMsg__, MsgOrArgs__)).

%% ?LOG_START(Type, Msg, Args) or ?LOG_START(CallID, Msg, Args)
-define(LOG_START(TypeOrCallID__, DebugMsg__, DebugData__), wh_alert:format('start', ?LOG_DEFAULT_DATA, TypeOrCallID__, DebugMsg__, DebugData__)).

%% ?LOG_START(Type, CallID, Msg, Args)
-define(LOG_START(Type__, CallID__, DebugMsg__, DebugData__), wh_alert:format('start', ?LOG_DEFAULT_DATA, Type__, CallID__, DebugMsg__, DebugData__)).



%% ?LOG_END("Debug Message Goes Here") -> "debug|callid|end|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_END(DebugMsg__), wh_alert:format('end', ?LOG_DEFAULT_DATA, DebugMsg__)).

%% ?LOG_END(Type, Msg) or ?LOG_END(Msg, Args)
-define(LOG_END(TypeOrMsg__, MsgOrArgs__), wh_alert:format('end', ?LOG_DEFAULT_DATA, TypeOrMsg__, MsgOrArgs__)).

%% ?LOG_END(Type, Msg, Args) or ?LOG_END(CallID, Msg, Args)
-define(LOG_END(TypeOrCallID__, DebugMsg__, DebugData__), wh_alert:format('end', ?LOG_DEFAULT_DATA, TypeOrCallID__, DebugMsg__, DebugData__)).

%% ?LOG_END(Type, CallID, Msg, Args)
-define(LOG_END(Type__, CallID__, DebugMsg__, DebugData__), wh_alert:format('end', ?LOG_DEFAULT_DATA, Type__, CallID__, DebugMsg__, DebugData__)).



%% ?LOG_SYS("Debug Message Goes Here") -> "debug|callid|sys|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_SYS(DebugMsg__), wh_alert:format('sys', ?LOG_DEFAULT_DATA, DebugMsg__)).

%% ?LOG_SYS(Type, Msg) or ?LOG_SYS(Msg, Args)
-define(LOG_SYS(TypeOrMsg__, MsgOrArgs__), wh_alert:format('sys', ?LOG_DEFAULT_DATA, TypeOrMsg__, MsgOrArgs__)).

%% ?LOG_SYS(Type, Msg, Args) or ?LOG_SYS(CallID, Msg, Args)
-define(LOG_SYS(TypeOrCallID__, DebugMsg__, DebugData__), wh_alert:format('sys', ?LOG_DEFAULT_DATA, TypeOrCallID__, DebugMsg__, DebugData__)).

%% ?LOG_SYS(Type, CallID, Msg, Args)
-define(LOG_SYS(Type__, CallID__, DebugMsg__, DebugData__), wh_alert:format('sys', ?LOG_DEFAULT_DATA, Type__, CallID__, DebugMsg__, DebugData__)).


%% ?LOG_STACKTRACE(ListOfStacktrace)
-define(LOG_STACKTRACE(StacktraceData__), [wh_alert:format('log', ?LOG_DEFAULT_DATA, "st: ~p", [STLine__]) 
                                           || STLine__ <- StacktraceData__
                                          ]).

-define(WHISTLE_LOG_MACROS, true).
-endif.
