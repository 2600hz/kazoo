-ifndef(WHISTLE_LOG_MACROS).

-define(LOG_SYSTEM_ID, "000000000000").

-define(LOG_CALLID_FUN(FunCallID),
        (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(FunCallID)).

%% ?LOG_START("Debug Message Goes Here") -> "callid|start|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_START(Format__For__Log__),
        (fun(DebugMsg__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogCallId, ?MODULE, ?LINE, self()])
         end)(Format__For__Log__)).

%% ?LOG_START(log_atom | "DebugMsg: ~p", "DebugMsg" | [Arg])
-define(LOG_START(Format__For__Log__, Data__For__Format__),
        (fun(LogLevel__, DebugMsg__) when is_atom(LogLevel__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:LogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogLevel__, LogCallId, ?MODULE, ?LINE, self()]);
             (DebugMsg__, DebugData__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogCallId, ?MODULE, ?LINE, self() | DebugData__])
         end)(Format__For__Log__, Data__For__Format__)).

%% ?LOG_START(log_atom | CallID, "DebugMsg: ~p", [Arg])
-define(LOG_START(CallId__, Format__For__Log__, Data__For__Format__),
        (fun(LogLevel__, DebugMsg__, DebugData__) when is_atom(LogLevel__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:LogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogLevel__, LogCallId, ?MODULE, ?LINE, self() | DebugData__]);
            (DebugCallID__, DebugMsg__, DebugData__) ->
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [DebugCallID__, ?MODULE, ?LINE, self() | DebugData__])
         end)(CallId__, Format__For__Log__, Data__For__Format__)).

%% ?LOG_START(log_atom, CallID, "DebugMsg: ~p", [Arg])
-define(LOG_START(LogLevel__, CallId__, Format__For__Log__, Data__For__Format__),
        (fun(DebugLogLevel__, DebugCallID__, DebugMsg__, DebugData__) when is_atom(DebugLogLevel__) ->
                 logger:DebugLogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [DebugLogLevel__, LogCallId, ?MODULE, ?LINE, self() | DebugData__]);
         end)(LogLevel__, CallId__, Format__For__Log__, Data__For__Format__)).

%% ?LOG_STACKTRACE(ListOfStacktrace)
-define(LOG_STACKTRACE(StacktraceData__),
        (fun(DebugST__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 [logger:debug("debug|~s|start|~p:~b (~w) st: ~p", [LogCallId, ?MODULE, ?LINE, self(), STLine__]) || STLine__ <- DebugST__]
         end)(StacktraceData__)).

%% ?LOG("Debug Message Goes Here") -> "callid|start|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG(Format__For__Log__),
        (fun(DebugMsg__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogCallId, ?MODULE, ?LINE, self()])
         end)(Format__For__Log__)).

%% ?LOG(log_atom | "DebugMsg: ~p", "DebugMsg" | [Arg])
-define(LOG(Format__For__Log__, Data__For__Format__),
        (fun(LogLevel__, DebugMsg__) when is_atom(LogLevel__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:LogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogLevel__, LogCallId, ?MODULE, ?LINE, self()]);
             (DebugMsg__, DebugData__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogCallId, ?MODULE, ?LINE, self() | DebugData__])
         end)(Format__For__Log__, Data__For__Format__)).

%% ?LOG(log_atom | CallID, "DebugMsg: ~p", [Arg])
-define(LOG(CallId__, Format__For__Log__, Data__For__Format__),
        (fun(LogLevel__, DebugMsg__, DebugData__) when is_atom(LogLevel__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:LogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogLevel__, LogCallId, ?MODULE, ?LINE, self() | DebugData__]);
            (DebugCallID__, DebugMsg__, DebugData__) ->
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [DebugCallID__, ?MODULE, ?LINE, self() | DebugData__])
         end)(CallId__, Format__For__Log__, Data__For__Format__)).

%% ?LOG(log_atom, CallID, "DebugMsg: ~p", [Arg])
-define(LOG(LogLevel__, CallId__, Format__For__Log__, Data__For__Format__),
        (fun(DebugLogLevel__, DebugCallID__, DebugMsg__, DebugData__) when is_atom(DebugLogLevel__) ->
                 logger:DebugLogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [DebugLogLevel__, LogCallId, ?MODULE, ?LINE, self() | DebugData__]);
         end)(LogLevel__, CallId__, Format__For__Log__, Data__For__Format__)).

%% ?LOG_END("Debug Message Goes Here") -> "callid|start|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_END(Format__For__Log__),
        (fun(DebugMsg__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogCallId, ?MODULE, ?LINE, self()])
         end)(Format__For__Log__)).

%% ?LOG_END(log_atom | "DebugMsg: ~p", "DebugMsg" | [Arg])
-define(LOG_END(Format__For__Log__, Data__For__Format__),
        (fun(LogLevel__, DebugMsg__) when is_atom(LogLevel__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:LogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogLevel__, LogCallId, ?MODULE, ?LINE, self()]);
             (DebugMsg__, DebugData__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogCallId, ?MODULE, ?LINE, self() | DebugData__])
         end)(Format__For__Log__, Data__For__Format__)).

%% ?LOG_END(log_atom | CallID, "DebugMsg: ~p", [Arg])
-define(LOG_END(CallId__, Format__For__Log__, Data__For__Format__),
        (fun(LogLevel__, DebugMsg__, DebugData__) when is_atom(LogLevel__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:LogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogLevel__, LogCallId, ?MODULE, ?LINE, self() | DebugData__]);
            (DebugCallID__, DebugMsg__, DebugData__) ->
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [DebugCallID__, ?MODULE, ?LINE, self() | DebugData__])
         end)(CallId__, Format__For__Log__, Data__For__Format__)).

%% ?LOG_END(log_atom, CallID, "DebugMsg: ~p", [Arg])
-define(LOG_END(LogLevel__, CallId__, Format__For__Log__, Data__For__Format__),
        (fun(DebugLogLevel__, DebugCallID__, DebugMsg__, DebugData__) when is_atom(DebugLogLevel__) ->
                 logger:DebugLogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [DebugLogLevel__, LogCallId, ?MODULE, ?LINE, self() | DebugData__]);
         end)(LogLevel__, CallId__, Format__For__Log__, Data__For__Format__)).

%% ?LOG_SYS("Debug Message Goes Here") -> "callid|start|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_SYS(Format__For__Log__),
        (fun(DebugMsg__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogCallId, ?MODULE, ?LINE, self()])
         end)(Format__For__Log__)).

%% ?LOG_SYS(log_atom | "DebugMsg: ~p", "DebugMsg" | [Arg])
-define(LOG_SYS(Format__For__Log__, Data__For__Format__),
        (fun(LogLevel__, DebugMsg__) when is_atom(LogLevel__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:LogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogLevel__, LogCallId, ?MODULE, ?LINE, self()]);
             (DebugMsg__, DebugData__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogCallId, ?MODULE, ?LINE, self() | DebugData__])
         end)(Format__For__Log__, Data__For__Format__)).

%% ?LOG_SYS(log_atom | CallID, "DebugMsg: ~p", [Arg])
-define(LOG_SYS(CallId__, Format__For__Log__, Data__For__Format__),
        (fun(LogLevel__, DebugMsg__, DebugData__) when is_atom(LogLevel__) ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 logger:LogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [LogLevel__, LogCallId, ?MODULE, ?LINE, self() | DebugData__]);
            (DebugCallID__, DebugMsg__, DebugData__) ->
                 logger:debug("debug|~s|start|~p:~b (~w) " ++ DebugMsg__, [DebugCallID__, ?MODULE, ?LINE, self() | DebugData__])
         end)(CallId__, Format__For__Log__, Data__For__Format__)).

%% ?LOG_SYS(log_atom, CallID, "DebugMsg: ~p", [Arg])
-define(LOG_SYS(LogLevel__, CallId__, Format__For__Log__, Data__For__Format__),
        (fun(DebugLogLevel__, DebugCallID__, DebugMsg__, DebugData__) when is_atom(DebugLogLevel__) ->
                 logger:DebugLogLevel__("~s|~s|start|~p:~b (~w) " ++ DebugMsg__, [DebugLogLevel__, LogCallId, ?MODULE, ?LINE, self() | DebugData__]);
         end)(LogLevel__, CallId__, Format__For__Log__, Data__For__Format__)).

-define(WHISTLE_LOG_MACROS, true).
-endif.
