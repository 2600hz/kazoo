-ifndef(WHISTLE_LOG_MACROS).

-define(LOG_SYSTEM_ID, "000000000000").

-define(LOG_CALLID_FUN(FunCallID),
        (fun(undefined) -> ?LOG_SYSTEM_ID; (LogCallID__) -> LogCallID__ end)(FunCallID)).

%% ?LOG("Debug Message Goes Here") -> "debug|callid|log|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG(DebugMsg__),
        logger:format_log(debug
                          ,"debug|~s|log|~p:~b (~w) " ++ DebugMsg__
                          ,[?LOG_CALLID_FUN(erlang:get(callid)), ?MODULE, ?LINE, self()]
                         )).

%% ?LOG(Type, Msg) or ?LOG(Msg, Args)
-define(LOG(TypeOrMsg__, MsgOrArgs__),
        logger:format_log(TypeOrMsg__
                          ,"~s|~s|log|~p:~b (~w) "
                          ,[?MODULE, ?LINE, self()]
                          ,MsgOrArgs__
                          ,?LOG_CALLID_FUN(erlang:get(callid))
                         )).

%% ?LOG(Type, Msg, Args) or ?LOG(CallID, Msg, Args)
-define(LOG(TypeOrCallID__, DebugMsg__, DebugData__),
        logger:format_log(TypeOrCallID__
                          ,"~s|~s|log|~p:~b (~w) " ++ DebugMsg__
                          ,[?MODULE, ?LINE, self() | DebugData__]
                          ,?LOG_CALLID_FUN(erlang:get(callid))
                         )).

%% ?LOG(Type, CallID, Msg, Args)
-define(LOG(Type__, CallID__, DebugMsg__, DebugData__),
        logger:format_log(Type
                          ,"~s|~s|log|~p:~b (~w) " ++ DebugMsg__
                          ,[Type__, CallID__, ?MODULE, ?LINE, self() | DebugData__]
                         )).


%% ?LOG_START("Debug Message Goes Here") -> "debug|callid|start|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_START(DebugMsg__),
        logger:format_log(debug
                          ,"debug|~s|start|~p:~b (~w) " ++ DebugMsg__
                          ,[?LOG_CALLID_FUN(erlang:get(callid)), ?MODULE, ?LINE, self()]
                         )).

%% ?LOG_START(Type, Msg) or ?LOG_START(Msg, Args)
-define(LOG_START(TypeOrMsg__, MsgOrArgs__),
        logger:format_log(TypeOrMsg__
                          ,"~s|~s|start|~p:~b (~w) "
                          ,[?MODULE, ?LINE, self()]
                          ,MsgOrArgs__
                          ,?LOG_CALLID_FUN(erlang:get(callid))
                         )).

%% ?LOG_START(Type, Msg, Args) or ?LOG_START(CallID, Msg, Args)
-define(LOG_START(TypeOrCallID__, DebugMsg__, DebugData__),
        logger:format_log(TypeOrCallID__
                          ,"~s|~s|start|~p:~b (~w) " ++ DebugMsg__
                          ,[?MODULE, ?LINE, self() | DebugData__]
                          ,?LOG_CALLID_FUN(erlang:get(callid))
                         )).

%% ?LOG_START(Type, CallID, Msg, Args)
-define(LOG_START(Type__, CallID__, DebugMsg__, DebugData__),
        logger:format_log(Type
                          ,"~s|~s|start|~p:~b (~w) " ++ DebugMsg__
                          ,[Type__, CallID__, ?MODULE, ?LINE, self() | DebugData__]
                         )).



%% ?LOG_END("Debug Message Goes Here") -> "debug|callid|end|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_END(DebugMsg__),
        logger:format_log(debug
                          ,"debug|~s|end|~p:~b (~w) " ++ DebugMsg__
                          ,[?LOG_CALLID_FUN(erlang:get(callid)), ?MODULE, ?LINE, self()]
                         )).

%% ?LOG_END(Type, Msg) or ?LOG_END(Msg, Args)
-define(LOG_END(TypeOrMsg__, MsgOrArgs__),
        logger:format_log(TypeOrMsg__
                          ,"~s|~s|end|~p:~b (~w) "
                          ,[?MODULE, ?LINE, self()]
                          ,MsgOrArgs__
                          ,?LOG_CALLID_FUN(erlang:get(callid))
                         )).

%% ?LOG_END(Type, Msg, Args) or ?LOG_END(CallID, Msg, Args)
-define(LOG_END(TypeOrCallID__, DebugMsg__, DebugData__),
        logger:format_log(TypeOrCallID__
                          ,"~s|~s|end|~p:~b (~w) " ++ DebugMsg__
                          ,[?MODULE, ?LINE, self() | DebugData__]
                          ,?LOG_CALLID_FUN(erlang:get(callid))
                         )).

%% ?LOG_END(Type, CallID, Msg, Args)
-define(LOG_END(Type__, CallID__, DebugMsg__, DebugData__),
        logger:format_log(Type
                          ,"~s|~s|end|~p:~b (~w) " ++ DebugMsg__
                          ,[Type__, CallID__, ?MODULE, ?LINE, self() | DebugData__]
                         )).




%% ?LOG_SYS("Debug Message Goes Here") -> "debug|callid|sys|module:## (<0.0.0>) Debug Message Goes Here"
-define(LOG_SYS(DebugMsg__),
        logger:format_log(debug
                          ,"debug|~s|sys|~p:~b (~w) " ++ DebugMsg__
                          ,[?LOG_CALLID_FUN(erlang:get(callid)), ?MODULE, ?LINE, self()]
                         )).

%% ?LOG_SYS(Type, Msg) or ?LOG_SYS(Msg, Args)
-define(LOG_SYS(TypeOrMsg__, MsgOrArgs__),
        logger:format_log(TypeOrMsg__
                          ,"~s|~s|sys|~p:~b (~w) "
                          ,[?MODULE, ?LINE, self()]
                          ,MsgOrArgs__
                          ,?LOG_CALLID_FUN(erlang:get(callid))
                         )).

%% ?LOG_SYS(Type, Msg, Args) or ?LOG_SYS(CallID, Msg, Args)
-define(LOG_SYS(TypeOrCallID__, DebugMsg__, DebugData__),
        logger:format_log(TypeOrCallID__
                          ,"~s|~s|sys|~p:~b (~w) " ++ DebugMsg__
                          ,[?MODULE, ?LINE, self() | DebugData__]
                          ,?LOG_CALLID_FUN(erlang:get(callid))
                         )).

%% ?LOG_SYS(Type, CallID, Msg, Args)
-define(LOG_SYS(Type__, CallID__, DebugMsg__, DebugData__),
        logger:format_log(Type
                          ,"~s|~s|sys|~p:~b (~w) " ++ DebugMsg__
                          ,[Type__, CallID__, ?MODULE, ?LINE, self() | DebugData__]
                         )).

%% ?LOG_STACKTRACE(ListOfStacktrace)
-define(LOG_STACKTRACE(StacktraceData__),
        (fun() ->
                 LogCallId = ?LOG_CALLID_FUN(erlang:get(callid)),
                 [logger:debug("debug|~s|start|~p:~b (~w) st: ~p", [LogCallId, ?MODULE, ?LINE, self(), STLine__]) || STLine__ <- StacktraceData__]
         end)()).

-define(WHISTLE_LOG_MACROS, true).
-endif.
