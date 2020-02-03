-ifndef(KZ_LOG_HRL).

-define(KEY_LOG_ID, <<"System-Log-ID">>).
-define(DEFAULT_LOG_SYSTEM_ID, <<"00000000000">>).

%% From https://github.com/tomas-abrahamsson/gpb/issues/134#issuecomment-386892877
%% Usage:
%% try
%%     ...
%% catch
%%     error:badarg ->
%%         whatever;
%%     ?STACKTRACE(E, R, Stack)
%%         {error, {E,R, Stack}}
%% end
%% kz.mk defines the macro if OTP version is >= 21
-ifdef(OTP_RELEASE).
%% >= OTP 21
-define(STACKTRACE(Type, Reason, Stacktrace), Type:Reason:Stacktrace ->).
-else.
%% =< OTP 20
-define(STACKTRACE(Type, Reason, Stacktrace), Type:Reason -> Stacktrace = erlang:get_stacktrace(), ).
-endif.

-ifdef(TEST).

-define(LOG_ALERT(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_CRITICAL(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_DEBUG(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_EMERGENCY(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_ERROR(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_INFO(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_NOTICE(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_WARNING(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_DEV(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).

-define(LOG_ALERT(F), ?LOG_ALERT(F, [])).
-define(LOG_CRITICAL(F), ?LOG_CRITICAL(F, [])).
-define(LOG_DEBUG(F), ?LOG_DEBUG(F, [])).
-define(LOG_EMERGENCY(F), ?LOG_EMERGENCY(F, [])).
-define(LOG_ERROR(F), ?LOG_ERROR(F, [])).
-define(LOG_INFO(F), ?LOG_INFO(F, [])).
-define(LOG_NOTICE(F), ?LOG_NOTICE(F, [])).
-define(LOG_WARNING(F), ?LOG_WARNING(F, [])).
-define(LOG_DEV(F), ?LOG_DEV(F, [])).

-else.

-define(LOG_ALERT(F, A), lager:alert(F, A)).
-define(LOG_CRITICAL(F, A), lager:critical(F, A)).
-define(LOG_DEBUG(F, A), lager:debug(F, A)).
-define(LOG_EMERGENCY(F, A), lager:emergency(F, A)).
-define(LOG_ERROR(F, A), lager:error(F, A)).
-define(LOG_INFO(F, A), lager:info(F, A)).
-define(LOG_NOTICE(F, A), lager:notice(F, A)).
-define(LOG_WARNING(F, A), lager:warning(F, A)).
-define(LOG_DEV(F, A), dev:debug(F, A)).

-define(LOG_ALERT(F), ?LOG_ALERT(F, [])).
-define(LOG_CRITICAL(F), ?LOG_CRITICAL(F, [])).
-define(LOG_DEBUG(F), ?LOG_DEBUG(F, [])).
-define(LOG_EMERGENCY(F), ?LOG_EMERGENCY(F, [])).
-define(LOG_ERROR(F), ?LOG_ERROR(F, [])).
-define(LOG_INFO(F), ?LOG_INFO(F, [])).
-define(LOG_NOTICE(F), ?LOG_NOTICE(F, [])).
-define(LOG_WARNING(F), ?LOG_WARNING(F, [])).
-define(LOG_DEV(F), ?LOG_DEV(F, [])).

-endif.

-define(SUP_LOG_DEBUG(F, A),
        begin
            lager:debug(F, A),
            io:format(F ++ "\n", A)
        end
       ).
-define(SUP_LOG_INFO(F, A),
        begin
            lager:info(F, A),
            io:format(F ++ "\n", A)
        end
       ).
-define(SUP_LOG_WARNING(F, A),
        begin
            lager:warning(F, A),
            io:format(F ++ "\n", A)
        end
       ).
-define(SUP_LOG_ERROR(F, A),
        begin
            lager:error(F, A),
            io:format(F ++ "\n", A)
        end
       ).

-define(SUP_LOG_DEBUG(F), ?SUP_LOG_DEBUG(F, [])).
-define(SUP_LOG_INFO(F), ?SUP_LOG_INFO(F, [])).
-define(SUP_LOG_WARNING(F), ?SUP_LOG_WARNING(F, [])).
-define(SUP_LOG_ERROR(F), ?SUP_LOG_ERROR(F, [])).

-define(DEV_LOG(F, A), io:format('user', "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(DEV_LOG(F), ?DEV_LOG(F, [])).

-define(KZ_LOG_HRL, 'true').
-endif.
