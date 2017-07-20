-ifndef(KAZOO_TEMPLATE_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-ifdef(TEST).
-define(LOG_ERROR(F,A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_WARN(F,A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_INFO(F,A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_DEBUG(F,A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_DEBUG(F), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE])).
-else.
-define(LOG_ERROR(F,A), lager:error(F,A)).
-define(LOG_WARN(F,A), lager:warning(F,A)).
-define(LOG_INFO(F,A), lager:info(F,A)).
-define(LOG_DEBUG(F,A), lager:debug(F,A)).
-define(LOG_DEBUG(F), lager:debug(F)).
-endif.

-define(KAZOO_TEMPLATE_HRL, 'true').
-endif.
