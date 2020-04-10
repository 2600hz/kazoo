-ifndef(KAZOO_PROPER_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"kazoo_proper">>).
-define(APP_VERSION, <<"4.3">>).

-define(FAILED_RESPONSE, <<"{}">>).

-define(DEBUG(Fmt)
       ,_ = data:debug(pqc_log:log_info(), Fmt)
       ).

-define(DEBUG(Fmt, Args)
       ,_ = data:debug(pqc_log:log_info(), Fmt, Args)
       ).

-define(INFO(Fmt)
       ,_ = data:info(pqc_log:log_info(), Fmt)
       ).

-define(INFO(Fmt, Args)
       ,_ = data:info(pqc_log:log_info(), Fmt, Args)
       ).

-define(ERROR(Fmt)
       ,_ = data:error(pqc_log:log_info(), Fmt)
       ).

-define(ERROR(Fmt, Args)
       ,_ = data:error(pqc_log:log_info(), Fmt, Args)
       ).

-define(KAZOO_PROPER_HRL, 'true').
-endif.
