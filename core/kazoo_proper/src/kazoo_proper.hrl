-ifndef(KAZOO_PROPER_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(FAILED_RESPONSE, <<"{}">>).

-define(DEBUG(Fmt)
       ,data:debug(pqc_log:log_info(), Fmt)
       ).

-define(DEBUG(Fmt, Args)
       ,data:debug(pqc_log:log_info(), Fmt, Args)
       ).

-define(INFO(Fmt)
       ,data:info(pqc_log:log_info(), Fmt)
       ).

-define(INFO(Fmt, Args)
       ,data:info(pqc_log:log_info(), Fmt, Args)
       ).

-define(ERROR(Fmt)
       ,data:error(pqc_log:log_info(), Fmt)
       ).

-define(ERROR(Fmt, Args)
       ,data:error(pqc_log:log_info(), Fmt, Args)
       ).


-define(KAZOO_PROPER_HRL, 'true').
-endif.
