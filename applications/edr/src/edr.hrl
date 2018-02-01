-ifndef(EDR_APP_HRL).
-define(EDR_APP_HRL, 'true').

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_edr/include/edr.hrl").

-define(APP_NAME, <<"edr">>).
-define(APP_VERSION, <<"4.0.0">>).

-record(backend, {name         :: kz_term:ne_binary()
                 ,type         :: kz_term:ne_binary()
                 ,enabled      :: boolean()
                 ,options      :: kz_json:object()
                 ,bindings     :: edr_bindings()
                 }).

-type backend() :: #backend{}.
-type work_result() :: 'ok' | {'error', Info :: any()} | {'exit', Reason ::any()}.
-type init_ret(S) :: {'ok', S} |
                     {'ok', S, timeout() | 'hibernate'} |
                     {'stop', any()} |
                     'ignore'.
-endif.
