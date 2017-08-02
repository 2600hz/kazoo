-ifndef(EDR_HRL).
-define(EDR_HRL, 'true').

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"edr">>).
-define(APP_VERSION, <<"4.0.0">>).
-record(backend, {name         :: ne_binary()
                 ,type         :: ne_binary()
                 ,enabled      :: boolean()
                 ,options      :: kz_json:object()
                 }).

-type backend() :: #backend{}.
-type work_result() :: 'ok' | {'error', Info :: any()} | {'exit', Reason ::any()}.
-type init_ret(S) :: {'ok', S} |
                     {'ok', S, timeout() | 'hibernate'} |
                     {'stop', any()} |
                     'ignore'.

-define(EDR_LEVELS, ['fatal', 'error', 'warn', 'info', 'debug', 'trace']).
-type edr_level() :: 'fatal' | 'error' | 'warn' | 'info' | 'debug' | 'trace'.

-record(event, {account_id     :: api_binary()
               ,account_tree   :: api_ne_binaries()
               ,app_name       :: ne_binary()
               ,app_version    :: ne_binary()
               ,level          :: edr_level()
               ,body           :: ne_binary()
               ,timestamp      :: ne_binary()
               ,gregorian_time :: pos_integer()
               }).
-type event() :: #event{}.
-endif.
