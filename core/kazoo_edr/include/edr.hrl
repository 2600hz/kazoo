-ifndef(EDR_HRL).
-define(EDR_HRL, 'true').

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% EDR
-define(EDR_VERBOSITY_LEVELS, ['trace', 'debug', 'info', 'warn', 'error', 'fatal']).
-type edr_verbosity() :: 'trace' | 'debug' | 'info' | 'warn' | 'error' | 'fatal'.

-define(EDR_SEVERITY_LEVELS, ['ok', 'warning', 'critical']).
-type edr_severity() :: 'ok' | 'warning' | 'critical'.

-record(edr_event, {account_id     :: api_ne_binary()
                   ,account_tree   :: api_ne_binaries()
                   ,app_name       :: ne_binary()
                   ,app_version    :: ne_binary()
                   ,body           :: ne_binary()
                   ,id             :: ne_binary()
                   ,node           :: ne_binary()
                   ,severity       :: edr_severity()
                   ,timestamp      :: ne_binary()
                   ,gregorian_time :: pos_integer()
                   ,verbosity      :: edr_verbosity()
                   }).
-type edr_event() :: #edr_event{}.

-record(edr_binding, {account_id          = <<"*">> :: api_ne_binary() | api_ne_binaries()
                     ,include_descendants = 'false' :: boolean()
                     ,app_name            = <<"*">> :: ne_binary() | ne_binaries()
                     ,severity            = 'ok'    :: edr_severity() | [edr_severity()]
                     ,exact_severity      = 'false' :: boolean()
                     ,verbosity           = 'info'  :: edr_verbosity() | [edr_verbosity()]
                     ,exact_verbosity     = 'false' :: boolean()
                     }).
-type edr_binding() :: #edr_binding{}.
-type edr_bindings() :: [edr_binding()].

-endif.
