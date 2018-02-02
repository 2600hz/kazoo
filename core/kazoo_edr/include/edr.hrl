-ifndef(EDR_HRL).
-define(EDR_HRL, 'true').

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% EDR
-define(EDR_VERBOSITY_LEVELS, ['trace', 'debug', 'info', 'warn', 'error', 'fatal']).
-define(EDR_VERBOSITY_BINARIES, [<<"trace">>, <<"debug">>, <<"info">>, <<"warn">>, <<"error">>, <<"fatal">>]).
-type edr_verbosity() :: 'trace' | 'debug' | 'info' | 'warn' | 'error' | 'fatal'.
-type edr_verbosities() :: [edr_verbosity()].

-define(EDR_SEVERITY_LEVELS, ['ok', 'warning', 'critical']).
-define(EDR_SEVERITY_BINARIES, [<<"ok">>, <<"warning">>, <<"critical">>]).
-type edr_severity() :: 'ok' | 'warning' | 'critical'.
-type edr_severities() :: [edr_severity()].

-record(edr_event, {account_id     :: kz_term:api_ne_binary()
                   ,account_tree   :: kz_term:api_ne_binaries()
                   ,app_name       :: kz_term:ne_binary()
                   ,app_version    :: kz_term:ne_binary()
                   ,body           :: kz_json:object()
                   ,id             :: kz_term:ne_binary()
                   ,node           :: kz_term:ne_binary()
                   ,severity       :: edr_severity()
                   ,timestamp      :: kz_term:ne_binary()
                   ,gregorian_time :: pos_integer()
                   ,verbosity      :: edr_verbosity()
                   }).
-type edr_event() :: #edr_event{}.

-record(edr_binding, {account_id          = <<"*">> :: kz_term:ne_binary() | kz_term:ne_binaries()
                     ,include_descendants = 'false' :: boolean()
                     ,app_name            = <<"*">> :: kz_term:ne_binary() | kz_term:ne_binaries()
                     ,severity            = 'ok'    :: edr_severity() | edr_severities()
                     ,exact_severity      = 'false' :: boolean()
                     ,verbosity           = 'info'  :: edr_verbosity() | edr_verbosities()
                     ,exact_verbosity     = 'false' :: boolean()
                     }).
-type edr_binding() :: #edr_binding{}.
-type edr_bindings() :: [edr_binding()].

-endif.
