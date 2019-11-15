-ifndef(CCCP_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_numbers/include/knm_phone_number.hrl").

-define(APP_NAME, <<"cccp">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(CCCP_CONFIG_CAT, <<"cccp">>).

-record(state, {a_leg_name :: kz_term:api_ne_binary()
               ,a_leg_number :: kz_term:api_ne_binary()
               ,b_leg_number :: kz_term:api_ne_binary()
               ,call = kapps_call:new() :: kapps_call:call()
               ,account_id :: kz_term:api_ne_binary()
               ,authorizing_id :: kz_term:api_ne_binary()
               ,queue :: kz_term:api_binary()
               ,parked_call_id :: kz_term:api_ne_binary()
               ,offnet_ctl_q :: kz_term:api_ne_binary()
               ,auth_doc_id :: kz_term:api_ne_binary()
               ,media_id :: kz_term:api_ne_binary()
               ,retain_cid :: kz_term:api_ne_binary()
               ,self = self() :: pid()
               ,consumer_pid :: kz_term:api_pid()
               ,callback_delay :: kz_term:api_integer()
               }).

-type state() :: #state{}.

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).

-define(CCCP_HRL, 'true').
-endif.
