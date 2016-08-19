-ifndef(CCCP_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(APP_NAME, <<"cccp">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(CCCP_CONFIG_CAT, <<"cccp">>).

-record(state, {a_leg_name :: ne_binary()
               ,a_leg_number :: ne_binary()
               ,b_leg_number :: ne_binary()
               ,call = kapps_call:new() :: kapps_call:call()
               ,account_id :: ne_binary()
               ,authorizing_id :: ne_binary()
               ,queue :: api_binary()
               ,parked_call_id :: ne_binary()
               ,offnet_ctl_q :: ne_binary()
               ,auth_doc_id :: ne_binary()
               ,media_id :: ne_binary()
               ,retain_cid :: ne_binary()
               ,self = self() :: pid()
               ,consumer_pid :: pid()
               ,callback_delay :: integer()
               }).

-type state() :: #state{}.

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).

-define(CCCP_HRL, 'true').
-endif.
