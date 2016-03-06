-ifndef(CCCP_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("whistle_apps/src/whapps_call_command_types.hrl").

-define(APP_NAME, <<"cccp">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(CCCP_CONFIG_CAT, <<"cccp">>).

-define(TIMEOUT, <<"timeout">>).
-define(DEFAULT_TIMEOUT, 15).

-record(state, {customer_number :: ne_binary()
                ,b_leg_number :: ne_binary()
                ,call = whapps_call:new() :: whapps_call:call()
                ,account_id :: ne_binary()
                ,account_cid :: ne_binary()
                ,queue :: api_binary()
                ,parked_call_id :: ne_binary()
                ,offnet_ctl_q :: ne_binary()
                ,auth_doc_id :: ne_binary()
                ,self = self() :: pid()
                ,consumer_pid :: pid()
                ,callback_delay :: integer()
               }).

-type state() :: #state{}.

-define(CCCP_HRL, 'true').
-endif.
