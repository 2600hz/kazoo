-ifndef(CCCP_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("kazoo_apps/src/kapps_call_command_types.hrl").

-define(APP_NAME, <<"cccp">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(CCCP_CONFIG_CAT, <<"cccp">>).

-define(TIMEOUT, <<"timeout">>).
-define(DEFAULT_TIMEOUT, 15).

-record(state, {customer_number :: ne_binary()
                ,b_leg_number :: ne_binary()
                ,call = kapps_call:new() :: kapps_call:call()
                ,account_id :: ne_binary()
                ,account_cid :: ne_binary()
                ,queue :: maybe(binary())
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
