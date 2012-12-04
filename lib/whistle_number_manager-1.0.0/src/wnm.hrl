-ifndef(WNM_HRL).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("include/wh_number_manager.hrl").

-define(WNM_CONFIG_CAT, <<"number_manager">>).

-define(APP_VERSION, <<"1.0.0">>).
-define(APP_NAME, <<"whistle_number_manager">>).

-record(number, {number :: wh_json:json_string()
                 ,number_db :: api_binary()
                 ,state = <<"discovery">> :: ne_binary()
                 ,current_state = <<"discovery">> :: ne_binary()
                 ,reserve_history = ordsets:new() :: ordsets:ordset(ne_binary())
                 ,assign_to :: api_binary()
                 ,assigned_to :: api_binary()
                 ,prev_assigned_to :: api_binary()
                 ,auth_by :: 'system' | api_binary()
                 ,module_name :: atom()
                 ,module_data = wh_json:new() :: wh_json:object()
                 ,features = sets:new() :: set()
                 ,current_features = sets:new() :: set()
                 ,number_doc = wh_json:new() :: wh_json:object()
                 ,current_number_doc = wh_json:new() :: wh_json:object()
                 ,phone_number_docs :: dict()
                 ,hard_delete = false :: boolean()
                 ,error_jobj = wh_json:new() :: wh_json:object()
                 ,activations = [] :: wh_json:objects()
                 ,services :: wh_services:services()
                 ,current_balance :: float() | integer()
                 ,billing_id :: api_binary()
                }).

-type wnm_number() :: #number{}.

-define(WNM_HRL, true).
-endif.
