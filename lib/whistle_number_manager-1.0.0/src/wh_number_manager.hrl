-ifndef(WH_NUMBER_MANAGER_HRL).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

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
                 ,module_data = wh_json:new() :: wh_json:json_object()
                 ,features = sets:new() :: set()
                 ,current_features = sets:new() :: set()
                 ,number_doc = wh_json:new() :: wh_json:json_object()
                 ,current_number_doc = wh_json:new() :: wh_json:json_object()
                 ,phone_number_docs :: dict()
                 ,hard_delete = false :: boolean()
                 ,error_jobj = wh_json:new() :: wh_json:json_object()
                 ,activations = [] :: wh_json:json_objects()
                 ,services :: wh_services:services()
                 ,current_balance :: float() | integer()
                 ,billing_id :: api_binary()
                }).

-type wnm_number() :: #number{}.

-define(WH_NUMBER_MANAGER_HRL, true).
-endif.
