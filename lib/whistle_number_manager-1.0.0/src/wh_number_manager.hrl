-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-define(WNM_CONFIG_CAT, <<"number_manager">>).

-define(APP_VERSION, <<"1.0.0">>).
-define(APP_NAME, <<"whistle_number_manager">>).

-type wnm_failures() :: invalid_state_transition |  
                        unauthorized |
                        number_exists |
                        no_change_required |
                        not_reconcilable |
                        database_error |
                        unknown_carrier |
                        service_restriction |
                        provider_fault |
                        carrier_fault.

-record(number, {number = 'undefined' :: 'undefined' | ne_binary()
                 ,number_db = 'undefined' :: 'undefined' | ne_binary()
                 ,state = <<"discovery">> :: ne_binary()
                 ,reserve_history = ordsets:new() :: ordsets:ordset()
                 ,assigned_to = 'undefined' :: 'undefined' | ne_binary()
                 ,prev_assigned_to = 'undefined' :: 'undefined' | ne_binary()
                 ,module_name = 'undefined' :: 'undefined' | atom()
                 ,module_data = wh_json:new() :: wh_json:json_object()
                 ,features = sets:new() :: set()
                 ,resellers 
                 ,error_jobj = wh_json:new() :: wh_json:json_object()
                 ,error = 'undefined' :: atom()
                 ,assign_to = 'undefined' :: 'undefined' | ne_binary()
                 ,auth_by = 'undefined' :: 'undefined' | 'system' | ne_binary()
                 ,number_doc = wh_json:new() :: wh_json:json_object()
                 ,current_number_doc = wh_json:new() :: wh_json:json_object()
                 ,phone_number_docs = 'undefined' :: 'undefined' | dict()
                 ,hard_delete = false :: boolean()
                }).

-type wnm_number() :: #number{}.
