%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kapps_account_config_test).

-include_lib("eunit/include/eunit.hrl").
-include("kazoo_config.hrl").

-define(JOBJ_VALUE_FROM_SYSTEM, kapps_config_util:fixture("object_value_from_system")).

get_with_strategy_test_() ->
    [strategy_no_account_id()
    %% ,get_startegy_global_from_account()
    ].

strategy_no_account_id() ->
    Value = ?JOBJ_VALUE_FROM_SYSTEM,
    [{" get config strategy with undefined account id should result in system_config"
     ,?_assertEqual(Value, kapps_account_config:get_with_strategy(<<"global">>, 'undefined', ?TEST_CAT, <<"root_obj_key">>))
     }
    ,{" get config strategy with empty call object account id should result in system_config"
     ,?_assertEqual(Value, kapps_account_config:get_with_strategy(<<"global">>, kapps_call:new(), ?TEST_CAT, <<"root_obj_key">>))
     }
    ,{" get config strategy with empty jobj object account id should result in system_config"
     ,?_assertEqual(Value, kapps_account_config:get_with_strategy(<<"global">>, kz_json:new(), ?TEST_CAT, <<"root_obj_key">>))
     }
    ].

%% get_startegy_global_from_account() ->
%%     Value = ?JOBJ_VALUE_FROM_SYSTEM,
%%     [{"get config global strategy from a sub-account"
%%      ,?_assertEqual(Value, kapps_account_config:get_with_strategy(<<"global">>, 'undefined', ?TEST_CAT, <<"root_obj_key">>, Value))
%%      }
%%     ].
