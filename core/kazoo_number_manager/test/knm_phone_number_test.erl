%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_phone_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

is_dirty_test_() ->
    {ok, OldPN} = knm_phone_number:fetch(?TEST_OLD_NUM),
    JObj = knm_phone_number:to_json(OldPN),
    NewJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_1_out.json"))),
    OldJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_1_in.json"))),
    [?_assertEqual(kz_json:get_value(<<"pvt_assigned_to">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_assigned_to">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_db_name">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_db_name">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_features_available">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_features_available">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_module_name">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_module_name">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_ported_in">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_ported_in">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_state">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_state">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_type">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_type">>, JObj)
                  )

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_is_billable">>, OldJObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_is_billable">>, JObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_is_billable">>, NewJObj))

    ,?_assertEqual(<<"undefined">>, kz_json:get_value(<<"pvt_created">>, OldJObj))
    ,?_assertNotEqual(<<"undefined">>, kz_json:get_value(<<"pvt_created">>, JObj))
    ,?_assertNotEqual(<<"undefined">>, kz_json:get_value(<<"pvt_created">>, NewJObj))

    ,?_assertEqual(<<"undefined">>, kz_json:get_value(<<"pvt_modified">>, OldJObj))
    ,?_assertNotEqual(<<"undefined">>, kz_json:get_value(<<"pvt_modified">>, JObj))
    ,?_assertNotEqual(<<"undefined">>, kz_json:get_value(<<"pvt_modified">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_features">>, OldJObj))
    ,?_assertEqual(kz_json:from_list([{<<"local">>, kz_json:new()}])
                  ,kz_json:get_value(<<"pvt_features">>, JObj)
                  )
    ,?_assertEqual(kz_json:from_list([{<<"local">>, kz_json:new()}])
                  ,kz_json:get_value(<<"pvt_features">>, NewJObj)
                  )
    ].
