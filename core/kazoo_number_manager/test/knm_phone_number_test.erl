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

-define(FEATURES_AVAILABLE, [<<"cnam">>
                            ,<<"e911">>
                            ,<<"failover">>
                            ,<<"port">>
                            ,<<"prepend">>
                            ]).

is_dirty_test_() ->
    {ok, OldPN} = knm_phone_number:fetch(?TEST_OLD_NUM),
    JObj = knm_phone_number:to_json(OldPN),
    NewJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_1_out.json"))),
    OldJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_1_in.json"))),
    [?_assertEqual(kz_json:get_value(<<"pvt_assigned_to">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_assigned_to">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_assigned_to">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_assigned_to">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_db_name">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_db_name">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_db_name">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_db_name">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_features_available">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_features_available">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_features_available">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_features_available">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_module_name">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_module_name">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_module_name">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_module_name">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_ported_in">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_ported_in">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_ported_in">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_ported_in">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_reserve_history">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_reserve_history">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_reserve_history">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_reserve_history">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_state">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_state">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_state">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_state">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_type">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_type">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_type">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_type">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"_id">>, NewJObj)
                  ,kz_json:get_value(<<"_id">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"_id">>, OldJObj)
                  ,kz_json:get_value(<<"_id">>, JObj)
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

    ,?_assertNotEqual(?KNM_DEFAULT_AUTH_BY, kz_json:get_value(<<"pvt_authorizing_account">>, OldJObj))
    ,?_assertEqual(?KNM_DEFAULT_AUTH_BY, kz_json:get_value(<<"pvt_authorizing_account">>, JObj))
    ,?_assertEqual(?KNM_DEFAULT_AUTH_BY, kz_json:get_value(<<"pvt_authorizing_account">>, NewJObj))
    ].


is_dirty2_test_() ->
    {ok, OldPN} = knm_phone_number:fetch(?TEST_OLD2_NUM),
    JObj = knm_phone_number:to_json(OldPN),
    NewJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_2_out.json"))),
    OldJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_2_in.json"))),
    [?_assertEqual(kz_json:get_value(<<"pvt_assigned_to">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_assigned_to">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_assigned_to">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_assigned_to">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_db_name">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_db_name">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_db_name">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_db_name">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_reserve_history">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_reserve_history">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_reserve_history">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_reserve_history">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"pvt_type">>, NewJObj)
                  ,kz_json:get_value(<<"pvt_type">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"pvt_type">>, OldJObj)
                  ,kz_json:get_value(<<"pvt_type">>, JObj)
                  )

    ,?_assertEqual(kz_json:get_value(<<"_id">>, NewJObj)
                  ,kz_json:get_value(<<"_id">>, JObj)
                  )
    ,?_assertEqual(kz_json:get_value(<<"_id">>, OldJObj)
                  ,kz_json:get_value(<<"_id">>, JObj)
                  )

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_features_available">>, OldJObj))
    ,?_assertEqual(?FEATURES_AVAILABLE, kz_json:get_value(<<"pvt_features_available">>, JObj))
    ,?_assertEqual(?FEATURES_AVAILABLE, kz_json:get_value(<<"pvt_features_available">>, NewJObj))

    ,?_assertEqual(<<"wnm_pacwest">>, kz_json:get_value(<<"pvt_module_name">>, OldJObj))
    ,?_assertEqual(<<"knm_pacwest">>, kz_json:get_value(<<"pvt_module_name">>, JObj))
    ,?_assertEqual(<<"knm_pacwest">>, kz_json:get_value(<<"pvt_module_name">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_ported_in">>, OldJObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, JObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, NewJObj))

    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_number_state">>, OldJObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, JObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_is_billable">>, OldJObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_is_billable">>, JObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_is_billable">>, NewJObj))

    ,?_assertEqual(63637990840, kz_json:get_value(<<"pvt_created">>, OldJObj))
    ,?_assertEqual(63637990840, kz_json:get_value(<<"pvt_created">>, JObj))
    ,?_assertEqual(63637990840, kz_json:get_value(<<"pvt_created">>, NewJObj))

    ,?_assertEqual(63637990853, kz_json:get_value(<<"pvt_modified">>, OldJObj))
    ,?_assertNotEqual(63637990853, kz_json:get_value(<<"pvt_modified">>, JObj))
    ,?_assertNotEqual(63637990853, kz_json:get_value(<<"pvt_modified">>, NewJObj))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, JObj)))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, NewJObj)))

    ,?_assertEqual([], kz_json:get_value(<<"pvt_features">>, OldJObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_features">>, JObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_features">>, NewJObj))

    ,?_assertEqual(<<>>, kz_json:get_value(<<"used_by">>, OldJObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_used_by">>, JObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_used_by">>, NewJObj))

    ,?_assertNotEqual(?KNM_DEFAULT_AUTH_BY, kz_json:get_value(<<"pvt_authorizing_account">>, OldJObj))
    ,?_assertEqual(?KNM_DEFAULT_AUTH_BY, kz_json:get_value(<<"pvt_authorizing_account">>, JObj))
    ,?_assertEqual(?KNM_DEFAULT_AUTH_BY, kz_json:get_value(<<"pvt_authorizing_account">>, NewJObj))
    ].
