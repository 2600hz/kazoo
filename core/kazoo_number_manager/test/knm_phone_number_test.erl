%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_phone_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

is_dirty1_test_() ->
    {ok, PN} = knm_phone_number:fetch(?TEST_OLD_NUM),
    JObj = knm_phone_number:to_json(PN),
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
    {ok, PN} = knm_phone_number:fetch(?TEST_OLD2_NUM),
    JObj = knm_phone_number:to_json(PN),
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

    ,?_assertEqual(<<"wnm_pacwest">>, kz_json:get_value(<<"pvt_module_name">>, OldJObj))
    ,?_assertEqual(<<"knm_pacwest">>, kz_json:get_value(<<"pvt_module_name">>, JObj))
    ,?_assertEqual(<<"knm_pacwest">>, kz_json:get_value(<<"pvt_module_name">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_ported_in">>, OldJObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, JObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, NewJObj))

    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_number_state">>, OldJObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, JObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, NewJObj))

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


is_dirty3_test_() ->
    {ok, PN} = knm_phone_number:fetch(?TEST_OLD3_NUM),
    JObj = knm_phone_number:to_json(PN),
    NewJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_3_out.json"))),
    OldJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_3_in.json"))),
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

    ,?_assertEqual(<<"knm_bandwidth2">>, kz_json:get_value(<<"pvt_module_name">>, OldJObj))
    ,?_assertEqual(<<"knm_bandwidth2">>, kz_json:get_value(<<"pvt_module_name">>, JObj))
    ,?_assertEqual(<<"knm_bandwidth2">>, kz_json:get_value(<<"pvt_module_name">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_ported_in">>, OldJObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, JObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, NewJObj))

    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_number_state">>, OldJObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, JObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, NewJObj))

    ,?_assertEqual(63646110391, kz_json:get_value(<<"pvt_created">>, OldJObj))
    ,?_assertEqual(63646110391, kz_json:get_value(<<"pvt_created">>, JObj))
    ,?_assertEqual(63646110391, kz_json:get_value(<<"pvt_created">>, NewJObj))

    ,?_assertEqual(63646110391, kz_json:get_value(<<"pvt_modified">>, OldJObj))
    ,?_assertNotEqual(63646110391, kz_json:get_value(<<"pvt_modified">>, JObj))
    ,?_assertNotEqual(63646110391, kz_json:get_value(<<"pvt_modified">>, NewJObj))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, JObj)))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, NewJObj)))

    ,?_assertEqual(kz_json:to_map(kz_json:get_value(<<"pvt_features">>, OldJObj))
                  ,kz_json:to_map(kz_json:get_value(<<"pvt_features">>, JObj)))
    ,?_assertEqual(kz_json:to_map(kz_json:get_value(<<"pvt_features">>, OldJObj))
                  ,kz_json:to_map(kz_json:get_value(<<"pvt_features">>, NewJObj)))

    ,?_assertEqual(<<"callflow">>, kz_json:get_value(<<"used_by">>, OldJObj))
    ,?_assertEqual(<<"callflow">>, kz_json:get_value(<<"pvt_used_by">>, JObj))
    ,?_assertEqual(<<"callflow">>, kz_json:get_value(<<"pvt_used_by">>, NewJObj))
    ].


is_dirty4_test_() ->
    {ok, PN} = knm_phone_number:fetch(?TEST_OLD4_NUM),
    JObj = knm_phone_number:to_json(PN),
    NewJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_4_out.json"))),
    OldJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_4_in.json"))),
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

    ,?_assertEqual(<<"wnm_local">>, kz_json:get_value(<<"pvt_module_name">>, OldJObj))
    ,?_assertEqual(<<"knm_local">>, kz_json:get_value(<<"pvt_module_name">>, JObj))
    ,?_assertEqual(<<"knm_local">>, kz_json:get_value(<<"pvt_module_name">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_ported_in">>, OldJObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, JObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, NewJObj))

    ,?_assertEqual(<<"reserved">>, kz_json:get_value(<<"pvt_number_state">>, OldJObj))
    ,?_assertEqual(<<"reserved">>, kz_json:get_value(<<"pvt_state">>, JObj))
    ,?_assertEqual(<<"reserved">>, kz_json:get_value(<<"pvt_state">>, NewJObj))

    ,?_assertEqual(63627551737, kz_json:get_value(<<"pvt_created">>, OldJObj))
    ,?_assertEqual(63627551737, kz_json:get_value(<<"pvt_created">>, JObj))
    ,?_assertEqual(63627551737, kz_json:get_value(<<"pvt_created">>, NewJObj))

    ,?_assertEqual(63627551739, kz_json:get_value(<<"pvt_modified">>, OldJObj))
    ,?_assertNotEqual(63627551739, kz_json:get_value(<<"pvt_modified">>, JObj))
    ,?_assertNotEqual(63627551739, kz_json:get_value(<<"pvt_modified">>, NewJObj))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, JObj)))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, NewJObj)))

    ,?_assertEqual([], kz_json:get_value(<<"pvt_features">>, OldJObj))
    ,?_assertEqual(#{<<"local">> => #{}}
                  ,kz_json:to_map(kz_json:get_value(<<"pvt_features">>, JObj)))
    ,?_assertEqual(#{<<"local">> => #{}}
                  ,kz_json:to_map(kz_json:get_value(<<"pvt_features">>, NewJObj)))

    ,?_assertEqual(<<>>, kz_json:get_value(<<"used_by">>, OldJObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_used_by">>, JObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_used_by">>, NewJObj))

    ,?_assertEqual(kz_json:to_map(kz_json:public_fields(JObj))
                  ,kz_json:to_map(kz_json:public_fields(NewJObj)))
    ,?_assertEqual(maps:remove(<<"e911">>, kz_json:to_map(kz_json:public_fields(JObj)))
                  ,kz_json:to_map(kz_json:public_fields(kz_json:delete_key(<<"used_by">>, OldJObj))))
    ].


maps_take(Key, Map) ->
    {maps:get(Key, Map), maps:remove(Key, Map)}.

features_5(OldJObj) ->
    OldFeatures = kz_json:to_map(kz_json:get_value(<<"pvt_features">>, OldJObj)),
    {E911, M} = maps_take(?LEGACY_DASH_E911, OldFeatures),
    M#{<<"e911">> => E911}.

public_fields_new_5(OldJObj) ->
    M = kz_json:to_map(
          kz_json:public_fields(
            kz_json:delete_key(<<"used_by">>, OldJObj))),
    M#{<<"e911">> => maps:get(?LEGACY_DASH_E911, M)}.

is_dirty5_test_() ->
    {ok, PN} = knm_phone_number:fetch(?TEST_OLD5_NUM),
    JObj = knm_phone_number:to_json(PN),
    NewJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_5_out.json"))),
    OldJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_5_in.json"))),
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

    ,?_assertEqual(<<"knm_bandwidth2">>, kz_json:get_value(<<"pvt_module_name">>, OldJObj))
    ,?_assertEqual(<<"knm_bandwidth2">>, kz_json:get_value(<<"pvt_module_name">>, JObj))
    ,?_assertEqual(<<"knm_bandwidth2">>, kz_json:get_value(<<"pvt_module_name">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_ported_in">>, OldJObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, JObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, NewJObj))

    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_number_state">>, OldJObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_state">>, OldJObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, JObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_created">>, OldJObj))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_created">>, JObj)))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_created">>, NewJObj)))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_modified">>, OldJObj))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, JObj)))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, NewJObj)))

    ,?_assertEqual(features_5(OldJObj)
                  ,kz_json:to_map(kz_json:get_value(<<"pvt_features">>, JObj)))
    ,?_assertEqual(features_5(OldJObj)
                  ,kz_json:to_map(kz_json:get_value(<<"pvt_features">>, NewJObj)))

    ,?_assertEqual(<<"trunkstore">>, kz_json:get_value(<<"used_by">>, OldJObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"used_by">>, JObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"used_by">>, NewJObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_used_by">>, JObj))
    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_used_by">>, NewJObj))

    ,?_assertEqual(public_fields_new_5(OldJObj), kz_json:to_map(kz_json:public_fields(JObj)))
    ,?_assertEqual(public_fields_new_5(OldJObj), kz_json:to_map(kz_json:public_fields(NewJObj)))
    ].


is_dirty6_test_() ->
    {ok, PN} = knm_phone_number:fetch(?TEST_OLD6_NUM),
    JObj = knm_phone_number:to_json(PN),
    NewJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_6_out.json"))),
    OldJObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_6_in.json"))),
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

    ,?_assertEqual(<<"knm_local">>, kz_json:get_value(<<"pvt_module_name">>, OldJObj))
    ,?_assertEqual(<<"knm_local">>, kz_json:get_value(<<"pvt_module_name">>, JObj))
    ,?_assertEqual(<<"knm_local">>, kz_json:get_value(<<"pvt_module_name">>, NewJObj))

    ,?_assertEqual(undefined, kz_json:get_value(<<"pvt_ported_in">>, OldJObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, JObj))
    ,?_assertEqual(false, kz_json:get_value(<<"pvt_ported_in">>, NewJObj))

    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, OldJObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, JObj))
    ,?_assertEqual(<<"in_service">>, kz_json:get_value(<<"pvt_state">>, NewJObj))

    ,?_assertEqual(63640935218, kz_json:get_value(<<"pvt_created">>, OldJObj))
    ,?_assertEqual(63640935218, kz_json:get_value(<<"pvt_created">>, JObj))
    ,?_assertEqual(63640935218, kz_json:get_value(<<"pvt_created">>, NewJObj))

    ,?_assertEqual(63640935218, kz_json:get_value(<<"pvt_modified">>, OldJObj))
    ,?_assertNotEqual(63640935218, kz_json:get_value(<<"pvt_modified">>, JObj))
    ,?_assertNotEqual(63640935218, kz_json:get_value(<<"pvt_modified">>, NewJObj))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, JObj)))
    ,?_assertEqual(true, is_integer(kz_json:get_value(<<"pvt_modified">>, NewJObj)))

    ,?_assertEqual(kz_json:to_map(kz_json:get_value(<<"pvt_features">>, OldJObj))
                  ,kz_json:to_map(kz_json:get_value(<<"pvt_features">>, JObj)))
    ,?_assertEqual(kz_json:to_map(kz_json:get_value(<<"pvt_features">>, OldJObj))
                  ,kz_json:to_map(kz_json:get_value(<<"pvt_features">>, NewJObj)))

    ,?_assertEqual(<<"trunkstore">>, kz_json:get_value(<<"used_by">>, OldJObj))
    ,?_assertEqual(<<"trunkstore">>, kz_json:get_value(<<"pvt_used_by">>, OldJObj))
    ,?_assertEqual(<<"trunkstore">>, kz_json:get_value(<<"pvt_used_by">>, JObj))
    ,?_assertEqual(<<"trunkstore">>, kz_json:get_value(<<"pvt_used_by">>, NewJObj))

    ,?_assertEqual(kz_json:to_map(kz_json:public_fields(JObj))
                  ,kz_json:to_map(kz_json:public_fields(NewJObj)))
    ,?_assertEqual(kz_json:to_map(kz_json:public_fields(JObj))
                  ,kz_json:to_map(kz_json:public_fields(kz_json:delete_key(<<"used_by">>, OldJObj))))
    ].
