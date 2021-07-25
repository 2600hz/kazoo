%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2021, 2600Hz
%%% @doc Account document
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_device_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kazoo_dbg.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").

-define(DEVICE_1_ID, <<"device00000000000000000000000001">>).
-define(DEVICE_2_ID, <<"device00000000000000000000000002">>).

kz_device_test_() ->
    {'setup'
    ,fun kzd_test_fixtures:setup/0
    ,fun kzd_test_fixtures:cleanup/1
    ,fun(_) ->
             [test_invalid_parameters()
             ,test_validate_fixtures()
             ,test_auxiliary_functions()
             ,test_custom_sip_headers()
             ,test_no_legacy_sip_headers()
             ,test_no_inbound_sip_headers()
             ,test_no_outbound_custom_sip_headers()
             ,test_custom_sip_headers_schema()
             ,test_outbound_flags()
             ,test_outbound_flags_static()
             ,test_outbound_dynamic_flags()
             ,test_device_param_setting()
             ,test_calculating_presence_id()
             ]
     end
    }.

test_invalid_parameters() ->
    [?_assertMatch({'error', 'invalid_parameters'}
                  ,kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, 256)
                  )
    ].

test_validate_fixtures() ->
    {'ok', Schema} = kz_json_schema:fload(<<"devices">>),
    {'ok', Device1} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    [{"validate device fixture 1", ?_assertMatch({'ok', _}, validate(Schema, Device1))}].

test_auxiliary_functions() ->
    {'ok', Device1} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    [?_assert(kzd_devices:is_device(Device1))
    ,?_assertEqual(<<"device_1_username">>, kzd_devices:sip_username(Device1))
    ,?_assertEqual(<<"device_1_password">>, kzd_devices:sip_password(Device1))
    ,?_assertEqual(<<"password">>, kzd_devices:sip_method(Device1))
    ,?_assertEqual(<<"2600hz.local:7000">>, kzd_devices:sip_route(Device1))
    ,?_assertEqual(<<"10.26.20.20">>, kzd_devices:sip_ip(Device1))
    ,?_assertEqual(<<"device-1@4a6863.sip.2600hz.local">>, kzd_devices:presence_id(Device1))
    ,?_assertEqual(<<"contact">>, kzd_devices:sip_invite_format(Device1))
    ,?_assertEqual(<<"Test Device 1">>, kzd_devices:name(Device1))
    ,?_assertEqual(<<"00:15:65:27:C9:8E">>, kzd_devices:mac_address(Device1))
    ,?_assertEqual(<<"fr-fr">>, kzd_devices:language(Device1))
    ,?_assertEqual(<<"sip_device">>, kzd_devices:device_type(Device1))
    ,?_assertEqual(<<"user0000000000000000000000000001">>, kzd_devices:owner_id(Device1))
    ,?_assertEqual(<<"America/New_York">>, kzd_devices:timezone(Device1))
    ,?_assert(kzd_devices:enabled(Device1))
    ].

test_custom_sip_headers() ->
    {'ok', TestDevice} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    [?_assertEqual(<<"foo">>, kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kzd_devices:custom_sip_header_outbound(TestDevice, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_outbound(TestDevice, <<"x-missing-header">>))
    ].

test_no_legacy_sip_headers() ->
    {'ok', Device} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    InCSH = kz_json:from_list([{<<"x-device-header">>, <<"foo">>}]),
    OutCSH = kz_json:from_list([{<<"x-outbound-header">>, <<"bar">>}]),
    CSH = kz_json:from_list([{<<"in">>, InCSH}
                            ,{<<"out">>, OutCSH}
                            ]
                           ),
    TestDevice = kzd_devices:set_sip_custom_sip_headers(Device, CSH),
    [?_assertEqual(<<"foo">>, kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kzd_devices:custom_sip_header_outbound(TestDevice, <<"x-outbound-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_outbound(TestDevice, <<"x-missing-header">>))
    ].

test_no_inbound_sip_headers() ->
    {'ok', Device} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    LegacyCSH = kz_json:from_list([{<<"x-device-header">>, <<"baz">>}
                                  ,{<<"x-legacy-header">>, <<"Hz">>}
                                  ]),
    OutCSH = kz_json:from_list([{<<"x-outbound-header">>, <<"bar">>}]),
    CSH = kz_json:set_value(<<"out">>, OutCSH, LegacyCSH),
    TestDevice = kzd_devices:set_sip_custom_sip_headers(Device, CSH),
    [?_assertEqual(<<"baz">>, kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kzd_devices:custom_sip_header_outbound(TestDevice, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_outbound(TestDevice, <<"x-missing-header">>))
    ].

test_no_outbound_custom_sip_headers() ->
    {'ok', Device} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    InCSH = kz_json:from_list([{<<"x-device-header">>, <<"foo">>}]),
    LegacyCSH = kz_json:from_list([{<<"x-device-header">>, <<"baz">>}
                                  ,{<<"x-legacy-header">>, <<"Hz">>}
                                  ]),

    CSH = kz_json:set_values([{<<"in">>, InCSH}
                             ]
                            ,LegacyCSH
                            ),
    TestDevice = kzd_devices:set_sip_custom_sip_headers(Device, CSH),
    [?_assertEqual(<<"foo">>, kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-device-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_outbound(TestDevice, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_inbound(TestDevice, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kzd_devices:custom_sip_header_outbound(TestDevice, <<"x-missing-header">>))
    ].

test_custom_sip_headers_schema() ->
    {'ok', Device} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    InCSH = kz_json:from_list([{<<"x-device-header">>, <<"foo">>}]),
    OutCSH = kz_json:from_list([{<<"x-outbound-header">>, <<"bar">>}]),
    LegacyCSH = kz_json:from_list([{<<"x-device-header">>, <<"baz">>}
                                  ,{<<"x-legacy-header">>, <<"Hz">>}
                                  ]),
    CSH = kz_json:set_values([{<<"in">>, InCSH}
                             ,{<<"out">>, OutCSH}
                             ]
                            ,LegacyCSH
                            ),
    TestDevice = kzd_devices:set_sip_custom_sip_headers(Device, CSH),
    {'ok', Schema} = kz_json_schema:fload(<<"devices">>),

    [{"valid schema check", ?_assertMatch({'ok', _}, validate(Schema, TestDevice))}
    ,{"invalid header value check", ?_assertMatch({'error', _}, validate(Schema, kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>, <<"out">>, <<"bad">>], kz_json:new(), TestDevice)))}
    ,{"invalid header check", ?_assertMatch({'error', _}, validate(Schema, kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>, <<"out">>, <<"bad@header">>], <<"ok">>, TestDevice)))}
    ].

test_outbound_flags() ->
    {'ok', OldData} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    ExpectedOldData = kz_json:decode("{\"static\": [\"device_old_static_flag\"]}"),

    {'ok', NewData} = kzd_devices:fetch(?FIXTURE_RESELLER_ACCOUNT_ID, ?DEVICE_2_ID),
    ExpectedNewData = kz_json:decode("{\"dynamic\": [\"not_exported\", \"from_realm\"], \"static\": [\"device_new_static_flag\"]}"),

    MissingData = kz_json:delete_key(<<"outbound_flags">>, NewData),

    UpdatedOldData = kzd_devices:set_outbound_flags(MissingData, [<<"device_old_static_flag">>]),
    Update = kz_json:from_list([{<<"dynamic">>, [<<"not_exported">>, <<"from_realm">>]}, {<<"static">>, [<<"device_new_static_flag">>]}]),
    UpdatedNewData = kzd_devices:set_outbound_flags(MissingData, Update),

    StaticUpdate = [<<"device_new_static_flag">>],
    DynamicUpdate = [<<"not_exported">>, <<"from_realm">>],
    UpdateBothNewData = kzd_devices:set_outbound_flags(MissingData, StaticUpdate, DynamicUpdate),

    [{"verify get for deprecated format"
     ,?_assertEqual(ExpectedOldData, kzd_devices:outbound_flags(OldData))
     }
    ,{"verify get for new format"
     ,?_assertEqual(ExpectedNewData, kzd_devices:outbound_flags(NewData))
     }
    ,{"verify get for missing data"
     ,?_assertEqual(kz_json:new(), kzd_devices:outbound_flags(MissingData))
     }
    ,{"verify deprecated update"
     ,?_assertEqual(ExpectedOldData, kzd_devices:outbound_flags(UpdatedOldData))
     }
    ,{"verify new update"
     ,?_assertEqual(ExpectedNewData, kzd_devices:outbound_flags(UpdatedNewData))
     }
    ,{"verify both update"
     ,?_assertEqual(ExpectedNewData, kzd_devices:outbound_flags(UpdateBothNewData))
     }
    ,{"verify both update with undefined static"
     ,?_assertEqual([], kzd_devices:outbound_static_flags(kzd_devices:set_outbound_flags(NewData, 'undefined', [<<"unrelated_flag">>])))
     }
    ,{"verify both update with undefined dynamic"
     ,?_assertEqual([], kzd_devices:outbound_dynamic_flags(kzd_devices:set_outbound_flags(NewData, [<<"unrelated_flag">>], 'undefined')))
     }
    ].

test_outbound_flags_static() ->
    {'ok', OldData} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    UpdatedOldData = kz_json:get_value(<<"outbound_flags">>, kzd_devices:set_outbound_static_flags(OldData, [<<"updated_flag">>])),
    ExpectedOldUpdate = kz_json:decode("{\"static\": [\"updated_flag\"]}"),

    {'ok', NewData} = kzd_devices:fetch(?FIXTURE_RESELLER_ACCOUNT_ID, ?DEVICE_2_ID),
    UpdatedNewData = kz_json:get_value(<<"outbound_flags">>, kzd_devices:set_outbound_static_flags(NewData, [<<"updated_flag">>])),
    ExpectedNewUpdate = kz_json:decode("{\"dynamic\": [\"not_exported\", \"from_realm\"], \"static\": [\"updated_flag\"]}"),

    [{"verify get for deprecated format"
     ,?_assertEqual([<<"device_old_static_flag">>], kzd_devices:outbound_static_flags(OldData))
     }
    ,{"verify get for new format"
     ,?_assertEqual([<<"device_new_static_flag">>], kzd_devices:outbound_static_flags(NewData))
     }
    ,{"verify set with old format converts to new"
     ,?_assertEqual(ExpectedOldUpdate, UpdatedOldData)
     }
    ,{"verify set with new format"
     ,?_assertEqual(ExpectedNewUpdate, UpdatedNewData)
     }
    ].

test_outbound_dynamic_flags() ->
    {'ok', OldData} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    UpdatedOldData = kz_json:get_value(<<"outbound_flags">>, kzd_devices:set_outbound_dynamic_flags(OldData, [<<"updated_flag">>])),
    ExpectedOldUpdate = kz_json:decode("{\"static\": [\"device_old_static_flag\"], \"dynamic\": [\"updated_flag\"]}"),

    {'ok', NewData} = kzd_devices:fetch(?FIXTURE_RESELLER_ACCOUNT_ID, ?DEVICE_2_ID),
    UpdatedNewData = kz_json:get_value(<<"outbound_flags">>, kzd_devices:set_outbound_dynamic_flags(NewData, [<<"updated_flag">>])),
    ExpectedNewUpdate = kz_json:decode("{\"dynamic\": [\"updated_flag\"], \"static\": [\"device_new_static_flag\"]}"),
    [{"verify get for deprecated format"
     ,?_assertEqual([], kzd_devices:outbound_dynamic_flags(OldData))
     }
    ,{"verify get for new format"
     ,?_assertEqual([<<"not_exported">>, <<"from_realm">>], kzd_devices:outbound_dynamic_flags(NewData))
     }
    ,{"verify set with old format converts to new"
     ,?_assertEqual(ExpectedOldUpdate, UpdatedOldData)
     }
    ,{"verify set with new format"
     ,?_assertEqual(ExpectedNewUpdate, UpdatedNewData)
     }
    ].

test_device_param_setting() ->
    {'ok', Device} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),

    Setup = [{<<"rick_and_morty">>, fun kzd_devices:set_sip_username/2, fun kzd_devices:sip_username/1}
            ,{<<"birdperson">>, fun kzd_devices:set_sip_password/2, fun kzd_devices:sip_password/1}
            ,{<<"OAuth">>, fun kzd_devices:set_sip_method/2, fun kzd_devices:sip_method/1}
            ,{<<"2600hz.local:8888">>, fun kzd_devices:set_sip_route/2, fun kzd_devices:sip_route/1}
            ,{<<"10.26.0.100">>, fun kzd_devices:set_sip_ip/2, fun kzd_devices:sip_ip/1}
            ,{<<"rick-l@4a6812.sip.2600hz.local">>, fun kzd_devices:set_presence_id/2, fun kzd_devices:presence_id/1}
            ,{<<"fax">>, fun kzd_devices:set_sip_invite_format/2, fun kzd_devices:sip_invite_format/1}
            ,{<<"Rick Sanchez">>, fun kzd_devices:set_name/2, fun kzd_devices:name/1}
            ,{<<"00:14:65:26:C9:8Z">>, fun kzd_devices:set_mac_address/2, fun kzd_devices:mac_address/1}
            ,{<<"us-en">>, fun kzd_devices:set_language/2, fun kzd_devices:language/1}
            ,{<<"fax_machine">>, fun kzd_devices:set_device_type/2, fun kzd_devices:device_type/1}
            ,{<<"user0000000000000000000000000002">>, fun kzd_devices:set_owner_id/2, fun kzd_devices:owner_id/1}
            ,{'false', fun kzd_devices:set_enabled/2, fun kzd_devices:enabled/1}
            ,{'false', fun kzd_devices:set_mwi_unsolicited_updates/2, fun kzd_devices:mwi_unsolicited_updates/1}
            ],

    [?_assertEqual(Value, Get(Set(Device, Value))) || {Value, Set, Get} <- Setup].

test_calculating_presence_id() ->
    {'ok', Device} = kzd_devices:fetch(?FIXTURE_MASTER_ACCOUNT_ID, ?DEVICE_1_ID),
    OwnerId = kzd_devices:owner_id(Device),
    {'ok', User} = kzd_users:fetch(?FIXTURE_MASTER_ACCOUNT_ID, OwnerId),

    SIPUsername = kz_binary:rand_hex(5),
    AccountRealm = <<"4a6863.sip.2600hz.local">>,

    BlankDevice = kz_json:delete_keys([<<"presence_id">>
                                      ,<<"owner_id">>
                                      ,<<"call_forward">>
                                      ,<<"call_recording">>
                                      ,<<"call_restriction">>
                                      ,<<"caller_id">>
                                      ,<<"dial_plan">>
                                      ,<<"media">>, <<"metaflows">>, <<"ringtones">>
                                      ]
                                     , Device),
    JustUsername = kzd_devices:set_sip_username(BlankDevice, SIPUsername),

    DevicePresenceId = kz_binary:rand_hex(5),
    WithDevicePresenceId = kzd_devices:set_presence_id(JustUsername, DevicePresenceId),

    OwnerPresenceId = kzd_users:presence_id(User),

    HotdeskedDevice = kzd_devices:set_hotdesk(JustUsername, kz_json:from_list([{<<"users">>, kz_json:from_list([{OwnerId, kz_binary:rand_hex(16)}])}])),
    HotdeskedAndOwnedDevice = kzd_devices:set_owner_id(HotdeskedDevice, OwnerId),

    Tests = [{"SIP username", <<SIPUsername/binary, "@", AccountRealm/binary>>, JustUsername}
            ,{"Device presence_id", <<DevicePresenceId/binary, "@", AccountRealm/binary>>, WithDevicePresenceId}
            ,{"Owner presence_id", OwnerPresenceId, Device}
            ,{"Hotdesked presence_id", OwnerPresenceId, HotdeskedDevice}
            ,{"Hotdesked and owned presence_id", OwnerPresenceId, HotdeskedAndOwnedDevice}
            ],
    [{Label, ?_assertEqual(PresenceId, kzd_devices:calculate_presence_id(DeviceJObj))}
     || {Label, PresenceId, DeviceJObj} <- Tests
    ].

validate(Schema, Device) ->
    kz_json_schema:validate(Schema
                           ,Device
                           ,[{'schema_loader_fun', fun kz_json_schema:fload/1}]
                           ).
