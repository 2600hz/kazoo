-module(kz_device_test).

-include_lib("eunit/include/eunit.hrl").

-define(MASTER_ACCOUNT, <<"account0000000000000000000000001">>).
-define(DEVICE_1_ID, <<"device00000000000000000000000001">>).

validate_fixtures_test_() ->
    {'ok', Schema} = kz_json_schema:fload(<<"devices">>),
    {'ok', Device1} = kz_device:fetch(?MASTER_ACCOUNT, ?DEVICE_1_ID),
    file:write_file("/tmp/error", io_lib:format("check: ~p~n", [validate(Schema, Device1)])),
    [{"validate device fixture 1", ?_assertMatch({'ok', _}, validate(Schema, Device1))}].

custom_sip_headers_test_() ->
    {'ok', TestDevice} = kz_device:fetch(?MASTER_ACCOUNT, ?DEVICE_1_ID),
    [?_assertEqual(<<"foo">>, kz_device:custom_sip_header_inbound(TestDevice, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kz_device:custom_sip_header_outbound(TestDevice, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kz_device:custom_sip_header_inbound(TestDevice, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(TestDevice, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(TestDevice, <<"x-missing-header">>))
    ].

no_legacy_sip_headers_test_() ->
    {'ok', Device} = kz_device:fetch(?MASTER_ACCOUNT, ?DEVICE_1_ID),
    InCSH = kz_json:from_list([{<<"x-device-header">>, <<"foo">>}]),
    OutCSH = kz_json:from_list([{<<"x-outbound-header">>, <<"bar">>}]),
    CSH = kz_json:from_list([{<<"in">>, InCSH}
                            ,{<<"out">>, OutCSH}
                            ]
                           ),
    TestDevice = kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>], CSH, Device),
    [?_assertEqual(<<"foo">>, kz_device:custom_sip_header_inbound(TestDevice, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kz_device:custom_sip_header_outbound(TestDevice, <<"x-outbound-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(TestDevice, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(TestDevice, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(TestDevice, <<"x-missing-header">>))
    ].

no_inbound_sip_headers_test_() ->
    {'ok', Device} = kz_device:fetch(?MASTER_ACCOUNT, ?DEVICE_1_ID),
    LegacyCSH = kz_json:from_list([{<<"x-device-header">>, <<"baz">>}
                                  ,{<<"x-legacy-header">>, <<"Hz">>}
                                  ]),
    OutCSH = kz_json:from_list([{<<"x-outbound-header">>, <<"bar">>}]),
    CSH = kz_json:set_value(<<"out">>, OutCSH, LegacyCSH),
    TestDevice = kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>], CSH, Device),
    [?_assertEqual(<<"baz">>, kz_device:custom_sip_header_inbound(TestDevice, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kz_device:custom_sip_header_outbound(TestDevice, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kz_device:custom_sip_header_inbound(TestDevice, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(TestDevice, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(TestDevice, <<"x-missing-header">>))
    ].

no_outbound_custom_sip_headers_test_() ->
    {'ok', Device} = kz_device:fetch(?MASTER_ACCOUNT, ?DEVICE_1_ID),
    InCSH = kz_json:from_list([{<<"x-device-header">>, <<"foo">>}]),
    LegacyCSH = kz_json:from_list([{<<"x-device-header">>, <<"baz">>}
                                  ,{<<"x-legacy-header">>, <<"Hz">>}
                                  ]),

    CSH = kz_json:set_values([{<<"in">>, InCSH}
                             ]
                            ,LegacyCSH
                            ),
    TestDevice = kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>], CSH, Device),
    [?_assertEqual(<<"foo">>, kz_device:custom_sip_header_inbound(TestDevice, <<"x-device-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(TestDevice, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kz_device:custom_sip_header_inbound(TestDevice, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(TestDevice, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(TestDevice, <<"x-missing-header">>))
    ].

custom_sip_headers_schema_test_() ->
    {'ok', Device} = kz_device:fetch(?MASTER_ACCOUNT, ?DEVICE_1_ID),
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
    TestDevice = kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>], CSH, Device),

    {'ok', Schema} = kz_json_schema:fload(<<"devices">>),

    [{"valid schema check", ?_assertMatch({'ok', _}, validate(Schema, TestDevice))}
    ,{"invalid header value check", ?_assertMatch({'error', _}, validate(Schema, kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>, <<"out">>, <<"bad">>], kz_json:new(), TestDevice)))}
    ,{"invalid header check", ?_assertMatch({'error', _}, validate(Schema, kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>, <<"out">>, <<"bad@header">>], <<"ok">>, TestDevice)))}
    ].

validate(Schema, Device) ->
    kz_json_schema:validate(Schema
                           ,Device
                           ,[{'schema_loader_fun', fun kz_json_schema:fload/1}]
                           ).
