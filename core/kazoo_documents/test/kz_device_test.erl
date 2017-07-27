-module(kz_device_test).

-include_lib("eunit/include/eunit.hrl").

custom_sip_headers_test_() ->
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
    SIP = kz_json:from_list([{<<"custom_sip_headers">>, CSH}]),
    Device = kz_json:from_list([{<<"sip">>, SIP}]),
    [?_assertEqual(<<"foo">>, kz_device:custom_sip_header_inbound(Device, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kz_device:custom_sip_header_outbound(Device, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kz_device:custom_sip_header_inbound(Device, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(Device, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(Device, <<"x-missing-header">>))
    ].

no_legacy_sip_headers_test_() ->
    InCSH = kz_json:from_list([{<<"x-device-header">>, <<"foo">>}]),
    OutCSH = kz_json:from_list([{<<"x-outbound-header">>, <<"bar">>}]),
    CSH = kz_json:from_list([{<<"in">>, InCSH}
                            ,{<<"out">>, OutCSH}
                            ]
                           ),
    SIP = kz_json:from_list([{<<"custom_sip_headers">>, CSH}]),
    Device = kz_json:from_list([{<<"sip">>, SIP}]),
    [?_assertEqual(<<"foo">>, kz_device:custom_sip_header_inbound(Device, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kz_device:custom_sip_header_outbound(Device, <<"x-outbound-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(Device, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(Device, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(Device, <<"x-missing-header">>))
    ].

no_inbound_sip_headers_test_() ->
    LegacyCSH = kz_json:from_list([{<<"x-device-header">>, <<"baz">>}
                                  ,{<<"x-legacy-header">>, <<"Hz">>}
                                  ]),
    OutCSH = kz_json:from_list([{<<"x-outbound-header">>, <<"bar">>}]),
    CSH = kz_json:set_value(<<"out">>, OutCSH, LegacyCSH),
    SIP = kz_json:from_list([{<<"custom_sip_headers">>, CSH}]),
    Device = kz_json:from_list([{<<"sip">>, SIP}]),
    [?_assertEqual(<<"baz">>, kz_device:custom_sip_header_inbound(Device, <<"x-device-header">>))
    ,?_assertEqual(<<"bar">>, kz_device:custom_sip_header_outbound(Device, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kz_device:custom_sip_header_inbound(Device, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(Device, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(Device, <<"x-missing-header">>))
    ].

no_outbound_custom_sip_headers_test_() ->
    InCSH = kz_json:from_list([{<<"x-device-header">>, <<"foo">>}]),
    LegacyCSH = kz_json:from_list([{<<"x-device-header">>, <<"baz">>}
                                  ,{<<"x-legacy-header">>, <<"Hz">>}
                                  ]),

    CSH = kz_json:set_values([{<<"in">>, InCSH}
                             ]
                            ,LegacyCSH
                            ),
    SIP = kz_json:from_list([{<<"custom_sip_headers">>, CSH}]),
    Device = kz_json:from_list([{<<"sip">>, SIP}]),
    [?_assertEqual(<<"foo">>, kz_device:custom_sip_header_inbound(Device, <<"x-device-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(Device, <<"x-outbound-header">>))
    ,?_assertEqual(<<"Hz">>, kz_device:custom_sip_header_inbound(Device, <<"x-legacy-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_inbound(Device, <<"x-missing-header">>))
    ,?_assertEqual('undefined', kz_device:custom_sip_header_outbound(Device, <<"x-missing-header">>))
    ].
