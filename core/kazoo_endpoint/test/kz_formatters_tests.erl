%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_formatters_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DOLLAR_SIGN, 36). % formatter still barfs on $ in regex

-define(SCHEMA_TEST_OPTIONS, [{'schema_loader_fun', fun kz_json_schema:fload/1}
                             ,{'allowed_errors', 'infinity'}
                             ,{'extra_validator', fun kz_json_schema_extensions:extra_validator/2}
                             ,{'setter_fun', fun kz_json:set_value/3}
                             ,{'validator_options', ['use_defaults'
                                                    ,'apply_defaults_to_empty_objects'
                                                    ]}
                             ]).

-define(OFFNET_REQ, kz_json:from_list([{<<"Custom-SIP-Headers">>
                                       ,kz_json:from_list([{<<"Diversions">>
                                                           ,[<<"sip:14158867900@1.2.3.4;counter=1">>]
                                                           }
                                                          ])
                                       }
                                      ,{<<"Invite-Format">>, <<"npan">>}
                                      ])).

-define(ROUTE_REQ, kz_json:from_list([{<<"To">>, <<"12345556789@2600Hz.com">>}
                                     ,{<<"From">>, <<"4158867900@2600Hz.com">>}
                                     ,{<<"Request">>, <<"+12345556789@2600Hz.com">>}
                                     ,{<<"Call-ID">>,<<"352401574@10.26.0.158">>}
                                     ,{<<"Caller-ID-Number">>, <<"+14158867900">>}
                                     ,{<<"Custom-SIP-Headers">>
                                      ,kz_json:from_list([{<<"X-AUTH-IP">>,<<"10.26.0.158">>}])
                                      }
                                     ])
       ).

from_test_() ->
    FromValue = <<"1234567890">>,
    Formatter = kz_json:from_list([{<<"From">>, kz_json:from_list([{<<"value">>, FromValue}])}]),
    Route = kz_formatters:apply(?ROUTE_REQ, Formatter, 'outbound'),
    [?_assertEqual(<<FromValue/binary, "@2600Hz.com">>, kz_json:get_value(<<"From">>, Route))].

regex_inbound_test_() ->
    Formatter = kz_json:from_list([{<<"to">>
                                   ,[kz_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})\$">>}
                                                       ,{<<"prefix">>, <<"+1">>}
                                                       ,{<<"direction">>, <<"inbound">>}
                                                       ])
                                    ]
                                   }
                                  ,{<<"from">>
                                   ,[kz_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})\$">>}
                                                       ,{<<"prefix">>, <<"+1">>}
                                                       ,{<<"direction">>, <<"inbound">>}
                                                       ])
                                    ]
                                   }
                                  ,{<<"request">>
                                   ,[kz_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})\$">>}
                                                       ,{<<"prefix">>, <<"+1">>}
                                                       ,{<<"direction">>, <<"inbound">>}
                                                       ])
                                    ]
                                   }
                                  ,{<<"caller_id_number">>
                                   ,[kz_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})\$">>}
                                                       ,{<<"direction">>, <<"inbound">>}
                                                       ])
                                    ]
                                   }
                                  ]),

    Route = kz_formatters:apply(?ROUTE_REQ, Formatter, 'inbound'),

    [?_assertEqual(<<"+12345556789@2600Hz.com">>, kz_json:get_value(<<"To">>, Route))
    ,?_assertEqual(<<"+12345556789@2600Hz.com">>, kz_json:get_value(<<"Request">>, Route))
    ,?_assertEqual(<<"+14158867900@2600Hz.com">>, kz_json:get_value(<<"From">>, Route))
    ,?_assertEqual(<<"4158867900">>, kz_json:get_value(<<"Caller-ID-Number">>, Route))
    ].

strip_inbound_test_() ->
    Formatter = kz_json:from_list([{<<"to">>
                                   ,[kz_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})\$">>}
                                                       ,{<<"prefix">>, <<"+1">>}
                                                       ,{<<"direction">>, <<"inbound">>}
                                                       ,{<<"strip">>, 'true'}
                                                       ])
                                    ]
                                   }
                                  ,{<<"caller_id_number">>
                                   ,[kz_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})\$">>}
                                                       ,{<<"direction">>, <<"inbound">>}
                                                       ,{<<"strip">>, 'true'}
                                                       ])
                                    ]
                                   }
                                  ]),

    Route = kz_formatters:apply(?ROUTE_REQ, Formatter, 'inbound'),

    [?_assertEqual('undefined', kz_json:get_value(<<"To">>, Route))
    ,?_assertEqual('undefined', kz_json:get_value(<<"Caller-ID-Number">>, Route))
    ].

diversion_match_invite_test_() ->
    Formatter = kz_json:from_list([{<<"diversions">>
                                   ,[kz_json:from_list([{<<"match_invite_format">>, 'true'}])]
                                   }
                                  ]),
    Bridge = kz_formatters:apply(?OFFNET_REQ, Formatter, 'outbound'),

    [?_assertEqual(<<"sip:4158867900@1.2.3.4">>
                  ,kzsip_diversion:address(
                     kz_json:get_value([<<"Custom-SIP-Headers">>, <<"Diversions">>], Bridge)
                    )
                  )
    ].

diversion_strip_test_() ->
    Formatter = kz_json:from_list([{<<"diversions">>
                                   ,[kz_json:from_list([{<<"strip">>, 'true'}])]
                                   }
                                  ]),
    Bridge = kz_formatters:apply(?OFFNET_REQ, Formatter, 'outbound'),

    [?_assertEqual('undefined'
                  ,kz_json:get_value([<<"Custom-SIP-Headers">>, <<"Diversions">>], Bridge)
                  )
    ].

replace_value_test_() ->
    Replace = kz_binary:rand_hex(10),

    Formatter = kz_json:from_list([{<<"from">>
                                   ,[kz_json:from_list([{<<"value">>, Replace}])]
                                   }
                                  ,{<<"caller_id_number">>
                                   ,[kz_json:from_list([{<<"value">>, Replace}])]
                                   }
                                  ]),

    Bridge = kz_formatters:apply(?ROUTE_REQ, Formatter, 'outbound'),
    [?_assertEqual(<<Replace/binary, "@2600Hz.com">>
                  ,kz_json:get_value(<<"From">>, Bridge)
                  )
    ,?_assertEqual(Replace, kz_json:get_value(<<"Caller-ID-Number">>, Bridge))
    ].


-define(FROM_ONE
       ,kz_json:from_list([{<<"direction">>, <<"inbound">>}
                          ,{<<"prefix">>, <<"+1">>}
                          ,{<<"regex">>, <<"^\\+?1?(\\d{10})", ?DOLLAR_SIGN>>}
                          ])
       ).
-define(FROM_TWO
       ,kz_json:from_list([{<<"direction">>, <<"outbound">>}
                          ,{<<"regex">>, <<"\\+?1?(\\d{10})", ?DOLLAR_SIGN>>}
                          ])
       ).

-define(FROM, kz_json:from_list([{<<"from">>, [?FROM_ONE, ?FROM_TWO]}])).

-define(DIVERSION_ONE
       ,kz_json:from_list([{<<"match_invite_format">>, 'true'}
                          ,{<<"direction">>, <<"outbound">>}
                          ])
       ).
-define(DIVERSION, kz_json:from_list([{<<"diversion">>, [?DIVERSION_ONE]}])).

-define(CALLER_ID_NAME, kz_json:from_list([{<<"caller_id_number">>, kz_json:from_list([{<<"value">>, <<"0099887766">>}])}])).

formatter_validation_test_() ->
    {'ok', Schema} = kz_json_schema:fload(<<"formatters">>),

    [?_assertMatch({'ok', _}, kz_json_schema:validate(Schema, ?FROM, ?SCHEMA_TEST_OPTIONS))
    ,?_assertMatch({'ok', _}, kz_json_schema:validate(Schema, ?DIVERSION, ?SCHEMA_TEST_OPTIONS))
    ,?_assertMatch({'ok', _}, kz_json_schema:validate(Schema, ?CALLER_ID_NAME, ?SCHEMA_TEST_OPTIONS))
    ].

formatters_test_() ->
    FromInboundRoute = kz_formatters:apply(?ROUTE_REQ, ?FROM, 'inbound'),
    FromOutboundRoute = kz_formatters:apply(?ROUTE_REQ, ?FROM, 'outbound'),

    CIDRoute = kz_formatters:apply(?ROUTE_REQ, ?CALLER_ID_NAME, 'outbound'),

    MergedFormatters = kz_json:merge(?FROM, ?CALLER_ID_NAME),
    MergedRoute = kz_formatters:apply(?ROUTE_REQ, MergedFormatters, 'inbound'),

    [?_assertEqual(<<"+14158867900@2600Hz.com">>, kz_json:get_ne_binary_value(<<"From">>, FromInboundRoute))
    ,?_assertEqual(<<"4158867900@2600Hz.com">>, kz_json:get_ne_binary_value(<<"From">>, FromOutboundRoute))
    ,?_assertEqual(<<"0099887766">>, kz_json:get_value(<<"Caller-ID-Number">>, CIDRoute))

    ,?_assertEqual(<<"+14158867900@2600Hz.com">>, kz_json:get_ne_binary_value(<<"From">>, MergedRoute))
    ,?_assertEqual(<<"0099887766">>, kz_json:get_value(<<"Caller-ID-Number">>, MergedRoute))
    ].
