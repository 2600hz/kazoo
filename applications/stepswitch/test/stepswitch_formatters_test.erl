%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(stepswitch_formatters_test).

-include_lib("eunit/include/eunit.hrl").

-define(OFFNET_REQ, wh_json:from_list([{<<"Custom-SIP-Headers">>
                                            ,wh_json:from_list([{<<"Diversions">>
                                                                 ,[<<"sip:14158867900@1.2.3.4;counter=1">>]
                                                                }
                                                               ])
                                       }
                                       ,{<<"Invite-Format">>, <<"npan">>}
                                      ])).

-define(ROUTE_REQ, wh_json:from_list([{<<"To">>, <<"12345556789@2600hz.com">>}
                                      ,{<<"From">>, <<"4158867900@2600hz.com">>}
                                      ,{<<"Request">>, <<"+12345556789@2600hz.com">>}
                                      ,{<<"Call-ID">>,<<"352401574@10.26.0.158">>}
                                      ,{<<"Caller-ID-Number">>, <<"+14158867900">>}
                                      ,{<<"Custom-SIP-Headers">>
                                        ,wh_json:from_list([{<<"X-AUTH-IP">>,<<"10.26.0.158">>}])
                                       }
                                     ])).

regex_inbound_test() ->
    Formatter = wh_json:from_list([{<<"to">>
                                    ,[wh_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})$">>}
                                                         ,{<<"prefix">>, <<"+1">>}
                                                         ,{<<"direction">>, <<"inbound">>}
                                                        ])
                                     ]
                                   }
                                   ,{<<"from">>
                                     ,[wh_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})$">>}
                                                          ,{<<"prefix">>, <<"+1">>}
                                                          ,{<<"direction">>, <<"inbound">>}
                                                         ])
                                      ]
                                    }
                                   ,{<<"request">>
                                     ,[wh_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})$">>}
                                                          ,{<<"prefix">>, <<"+1">>}
                                                          ,{<<"direction">>, <<"inbound">>}
                                                         ])
                                      ]
                                    }
                                   ,{<<"caller_id_number">>
                                     ,[wh_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})$">>}
                                                          ,{<<"direction">>, <<"inbound">>}
                                                         ])
                                      ]
                                    }
                                  ]),

    Route = stepswitch_formatters:apply(?ROUTE_REQ, Formatter, 'inbound'),

    ?assertEqual(<<"+12345556789@2600hz.com">>, wh_json:get_value(<<"To">>, Route)),
    ?assertEqual(<<"+12345556789@2600hz.com">>, wh_json:get_value(<<"Request">>, Route)),
    ?assertEqual(<<"+14158867900@2600hz.com">>, wh_json:get_value(<<"From">>, Route)),
    ?assertEqual(<<"4158867900">>, wh_json:get_value(<<"Caller-ID-Number">>, Route)).

strip_inbound_test() ->
    Formatter = wh_json:from_list([{<<"to">>
                                    ,[wh_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})$">>}
                                                         ,{<<"prefix">>, <<"+1">>}
                                                         ,{<<"direction">>, <<"inbound">>}
                                                         ,{<<"strip">>, 'true'}
                                                        ])
                                     ]
                                   }
                                   ,{<<"caller_id_number">>
                                     ,[wh_json:from_list([{<<"regex">>, <<"^\\+?1?(\\d{10})$">>}
                                                          ,{<<"direction">>, <<"inbound">>}
                                                          ,{<<"strip">>, 'true'}
                                                         ])
                                      ]
                                    }
                                  ]),

    Route = stepswitch_formatters:apply(?ROUTE_REQ, Formatter, 'inbound'),

    ?assertEqual('undefined', wh_json:get_value(<<"To">>, Route)),
    ?assertEqual('undefined', wh_json:get_value(<<"Caller-ID-Number">>, Route)).

diversion_match_invite_test() ->
    Formatter = wh_json:from_list([{<<"diversions">>
                                    ,[wh_json:from_list([{<<"match_invite_format">>, 'true'}])]
                                   }
                                  ]),
    Bridge = stepswitch_formatters:apply(?OFFNET_REQ, Formatter, 'outbound'),

    ?assertEqual(<<"sip:4158867900@1.2.3.4">>
                 ,kzsip_diversion:address(
                    wh_json:get_value([<<"Custom-SIP-Headers">>, <<"Diversions">>], Bridge)
                   )
                ).

diversion_strip_test() ->
    Formatter = wh_json:from_list([{<<"diversions">>
                                    ,[wh_json:from_list([{<<"strip">>, 'true'}])]
                                   }
                                  ]),
    Bridge = stepswitch_formatters:apply(?OFFNET_REQ, Formatter, 'outbound'),

    ?assertEqual('undefined'
                 ,wh_json:get_value([<<"Custom-SIP-Headers">>, <<"Diversions">>], Bridge)
                ).
