%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_telnyx_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

api_test_() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_telnyx">>]}
              ,{'query_id', <<"QID">>}
              ],
    {setup
    ,fun () -> {'ok', Pid} = knm_search:start_link(), Pid end
    ,fun gen_server:stop/1
    ,fun (_ReturnOfSetup) ->
             [find_numbers(Options)
             ,find_international_numbers(Options)
             ]
     end
    }.

find_numbers(Options0) ->
    [[{"Verify found numbers"
      ,?_assertEqual(Limit, length(Results))
      }
     ,{"Verify results match queried prefix"
      ,?_assertEqual('true', lists:all(matcher(<<"+1">>, Prefix), Results))
      }
     ]
     || {Prefix, Limit} <- [{<<"301359">>, 5}
                           ,{<<"800">>, 2}
                           ],
        Options <- [[{quantity, Limit}
                    ,{prefix, Prefix}
                     | Options0
                    ]],
        Results <- [knm_search:find(Options)]
    ].

find_international_numbers(Options0) ->
    Country = <<"GB">>,
    [[{"Verify found numbers"
      ,?_assertEqual(Limit, length(Results))
      }
     ,{"Verify results match queried prefix"
      ,?_assertEqual('true', lists:all(matcher(<<"+44">>, Prefix), Results))
      }
     ]
     || {Prefix, Limit} <- [{<<"1">>, 2}
                           ],
        Options <- [[{country, Country}
                    ,{quantity, Limit}
                    ,{prefix, Prefix}
                     | Options0
                    ]],
        Results <- [knm_search:find(Options)]
    ].

matcher(Dialcode, Prefix) ->
    fun (Result) ->
            Num = <<Dialcode/binary, Prefix/binary>>,
            Size = byte_size(Num),
            case kz_json:get_value(<<"number">>, Result) of
                <<Num:Size/binary, _/binary>> -> 'true';
                _Else -> 'false'
            end
    end.

acquire_number_test_() ->
    N = ?TEST_TELNYX_NUM,
    PhoneNumber = knm_phone_number:from_number(N),
    Number = knm_number:set_phone_number(knm_number:new(), PhoneNumber),
    Result = knm_telnyx:acquire_number(Number),
    [{"Verify number is still one inputed"
     ,?_assertEqual(N, knm_phone_number:number(knm_number:phone_number(Result)))
     }
    ].

e911_test_() ->
    E911 = kz_json:from_list(
             [{?E911_STREET1, <<"301 Marina Blvd.">>}
             ,{?E911_CITY, <<"San Francisco">>}
             ,{?E911_STATE, <<"CA">>}
             ,{?E911_ZIP, <<"94123">>}
             ]),
    JObj = kz_json:from_list([{?FEATURE_E911, E911}]),
    Options = [{'auth_by', ?MASTER_ACCOUNT_ID}
              ,{'assign_to', ?RESELLER_ACCOUNT_ID}
              ,{<<"auth_by_account">>, kz_json:new()}
              ,{'public_fields', JObj}
              ],
    {'ok', N1} = knm_number:create(?TEST_TELNYX_NUM, Options),
    #{'ok' := [N2]} = knm_numbers:update([N1], [{fun knm_phone_number:reset_doc/2, JObj}]),
    PN1 = knm_number:phone_number(N1),
    PN2 = knm_number:phone_number(N2),
    [{"Verify feature is properly set"
     ,?_assertEqual(E911, knm_phone_number:feature(PN1, ?FEATURE_E911))
     }
    ,{"Verify we are keeping track of intermediary address_id"
     ,?_assertEqual(<<"421564943280637078">>
                   ,kz_json:get_value(<<"address_id">>, knm_phone_number:carrier_data(PN1))
                   )
     }
    ,{"Verify feature is still properly set"
     ,?_assertEqual(E911, knm_phone_number:feature(PN2, ?FEATURE_E911))
     }
    ,{"Verify we are keeping track of same intermediary address_id"
     ,?_assertEqual(<<"421564943280637078">>
                   ,kz_json:get_value(<<"address_id">>, knm_phone_number:carrier_data(PN2))
                   )
     }
    ].

cnam_test_() ->
    CNAM = kz_json:from_list(
             [{?CNAM_INBOUND_LOOKUP, true}
             ,{?CNAM_DISPLAY_NAME, <<"my CNAM">>}
             ]),
    JObj = kz_json:from_list([{?FEATURE_CNAM, CNAM}]),
    Options = [{'auth_by', ?MASTER_ACCOUNT_ID}
              ,{'assign_to', ?RESELLER_ACCOUNT_ID}
              ,{<<"auth_by_account">>, kz_json:new()}
              ,{'public_fields', JObj}
              ],
    {'ok', N1} = knm_number:create(?TEST_TELNYX_NUM, Options),
    PN1 = knm_number:phone_number(N1),
    #{'ok' := [N2]} = knm_numbers:update([N1], [{fun knm_phone_number:reset_doc/2, JObj}]),
    PN2 = knm_number:phone_number(N2),
    Deactivate = kz_json:from_list(
                   [{?CNAM_INBOUND_LOOKUP, false}
                   ,{?CNAM_DISPLAY_NAME, undefined}
                   ]),
    #{'ok' := [N3]} = knm_numbers:update([N2], [{fun knm_phone_number:reset_doc/2, Deactivate}]),
    PN3 = knm_number:phone_number(N3),
    [{"Verify inbound CNAM is properly activated"
     ,?_assertEqual(true, is_cnam_activated(PN1))
     }
    ,{"Verify outbound CNAM is properly set"
     ,?_assertEqual(<<"my CNAM">>, cnam_name(PN1))
     }
    ,{"Verify inbound CNAM is still properly activated"
     ,?_assertEqual(true, is_cnam_activated(PN2))
     }
    ,{"Verify outbound CNAM is still properly set"
     ,?_assertEqual(<<"my CNAM">>, cnam_name(PN2))
     }
    ,{"Verify inbound CNAM is indeed deactivated"
     ,?_assertEqual(false, is_cnam_activated(PN3))
     }
    ,{"Verify outbound CNAM is indeed reset"
     ,?_assertEqual(undefined, cnam_name(PN3))
     }
    ].

is_cnam_activated(PN) ->
    kz_json:is_true(?CNAM_INBOUND_LOOKUP, knm_phone_number:feature(PN, ?FEATURE_CNAM_INBOUND)).

cnam_name(PN) ->
    Outbound = knm_phone_number:feature(PN, ?FEATURE_CNAM_OUTBOUND),
    kz_json:get_ne_binary_value(?CNAM_DISPLAY_NAME, Outbound).

rename_carrier_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{assign_to, ?RESELLER_ACCOUNT_ID}
              ,{<<"auth_by_account">>, kz_json:new()}
              ],
    {ok, N1} = knm_number:create(?TEST_TELNYX_NUM, Options),
    PN1 = knm_number:phone_number(N1),
    JObj1 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"local">>}]),
    #{ok := [N2]} = knm_numbers:update([N1], [{fun knm_phone_number:reset_doc/2, JObj1}]),
    PN2 = knm_number:phone_number(N2),
    JObj2 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"wrong_carrier">>}]),
    #{ko := #{?TEST_TELNYX_NUM := Error3}} =
        knm_numbers:update([N2], [{fun knm_phone_number:reset_doc/2, JObj2}]),
    #{ok := [N4]} = knm_numbers:update([N2], [{fun knm_phone_number:reset_doc/2, kz_json:new()}]),
    PN4 = knm_number:phone_number(N4),
    JObj3 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"telnyx">>}]),
    #{ko := #{?TEST_TELNYX_NUM := Error5}} = knm_numbers:update([N4]
                                                               ,[{fun knm_phone_number:reset_doc/2, JObj3}]
                                                               ,[{auth_by, ?RESELLER_ACCOUNT_ID}]
                                                               ),
    JObj4 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"gen_carrier">>}]),
    #{ko := #{?TEST_TELNYX_NUM := Error6}} =
        knm_numbers:update([N2], [{fun knm_phone_number:reset_doc/2, JObj4}]),
    [{"Verify carrier name is right"
     ,?_assertEqual(<<"knm_telnyx">>, knm_phone_number:module_name(PN1))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN1)))
    ,{"Verify carrier name is changed"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN2))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN2)))
    ,{"Verify setting wrong carrier is forbidden"
     ,?_assertEqual(<<"invalid">>, knm_errors:message(Error3))
     }
    ,{"Verify carrier name is still changed"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN4))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN4)))
    ,{"Verify setting carrier as non-admin is forbidden"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(Error5))
     }
    ,{"Verify setting carrier as non-admin is forbidden"
     ,?_assertEqual(<<"invalid">>, knm_errors:error(Error6))
     }
    ].
