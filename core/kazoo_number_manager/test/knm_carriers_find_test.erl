%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_carriers_find_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

find_local_test_() ->
    [{"Finding local numbers not supported"
     ,?_assertMatch({'error', 'not_available'}
                   ,knm_local:find_numbers(<<"415">>, 1, [])
                   )
     }
    ,{"Finding local numbers returns empty list"
     ,?_assertEqual([], knm_carriers:find(<<"415">>))
     }
    ].

find_other_test_() ->
    [find_no_phonebook()
    ,find_blocks()
    ,find_numbers()
    ].

find_no_phonebook() ->
    Options = [{'carriers', [?CARRIER_OTHER]}],

    [{"Verify no phonebook url yields no results"
     ,?_assertEqual([], knm_carriers:find(<<"415">>, 1, Options))
     }
    ].

find_blocks() ->
    Options = [{'phonebook_url', ?BLOCK_PHONEBOOK_URL}
              ,{'blocks', 'true'}
              ,{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [?CARRIER_OTHER]}
              ],
    Limit = 10,

    {'bulk', [StartNumber, EndNumber]=Numbers} =
        knm_other:find_numbers(<<"415">>, Limit, Options),
    [StartJObj, EndJObj]=Results =
        knm_carriers:find(<<"415">>, Limit, Options),

    [{"Verify the same amount of numbers and results"
     ,?_assertEqual(length(Numbers), length(Results))
     }
     | verify_start(StartNumber, StartJObj)
     ++ verify_end(EndNumber, EndJObj)
    ].

verify_start(StartNumber, StartJObj) ->
    PhoneNumber = knm_number:phone_number(StartNumber),
    verify_block(PhoneNumber, StartJObj, ?START_BLOCK, 5.0).

verify_end(EndNumber, EndJObj) ->
    PhoneNumber = knm_number:phone_number(EndNumber),
    verify_block(PhoneNumber, EndJObj, ?END_BLOCK, 'undefined').

verify_block(PhoneNumber, JObj, DID, Activation) ->
    [{"Verify start number matches start of block"
     ,?_assertEqual(DID, knm_phone_number:number(PhoneNumber))
     }
    ,{"Verify start number carrier module"
     ,?_assertEqual(?CARRIER_OTHER, knm_phone_number:module_name(PhoneNumber))
     }
    ,{"Verify auth_by account id"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:auth_by(PhoneNumber))
     }
    ,{"Verify assigned_to account id"
     ,?_assertEqual('undefined', knm_phone_number:assigned_to(PhoneNumber))
     }
    ,{"Verify phone number database"
     ,?_assertEqual(<<"numbers%2F%2B1415">>, knm_phone_number:number_db(PhoneNumber))
     }

    ,{"Verify JObj number is start number"
     ,?_assertEqual(DID, kz_json:get_value(<<"number">>, JObj))
     }
    ,{"Verify JObj activation charge"
     ,?_assertEqual(Activation, kz_json:get_value(<<"activation_charge">>, JObj))
     }
    ].

find_numbers() ->
    Options = [{'phonebook_url', ?NUMBER_PHONEBOOK_URL}
              ,{'account_id', ?MASTER_ACCOUNT_ID}
              ,{'carriers', [?CARRIER_OTHER]}
              ],
    Limit = 10,
    Results = knm_carriers:find(<<"415">>, Limit, Options),

    [{"Verify results returned is the expected amount"
     ,?_assertEqual(Limit, length(Results))
     }
     | verify_number_results(Results)
    ].

verify_number_results(Results) ->
    {Tests, _} =
        lists:foldl(fun verify_number_result/2
                   ,{[], 0}
                   ,lists:reverse(Results)
                   ),
    Tests.

verify_number_result(Result, {Tests, N}) ->
    {[{"Verify result DID"
      ,?_assertEqual(<<"+1415886790", (N+$0)>>
                    ,kz_json:get_value(<<"number">>, Result)
                    )
      }
     ,{"Verify result activation charge"
      ,activation_charge_test(Result, <<"+1415886790", (N+$0)>>)
      }
      | Tests
     ]
    ,N+1
    }.

activation_charge_test(Result, ?START_BLOCK) ->
    {"Verify start of range activation"
    ,?_assertEqual(5.0, kz_json:get_value(<<"activation_charge">>, Result))
    };
activation_charge_test(Result, ?END_BLOCK) ->
    {"Verify end of range activation"
    ,?_assertEqual('undefined', kz_json:get_value(<<"activation_charge">>, Result))
    };
activation_charge_test(Result, _DID) ->
    {"Verify inner range activation"
    ,?_assertEqual(1.0, kz_json:get_value(<<"activation_charge">>, Result))
    }.
