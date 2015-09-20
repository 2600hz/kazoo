%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_carriers_find_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

find_local_test_() ->
    [{"Finding local numbers not supported"
      ,?_assertMatch({'error', 'non_available'}
                     ,knm_local:find_numbers(<<"415">>, 1, [])
                    )
     }
     ,{"Finding local numbers returns empty list"
       ,?_assertEqual([], knm_carriers:find(<<"415">>, 1))
      }
    ].

find_other_test_() ->
    [find_no_phonebook()
     ,find_blocks()
     ,find_numbers()
    ].

find_no_phonebook() ->
    [{"Verify no phonebook url yields no results"
      ,?_assertMatch({'error', 'non_available'}
                     ,knm_other:find_numbers(<<"415">>, 1, [])
                    )
     }
    ].

find_blocks() ->
    Options = [{<<"phonebook_url">>, ?BLOCK_PHONEBOOK_URL}
               ,{<<"blocks">>, 'true'}
               ,{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
              ],
    {'bulk', Numbers} = knm_other:find_numbers(<<"415">>, 10, Options),
    Results = knm_carriers:process_bulk_carrier_results([], Numbers),
    [{"Verify the same number of numbers and results"
     ,?_assertEqual(length(Numbers), length(Results))
     }
     | verify_start(hd(Numbers), hd(Results))
     ++ verify_end(hd(tl(Numbers)), hd(tl(Results)))
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
     ,{"Verify assign_to account id"
       ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assign_to(PhoneNumber))
      }
     ,{"Verify phone number database"
       ,?_assertEqual(<<"numbers%2F%2B1415">>, knm_phone_number:number_db(PhoneNumber))
      }

     ,{"Verify JObj number is start number"
       ,?_assertEqual(DID, wh_json:get_value(<<"number">>, JObj))
      }
     ,{"Verify JObj activation charge"
       ,?_assertEqual(Activation, wh_json:get_value(<<"activation_charge">>, JObj))
      }
    ].

find_numbers() ->
    [].
%% test for assign_to!
