%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
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
              ],
    [find_numbers(Options)
    ,acquire_number()
    ].


find_numbers(Options) ->
    [[{"Verify found numbers"
      ,?_assertEqual(Limit, length(Results))
      }
     ,{"Verify results match queried prefix"
      ,?_assertEqual('true', lists:all(matcher(Prefix), Results))
      }
     ]
     || {Prefix, Limit} <- [{<<"301359">>, 5}
                           ,{<<"800">>, 2}
                           ],
        Results <- [knm_carriers:find(Prefix, Limit, Options)]
    ].

matcher(Prefix) ->
    fun (Result) ->
            Size = byte_size(Prefix),
            case kz_json:get_value(<<"number">>, Result) of
                <<"+1", Prefix:Size/binary, _/binary>> -> 'true';
                _Else -> 'false'
            end
    end.

acquire_number() ->
    N = <<"+14352154006">>,
    PhoneNumber = knm_phone_number:set_number(knm_phone_number:new(), N),
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
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{<<"auth_by_account">>, kz_json:new()}
            ,{'public_fields', kz_json:from_list([{?FEATURE_E911, E911}])}
            ],
    {'ok', N} = knm_number:create(?TEST_AVAILABLE_NUM, Props),
    PN = knm_number:phone_number(N),
    [{"Verify feature is properly set"
     ,?_assertEqual(E911, knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify we are keeping track of intermediary address_id"
     ,?_assertEqual(<<"421564943280637078">>
                   ,kz_json:get_value(<<"address_id">>, knm_phone_number:carrier_data(PN))
                   )
     }
    ].
