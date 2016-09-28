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
     || {Prefix, Limit} <- [{<<"301359">>, 36}
                           ,{<<"800">>, 260}
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
