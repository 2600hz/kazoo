%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_voip_innovations_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_number_manager/include/knm.hrl").

api_test_() ->
    Options = [{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
               ,{<<"carriers">>, [<<"knm_voip_innovations">>]}
              ],
    [find_numbers(Options)
     ,acquire_number()
     ,disconnect_number()
    ].


find_numbers(Options) ->
    Limit = 2,
    Prefix = <<"435">>,
    MatchPrefix =
        fun (Result) ->
                Size = byte_size(Prefix),
                Number = kz_json:get_value(<<"number">>, Result),
                case Number of
                    <<"+1", Prefix:Size/binary, _/binary>> -> 'true';
                    _Else -> 'false'
                end
        end,
    Results = knm_carriers:find(Prefix, Limit, Options),
    [{"Verify found numbers"
      ,?_assertEqual(Limit, length(Results))
     }
     ,{"Verify results match queried prefix"
       ,?_assertEqual('true', lists:all(MatchPrefix, Results))
      }
    ].

acquire_number() ->
    N = <<"+14352154006">>,
    PhoneNumber = knm_phone_number:set_number(knm_phone_number:new(), N),
    Number = knm_number:set_phone_number(knm_number:new(), PhoneNumber),
    Result = knm_voip_innovations:acquire_number(Number),
    [{"Verify number is still one inputed"
      ,?_assertEqual(N, knm_phone_number:number(knm_number:phone_number(Result)))
     }
    ].

disconnect_number() ->
    N = <<"+14352154974">>,
    PhoneNumber = knm_phone_number:set_number(knm_phone_number:new(), N),
    Number = knm_number:set_phone_number(knm_number:new(), PhoneNumber),
    Msg = <<"Number currently available">>,
    [{"Verify cannot release number not detained"
      ,?_assertException('throw', {'error','by_carrier',Number,{'knm_voip_innovations',Msg}}, knm_voip_innovations:disconnect_number(Number))
     }
    ].
