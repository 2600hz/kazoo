%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_bandwidth2_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_number_manager/src/knm.hrl").

api_test_() ->
    Options = [{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
               ,{<<"carriers">>, [<<"knm_bandwidth2">>]}
              ],
    [find_numbers(Options)
    ,find_tollfree_numbers(Options)
    ,acquire_number()
    ].


find_numbers(Options) ->
    Limit = 2,
    Prefix = <<"973">>,
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

find_tollfree_numbers(Options) ->
    Limit = 15,
    Prefix = <<"85">>,
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
    N = <<"+19734096113">>,
    Number = knm_number(N),
    Result = knm_bandwidth2:acquire_number(Number),
    [{"Verify number is still one inputed"
      ,?_assertEqual(N, knm_phone_number:number(knm_number:phone_number(Result)))
     }
    ].

%%% Internals

knm_number(N=?NE_BINARY) ->
    PhoneNumber = knm_phone_number:set_number(knm_phone_number:new(), N),
    knm_number:set_phone_number(knm_number:new(), PhoneNumber).

%%% End of Module
