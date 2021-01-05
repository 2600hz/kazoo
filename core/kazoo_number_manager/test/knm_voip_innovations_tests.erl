%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2021, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_voip_innovations_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

api_test_() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_voip_innovations">>]}
              ,{'query_id', <<"QID">>}
              ],
    {setup
    ,fun () -> case knm_search:start_link() of
                   {'ok', Pid} -> Pid;
                   {'error', {'already_started', Pid}} -> Pid
               end
     end
    ,fun gen_server:stop/1
    ,fun (_ReturnOfSetup) ->
             [find_numbers(Options)
             ]
     end
    }.

find_numbers(Options0) ->
    [[{"Verify found numbers"
      ,?_assertEqual(Limit, length(Results))
      }
     ,{"Verify results match queried prefix"
      ,?_assertEqual('true', lists:all(matcher(Prefix), Results))
      }
     ]
     || {Prefix, Limit} <- [{<<"435">>, 2}
                           ,{<<"877">>, 1}
                           ],
        Options <- [[{quantity, Limit}
                    ,{prefix, Prefix}
                     | Options0
                    ]],
        Results <- [knm_search:find(Options)]
    ].

matcher(Prefix) ->
    fun (Result) ->
            Size = byte_size(Prefix),
            case kz_json:get_value(<<"number">>, Result) of
                <<"+1", Prefix:Size/binary, _/binary>> -> 'true';
                _Else -> 'false'
            end
    end.

acquire_number_test_() ->
    Num = <<"+14352154006">>,
    PN = knm_phone_number:from_number(Num),
    N = knm_number:set_phone_number(knm_number:new(), PN),
    Result = knm_voip_innovations:acquire_number(N),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify number is still one inputed"
     ,?_assertEqual(Num, knm_phone_number:number(knm_number:phone_number(Result)))
     }
    ].

disconnect_number_test_() ->
    N = <<"+14352154974">>,
    PhoneNumber = knm_phone_number:from_number(N),
    Number = knm_number:set_phone_number(knm_number:new(), PhoneNumber),
    Msg = <<"Number currently available">>,
    [{"Verify cannot release number not detained"
     ,?_assertException('throw', {'error','by_carrier',N,{'knm_voip_innovations',Msg}}, knm_voip_innovations:disconnect_number(Number))
     }
    ].
