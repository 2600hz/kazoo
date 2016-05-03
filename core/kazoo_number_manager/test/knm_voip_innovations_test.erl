%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_voip_innovations_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

-define(MODULE_TESTED, knm_voip_innovations).

-export([find_numbers/1
         ,get_number_data/0
         ,acquire_number/0
         ,disconnect_number/0
        ]).

api_test_() ->
    Options = [{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
               ,{<<"carriers">>, [<<"knm_voip_innovations">>]}
              ],
    [find_numbers(Options)
     ,get_number_data()
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

get_number_data() ->
    N = <<"4353198001">>,
    Result = ?MODULE_TESTED:get_number_data(N),
    [{"Verify right number is returned"
      ,?_assertEqual(<<"+1",N/binary>>, kz_json:get_value(<<"e164">>, Result))
     }
     ,{"Verify number status"
       ,?_assertEqual(<<"assigned">>, kz_json:get_value(<<"status">>, Result))
      }
     ,{"Verify number msg"
       ,?_assertEqual(<<"Number currently assigned to you with refid '' rewritten as '4353198001' to endpoint '13550'">>, kz_json:get_value(<<"msg">>, Result))
      }
     ,{"Verify debug code"
       ,?_assertEqual(<<"100">>, kz_json:get_value(<<"code">>, Result))
      }
     ,{"Verify expiration data"
       ,?_assertEqual(<<"2013-11-25T17:32:48.707">>, kz_json:get_value(<<"expireDate">>, Result))
      }
     ,{"Verify 411 state"
       ,?_assertEqual('false', kz_json:get_value(<<"has411">>, Result))
      }
     ,{"Verify 911 state"
       ,?_assertEqual('false', kz_json:get_value(<<"has911">>, Result))
      }
     ,{"Verify t38 state"
       ,?_assertEqual('true', kz_json:get_value(<<"t38">>, Result))
      }
     ,{"Verify CNAM state"
       ,?_assertEqual('true', kz_json:get_value(<<"cnam">>, Result))
     }
    ].

acquire_number() ->
    N = <<"+14352154006">>,
    PhoneNumber = knm_phone_number:set_number(knm_phone_number:new(), N),
    Number = knm_number:set_phone_number(knm_number:new(), PhoneNumber),
    Result = ?MODULE_TESTED:acquire_number(Number),
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
      ,?_assertException('throw', {'error','by_carrier',Number,{?MODULE_TESTED,Msg}}, ?MODULE_TESTED:disconnect_number(Number))
     }
    ].
