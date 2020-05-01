%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_voip_innovations_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0
        ,setup/1
        ,cleanup/1
        ]).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0, fun setup/1, fun cleanup/1).

db_dependant() ->
    [api()
    ,acquire_number()
    ,disconnect_number()
    ].

setup(TestState) ->
    Pid = case knm_search:start_link() of
              {'ok', P} -> P;
              {'error', {'already_started', P}} -> P
          end,
    TestState#{search_pid => Pid}.

cleanup(#{search_pid := Pid}) ->
    gen_server:stop(Pid).

api() ->
    Options0 = [{'account_id', ?RESELLER_ACCOUNT_ID}
               ,{'carriers', [<<"knm_voip_innovations">>]}
               ,{'query_id', <<"QID">>}
               ],
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

acquire_number() ->
    Num = <<"+14352154006">>,
    PN = knm_phone_number:from_number(Num),
    Result = knm_voip_innovations:acquire_number(PN),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify number is still one inputed"
     ,?_assertEqual(Num, knm_phone_number:number(Result))
     }
    ].

disconnect_number() ->
    N = <<"+14352154974">>,
    PN = knm_phone_number:from_number(N),
    Msg = <<"Number currently available">>,
    [{"Verify cannot release number not detained"
     ,?_assertException('throw', {'error','by_carrier',N,{'knm_voip_innovations',Msg}}, knm_voip_innovations:disconnect_number(PN))
     }
    ].
