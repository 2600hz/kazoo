%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_move_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

move_to_child_test_() ->
    {'ok', N} = knm_number:move(?TEST_AVAILABLE_NUM, ?CHILD_ACCOUNT_ID),
    PN = knm_number:phone_number(N),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"verify assigned_to is child account"
     ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ].

move_changing_public_fields_test_() ->
    Key = <<"my_key">>,
    Fields = [{<<"a">>, <<"bla">>}
             ,{Key, 42}
             ],
    Options = [{public_fields, kz_json:from_list(Fields)}
               |knm_number_options:default()
              ],
    {ok, N0} = knm_number:get(?TEST_AVAILABLE_NUM),
    {ok, N} = knm_number:move(?TEST_AVAILABLE_NUM, ?RESELLER_ACCOUNT_ID, Options),
    [?_assert(knm_phone_number:is_dirty(knm_number:phone_number(N)))
    ,{"verify a public key is set"
     ,?_assertEqual(<<"my string">>
                   ,kz_json:get_value(Key, knm_number:to_public_json(N0))
                   )
     }
    ,{"verify that key got updated"
     ,?_assertEqual(42
                   ,kz_json:get_value(Key, knm_number:to_public_json(N))
                   )
     }
    ,{"verify another key was added"
     ,?_assertEqual(<<"bla">>
                   ,kz_json:get_value(<<"a">>, knm_number:to_public_json(N))
                   )
     }
    ,{"verify that that other key is really new"
     ,?_assertEqual(undefined
                   ,kz_json:get_value(<<"a">>, knm_number:to_public_json(N0))
                   )
     }
    ].
