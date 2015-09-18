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
