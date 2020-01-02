%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_carriers_find_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0]).

knm_carriers_find_test_() ->
    knm_test_util:start_db(fun db_dependant/0).

db_dependant() ->
    [carrier_info()
    ,is_number_billable()
    ,check()
    ,find_local()
    ,find_other()
    ].

carrier_info() ->
    InfoJObj1 = knm_carriers:info(?MASTER_ACCOUNT_ID, ?RESELLER_ACCOUNT_ID, ?RESELLER_ACCOUNT_ID),
    InfoJObj2 = knm_carriers:info(?MASTER_ACCOUNT_ID, ?CHILD_ACCOUNT_ID, ?RESELLER_ACCOUNT_ID),
    InfoJObj3 = knm_carriers:info(?MASTER_ACCOUNT_ID, ?CHILD_ACCOUNT_ID, undefined),
    InfoJObj4 = knm_carriers:info(?MASTER_ACCOUNT_ID, ?CHILD_ACCOUNT_ID, ?CHILD_ACCOUNT_ID),
    InfoJObj5 = knm_carriers:info(?RESELLER_ACCOUNT_ID, ?RESELLER_ACCOUNT_ID, ?RESELLER_ACCOUNT_ID),
    InfoJObj6 = knm_carriers:info(?CHILD_ACCOUNT_ID, ?CHILD_ACCOUNT_ID, ?CHILD_ACCOUNT_ID),
    [?_assertEqual(10, kz_json:get_value(<<"maximal_prefix_length">>, InfoJObj1))
    ,?_assertEqual(10, kz_json:get_value(<<"maximal_prefix_length">>, InfoJObj2))
    ,?_assertEqual(10, kz_json:get_value(<<"maximal_prefix_length">>, InfoJObj3))
    ,?_assertEqual(3, kz_json:get_value(<<"maximal_prefix_length">>, InfoJObj4))
    ,?_assert(lists:member(<<"local">>, kz_json:get_value(<<"usable_carriers">>, InfoJObj4)))
    ,?_assert(sets:is_subset(sets:from_list([?NUMBER_STATE_IN_SERVICE
                                            ,?NUMBER_STATE_RESERVED
                                            ,?NUMBER_STATE_AVAILABLE
                                            ])
                            ,sets:from_list(kz_json:get_value(<<"usable_creation_states">>, InfoJObj4))
                            )
             )
    ,?_assertEqual([?NUMBER_STATE_IN_SERVICE, ?NUMBER_STATE_RESERVED]
                  ,lists:usort(kz_json:get_value(<<"usable_creation_states">>, InfoJObj5))
                  )
    ,?_assertEqual([], kz_json:get_value(<<"usable_creation_states">>, InfoJObj6))
    ].

is_number_billable() ->
    {ok, PN1} = knm_number:get(?TEST_OLD1_NUM),
    PN2 = knm_phone_number:set_module_name(PN1, <<"knm_bandwidth2">>),
    PN3 = knm_phone_number:set_module_name(PN1, <<"wnm_pacwest">>),
    [?_assertEqual(false, knm_carriers:is_number_billable(PN1))
    ,?_assertEqual(true, knm_carriers:is_number_billable(PN2))
    ,?_assertEqual(true, knm_carriers:is_number_billable(PN3))
    ].

check() ->
    Nums = [?TEST_AVAILABLE_NUM
           ,?TEST_IN_SERVICE_NUM
           ,?TEST_IN_SERVICE_WITH_HISTORY_NUM
           ,?TEST_EXISTING_TOLL
           ],
    [?_assertEqual(kz_json:new(), knm_carriers:check([]))
    ,{"Checking numbers against unconfigured carriers"
     ,?_assertEqual(#{?TEST_AVAILABLE_NUM => <<"error">>
                     ,?TEST_IN_SERVICE_NUM => <<"error">>
                     ,?TEST_IN_SERVICE_WITH_HISTORY_NUM => <<"error">>
                     ,?TEST_EXISTING_TOLL => <<"error">>
                     }
                   ,kz_json:to_map(knm_carriers:check(Nums))
                   )
     }
    ].

find_local() ->
    [{"Finding local numbers not supported"
     ,?_assertMatch({'error', 'not_available'}, knm_local:find_numbers(<<"415">>, 1, []))
     }
    ].

find_other() ->
    Options = [{carriers, [?CARRIER_OTHER]}
              ,{'query_id', <<"QID">>}
              ],
    {setup
    ,fun () -> {'ok', Pid} = knm_search:start_link(), Pid end
    ,fun gen_server:stop/1
    ,fun (_ReturnOfSetup) ->
             [find_no_phonebook(Options)
             ,?_assertEqual([], knm_search:find([{prefix, <<"415">>}]))
             ,find_numbers(Options)
             ,find_blocks(Options)
             ]
     end
    }.

find_no_phonebook(Options0) ->
    Prefix = <<"415">>,
    Options = [{prefix, Prefix}
               |Options0
              ],
    [{"Verify no phonebook url yields no results"
     ,?_assertEqual([], knm_search:find(Options))
     }
    ].

find_blocks(Options0) ->
    Prefix = <<"415">>,
    Limit = 10,
    Options = [{'phonebook_url', ?BLOCK_PHONEBOOK_URL}
              ,{'blocks', 'true'}
              ,{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'quantity', Limit}
              ,{prefix, Prefix}
               | Options0
              ],

    {'bulk', [StartNumber, EndNumber]=Numbers} = knm_other:find_numbers(Prefix, Limit, Options),
    [StartRep, EndRep]=Results = knm_search:find(Options),

    [?_assertEqual(length(Numbers), length(Results))
    ,?_assertEqual(?START_BLOCK, kz_json:get_value(<<"number">>, StartRep))
    ,?_assertEqual(?END_BLOCK, kz_json:get_value(<<"number">>, EndRep))
    ,?_assertEqual(?START_BLOCK, element(1,element(2,StartNumber)))
    ,?_assertEqual(?END_BLOCK, element(1,element(2,EndNumber)))
    ,?_assertEqual(?CARRIER_OTHER, kz_term:to_binary(element(2,element(2,StartNumber))))
    ,?_assertEqual(?CARRIER_OTHER, kz_term:to_binary(element(2,element(2,EndNumber))))
    ].

find_numbers(Options0) ->
    Prefix = <<"415">>,
    Limit = 10,
    Options = [{'phonebook_url', ?NUMBER_PHONEBOOK_URL}
              ,{'account_id', ?MASTER_ACCOUNT_ID}
              ,{'quantity', 10}
              ,{'prefix', Prefix}
               | Options0
              ],

    Results = knm_search:find(Options),

    [{"Verify results returned is the expected amount"
     ,?_assertEqual(Limit, length(Results))
     }
     | verify_number_results(Results)
    ].

verify_number_results(Results) ->
    {Tests, _} = lists:foldl(fun verify_number_result/2, {[], 0}, Results),
    Tests.

verify_number_result(Result, {Tests, N}) ->
    {[{"Verify result DID"
      ,?_assertEqual(<<"+1415886790", (N+$0)>>
                    ,kz_json:get_value(<<"number">>, Result)
                    )
      }
      | Tests
     ]
    ,N+1
    }.
