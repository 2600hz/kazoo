%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016, Voxter Communications Inc.
%%% @doc
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_manager_test).

-include_lib("eunit/include/eunit.hrl").

-include("../src/acdc.hrl").
-include("../src/acdc_queue_manager.hrl").

-define(ACCOUNT_ID, <<"account_id">>).
-define(AGENT_ID, <<"agent_id">>).
-define(QUEUE_ID, <<"queue_id">>).
-define(SBRR_LOAD_TEST_ENABLED, 'true').

%%% =====
%%% TESTS
%%% =====

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
ss_size_empty_test_() ->
    SS = #strategy_state{agents=[]},
    [?_assertEqual(0, acdc_queue_manager:ss_size('mi', SS, 'free'))
    ,?_assertEqual(0, acdc_queue_manager:ss_size('mi', SS, 'logged_in'))].

ss_size_one_busy_test_() ->
    State = #state{strategy='mi'
                  ,strategy_state=#strategy_state{agents=[]}
                  },
    SS1 = acdc_queue_manager:update_strategy_with_agent(State, ?AGENT_ID, 0, [], 'add', 'undefined'),
    State1 = State#state{strategy_state=SS1},
    SS2 = acdc_queue_manager:update_strategy_with_agent(State1, ?AGENT_ID, 0, [], 'remove', 'busy'),
    [?_assertEqual(0, acdc_queue_manager:ss_size('mi', SS2, 'free'))
    ,?_assertEqual(1, acdc_queue_manager:ss_size('mi', SS2, 'logged_in'))].

%%--------------------------------------------------------------------
%% @doc Test a skills-based round robin situation where multiple agents may
%% be the candidate for a call, but there's an ideal mapping that
%% should be used.
%% @end
%%--------------------------------------------------------------------
sbrr_multiple_candidates_test_() ->
    S = create_state(),

    %% Add agents 3-5, then 1, 2
    Agents = [{<<"3">>, [<<"A">>, <<"B">>, <<"C">>]}
             ,{<<"4">>, [<<"A">>, <<"B">>, <<"C">>]}
             ,{<<"5">>, [<<"A">>, <<"B">>]}
             ,{<<"1">>, [<<"A">>]}
             ,{<<"2">>, [<<"A">>, <<"B">>, <<"C">>]}
             ],
    ExpectSkillMap = #{[] => sets:from_list([<<"2">>, <<"1">>, <<"5">>, <<"4">>, <<"3">>])
                      ,[<<"A">>] => sets:from_list([<<"2">>, <<"1">>, <<"5">>, <<"4">>, <<"3">>])
                      ,[<<"A">>, <<"B">>] => sets:from_list([<<"2">>, <<"5">>, <<"4">>, <<"3">>])
                      ,[<<"A">>, <<"B">>, <<"C">>] => sets:from_list([<<"2">>, <<"4">>, <<"3">>])
                      ,[<<"A">>, <<"C">>] => sets:from_list([<<"2">>, <<"4">>, <<"3">>])
                      ,[<<"B">>] => sets:from_list([<<"2">>, <<"5">>, <<"4">>, <<"3">>])
                      ,[<<"B">>, <<"C">>] => sets:from_list([<<"2">>, <<"4">>, <<"3">>])
                      ,[<<"C">>] => sets:from_list([<<"2">>, <<"4">>, <<"3">>])
                      },
    S1 = #state{strategy_state=#strategy_state{agents=#{rr_queue := RRQueue
                                                       ,skill_map := SkillMap
                                                       }}} = lists:foldl(fun add_agent/2, S, Agents),

    %% Add calls 1-4
    Calls = [{<<"1">>, [<<"A">>, <<"B">>]}
            ,{<<"2">>, [<<"A">>, <<"B">>]}
            ,{<<"3">>, [<<"A">>, <<"B">>, <<"C">>]}
            ,{<<"4">>, [<<"A">>]}
            ],
    ExpectAgentIdMap = #{<<"1">> => <<"4">>
                        ,<<"3">> => <<"2">>
                        ,<<"4">> => <<"3">>
                        ,<<"5">> => <<"1">>
                        },
    ExpectCallIdMap = #{<<"1">> => <<"5">>
                       ,<<"2">> => <<"3">>
                       ,<<"3">> => <<"4">>
                       ,<<"4">> => <<"1">>
                       },
    #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap
                                                  ,call_id_map := CallIdMap
                                                  }}} = lists:foldl(fun add_call/2, S1, Calls),

    [?_assertEqual([<<"3">>, <<"4">>, <<"5">>, <<"1">>, <<"2">>], pqueue4:to_list(RRQueue))
    ,?_assertEqual(ExpectSkillMap, SkillMap)
    ,?_assertEqual(ExpectAgentIdMap, AgentIdMap)
    ,?_assertEqual(ExpectCallIdMap, CallIdMap)
    ].

%%--------------------------------------------------------------------
%% @doc Test a skills-based round robin queue with a series of adds/removes
%% of calls and agents. Verifies the state as it mutates.
%% @end
%%--------------------------------------------------------------------
sbrr_multi_phase_test_() ->
    S = create_state(),

    %% Add agents 1-3
    Agents = [{<<"1">>, [<<"EN">>]}
             ,{<<"2">>, [<<"EN">>, <<"SP">>, <<"RF">>]}
             ,{<<"3">>, [<<"EN">>]}
             ],
    ExpectSkillMap = #{[] => sets:from_list([<<"3">>, <<"2">>, <<"1">>])
                      ,[<<"EN">>] => sets:from_list([<<"3">>, <<"2">>, <<"1">>])
                      ,[<<"EN">>, <<"RF">>] => sets:from_list([<<"2">>])
                      ,[<<"EN">>, <<"RF">>, <<"SP">>] => sets:from_list([<<"2">>])
                      ,[<<"EN">>, <<"SP">>] => sets:from_list([<<"2">>])
                      ,[<<"RF">>] => sets:from_list([<<"2">>])
                      ,[<<"RF">>, <<"SP">>] => sets:from_list([<<"2">>])
                      ,[<<"SP">>] => sets:from_list([<<"2">>])
                      },
    S1 = #state{strategy_state=#strategy_state{agents=#{rr_queue := RRQueue
                                                       ,skill_map := SkillMap
                                                       }}} = lists:foldl(fun add_agent/2, S, Agents),

    %% Add calls 1-5
    Calls = [{<<"1">>, [<<"EN">>, <<"RF">>]}
            ,{<<"2">>, [<<"EN">>]}
            ,{<<"3">>, [<<"SP">>, <<"RF">>]}
            ,{<<"4">>, [<<"EN">>]}
            ,{<<"5">>, [<<"EN">>]}
            ],
    ExpectAgentIdMap = #{<<"1">> => <<"2">>
                        ,<<"2">> => <<"1">>
                        ,<<"3">> => <<"4">>
                        },
    ExpectCallIdMap = #{<<"1">> => <<"2">>
                       ,<<"2">> => <<"1">>
                       ,<<"4">> => <<"3">>
                       },
    S2 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap
                                                       ,call_id_map := CallIdMap
                                                       }}} = lists:foldl(fun add_call/2, S1, Calls),

    %% Remove agent 2
    ExpectAgentIdMap1 = #{<<"1">> => <<"2">>
                         ,<<"3">> => <<"4">>
                         },
    ExpectCallIdMap1 = #{<<"2">> => <<"1">>
                        ,<<"4">> => <<"3">>
                        },
    ExpectSkillMap1 = #{[] => sets:from_list([<<"3">>, <<"1">>])
                       ,[<<"EN">>] => sets:from_list([<<"3">>, <<"1">>])
                       },
    S3 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap1
                                                       ,call_id_map := CallIdMap1
                                                       ,rr_queue := RRQueue1
                                                       ,skill_map := SkillMap1
                                                       }}} = remove_agent(<<"2">>, S2),

    %% Re-add agent 1 (shift to back of rr_queue)
    ExpectAgentIdMap2 = #{<<"1">> => <<"4">>
                         ,<<"3">> => <<"2">>
                         },
    ExpectCallIdMap2 = #{<<"2">> => <<"3">>
                        ,<<"4">> => <<"1">>
                        },
    ExpectSkillMap2 = #{[] => sets:from_list([<<"3">>, <<"1">>])
                       ,[<<"EN">>] => sets:from_list([<<"3">>, <<"1">>])
                       },
    S4 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap2
                                                       ,call_id_map := CallIdMap2
                                                       ,rr_queue := RRQueue2
                                                       ,skill_map := SkillMap2
                                                       }}} = add_agent({<<"1">>, [<<"EN">>]}, S3),

    %% Remove calls 1, 2, 4
    RemoveCalls = [<<"1">>, <<"2">>, <<"4">>],
    ExpectAgentIdMap3 = #{<<"3">> => <<"5">>},
    ExpectCallIdMap3 = #{<<"5">> => <<"3">>},
    S5 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap3
                                                       ,call_id_map := CallIdMap3
                                                       }}} = lists:foldl(fun remove_call/2, S4, RemoveCalls),

    %% Add calls 6, 7
    Calls1 = [{<<"6">>, [<<"DE">>]}
             ,{<<"7">>, [<<"EN">>, <<"RF">>]}
             ],
    ExpectAgentIdMap4 = #{<<"3">> => <<"5">>},
    ExpectCallIdMap4 = #{<<"5">> => <<"3">>},
    S6 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap4
                                                       ,call_id_map := CallIdMap4
                                                       }}} = lists:foldl(fun add_call/2, S5, Calls1),

    %% Remove agent 3
    ExpectAgentIdMap5 = #{<<"1">> => <<"5">>},
    ExpectCallIdMap5 = #{<<"5">> => <<"1">>},
    ExpectSkillMap5 = #{[] => sets:from_list([<<"1">>])
                       ,[<<"EN">>] => sets:from_list([<<"1">>])
                       },
    S7 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap5
                                                       ,call_id_map := CallIdMap5
                                                       ,rr_queue := RRQueue5
                                                       ,skill_map := SkillMap5
                                                       }}} = remove_agent(<<"3">>, S6),

    %% Add agents 2, 4 (different skills this time)
    Agents1 = [{<<"2">>, [<<"EN">>, <<"SP">>, <<"RF">>]}
              ,{<<"4">>, [<<"EN">>, <<"DE">>, <<"RF">>]}
              ],
    ExpectAgentIdMap6 = #{<<"1">> => <<"5">>
                         ,<<"2">> => <<"3">>
                         ,<<"4">> => <<"6">>
                         },
    ExpectCallIdMap6 = #{<<"3">> => <<"2">>
                        ,<<"5">> => <<"1">>
                        ,<<"6">> => <<"4">>
                        },
    ExpectSkillMap6 = #{[] => sets:from_list([<<"4">>, <<"2">>, <<"1">>])
                       ,[<<"DE">>] => sets:from_list([<<"4">>])
                       ,[<<"DE">>, <<"EN">>] => sets:from_list([<<"4">>])
                       ,[<<"DE">>, <<"EN">>, <<"RF">>] => sets:from_list([<<"4">>])
                       ,[<<"DE">>, <<"RF">>] => sets:from_list([<<"4">>])
                       ,[<<"EN">>] => sets:from_list([<<"4">>, <<"2">>, <<"1">>])
                       ,[<<"EN">>, <<"RF">>] => sets:from_list([<<"4">>, <<"2">>])
                       ,[<<"EN">>, <<"RF">>, <<"SP">>] => sets:from_list([<<"2">>])
                       ,[<<"EN">>, <<"SP">>] => sets:from_list([<<"2">>])
                       ,[<<"RF">>] => sets:from_list([<<"4">>,<<"2">>])
                       ,[<<"RF">>, <<"SP">>] => sets:from_list([<<"2">>])
                       ,[<<"SP">>] => sets:from_list([<<"2">>])
                       },
    S8 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap6
                                                       ,call_id_map := CallIdMap6
                                                       ,rr_queue := RRQueue6
                                                       ,skill_map := SkillMap6
                                                       }}} = lists:foldl(fun add_agent/2, S7, Agents1),

    %% Remove calls 3, 5, 6
    RemoveCalls1 = [<<"3">>, <<"5">>, <<"6">>],
    ExpectAgentIdMap7 = #{<<"2">> => <<"7">>},
    ExpectCallIdMap7 = #{<<"7">> => <<"2">>},
    S9 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap7
                                                       ,call_id_map := CallIdMap7
                                                       }}} = lists:foldl(fun remove_call/2, S8, RemoveCalls1),

    %% Call 7 exits, returns with different skills
    ExpectAgentIdMap8 = #{<<"1">> => <<"7">>},
    ExpectCallIdMap8 = #{<<"7">> => <<"1">>},
    S10 = #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap8
                                                        ,call_id_map := CallIdMap8
                                                        }}} = add_call({<<"7">>, [<<"EN">>]}, remove_call(<<"7">>, S9)),

    %% Add call 8
    ExpectAgentIdMap9 = #{<<"1">> => <<"7">>
                         ,<<"4">> => <<"8">>
                         },
    ExpectCallIdMap9 = #{<<"7">> => <<"1">>
                        ,<<"8">> => <<"4">>
                        },
    #state{strategy_state=#strategy_state{agents=#{agent_id_map := AgentIdMap9
                                                  ,call_id_map := CallIdMap9
                                                  }}} = add_call({<<"8">>, [<<"DE">>]}, S10),

    [?_assertEqual([<<"1">>, <<"2">>, <<"3">>], pqueue4:to_list(RRQueue))
    ,?_assertEqual(ExpectSkillMap, SkillMap)
    ,?_assertEqual(ExpectAgentIdMap, AgentIdMap)
    ,?_assertEqual(ExpectCallIdMap, CallIdMap)
    ,?_assertEqual([<<"1">>, <<"3">>], pqueue4:to_list(RRQueue1))
    ,?_assertEqual(ExpectAgentIdMap1, AgentIdMap1)
    ,?_assertEqual(ExpectCallIdMap1, CallIdMap1)
    ,?_assertEqual(ExpectSkillMap1, SkillMap1)
    ,?_assertEqual([<<"3">>, <<"1">>], pqueue4:to_list(RRQueue2))
    ,?_assertEqual(ExpectAgentIdMap2, AgentIdMap2)
    ,?_assertEqual(ExpectCallIdMap2, CallIdMap2)
    ,?_assertEqual(ExpectSkillMap2, SkillMap2)
    ,?_assertEqual(ExpectAgentIdMap3, AgentIdMap3)
    ,?_assertEqual(ExpectCallIdMap3, CallIdMap3)
    ,?_assertEqual(ExpectAgentIdMap4, AgentIdMap4)
    ,?_assertEqual(ExpectCallIdMap4, CallIdMap4)
    ,?_assertEqual([<<"1">>], pqueue4:to_list(RRQueue5))
    ,?_assertEqual(ExpectAgentIdMap5, AgentIdMap5)
    ,?_assertEqual(ExpectCallIdMap5, CallIdMap5)
    ,?_assertEqual(ExpectSkillMap5, SkillMap5)
    ,?_assertEqual([<<"1">>, <<"2">>, <<"4">>], pqueue4:to_list(RRQueue6))
    ,?_assertEqual(ExpectAgentIdMap6, AgentIdMap6)
    ,?_assertEqual(ExpectCallIdMap6, CallIdMap6)
    ,?_assertEqual(ExpectSkillMap6, SkillMap6)
    ,?_assertEqual(ExpectAgentIdMap7, AgentIdMap7)
    ,?_assertEqual(ExpectCallIdMap7, CallIdMap7)
    ,?_assertEqual(ExpectAgentIdMap8, AgentIdMap8)
    ,?_assertEqual(ExpectCallIdMap8, CallIdMap8)
    ,?_assertEqual(ExpectAgentIdMap9, AgentIdMap9)
    ,?_assertEqual(ExpectCallIdMap9, CallIdMap9)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc Runs a load test on the sbrr strategy with a given number of
%% agents, calls, and unique skills. Each agent and call has a random
%% assortment of skills assigned to it.
%% @end
%%--------------------------------------------------------------------
sbrr_load_test_() ->
    sbrr_load_test(?SBRR_LOAD_TEST_ENABLED).

sbrr_load_test('false') -> [];
sbrr_load_test('true') ->
    NumAgents = 1000,
    NumCalls = 500,
    NumSkills = 10,
    S = create_state(),

    %% Generate skills
    Start = kz_time:now(),
    SkillPool = apply_n({fun(Skills) ->
                                 Skill = <<"skill", (kz_term:to_binary(length(Skills)))/binary>>,
                                 [Skill | Skills]
                         end, []}
                       ,[]
                       ,NumSkills
                       ),
    ?debugFmt("skill generation time: ~b", [kz_time:elapsed_ms(Start)]),

    %% Add agents
    Start1 = kz_time:now(),
    AgentPool = apply_n({fun(Agents) ->
                                 AgentId = <<"agent", (kz_term:to_binary(length(Agents)))/binary>>,
                                 Skills = lists:filter(fun(_) ->
                                                               rand:uniform(2) > 1
                                                       end
                                                      ,SkillPool
                                                      ),
                                 [{AgentId, Skills} | Agents]
                         end, []}
                       ,[]
                       ,NumAgents
                       ),
    ?debugFmt("agent generation time: ~b", [kz_time:elapsed_ms(Start1)]),
    Start2 = kz_time:now(),
    S1 = lists:foldl(fun add_agent/2, S, AgentPool),
    ?debugFmt("agent average add time: ~f", [kz_time:elapsed_ms(Start2) / NumAgents]),

    %% Add calls
    Start3 = kz_time:now(),
    CallPool = apply_n({fun(Calls) ->
                                CallId = <<"call", (kz_term:to_binary(length(Calls)))/binary>>,
                                Skills = lists:filter(fun(_) ->
                                                              rand:uniform(2) > 1
                                                      end
                                                     ,SkillPool
                                                     ),
                                [{CallId, Skills} | Calls]
                        end, []}
                      ,[]
                      ,NumCalls
                      ),
    ?debugFmt("call generation time: ~b", [kz_time:elapsed_ms(Start3)]),
    Start4 = kz_time:now(),
    lists:foldl(fun add_call/2, S1, CallPool),
    ?debugFmt("call average add/assign time: ~f", [kz_time:elapsed_ms(Start4) / NumCalls]),

    [].

apply_n(_, State, 0) -> State;
apply_n({Fun, Args}, State, N) ->
    apply_n({Fun, Args}, apply(Fun, [State | Args]), N-1).

%%--------------------------------------------------------------------
%% @private
%% @doc Create a state for skills-based round robin tests.
%% @end
%%--------------------------------------------------------------------
create_state() ->
    SS = #strategy_state{agents=#{agent_id_map => #{}
                                 ,call_id_map => #{}
                                 ,rr_queue => pqueue4:new()
                                 ,skill_map => #{}
                                 }
                        },
    {'state', dict:new(), ?ACCOUNT_ID, ?QUEUE_ID, 'undefined', 'sbrr', SS, 'true', 'undefined', [], [], #{}, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc Add an agent with specified skills to an sbrr queue state.
%% @end
%%--------------------------------------------------------------------
add_agent({AgentId, Skills}, State) ->
    SS = acdc_queue_manager:update_strategy_with_agent(State, AgentId, 0, Skills, 'add', 'undefined'),
    State#state{strategy_state=SS}.

%%--------------------------------------------------------------------
%% @private
%% @doc Remove an agent from an sbrr queue state.
%% @end
%%--------------------------------------------------------------------
remove_agent(AgentId, State) ->
    SS = acdc_queue_manager:update_strategy_with_agent(State, AgentId, 0, [], 'remove', 'busy'),
    State#state{strategy_state=SS}.

%%--------------------------------------------------------------------
%% @private
%% @doc Add a call with specified required skills to an sbrr queue state.
%% @end
%%--------------------------------------------------------------------
add_call({CallId, Skills}, #state{current_member_calls=Calls}=State) ->
    %% Sort skills because we aren't going through add_queue_member which would
    %% sort for us
    Skills1 = lists:sort(Skills),
    Call = kapps_call:set_call_id(CallId, kapps_call:kvs_store(?ACDC_REQUIRED_SKILLS_KEY, Skills1, kapps_call:new())),
    Calls1 = Calls ++ [{0, Call}],
    State1 = #state{strategy_state=#strategy_state{agents=SBRRSS}=SS} = State#state{current_member_calls=Calls1},
    SBRRSS1 = acdc_queue_manager:reseed_sbrrss_maps(SBRRSS, acdc_queue_manager:ss_size('sbrr', SS, 'free'), Calls1),
    State1#state{strategy_state=SS#strategy_state{agents=SBRRSS1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Remove a call from an sbrr queue state.
%% @end
%%--------------------------------------------------------------------
remove_call(CallId, #state{current_member_calls=Calls}=State) ->
    Calls1 = lists:filter(fun({_, Call1}) ->
                                  kapps_call:call_id(Call1) =/= CallId
                          end, Calls),
    State1 = #state{strategy_state=#strategy_state{agents=SBRRSS}=SS} = State#state{current_member_calls=Calls1},
    SBRRSS1 = acdc_queue_manager:reseed_sbrrss_maps(SBRRSS, acdc_queue_manager:ss_size('sbrr', SS, 'free'), Calls1),
    State1#state{strategy_state=SS#strategy_state{agents=SBRRSS1}}.
