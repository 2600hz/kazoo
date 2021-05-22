%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016, Voxter Communications Inc.
%%% @doc
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_manager_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/acdc.hrl").
-include("../src/acdc_queue_manager.hrl").

-define(AGENT_ID, <<"agent_id">>).

%%% =====
%%% TESTS
%%% =====

%%------------------------------------------------------------------------------
%% @doc Test the reported count of agents when the "most idle" strategy state is
%% empty.
%% @end
%%------------------------------------------------------------------------------
agent_count_0_agents_test_() ->
    State = #state{strategy='mi'
                  ,strategy_state=#strategy_state{agents=[]}
                  },
    [?_assertEqual(0, acdc_queue_manager:assignable_agent_count(State))
    ,?_assertEqual(0, acdc_queue_manager:agent_count(State))].

%%------------------------------------------------------------------------------
%% @doc Test the reported count of agents when the "most idle" strategy state
%% has an agent added and then flagged as busy.
%% @end
%%------------------------------------------------------------------------------
agent_count_1_busy_agent_test_() ->
    State = #state{strategy='mi'
                  ,strategy_state=#strategy_state{agents=[]}
                  },
    SS1 = acdc_queue_manager:update_strategy_with_agent(State, ?AGENT_ID, 'available'),
    State1 = State#state{strategy_state=SS1},
    SS2 = acdc_queue_manager:update_strategy_with_agent(State1, ?AGENT_ID, 'busy'),
    State2 = State1#state{strategy_state=SS2},
    [?_assertEqual(0, acdc_queue_manager:assignable_agent_count(State2))
    ,?_assertEqual(1, acdc_queue_manager:agent_count(State2))].
