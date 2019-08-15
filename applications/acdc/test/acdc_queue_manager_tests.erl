%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016, Voxter Communications Inc.
%%% @doc
%%% @author Daniel Finke
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
%% @doc
%% @end
%%------------------------------------------------------------------------------
ss_size_empty_test_() ->
    SS = #strategy_state{agents=[]},
    [?_assertEqual(0, acdc_queue_manager:ss_size(SS, 'free'))
    ,?_assertEqual(0, acdc_queue_manager:ss_size(SS, 'logged_in'))].

ss_size_one_busy_test_() ->
    SS = #strategy_state{agents=[]},
    SS1 = acdc_queue_manager:update_strategy_with_agent('mi', SS, ?AGENT_ID, 'add', 'undefined'),
    SS2 = acdc_queue_manager:update_strategy_with_agent('mi', SS1, ?AGENT_ID, 'remove', 'busy'),
    [?_assertEqual(0, acdc_queue_manager:ss_size(SS2, 'free'))
    ,?_assertEqual(1, acdc_queue_manager:ss_size(SS2, 'logged_in'))].
