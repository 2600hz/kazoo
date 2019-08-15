%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_fsm_tests).

-include_lib("eunit/include/eunit.hrl").

changed_endpoints_test_() ->
    X = kz_json:from_list([{<<"_id">>, <<"x">>}]),
    Y = kz_json:from_list([{<<"_id">>, <<"y">>}]),

    [?_assertEqual({[], []}, acdc_agent_fsm:changed_endpoints([], []))
    ,?_assertEqual({[], []}, acdc_agent_fsm:changed_endpoints([X], [X]))

    ,?_assertEqual({[], []}, acdc_agent_fsm:changed_endpoints([X, Y], [X, Y]))
    ,?_assertEqual({[], []}, acdc_agent_fsm:changed_endpoints([X, Y], [Y, X]))

    ,?_assertEqual({[X], []}, acdc_agent_fsm:changed_endpoints([], [X]))
    ,?_assertEqual({[], [X]}, acdc_agent_fsm:changed_endpoints([X], []))

    ,?_assertEqual({[X, Y], []}, acdc_agent_fsm:changed_endpoints([], [X, Y]))
    ,?_assertEqual({[], [X, Y]}, acdc_agent_fsm:changed_endpoints([X, Y], []))

    ,?_assertEqual({[Y], []}, acdc_agent_fsm:changed_endpoints([X], [X, Y]))
    ,?_assertEqual({[], [X]}, acdc_agent_fsm:changed_endpoints([X, Y], [Y]))

    ,?_assertEqual({[X], [Y]}, acdc_agent_fsm:changed_endpoints([Y], [X]))
    ].
