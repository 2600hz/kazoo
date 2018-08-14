%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
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
