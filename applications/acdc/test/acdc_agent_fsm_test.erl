%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_fsm_test).

-include_lib("eunit/include/eunit.hrl").

changed_endpoints_test() ->
    X = kz_json:from_list([{<<"_id">>, <<"x">>}]),
    Y = kz_json:from_list([{<<"_id">>, <<"y">>}]),

    ?assertEqual({[], []}, acdc_agent_fsm:changed_endpoints([], [])),
    ?assertEqual({[], []}, acdc_agent_fsm:changed_endpoints([X], [X])),

    ?assertEqual({[], []}, acdc_agent_fsm:changed_endpoints([X, Y], [X, Y])),
    ?assertEqual({[], []}, acdc_agent_fsm:changed_endpoints([X, Y], [Y, X])),

    ?assertEqual({[X], []}, acdc_agent_fsm:changed_endpoints([], [X])),
    ?assertEqual({[], [X]}, acdc_agent_fsm:changed_endpoints([X], [])),

    ?assertEqual({[X, Y], []}, acdc_agent_fsm:changed_endpoints([], [X, Y])),
    ?assertEqual({[], [X, Y]}, acdc_agent_fsm:changed_endpoints([X, Y], [])),

    ?assertEqual({[Y], []}, acdc_agent_fsm:changed_endpoints([X], [X, Y])),
    ?assertEqual({[], [X]}, acdc_agent_fsm:changed_endpoints([X, Y], [Y])),

    ?assertEqual({[X], [Y]}, acdc_agent_fsm:changed_endpoints([Y], [X])).
