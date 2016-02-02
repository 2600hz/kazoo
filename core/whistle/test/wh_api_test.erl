%%%-------------------------------------------------------------------
%%% @Copyright (C) 2010-2015, 2600Hz
%%% @doc
%%% Whistle API Tests
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_api_test).

%% EUNIT TESTING
-include_lib("eunit/include/eunit.hrl").

has_all_test() ->
    Prop = [{<<"k1">>, <<"v1">>}
            ,{<<"k2">>, <<"v2">>}
            ,{<<"k3">>, <<"v3">>}
           ],
    Headers = [<<"k1">>, <<"k2">>, <<"k3">>],
    ?assertEqual('true', wh_api:has_all(Prop, Headers)),
    ?assertEqual('false', wh_api:has_all(Prop, [<<"k4">> | Headers])).

has_any_test() ->
    Prop = [{<<"k1">>, <<"v1">>}
            ,{<<"k2">>, <<"v2">>}
            ,{<<"k3">>, <<"v3">>}
           ],
    Headers = [<<"k1">>, <<"k2">>, <<"k3">>],
    ?assertEqual('true', wh_api:has_any(Prop, Headers)),
    ?assertEqual('false', wh_api:has_any(Prop, [<<"k4">>])).
