%%%-----------------------------------------------------------------------------
%%% @Copyright (C) 2010-2015, 2600Hz
%%% @doc Kazoo API Tests
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_api_tests).

%% EUNIT TESTING
-include_lib("eunit/include/eunit.hrl").

has_all_test_() ->
    Prop = [{<<"k1">>, <<"v1">>}
           ,{<<"k2">>, <<"v2">>}
           ,{<<"k3">>, <<"v3">>}
           ],
    Headers = [<<"k1">>, <<"k2">>, <<"k3">>],
    [?_assertEqual('true', kz_api:has_all(Prop, Headers))
    ,?_assertEqual('false', kz_api:has_all(Prop, [<<"k4">> | Headers]))
    ].

has_any_test_() ->
    Prop = [{<<"k1">>, <<"v1">>}
           ,{<<"k2">>, <<"v2">>}
           ,{<<"k3">>, <<"v3">>}
           ],
    Headers = [<<"k1">>, <<"k2">>, <<"k3">>],
    [?_assertEqual('true', kz_api:has_any(Prop, Headers))
    ,?_assertEqual('false', kz_api:has_any(Prop, [<<"k4">>]))
    ].

-ifdef(PERF).
-define(Q, <<"pid://<0.1.0>/2bdace1ec243711cc5236e01d5849d18.2bdace1ec243711cc5236e01d5849d18.2bdace1ec243711cc5236e01d5849d18.2bdace1ec243711cc5236e01d5849d180">>).
-define(REPEAT, 100000).

%% when i run locally:
%% kz_api_tests:pid in 0.879099s
%% kz_api_tests:pid2 in 0.057182s


horse_pid() ->
    horse:repeat(?REPEAT
                ,kapi:decode_pid(?Q)
                ).

horse_pid2() ->
    horse:repeat(?REPEAT
                ,kapi:decode_pid2(?Q)
                ).
-endif.
