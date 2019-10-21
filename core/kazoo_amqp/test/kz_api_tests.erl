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

get_event_type_test_() ->
    EventCategory = {<<"Event-Category">>, <<"call">>},
    EventName = {<<"Event-Name">>, <<"CHANNEL_CONNECTED">>},
    [?_assertEqual({undefined,undefined}, kz_api:get_event_type([]))
    ,?_assertEqual({undefined,undefined}, kz_api:get_event_type(kz_json:from_list([])))
    ,?_assertEqual({<<"call">>,undefined}, kz_api:get_event_type([EventCategory]))
    ,?_assertEqual({<<"call">>,undefined}, kz_api:get_event_type(kz_json:from_list([EventCategory])))
    ,?_assertEqual({undefined,<<"CHANNEL_CONNECTED">>}, kz_api:get_event_type([EventName]))
    ,?_assertEqual({undefined,<<"CHANNEL_CONNECTED">>}, kz_api:get_event_type(kz_json:from_list([EventName])))
    ,?_assertEqual({<<"call">>,<<"CHANNEL_CONNECTED">>}, kz_api:get_event_type([EventCategory,EventName]))
    ,?_assertEqual({<<"call">>,<<"CHANNEL_CONNECTED">>}, kz_api:get_event_type(kz_json:from_list([EventCategory,EventName])))
    ].
