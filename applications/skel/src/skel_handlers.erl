%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(skel_handlers).

-export([handle_route_req/2
        ,handle_route_win/2
        ]).

-include("skel.hrl").

-spec handle_route_req(kz_json:object(), kz_term:proplist()) -> any().
handle_route_req(JObj, Props) ->
    %% First validate the JSON
    'true' = kapi_route:req_v(JObj),

    %% Populate a kapps_call record from the route_req JSON
    Call = kapps_call:from_route_req(JObj),

    %% Do some logic to see if your app should respond
    %% If so, send a route_resp

    %% This is the AMQP queue to send the route_win, if appropriate
    Q = props:get_value('queue', Props),

    %% Create a response that will just park the call
    Resp = props:filter_undefined([{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                                  ,{<<"Method">>, <<"park">>}
                                   | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),

    %% Find who to send the response back to
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,

    %% Use an AMQP worker to send the response
    kz_amqp_worker:cast(Resp, Publisher),

    %% now we can cache the kapps_call record in case we receive the route_win
    %% we pass along APP_NAME to namespace our call from other apps in the same VM
    %% who may be caching it too
    kapps_call:cache(Call, ?APP_NAME).

%% receiving the route_win means we are in control of the call
-spec handle_route_win(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_route_win(JObj, _Props) ->
    %% Yay, we have control of the call
    %% First, validate the JSON
    'true' = kapi_route:win_v(JObj),

    %% Now we can fetch the kapps_call record from the cache
    case kapps_call:retrieve(kz_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME) of
        {'ok', Call} ->
            %% we augment the record with route_win JSON values
            process_call(kapps_call:from_route_win(JObj, Call));
        {'error', _R} ->
            lager:debug("unable to find call record during route_win")
    end.

-spec process_call(kapps_call:call()) -> 'ok'.
process_call(Call) ->
    %% First, we need to answer the call!
    kapps_call_command:answer(Call),

    %% Let's let the caller know we're here
    kapps_call_command:b_tts(<<"Hello caller, welcome to Kazoo">>, Call),

    %% Hangup the call
    kapps_call_command:hangup(Call).
