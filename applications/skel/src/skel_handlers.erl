%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(skel_handlers).

-export([handle_route_req/2
         ,handle_route_win/2
        ]).

-include("skel.hrl").

-spec handle_route_req(wh_json:object(), wh_proplist()) -> _.
handle_route_req(JObj, Props) ->
    %% First validate the JSON
    'true' = wapi_route:req_v(JObj),

    %% Populate a whapps_call record from the route_req JSON
    Call = whapps_call:from_route_req(JObj),

    %% Do some logic to see if your app should respond
    %% If so, send a route_resp

    %% This is the AMQP queue to send the route_win, if appropriate
    Q = props:get_value('queue', Props),

    %% Create a response that will just park the call
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),

    %% Find who to send the response back to
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,

    %% Use an AMQP worker to send the response
    whapps_util:amqp_pool_send(Resp, Publisher),

    %% now we can cache the whapps_call record in case we receive the route_win
    %% we pass along APP_NAME to namespace our call from other apps in the same VM
    %% who may be caching it too
    whapps_call:cache(Call, ?APP_NAME).

%% receiving the route_win means we are in control of the call
-spec handle_route_win(wh_json:object(), wh_proplist()) -> 'ok'.
handle_route_win(JObj, _Props) ->
    %% Yay, we have control of the call
    %% First, validate the JSON
    'true' = wapi_route:win_v(JObj),

    %% Now we can fetch the whapps_call record from the cache
    case whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME) of
        {'ok', Call} ->
            %% we augment the record with route_win JSON values
            process_call(whapps_call:from_route_win(JObj, Call));
        {'error', _R} ->
            lager:debug("unable to find call record during route_win")
    end.

-spec process_call(whapps_call:call()) -> 'ok'.
process_call(Call) ->
    %% First, we need to answer the call!
    whapps_call_command:answer(Call),

    %% Let's let the caller know we're here
    whapps_call_command:b_tts(<<"Hello caller, welcome to Kazoo">>, Call),

    %% Hangup the call
    whapps_call_command:hangup(Call).
