%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(pivot_handlers).

-export([handle_route_req/2
         ,handle_route_win/2
         ,handle_pivot_req/2
        ]).

-include("pivot.hrl").

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj).

%% receiving the route_win means we are in control of the call
-spec handle_route_win(wh_json:object(), wh_proplist()) -> any().
handle_route_win(JObj, _Props) ->
    'true' = wapi_route:win_v(JObj),

    %% Create the call data structure
    _Call = whapps_call:from_json(JObj),
    'ok'.

-spec handle_pivot_req(wh_json:object(), wh_proplist()) -> any().
handle_pivot_req(JObj, _Props) ->
    'true' = wapi_pivot:req_v(JObj),

    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    pivot_calls_sup:new(Call, JObj).
