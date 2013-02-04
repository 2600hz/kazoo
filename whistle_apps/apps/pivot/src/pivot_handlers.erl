%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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

-spec handle_route_req(wh_json:json_object(), wh_proplist()) -> any().
handle_route_req(JObj, Props) ->
    true = wapi_route:req_v(JObj),
    _Q = props:get_value(queue, Props),

    %% sends route_resp with "park" by default
    ok.

%% receiving the route_win means we are in control of the call
-spec handle_route_win(wh_json:json_object(), wh_proplist()) -> any().
handle_route_win(JObj, _Props) ->
    true = wapi_route:win_v(JObj),

    %% Create the call data structure
    Call = whapps_call:from_json(JObj),

    %% 1) because some commands can alter a call's state, we always return the new state
    %% 2) by convention b_function is the blocking version of the function.
    %% so, play/2 would send the play command and immediate continue to the hangup below.
    %% instead, we prefer to wait until the media is done playing before continuing,
    %% so we call b_play to block us until the media is finished.
    %% 3) we explicity match {ok, Call1} to indicate the playing finished without error;
    %% if we wanted to handle when unexpected things happen (like when the caller hangs up
    %% while the audio is still playing, we would use a case and match for
    %% {error, hungup, Call2} and do cleanup of some kind.
    %% since we don't care, we'll just match successful completion of the audio and crash
    %% otherwise (why send a hangup to a call that's hungup).
    _ = whapps_call_command:b_play(<<"local_stream://some/media.mp3">>, Call),

    %% the return of this function is ignored anyway, and the call is finished, so no need
    %% to match the return here.
    whapps_call_command:hangup(Call).

handle_pivot_req(JObj, _Props) ->
    true = wapi_pivot:req_v(JObj),

    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    pivot_calls_sup:new(Call, JObj).
