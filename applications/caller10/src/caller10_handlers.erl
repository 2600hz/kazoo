%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(caller10_handlers).

-export([handle_route_req/2
         ,handle_route_win/2
         ,handle_config_change/2
        ]).

-include("caller10.hrl").

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, Props) ->
    Q = props:get_value('queue', Props),

    %% sends route_resp with "park" by default
    whapps_call:route_response(JObj, Q, 'park').

%% receiving the route_win means we are in control of the call
-spec handle_route_win(wh_json:object(), wh_proplist()) -> any().
handle_route_win(JObj, _Props) ->
    %% Create the call data structure
    {'ok', Call} = whapps_call:answer(JObj),

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
    {'ok', Call1} = whapps_call:b_play(Call, <<"local_stream://some/media.mp3">>),

    %% the return of this function is ignored anyway, and the call is finished, so no need
    %% to match the return here.
    whapps_call:hangup(Call1).

-spec handle_config_change(wh_json:object(), wh_proplist()) -> 'ok'.
handle_config_change(JObj, _Props) ->
    'true' = wapi_conf:doc_update_v(JObj),
    handle_contest_change(JObj, wh_json:get_value(<<"Event-Name">>, JObj)).

-spec handle_contest_change(wh_json:object(), ne_binary()) -> 'ok'.
handle_contest_change(JObj, <<"doc_created">>) ->
    AccountDb = wh_json:get_value(<<"Database">>, JObj),
    DocId = wh_json:get_value(<<"ID">>, JObj),

    lager:debug("new contest ~s from ~s", [DocId, wh_json:get_value(<<"Account-ID">>, JObj)]),

    {'ok', Contest} = couch_mgr:open_doc(AccountDb, DocId),
    caller10_contests:created(Contest);
handle_contest_change(JObj, <<"doc_edited">>) ->
    AccountDb = wh_json:get_value(<<"Database">>, JObj),
    DocId = wh_json:get_value(<<"ID">>, JObj),

    lager:debug("updated contest ~s from ~s", [DocId, wh_json:get_value(<<"Account-ID">>, JObj)]),

    {'ok', Contest} = couch_mgr:open_doc(AccountDb, DocId),
    caller10_contests:updated(Contest);
handle_contest_change(JObj, <<"doc_deleted">>) ->
    DocId = wh_json:get_value(<<"ID">>, JObj),

    lager:debug("new contest ~s from ~s", [DocId, wh_json:get_value(<<"Account-ID">>, JObj)]),

    caller10_contests:deleted(DocId).
