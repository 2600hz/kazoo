%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%% Handles starting/stopping a call recording
%%%
%%% "data":{
%%%   "action":["start","stop"] // one of these
%%%   ,"time_limit":600 // in seconds, how long to record the call
%%%   ,"format":["mp3","wav"] // what format to store the recording in
%%%   ,"url":"http://server.com/path/to/dump/file" // what URL to PUT the file to
%%%   ,"record_on_answer": boolean() // whether to delay the start of the recording
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%
%%%-------------------------------------------------------------------
-module(kz_media).

-export([start_recording/2
        ,stop_recording/1
        ]).

-include("kazoo_media.hrl").

-define(RECORDING_SUPERVISOR, 'kz_media_recording_sup').

-spec start_recording(kapps_call:call(), kz_json:object()) -> sup_startchild_ret().
start_recording(Call, Data) ->
    supervisor:start_child(?RECORDING_SUPERVISOR, [Call, Data]).

-spec stop_recording(pid()) -> 'ok'.
stop_recording(Pid) ->
    gen_server:cast(Pid, 'stop_recording').
