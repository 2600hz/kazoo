%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%% Handles starting/stopping a call recording
%%%
%%% "data":{
%%%   "time_limit":600 // in seconds, how long to record the call
%%%   ,"format":["mp3","wav"] // what format to store the recording in
%%%   ,"url":"http://server.com/path/to/dump/file" // what URL to PUT the file to
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_record_caller).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Url = wh_json:get_value(<<"url">>, Data),
    case wh_media_recording:should_store_recording(Url) of
        'false' ->
            lager:debug("cannot store the recording, bad or no URL"),
            cf_exe:continue(Call);
        _Store ->
            lager:debug("storing the recording: ~p", [_Store]),
            record_caller(Data, Call, Url),
            cf_exe:continue(Call)
    end.

-spec record_caller(wh_json:object(), whapps_call:call(), ne_binary()) -> 'ok'.
record_caller(Data, Call, Url) ->
    whapps_call_command:answer_now(Call),

    Format = wh_media_recording:get_format(wh_json:get_value(<<"format">>, Data)),
    MediaName = wh_media_recording:get_media_name(whapps_call:call_id(Call), Format),

    _ = set_recording_url(Data, Call, Url, MediaName),

    _ = whapps_call_command:b_record(MediaName
                                     ,?ANY_DIGIT
                                     ,wh_media_recording:get_timelimit(Data)
                                     ,Call
                                    ),
    lager:debug("recording ended").

-spec set_recording_url(wh_json:object(), whapps_call:call(), ne_binary(), ne_binary()) -> _.
set_recording_url(Data, Call, Url, MediaName) ->
    lager:debug("store to ~s to ~s", [MediaName, Url]),

    whapps_call:set_custom_channel_vars(
      [{<<"Media-Name">>, MediaName}
       ,{<<"Media-Transfer-Method">>, wh_json:get_value(<<"method">>, Data, <<"put">>)}
       ,{<<"Media-Transfer-Destination">>, Url}
      ]
      ,Call
     ).
