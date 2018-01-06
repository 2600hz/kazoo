%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
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

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Url = kz_json:get_value(<<"url">>, Data),
    case kzc_recording:should_store_recording(kapps_call:account_id(Call), Url) of
        'false' ->
            lager:debug("cannot store the recording, bad or no URL"),
            cf_exe:continue(Call);
        _Store ->
            lager:debug("storing the recording: ~p", [_Store]),
            record_caller(Data, Call, Url),
            cf_exe:continue(Call)
    end.

-spec record_caller(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
record_caller(Data, Call, Url) ->
    kapps_call_command:answer_now(Call),

    Format = kzc_recording:get_format(kz_json:get_value(<<"format">>, Data)),
    MediaName = kzc_recording:get_media_name(kapps_call:call_id(Call), Format),

    _ = set_recording_url(Data, Call, Url, MediaName),

    _ = kapps_call_command:b_record(MediaName
                                   ,?ANY_DIGIT
                                   ,kzc_recording:get_timelimit(Data)
                                   ,Call
                                   ),
    lager:debug("recording ended").

-spec set_recording_url(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
set_recording_url(Data, Call, Url, MediaName) ->
    lager:debug("store to ~s to ~s", [MediaName, Url]),

    kapps_call:set_custom_channel_vars([{<<"Media-Name">>, MediaName}
                                       ,{<<"Media-Transfer-Method">>, kz_json:get_value(<<"method">>, Data, <<"put">>)}
                                       ,{<<"Media-Transfer-Destination">>, Url}
                                       ]
                                      ,Call
                                      ).
