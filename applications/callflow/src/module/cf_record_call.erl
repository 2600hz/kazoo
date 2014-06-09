%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
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
%%%-------------------------------------------------------------------
-module(cf_record_call).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
-spec handle(wh_json:object(), whapps_call:call(), ne_binary()) -> 'ok'.
handle(Data, Call) ->
    handle(Data
           ,set_recording_url(Data, Call)
           ,get_action(wh_json:get_value(<<"action">>, Data))),
    cf_exe:continue(Call).

handle(Data, Call, <<"start">>) ->
    case wh_json:is_true(<<"record_on_answer">>, Data, 'false') of
        'true' ->
            'ok' = cf_exe:add_event_listener(Call, {'wh_media_recording', [Data]}),
            lager:debug("started wh_media_recording to handle recording");
        'false' ->
            Format = wh_media_recording:get_format(wh_json:get_value(<<"format">>, Data)),
            MediaName = wh_media_recording:get_media_name(whapps_call:call_id(Call), Format),
            Props = [{<<"Media-Name">>, MediaName}
                     ,{<<"Media-Transfer-Method">>, wh_json:get_value(<<"method">>, Data, <<"put">>)}
                     ,{<<"Media-Transfer-Destination">>, wh_json:get_value(<<"url">>, Data)}
                     ,{<<"Additional-Headers">>, wh_json:get_value(<<"additional_headers">>, Data)}
                     ,{<<"Time-Limit">>, wh_json:get_value(<<"time_limit">>, Data)}
                    ],
            _ = whapps_call_command:record_call(Props, <<"start">>, Call)
    end;
handle(Data, Call, <<"stop">> = Action) ->
    Format = wh_media_recording:get_format(wh_json:get_value(<<"format">>, Data)),
    MediaName = wh_media_recording:get_media_name(whapps_call:call_id(Call), Format),
    _ = whapps_call_command:record_call([{<<"Media-Name">>, MediaName}], Action, Call),
    lager:debug("send command to stop recording").

-spec get_action(api_binary()) -> ne_binary().
get_action('undefined') -> <<"start">>;
get_action(<<"stop">>) -> <<"stop">>;
get_action(_) -> <<"start">>.

-spec set_recording_url(wh_json:object(), whapps_call:call()) -> whapps_call:call().
set_recording_url(Data, Call) ->
    CallId = whapps_call:call_id(Call),
    Media = wh_media_util:recording_url(CallId, Data),
    whapps_call:set_custom_channel_var(<<"Recording-Url">>, Media, Call).
