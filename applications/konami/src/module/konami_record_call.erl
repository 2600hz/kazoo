%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Record something
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
-module(konami_record_call).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()} |
                    no_return().
handle(Data, Call) ->
    handle(Data, Call, get_action(wh_json:get_value(<<"action">>, Data))),
    {'continue', Call}.

handle(Data, Call, <<"start">>) ->
    lager:debug("starting recording, see you on the other side"),
    wh_media_recording:start_recording(Call, Data);
handle(Data, Call, <<"stop">> = Action) ->
    Format = wh_media_recording:get_format(wh_json:get_value(<<"format">>, Data)),
    MediaName = wh_media_recording:get_media_name(whapps_call:call_id(Call), Format),

    _ = whapps_call_command:record_call(MediaName, Action, Call),
    lager:debug("sent command to stop recording").

-spec get_action(api_binary()) -> ne_binary().
get_action('undefined') -> <<"start">>;
get_action(<<"stop">>) -> <<"stop">>;
get_action(_) -> <<"start">>.
