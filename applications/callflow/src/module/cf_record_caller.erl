%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handles starting/stopping a call recording.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`time_limit'</dt>
%%%   <dd>How long to record the call, in seconds. Default is 600 seconds.</dd>
%%%
%%%   <dt>`format'</dt>
%%%   <dd>What format to store the recording in, e.g. `mp3' or `wav'.</dd>
%%%
%%%   <dt>`url'</dt>
%%%   <dd>What URL to PUT the file to.</dd>
%%% </dl>
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_record_caller).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Url = kz_json:get_value(<<"url">>, Data),
    case kapps_call_recording:should_store_recording(kapps_call:account_id(Call), Url) of
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

    Format = kapps_call_recording:get_format(kz_json:get_value(<<"format">>, Data)),
    MediaName = kapps_call_recording:get_media_name(kapps_call:call_id(Call), Format),

    _ = set_recording_url(Data, Call, Url, MediaName),

    lager:info("recording caller starting"),
    _ = kapps_call_command:b_record(MediaName
                                   ,?ANY_DIGIT
                                   ,kapps_call_recording:get_timelimit(Data)
                                   ,Call
                                   ),
    lager:debug("recording caller ended").

-spec set_recording_url(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
set_recording_url(Data, Call, Url, MediaName) ->
    lager:debug("store to ~s to ~s", [MediaName, Url]),

    kapps_call:set_custom_channel_vars([{<<"Media-Name">>, MediaName}
                                       ,{<<"Media-Transfer-Method">>, kz_json:get_value(<<"method">>, Data, <<"put">>)}
                                       ,{<<"Media-Transfer-Destination">>, Url}
                                       ]
                                      ,Call
                                      ).
