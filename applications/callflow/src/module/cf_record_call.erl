%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%% Handles starting/stopping a call recording
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%
%%% Fix KAZOO-3406: Sponsored by Velvetech LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(cf_record_call).

-export([handle/2]).

-include("../callflow.hrl").

-define(RECORDINGS_KEY, <<"Recordings">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
-spec handle(wh_json:object(), whapps_call:call(), ne_binary()) -> 'ok'.
handle(Data, Call) ->
    cf_exe:continue(handle(Data, Call, get_action(Data))).

handle(Data, Call, <<"start">>) ->
    RecID = wh_util:rand_hex_binary(16),
    Format = wh_media_recording:get_format(wh_json:get_value(<<"format">>, Data)),
    MediaName = wh_media_recording:get_media_name(RecID, Format),
    Args = wh_json:set_value(?CF_RECORDING_ID_KEY, MediaName, Data),
    Routines = [{fun store_recording/2, MediaName}],
    cf_util:start_call_recording(Args, cf_exe:update_call(Call, Routines));

handle(_Data, OriginalCall, <<"stop">>) ->
    case retrieve_recording(OriginalCall) of
        {'ok', _MediaName, Call} ->
            Mod = cf_util:recording_module(Call),
            case cf_event_handler_sup:worker(cf_util:event_listener_name(Call, Mod)) of
                'undefined' -> lager:debug("no recording process to stop");
                RecorderPid -> Mod:stop_recording(RecorderPid)
            end,
            Call;
        {'empty', Call} ->
            lager:debug("no recording to stop"),
            Call
    end.

-spec store_recording(ne_binary(), whapps_call:call()) -> whapps_call:call().
store_recording(MediaId, Call) ->
    Q = queue:in(MediaId, get_recordings(Call)),
    whapps_call:kvs_store(?RECORDINGS_KEY, Q, Call).

-type store_return() :: {'ok', ne_binary(), whapps_call:call()} | {'empty', whapps_call:call()}.

-spec retrieve_recording(whapps_call:call()) -> store_return().
retrieve_recording(Call) ->
    case queue:out_r(get_recordings(Call)) of
        {{'value', MediaId}, Q} ->
            Routines = [{fun whapps_call:kvs_store/3, ?RECORDINGS_KEY, Q}],
            {'ok', MediaId, cf_exe:update_call(Call, Routines)};
        {'empty', _} ->
            {'empty', Call}
    end.

-spec get_recordings(whapps_call:call()) -> queue:queue().
get_recordings(Call) ->
    case whapps_call:kvs_fetch(?RECORDINGS_KEY, Call) of
        'undefined' -> queue:new();
        Q -> Q
    end.

-spec get_action(api_object()) -> ne_binary().
get_action('undefined') -> <<"start">>;
get_action(Data) ->
    case wh_json:get_value(<<"action">>, Data) of
        <<"stop">> -> <<"stop">>;
        _ -> <<"start">>
    end.
