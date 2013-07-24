%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Handles starting/stopping a call recording
%%%
%%% "data":{
%%%   "action":["start","stop"] // one of these
%%%   ,"time_limit":600 // in seconds, how long to record the call
%%%   ,"format":["mp3","wav"] // what format to store the recording in
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_record_call).

-export([handle/2
         ,start_event_listener/2
        ]).

-include("../callflow.hrl").

-spec start_event_listener(whapps_call:call(), wh_json:object()) -> 'ok'.
start_event_listener(Call, Data) ->
    put('callid', whapps_call:call_id(Call)),
    TimeLimit = get_timelimit(wh_json:get_integer_value(<<"time_limit">>, Data)),
    lager:info("listening for record stop (or ~b s), then storing the recording", [TimeLimit]),

    _Wait = whapps_call_command:wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, (TimeLimit + 10) * 1000),
    lager:info("ok, done waiting: ~p", [_Wait]),

    Format = get_format(wh_json:get_value(<<"format">>, Data)),
    MediaName = get_media_name(whapps_call:call_id(Call), Format),

    save_recording(Call, MediaName, Format).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Action = get_action(wh_json:get_value(<<"action">>, Data)),
    handle(Data, Call, Action),
    cf_exe:continue(Call).

handle(Data, Call, <<"start">> = Action) ->
    TimeLimit = get_timelimit(wh_json:get_integer_value(<<"time_limit">>, Data)),

    Format = get_format(wh_json:get_value(<<"format">>, Data)),
    MediaName = get_media_name(whapps_call:call_id(Call), Format),

    _P = cf_exe:add_event_listener(Call, {?MODULE, 'start_event_listener', [Data]}),

    lager:info("recording ~s starting, evt listener at ~p", [MediaName, _P]),
    whapps_call_command:record_call(MediaName, Action, TimeLimit, Call);
handle(Data, Call, <<"stop">> = Action) ->
    Format = get_format(wh_json:get_value(<<"format">>, Data)),
    MediaName = get_media_name(whapps_call:call_id(Call), Format),

    _ = whapps_call_command:record_call(MediaName, Action, Call),
    lager:info("recording of ~s stopped", [MediaName]),

    save_recording(Call, MediaName, Format).

save_recording(Call, MediaName, Format) ->
    {'ok', MediaJObj} = store_recording_meta(Call, MediaName, Format),
    lager:info("stored meta: ~p", [MediaJObj]),

    StoreUrl = store_url(Call, MediaJObj),
    lager:info("store url: ~s", [StoreUrl]),

    store_recording(MediaName, StoreUrl, Call).

-spec store_recording(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
store_recording(MediaName, StoreUrl, Call) ->
    'ok' = whapps_call_command:store(MediaName, StoreUrl, Call).

-spec get_action(api_binary()) -> ne_binary().
get_action('undefined') -> <<"start">>;
get_action(<<"stop">>) -> <<"stop">>;
get_action(_) -> <<"start">>.

-spec get_timelimit('undefined' | integer()) -> pos_integer().
get_timelimit('undefined') ->
    whapps_config:get(?CF_CONFIG_CAT, <<"max_recording_time_limit">>, 600);
get_timelimit(TL) ->
    case (Max = whapps_config:get(?CF_CONFIG_CAT, <<"max_recording_time_limit">>, 600)) > TL of
        'true' -> TL;
        'false' when Max > 0 -> Max;
        'false' -> Max
    end.

get_format('undefined') -> whapps_config:get(?CF_CONFIG_CAT, [<<"call_recording">>, <<"extension">>], <<"mp3">>);
get_format(<<"mp3">> = MP3) -> MP3;
get_format(<<"wav">> = WAV) -> WAV;
get_format(_) -> get_format('undefined').

-spec store_recording_meta(whapps_call:call(), ne_binary(), cf_api_binary()) ->
                                        {'ok', wh_json:object()} |
                                        {'error', any()}.
store_recording_meta(Call, MediaName, Ext) ->
    AcctDb = whapps_call:account_db(Call),
    CallId = whapps_call:call_id(Call),

    MediaDoc = wh_doc:update_pvt_parameters(
                 wh_json:from_list(
                   [{<<"name">>, MediaName}
                    ,{<<"description">>, <<"recording ", MediaName/binary>>}
                    ,{<<"content_type">>, ext_to_mime(Ext)}
                    ,{<<"media_type">>, Ext}
                    ,{<<"media_source">>, <<"recorded">>}
                    ,{<<"source_type">>, wh_util:to_binary(?MODULE)}
                    ,{<<"pvt_type">>, <<"private_media">>}
                    ,{<<"from">>, whapps_call:from(Call)}
                    ,{<<"to">>, whapps_call:to(Call)}
                    ,{<<"caller_id_number">>, whapps_call:caller_id_number(Call)}
                    ,{<<"caller_id_name">>, whapps_call:caller_id_name(Call)}
                    ,{<<"call_id">>, CallId}
                    ,{<<"_id">>, get_recording_doc_id(CallId)}
                   ])
                 ,AcctDb
                ),
    couch_mgr:save_doc(AcctDb, MediaDoc).

ext_to_mime(<<"wav">>) -> <<"audio/x-wav">>;
ext_to_mime(_) -> <<"audio/mp3">>.

get_recording_doc_id(CallId) -> <<"call_recording_", CallId/binary>>.

-spec get_media_name(ne_binary(), cf_api_binary()) -> ne_binary().
get_media_name(CallId, Ext) ->
    <<(get_recording_doc_id(CallId))/binary, ".", Ext/binary>>.

-spec store_url(whapps_call:call(), wh_json:object()) -> ne_binary().
store_url(Call, JObj) ->
    AccountDb = whapps_call:account_db(Call),
    MediaId = wh_json:get_value(<<"_id">>, JObj),
    MediaName = wh_json:get_value(<<"name">>, JObj),
    {'ok', URL} = wh_media_url:store(AccountDb, MediaId, MediaName),
    URL.
