%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Handles starting/stopping a call recording
%%%
%%% "data":{
%%%   "action":["start","stop"] // one of these
%%%   ,"time_limit":600 // in seconds, how long to record the call
%%%   ,"format":["mp3","wav"] // what format to store the recording in
%%%   ,"url":"http://server.com/path/to/dump/file" // what URL to PUT the file to
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_record_call).

-export([handle/2
         ,get_timelimit/1
         ,get_format/1
         ,get_url/1
         ,get_media_name/2
         ,should_store_recording/1
         ,save_recording/4
        ]).

-include("../callflow.hrl").

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

    'ok' = cf_exe:add_event_listener(Call, {'cf_record_call_listener', [Data]}),

    lager:debug("starting recording of ~s", [MediaName]),
    whapps_call_command:record_call(MediaName, Action, TimeLimit, Call);
handle(Data, Call, <<"stop">> = Action) ->
    Format = get_format(wh_json:get_value(<<"format">>, Data)),
    MediaName = get_media_name(whapps_call:call_id(Call), Format),

    _ = whapps_call_command:record_call(MediaName, Action, Call),
    lager:info("recording of ~s stopped", [MediaName]),

    save_recording(Call, MediaName, Format, should_store_recording(get_url(Data))).

save_recording(_Call, _MediaName, _Format, 'false') ->
    lager:debug("not configured to store recording ~s", [_MediaName]);
save_recording(Call, MediaName, Format, {'true', 'local'}) ->
    {'ok', MediaJObj} = store_recording_meta(Call, MediaName, Format),
    lager:info("stored meta: ~p", [MediaJObj]),

    StoreUrl = store_url(Call, MediaJObj),
    lager:info("store local url: ~s", [StoreUrl]),

    store_recording(MediaName, StoreUrl, Call);
save_recording(Call, MediaName, _Format, {'true', Url}) ->
    lager:debug("store remote url: ~s", [Url]),
    store_recording(MediaName, Url, Call).

-spec store_recording(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
store_recording(MediaName, Url, Call) ->
    StoreUrl = append_path(Url, MediaName),
    lager:debug("appending filename to url: ~s", [StoreUrl]),
    'ok' = whapps_call_command:store(MediaName, StoreUrl, Call).

append_path(Url, MediaName) ->
    S = byte_size(Url)-1,

    Encoded = cowboy_http:urlencode(MediaName),

    case Url of
        <<_:S/binary, "/">> -> <<Url/binary, Encoded/binary>>;
        _ -> <<Url/binary, "/", Encoded/binary>>
    end.

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

-spec should_store_recording(api_binary()) -> {'true', ne_binary() | 'local'} | 'false'.
should_store_recording('undefined') ->
    case whapps_config:get_is_true(?CF_CONFIG_CAT, <<"store_recordings">>, 'false') of
        'true' -> {'true', 'local'};
        'false' -> 'false'
    end;
should_store_recording(Url) -> {'true', Url}.

get_url(Data) ->
    wh_json:get_value(<<"url">>, Data).
