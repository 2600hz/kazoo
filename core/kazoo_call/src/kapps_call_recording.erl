%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handles endpoint inbound recording
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_call_recording).

-export([record_call_command/2
        ,get_media_name/2
        ,get_format/1
        ,get_timelimit/1
        ,should_store_recording/0, should_store_recording/2
        ]).

-include("kapps_call_command.hrl").

-type store_url() :: 'false' |
                     {'true', 'local'} |
                     {'true', 'other', kz_term:ne_binary()}.

-define(STORAGE_RETRY_TIMES(AccountId)
       ,kz_media_config:storage_retry_times(AccountId)
       ).

-define(RECORDING_ID_KEY, <<"media_name">>).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_timelimit(kz_json:object() | kz_term:api_integer()) -> pos_integer().
get_timelimit('undefined') ->
    kz_media_util:max_recording_time_limit();
get_timelimit(TL) when is_integer(TL) ->
    get_timelimit(TL, kz_media_util:max_recording_time_limit());
get_timelimit(JObj) ->
    get_timelimit(kz_json:get_integer_value(<<"time_limit">>, JObj)).

-spec get_timelimit(non_neg_integer(), integer()) -> pos_integer().
get_timelimit(TL, Max) when Max > TL -> TL;
get_timelimit(_TL, Max) when Max > 0 -> Max;
get_timelimit(_TL, Max) -> Max.

-spec get_format(kz_term:api_ne_binary()) -> kz_term:ne_binary().
get_format('undefined') -> kz_media_config:call_recording_extension();
get_format(<<"mp3">> = MP3) -> MP3;
get_format(<<"mp4">> = MP4) -> MP4;
get_format(<<"wav">> = WAV) -> WAV;
get_format(_) -> get_format('undefined').

-spec get_media_name(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
get_media_name(Name, Ext) ->
    case filename:extension(Name) of
        Ext -> Name;
        _ -> <<Name/binary, ".", Ext/binary>>
    end.

-spec handler_from_url(kz_term:ne_binary()) -> 'kz_att_ftp' | 'kz_att_http' | 'undefined'.
handler_from_url(Url) ->
    case kz_http_util:urlsplit(Url) of
        {<<"ftp">>, _, _, _, _} -> 'kz_att_ftp';
        {<<"ftps">>, _, _, _, _} -> 'kz_att_ftp';
        {<<"http">>, _, _, _, _} -> 'kz_att_http';
        {<<"https">>, _, _, _, _} -> 'kz_att_http';
        _ -> 'undefined'
    end.

-spec should_store_recording(kz_term:ne_binary(), kz_term:api_binary()) -> store_url().
should_store_recording(AccountId, Url) ->
    case kz_term:is_empty(Url) of
        'true' -> maybe_storage_plan(AccountId);
        'false' ->
            case handler_from_url(Url) of
                'undefined' ->
                    lager:debug("invalid protocol for url ~s : not saving attachment"),
                    'false';
                _ -> {'true', 'other', Url}
            end
    end.

-spec maybe_storage_plan(kz_term:ne_binary()) -> store_url().
maybe_storage_plan(AccountId) ->
    AccountDb = kz_util:format_account_mod_id(AccountId),
    Plan = kzs_plan:get_dataplan(AccountDb, <<"call_recording">>),
    case maps:get('tag', Plan, <<"local">>) =/= <<"local">>
        orelse maps:is_key('att_handler', Plan) of
        'true' -> {'true', 'local'};
        'false' -> should_store_recording()
    end.

-spec should_store_recording() -> store_url().
should_store_recording() ->
    case kz_media_config:should_store_recordings() of
        'true' -> {'true', 'local'};
        'false' -> 'false'
    end.

-spec media_recorder(kz_json:object(), kapps_call:call()) -> kz_term:api_ne_binary().
media_recorder(Data, Call) ->
    AccountId = kapps_call:account_id(Call),
    Url = kz_json:get_ne_binary_value(<<"url">>, Data),
    case should_store_recording(AccountId, Url) of
        'false' -> 'undefined';
        _ -> ?KZ_RECORDER
    end.

-spec record_call_command(kz_json:object(), kapps_call:call()) -> kz_json:object().
record_call_command(Data, Call) ->
    Format = get_format(kz_json:get_ne_binary_value(<<"format">>, Data)),
    TimeLimit = get_timelimit(kz_json:get_integer_value(<<"time_limit">>, Data)),
    SampleRate = kz_json:get_integer_value(<<"record_sample_rate">>, Data),
    DefaultRecordMinSec = kz_media_config:record_min_sec(),
    RecordMinSec = kz_json:get_integer_value(<<"record_min_sec">>, Data, DefaultRecordMinSec),
    {Year, Month, _} = erlang:date(),
    CallId = kapps_call:call_id(Call),
    RecordingId = kz_binary:rand_hex(16),
    MediaDocId = ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month), RecordingId),
    DefaultMediaName = get_media_name(kz_binary:rand_hex(16), Format),
    MediaName = kz_json:get_value(?RECORDING_ID_KEY, Data, DefaultMediaName),
    FollowTransfer = kapps_call:kvs_fetch('recording_follow_transfer', 'false', Call),
    Recorder = media_recorder(Data, Call),
    Vars = [{<<"Name">>, MediaName}
           ,{<<"Recorder">>, Recorder}
           ,{<<"ID">>, MediaDocId}
           ,{<<"Data">>, base64:encode(term_to_binary(Data))}
           ],
    Media = [{<<"Application-Name">>, <<"record_call">>}
            ,{<<"Record-Action">>, <<"start">>}
            ,{<<"Follow-Transfer">>, FollowTransfer}
            ,{<<"Time-Limit">>, TimeLimit}
            ,{<<"Media-Name">>, MediaName}
            ,{<<"Media-Recording-ID">>, MediaDocId}
            ,{<<"Record-Sample-Rate">>, SampleRate}
            ,{<<"Record-Min-Sec">>, kz_term:to_binary(RecordMinSec)}
            ,{<<"Media-Recorder">>, Recorder}
            ,{<<"Recording-Variables">>, kz_json:from_list(Vars)}
            ,{<<"Call-ID">>, CallId}
            ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
             | kz_api:default_headers(<<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    kz_json:from_list(Media).
