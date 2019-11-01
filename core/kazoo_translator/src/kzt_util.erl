%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzt_util).

-export([http_method/1
        ,resolve_uri/2
        ,offnet_req/2
        ,update_call_status/2, get_call_status/1
        ,get_request_vars/1
        ,add_error/3, get_errors/1
        ,set_hangup_dtmf/2, get_hangup_dtmf/1
        ,set_digit_pressed/2, get_digit_pressed/1
        ,set_call_timeout/2, get_call_timeout/1
        ,set_call_time_limit/2, get_call_time_limit/1
        ,set_record_call/2, get_record_call/1
        ,set_voice_uri/2, get_voice_uri/1
        ,set_voice_uri_method/2, get_voice_uri_method/1

        ,set_digits_collected/2, get_digits_collected/1
        ,add_digit_collected/2, clear_digits_collected/1

        ,set_recording_url/2, get_recording_url/1
        ,set_recording_duration/2, get_recording_duration/1
        ,set_recording_sid/2, get_recording_sid/1

        ,set_transcription_sid/2, get_transcription_sid/1
        ,set_transcription_text/2, get_transcription_text/1
        ,set_transcription_status/2, get_transcription_status/1
        ,set_transcription_url/2, get_transcription_url/1

        ,set_dial_call_status/2, get_dial_call_status/1
        ,set_dial_call_sid/2, get_dial_call_sid/1
        ,set_dial_call_duration/2, get_dial_call_duration/1
        ,set_queue_sid/2, get_queue_sid/1
        ,set_dequeue_result/2, get_dequeue_result/1
        ,set_dequeued_call_sid/2, get_dequeued_call_sid/1
        ,set_dequeued_call_queue_time/2, get_dequeued_call_queue_time/1
        ,set_dequeued_call_duration/2, get_dequeued_call_duration/1
        ,set_media_meta/2, get_media_meta/1
        ,set_amqp_listener/2, get_amqp_listener/1

        ,set_gather_pidref/2, get_gather_pidref/1

        ,set_conference_profile/2, get_conference_profile/1
        ,set_caller_controls/2, get_caller_controls/1
        ,set_advertise/2, get_advertise/1
        ,set_chat_permissions/2, get_chat_permissions/1

        ,iteration/1, increment_iteration/1
        ]).

-include("kzt.hrl").

-define(DEFAULT_HTTP_METHOD, 'post').

-spec http_method(kz_term:api_binary() | list()) -> 'get' | 'post'.
http_method(L) when is_list(L) ->
    http_method(kz_term:to_binary(props:get_value('method', L)));
http_method('undefined') -> ?DEFAULT_HTTP_METHOD;
http_method(<<_/binary>> = Method) ->
    MethodBin = kz_term:to_lower_binary(Method),

    try kz_term:to_atom(MethodBin) of
        'get' -> 'get';
        'post' -> 'post';
        'undefined' -> ?DEFAULT_HTTP_METHOD
    catch
        _E:_R -> ?DEFAULT_HTTP_METHOD
    end;
http_method(Method) -> http_method(kz_term:to_binary(Method)).

-spec resolve_uri(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
resolve_uri(Path, NewPath) ->
    kz_http_util:resolve_uri(Path, NewPath).

%% see cf_offnet.erl
-spec offnet_req(kz_term:proplist(), kapps_call:call()) -> 'ok'.
offnet_req(Data, Call) ->
    {ECIDNum, ECIDName} = kz_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = kz_attributes:caller_id(<<"external">>, Call),
    CIDNum = case kapps_call:kvs_fetch('dynamic_cid', Call) of
                 'undefined' -> CIDNumber;
                 DynamicCID -> DynamicCID
             end,

    Req = [{<<"Call-ID">>, kapps_call:call_id(Call)}
          ,{<<"Resource-Type">>, <<"audio">>}
          ,{<<"To-DID">>, kapps_call:request_user(Call)}
          ,{<<"Account-ID">>, kapps_call:account_id(Call)}
          ,{<<"Account-Realm">>, kapps_call:from_realm(Call)}
          ,{<<"Control-Queue">>, kapps_call:control_queue(Call)}
          ,{<<"Application-Name">>, <<"bridge">>}
          ,{<<"Flags">>, props:get_value(<<"flags">>, Data)}
          ,{<<"Timeout">>, props:get_value(<<"timeout">>, Data)}
          ,{<<"Ignore-Early-Media">>, props:get_value(<<"ignore_early_media">>, Data)}
          ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
          ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
          ,{<<"Outbound-Caller-ID-Name">>, CIDName}
          ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
          ,{<<"Presence-ID">>, kz_attributes:presence_id(Call)}
          ,{<<"Ringback">>, props:get_value(<<"ringback">>, Data)}
           | kz_api:default_headers(kapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
          ] ++ Data,

    kz_amqp_worker:cast(Req, fun kapi_offnet_resource:publish_req/1).

-spec update_call_status(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
update_call_status(Status, Call) ->
    kapps_call:kvs_store(<<"call_status">>, Status, Call).

-spec get_call_status(kapps_call:call()) -> kz_term:api_binary().
get_call_status(Call) ->
    kapps_call:kvs_fetch(<<"call_status">>, Call).

-spec add_error(kapps_call:call(), kz_term:ne_binary(), any()) -> kapps_call:call().
add_error(Call, K, V) ->
    case kapps_call:kvs_fetch(<<"response_errors">>, Call) of
        'undefined' ->
            kapps_call:kvs_store(<<"response_errors">>, [{K, V}], Call);
        Vs ->
            kapps_call:kvs_append_list(<<"response_errors">>, [{K, V}|Vs], Call)
    end.

-spec get_errors(kapps_call:call()) -> kz_term:proplist().
get_errors(Call) -> kapps_call:kvs_fetch(<<"response_errors">>, Call).

-spec set_hangup_dtmf(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_hangup_dtmf(DTMF, Call) -> kapps_call:kvs_store(<<"hangup_dtmf">>, DTMF, Call).

-spec get_hangup_dtmf(kapps_call:call()) -> kz_term:api_binary().
get_hangup_dtmf(Call) -> kapps_call:kvs_fetch(<<"hangup_dtmf">>, Call).

-spec set_digit_pressed(kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
set_digit_pressed(DTMF, Call) -> kapps_call:kvs_store(<<"digit_pressed">>, DTMF, Call).

-spec get_digit_pressed(kapps_call:call()) -> kz_term:api_binary() | kz_json:object().
get_digit_pressed(Call) ->
    case kapps_call:kvs_fetch(<<"digit_pressed">>, Call) of
        'undefined' -> get_digits_pressed(Call);
        D -> D
    end.

-spec get_digits_pressed(kapps_call:call()) -> kz_term:api_object().
get_digits_pressed(Call) ->
    kapps_call:kvs_fetch(<<"dtmf_collections">>, Call).

-spec set_record_call(boolean(), kapps_call:call()) -> kapps_call:call().
set_record_call(R, Call) -> kapps_call:kvs_store(<<"record_call">>, R, Call).

-spec get_record_call(kapps_call:call()) -> kz_term:api_boolean().
get_record_call(Call) -> kapps_call:kvs_fetch(<<"record_call">>, Call).

-spec set_call_timeout(pos_integer(), kapps_call:call()) -> kapps_call:call().
set_call_timeout(T, Call) -> kapps_call:kvs_store(<<"call_timeout">>, T, Call).

-spec get_call_timeout(kapps_call:call()) -> kz_term:api_integer().
get_call_timeout(Call) -> kapps_call:kvs_fetch(<<"call_timeout">>, Call).

-spec set_call_time_limit(pos_integer(), kapps_call:call()) -> kapps_call:call().
set_call_time_limit(T, Call) -> kapps_call:kvs_store(<<"call_time_limit">>, T, Call).

-spec get_call_time_limit(kapps_call:call()) -> kz_term:api_integer().
get_call_time_limit(Call) -> kapps_call:kvs_fetch(<<"call_time_limit">>, Call).

-spec set_voice_uri(kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
set_voice_uri(Uri, Call) -> kapps_call:kvs_store(<<"voice_uri">>, Uri, Call).

-spec get_voice_uri(kapps_call:call()) -> kz_term:api_binary().
get_voice_uri(Call) -> kapps_call:kvs_fetch(<<"voice_uri">>, Call).

-spec set_voice_uri_method('get' | 'post', kapps_call:call()) -> kapps_call:call().
set_voice_uri_method(M, Call) -> kapps_call:kvs_store(<<"voice_uri_method">>, M, Call).

-spec get_voice_uri_method(kapps_call:call()) -> 'get' | 'post'.
get_voice_uri_method(Call) -> kapps_call:kvs_fetch(<<"voice_uri_method">>, Call).

-spec set_digits_collected(kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
set_digits_collected(Ds, Call) -> kapps_call:kvs_store(<<"digits_collected">>, Ds, Call).

-spec get_digits_collected(kapps_call:call()) -> binary().
get_digits_collected(Call) -> kapps_call:kvs_fetch(<<"digits_collected">>, Call).

-spec clear_digits_collected(kapps_call:call()) -> kapps_call:call().
clear_digits_collected(Call) -> kapps_call:kvs_store(<<"digits_collected">>, <<>>, Call).

-spec add_digit_collected(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
add_digit_collected(D, Call) ->
    kapps_call:kvs_update(<<"digits_collected">>, fun(<<_/binary>> = Ds) -> <<Ds/binary, D/binary>>;
                                                     (_) -> D
                                                  end
                         ,D, Call).

-spec set_recording_url(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_recording_url(RU, Call) -> kapps_call:kvs_store(<<"recording_url">>, RU, Call).

-spec get_recording_url(kapps_call:call()) -> kz_term:api_binary().
get_recording_url(Call) -> kapps_call:kvs_fetch(<<"recording_url">>, Call).

-spec set_recording_duration(pos_integer(), kapps_call:call()) -> kapps_call:call().
set_recording_duration(RD, Call) -> kapps_call:kvs_store(<<"recording_duration">>, RD, Call).

-spec get_recording_duration(kapps_call:call()) -> kz_term:api_integer().
get_recording_duration(Call) -> kapps_call:kvs_fetch(<<"recording_duration">>, Call).

-spec set_recording_sid(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_recording_sid(SID, Call) -> kapps_call:kvs_store(<<"recording_sid">>, SID, Call).

-spec get_recording_sid(kapps_call:call()) -> kz_term:api_binary().
get_recording_sid(Call) -> kapps_call:kvs_fetch(<<"recording_sid">>, Call).

-spec set_transcription_sid(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_transcription_sid(SID, Call) -> kapps_call:kvs_store(<<"transcription_sid">>, SID, Call).

-spec get_transcription_sid(kapps_call:call()) -> kz_term:api_binary().
get_transcription_sid(Call) -> kapps_call:kvs_fetch(<<"transcription_sid">>, Call).

-spec set_transcription_text(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_transcription_text(RD, Call) -> kapps_call:kvs_store(<<"transcription_text">>, RD, Call).

-spec get_transcription_text(kapps_call:call()) -> kz_term:api_binary().
get_transcription_text(Call) -> kapps_call:kvs_fetch(<<"transcription_text">>, Call).

-spec set_transcription_status(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_transcription_status(RD, Call) -> kapps_call:kvs_store(<<"transcription_status">>, RD, Call).

-spec get_transcription_status(kapps_call:call()) -> kz_term:api_binary().
get_transcription_status(Call) -> kapps_call:kvs_fetch(<<"transcription_status">>, Call).

-spec set_transcription_url(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_transcription_url(RD, Call) -> kapps_call:kvs_store(<<"transcription_url">>, RD, Call).

-spec get_transcription_url(kapps_call:call()) -> kz_term:api_binary().
get_transcription_url(Call) -> kapps_call:kvs_fetch(<<"transcription_url">>, Call).

-spec set_dial_call_status(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_dial_call_status(DCS, Call) -> kapps_call:kvs_store(<<"dial_call_status">>, DCS, Call).

-spec get_dial_call_status(kapps_call:call()) -> kz_term:api_binary().
get_dial_call_status(Call) -> kapps_call:kvs_fetch(<<"dial_call_status">>, Call).

-spec set_dial_call_sid(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_dial_call_sid(DCS, Call) -> kapps_call:kvs_store(<<"dial_call_sid">>, DCS, Call).

-spec get_dial_call_sid(kapps_call:call()) -> kz_term:api_binary().
get_dial_call_sid(Call) -> kapps_call:kvs_fetch(<<"dial_call_sid">>, Call).

-spec set_dial_call_duration(pos_integer(), kapps_call:call()) -> kapps_call:call().
set_dial_call_duration(DCS, Call) -> kapps_call:kvs_store(<<"dial_call_duration">>, DCS, Call).

-spec get_dial_call_duration(kapps_call:call()) -> kz_term:api_integer().
get_dial_call_duration(Call) -> kapps_call:kvs_fetch(<<"dial_call_duration">>, Call).

-spec set_queue_sid(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_queue_sid(DCS, Call) -> kapps_call:kvs_store(<<"queue_sid">>, DCS, Call).

-spec get_queue_sid(kapps_call:call()) -> kz_term:api_binary().
get_queue_sid(Call) -> kapps_call:kvs_fetch(<<"queue_sid">>, Call).

-spec set_dequeue_result(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_dequeue_result(DCS, Call) -> kapps_call:kvs_store(<<"dequeue_result">>, DCS, Call).

-spec get_dequeue_result(kapps_call:call()) -> kz_term:api_binary().
get_dequeue_result(Call) -> kapps_call:kvs_fetch(<<"dequeue_result">>, Call).

-spec set_dequeued_call_sid(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_dequeued_call_sid(DCS, Call) -> kapps_call:kvs_store(<<"dequeued_call_sid">>, DCS, Call).

-spec get_dequeued_call_sid(kapps_call:call()) -> kz_term:api_binary().
get_dequeued_call_sid(Call) -> kapps_call:kvs_fetch(<<"dequeued_call_sid">>, Call).

-spec set_dequeued_call_queue_time(pos_integer(), kapps_call:call()) -> kapps_call:call().
set_dequeued_call_queue_time(DCS, Call) -> kapps_call:kvs_store(<<"dequeued_call_queue_time">>, DCS, Call).

-spec get_dequeued_call_queue_time(kapps_call:call()) -> kz_term:api_integer().
get_dequeued_call_queue_time(Call) -> kapps_call:kvs_fetch(<<"dequeued_call_queue_time">>, Call).

-spec set_dequeued_call_duration(pos_integer(), kapps_call:call()) -> kapps_call:call().
set_dequeued_call_duration(DCS, Call) -> kapps_call:kvs_store(<<"dequeued_call_duration">>, DCS, Call).

-spec get_dequeued_call_duration(kapps_call:call()) -> kz_term:api_integer().
get_dequeued_call_duration(Call) -> kapps_call:kvs_fetch(<<"dequeued_call_duration">>, Call).

-spec set_media_meta(kz_json:object(), kapps_call:call()) -> kapps_call:call().
set_media_meta(DCS, Call) -> kapps_call:kvs_store(<<"media_meta">>, DCS, Call).

-spec get_media_meta(kapps_call:call()) -> kz_term:api_object().
get_media_meta(Call) -> kapps_call:kvs_fetch(<<"media_meta">>, Call).

-spec set_amqp_listener(pid(), kapps_call:call()) -> kapps_call:call().
set_amqp_listener(Pid, Call) -> kapps_call:kvs_store(<<"amqp_listener">>, Pid, Call).

-spec get_amqp_listener(kapps_call:call()) -> kz_term:api_pid().
get_amqp_listener(Call) -> kapps_call:kvs_fetch(<<"amqp_listener">>, Call).

-spec set_gather_pidref(kz_term:pid_ref() | 'undefined', kapps_call:call()) ->
                               kapps_call:call().
set_gather_pidref('undefined', Call) ->
    kapps_call:kvs_store(<<"gather_pidref">>, 'undefined', Call);
set_gather_pidref({_, _}=PidRef, Call) ->
    gen_listener:cast(get_amqp_listener(Call), {'add_event_handler', PidRef}),
    kapps_call:kvs_store(<<"gather_pidref">>, PidRef, Call).

-spec get_gather_pidref(kapps_call:call()) ->
                               kz_term:pid_ref() | 'undefined'.
get_gather_pidref(Call) -> kapps_call:kvs_fetch(<<"gather_pidref">>, Call).

-spec set_conference_profile(kz_json:object(), kapps_call:call()) ->
                                    kapps_call:call().
set_conference_profile(JObj, Call) ->
    kapps_call:kvs_store(<<"conference_profile">>, JObj, Call).

-spec get_conference_profile(kapps_call:call()) ->
                                    kz_json:object().
get_conference_profile(Call) ->
    kapps_call:kvs_fetch(<<"conference_profile">>, Call).

-spec set_caller_controls(kz_json:object(), kapps_call:call()) ->
                                 kapps_call:call().
set_caller_controls(JObj, Call) ->
    kapps_call:kvs_store(<<"caller_controls">>, JObj, Call).

-spec get_caller_controls(kapps_call:call()) ->
                                 kz_json:object().
get_caller_controls(Call) ->
    kapps_call:kvs_fetch(<<"caller_controls">>, Call).

-spec set_advertise(kz_json:object(), kapps_call:call()) ->
                           kapps_call:call().
set_advertise(JObj, Call) ->
    kapps_call:kvs_store(<<"advertise">>, JObj, Call).

-spec get_advertise(kapps_call:call()) ->
                           kz_json:object().
get_advertise(Call) ->
    kapps_call:kvs_fetch(<<"advertise">>, Call).

-spec set_chat_permissions(kz_json:object(), kapps_call:call()) ->
                                  kapps_call:call().
set_chat_permissions(JObj, Call) ->
    kapps_call:kvs_store(<<"chat_permissions">>, JObj, Call).

-spec get_chat_permissions(kapps_call:call()) -> kz_json:object().
get_chat_permissions(Call) ->
    kapps_call:kvs_fetch(<<"chat_permissions">>, Call).

-spec get_request_vars(kapps_call:call()) -> kz_json:object().
get_request_vars(Call) ->
    kz_json:from_list(
      [{<<"Digits">>, get_digits_collected(Call)}
      ,{<<"RecordingUrl">>, get_recording_url(Call)}
      ,{<<"RecordingDuration">>, get_recording_duration(Call)}
      ,{<<"DialCallStatus">>, get_dial_call_status(Call)}
      ,{<<"DialCallSid">>, get_dial_call_sid(Call)}
      ,{<<"DialCallDuration">>, get_dial_call_duration(Call)}
      ,{<<"QueueSid">>, get_queue_sid(Call)}
      ,{<<"CallStatus">>, get_call_status(Call)}
      ]).

-spec iteration(kapps_call:call()) -> pos_integer().
iteration(Call) ->
    kapps_call:kvs_fetch('pivot_counter', 1, Call).

-spec increment_iteration(kapps_call:call()) -> kapps_call:call().
increment_iteration(Call) ->
    kapps_call:kvs_update_counter('pivot_counter', 1, Call).
