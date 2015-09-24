%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
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
        ]).

-include("kzt.hrl").

-define(DEFAULT_HTTP_METHOD, 'post').

-spec http_method(api_binary() | list()) -> 'get' | 'post'.
http_method(L) when is_list(L) ->
    http_method(wh_util:to_binary(props:get_value('method', L)));
http_method('undefined') -> ?DEFAULT_HTTP_METHOD;
http_method(<<_/binary>> = Method) ->
    MethodBin = wh_util:to_lower_binary(Method),

    try wh_util:to_atom(MethodBin) of
        'get' -> 'get';
        'post' -> 'post';
        'undefined' -> ?DEFAULT_HTTP_METHOD
    catch
        _E:_R -> ?DEFAULT_HTTP_METHOD
    end;
http_method(Method) -> http_method(wh_util:to_binary(Method)).

-spec resolve_uri(ne_binary(), ne_binary()) -> ne_binary().
resolve_uri(Path, NewPath) ->
    wh_util:resolve_uri(Path, NewPath).

%% see cf_offnet.erl
-spec offnet_req(wh_proplist(), whapps_call:call()) -> 'ok'.
offnet_req(Data, Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    CIDNum = case whapps_call:kvs_fetch('dynamic_cid', Call) of
                 'undefined' -> CIDNumber;
                 DynamicCID -> DynamicCID
             end,

    Req = [{<<"Call-ID">>, whapps_call:call_id(Call)}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"To-DID">>, whapps_call:request_user(Call)}
           ,{<<"Account-ID">>, whapps_call:account_id(Call)}
           ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
           ,{<<"Control-Queue">>, whapps_call:control_queue(Call)}
           ,{<<"Application-Name">>, <<"bridge">>}
           ,{<<"Flags">>, props:get_value(<<"flags">>, Data)}
           ,{<<"Timeout">>, props:get_value(<<"timeout">>, Data)}
           ,{<<"Ignore-Early-Media">>, props:get_value(<<"ignore_early_media">>, Data)}
           ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
           ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
           ,{<<"Outbound-Caller-ID-Name">>, CIDName}
           ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
           ,{<<"Presence-ID">>, cf_attributes:presence_id(Call)}
           ,{<<"Ringback">>, props:get_value(<<"ringback">>, Data)}
           | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
          ] ++ Data,

    wh_amqp_worker:cast(Req, fun wapi_offnet_resource:publish_req/1).

-spec update_call_status(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_call_status(whapps_call:call()) -> api_binary().
update_call_status(Status, Call) ->
    whapps_call:kvs_store(<<"call_status">>, Status, Call).
get_call_status(Call) ->
    whapps_call:kvs_fetch(<<"call_status">>, Call).

-spec add_error(whapps_call:call(), ne_binary(), any()) -> whapps_call:call().
add_error(Call, K, V) ->
    case whapps_call:kvs_fetch(<<"response_errors">>, Call) of
        'undefined' ->
            whapps_call:kvs_store(<<"response_errors">>, [{K, V}], Call);
        Vs ->
            whapps_call:kvs_append_list(<<"response_errors">>, [{K, V}|Vs], Call)
    end.

-spec get_errors(whapps_call:call()) -> wh_proplist().
get_errors(Call) -> whapps_call:kvs_fetch(<<"response_errors">>, Call).

-spec set_hangup_dtmf(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_hangup_dtmf(whapps_call:call()) -> api_binary().
set_hangup_dtmf(DTMF, Call) -> whapps_call:kvs_store(<<"hangup_dtmf">>, DTMF, Call).
get_hangup_dtmf(Call) -> whapps_call:kvs_fetch(<<"hangup_dtmf">>, Call).

-spec set_digit_pressed(api_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_digit_pressed(whapps_call:call()) -> api_binary() | wh_json:object().
set_digit_pressed(DTMF, Call) -> whapps_call:kvs_store(<<"digit_pressed">>, DTMF, Call).
get_digit_pressed(Call) ->
    case whapps_call:kvs_fetch(<<"digit_pressed">>, Call) of
        'undefined' -> get_digits_pressed(Call);
        D -> D
    end.

-spec get_digits_pressed(whapps_call:call()) -> api_object().
get_digits_pressed(Call) ->
    whapps_call:kvs_fetch(<<"dtmf_collections">>, Call).

-spec set_record_call(boolean(), whapps_call:call()) -> whapps_call:call().
-spec get_record_call(whapps_call:call()) -> api_boolean().
set_record_call(R, Call) -> whapps_call:kvs_store(<<"record_call">>, R, Call).
get_record_call(Call) -> whapps_call:kvs_fetch(<<"record_call">>, Call).

-spec set_call_timeout(pos_integer(), whapps_call:call()) -> whapps_call:call().
-spec get_call_timeout(whapps_call:call()) -> api_integer().
set_call_timeout(T, Call) -> whapps_call:kvs_store(<<"call_timeout">>, T, Call).
get_call_timeout(Call) -> whapps_call:kvs_fetch(<<"call_timeout">>, Call).

-spec set_call_time_limit(pos_integer(), whapps_call:call()) -> whapps_call:call().
-spec get_call_time_limit(whapps_call:call()) -> api_integer().
set_call_time_limit(T, Call) -> whapps_call:kvs_store(<<"call_time_limit">>, T, Call).
get_call_time_limit(Call) -> whapps_call:kvs_fetch(<<"call_time_limit">>, Call).

-spec set_voice_uri(api_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_voice_uri(whapps_call:call()) -> api_binary().
set_voice_uri(Uri, Call) -> whapps_call:kvs_store(<<"voice_uri">>, Uri, Call).
get_voice_uri(Call) -> whapps_call:kvs_fetch(<<"voice_uri">>, Call).

-spec set_voice_uri_method('get' | 'post', whapps_call:call()) -> whapps_call:call().
-spec get_voice_uri_method(whapps_call:call()) -> 'get' | 'post'.
set_voice_uri_method(M, Call) -> whapps_call:kvs_store(<<"voice_uri_method">>, M, Call).
get_voice_uri_method(Call) -> whapps_call:kvs_fetch(<<"voice_uri_method">>, Call).

-spec set_digits_collected(api_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_digits_collected(whapps_call:call()) -> binary().
-spec clear_digits_collected(whapps_call:call()) -> whapps_call:call().
-spec add_digit_collected(ne_binary(), whapps_call:call()) -> whapps_call:call().
set_digits_collected(Ds, Call) -> whapps_call:kvs_store(<<"digits_collected">>, Ds, Call).
get_digits_collected(Call) -> whapps_call:kvs_fetch(<<"digits_collected">>, Call).
clear_digits_collected(Call) -> whapps_call:kvs_store(<<"digits_collected">>, <<>>, Call).
add_digit_collected(D, Call) ->
    whapps_call:kvs_update(<<"digits_collected">>, fun(<<_/binary>> = Ds) -> <<Ds/binary, D/binary>>;
                                                      (_) -> D
                                                   end
                           ,D, Call).

-spec set_recording_url(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_recording_url(whapps_call:call()) -> api_binary().
set_recording_url(RU, Call) -> whapps_call:kvs_store(<<"recording_url">>, RU, Call).
get_recording_url(Call) -> whapps_call:kvs_fetch(<<"recording_url">>, Call).

-spec set_recording_duration(pos_integer(), whapps_call:call()) -> whapps_call:call().
-spec get_recording_duration(whapps_call:call()) -> api_integer().
set_recording_duration(RD, Call) -> whapps_call:kvs_store(<<"recording_duration">>, RD, Call).
get_recording_duration(Call) -> whapps_call:kvs_fetch(<<"recording_duration">>, Call).

-spec set_recording_sid(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_recording_sid(whapps_call:call()) -> api_binary().
set_recording_sid(SID, Call) -> whapps_call:kvs_store(<<"recording_sid">>, SID, Call).
get_recording_sid(Call) -> whapps_call:kvs_fetch(<<"recording_sid">>, Call).

-spec set_transcription_sid(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_transcription_sid(whapps_call:call()) -> api_binary().
set_transcription_sid(SID, Call) -> whapps_call:kvs_store(<<"transcription_sid">>, SID, Call).
get_transcription_sid(Call) -> whapps_call:kvs_fetch(<<"transcription_sid">>, Call).

-spec set_transcription_text(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_transcription_text(whapps_call:call()) -> api_binary().
set_transcription_text(RD, Call) -> whapps_call:kvs_store(<<"transcription_text">>, RD, Call).
get_transcription_text(Call) -> whapps_call:kvs_fetch(<<"transcription_text">>, Call).

-spec set_transcription_status(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_transcription_status(whapps_call:call()) -> api_binary().
set_transcription_status(RD, Call) -> whapps_call:kvs_store(<<"transcription_status">>, RD, Call).
get_transcription_status(Call) -> whapps_call:kvs_fetch(<<"transcription_status">>, Call).

-spec set_transcription_url(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_transcription_url(whapps_call:call()) -> api_binary().
set_transcription_url(RD, Call) -> whapps_call:kvs_store(<<"transcription_url">>, RD, Call).
get_transcription_url(Call) -> whapps_call:kvs_fetch(<<"transcription_url">>, Call).

-spec set_dial_call_status(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_dial_call_status(whapps_call:call()) -> api_binary().
set_dial_call_status(DCS, Call) -> whapps_call:kvs_store(<<"dial_call_status">>, DCS, Call).
get_dial_call_status(Call) -> whapps_call:kvs_fetch(<<"dial_call_status">>, Call).

-spec set_dial_call_sid(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_dial_call_sid(whapps_call:call()) -> api_binary().
set_dial_call_sid(DCS, Call) -> whapps_call:kvs_store(<<"dial_call_sid">>, DCS, Call).
get_dial_call_sid(Call) -> whapps_call:kvs_fetch(<<"dial_call_sid">>, Call).

-spec set_dial_call_duration(pos_integer(), whapps_call:call()) -> whapps_call:call().
-spec get_dial_call_duration(whapps_call:call()) -> api_integer().
set_dial_call_duration(DCS, Call) -> whapps_call:kvs_store(<<"dial_call_duration">>, DCS, Call).
get_dial_call_duration(Call) -> whapps_call:kvs_fetch(<<"dial_call_duration">>, Call).

-spec set_queue_sid(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_queue_sid(whapps_call:call()) -> api_binary().
set_queue_sid(DCS, Call) -> whapps_call:kvs_store(<<"queue_sid">>, DCS, Call).
get_queue_sid(Call) -> whapps_call:kvs_fetch(<<"queue_sid">>, Call).

-spec set_dequeue_result(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_dequeue_result(whapps_call:call()) -> api_binary().
set_dequeue_result(DCS, Call) -> whapps_call:kvs_store(<<"dequeue_result">>, DCS, Call).
get_dequeue_result(Call) -> whapps_call:kvs_fetch(<<"dequeue_result">>, Call).

-spec set_dequeued_call_sid(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_dequeued_call_sid(whapps_call:call()) -> api_binary().
set_dequeued_call_sid(DCS, Call) -> whapps_call:kvs_store(<<"dequeued_call_sid">>, DCS, Call).
get_dequeued_call_sid(Call) -> whapps_call:kvs_fetch(<<"dequeued_call_sid">>, Call).

-spec set_dequeued_call_queue_time(pos_integer(), whapps_call:call()) -> whapps_call:call().
-spec get_dequeued_call_queue_time(whapps_call:call()) -> api_integer().
set_dequeued_call_queue_time(DCS, Call) -> whapps_call:kvs_store(<<"dequeued_call_queue_time">>, DCS, Call).
get_dequeued_call_queue_time(Call) -> whapps_call:kvs_fetch(<<"dequeued_call_queue_time">>, Call).

-spec set_dequeued_call_duration(pos_integer(), whapps_call:call()) -> whapps_call:call().
-spec get_dequeued_call_duration(whapps_call:call()) -> api_integer().
set_dequeued_call_duration(DCS, Call) -> whapps_call:kvs_store(<<"dequeued_call_duration">>, DCS, Call).
get_dequeued_call_duration(Call) -> whapps_call:kvs_fetch(<<"dequeued_call_duration">>, Call).

-spec set_media_meta(wh_json:object(), whapps_call:call()) -> whapps_call:call().
-spec get_media_meta(whapps_call:call()) -> api_object().
set_media_meta(DCS, Call) -> whapps_call:kvs_store(<<"media_meta">>, DCS, Call).
get_media_meta(Call) -> whapps_call:kvs_fetch(<<"media_meta">>, Call).

-spec set_amqp_listener(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_amqp_listener(whapps_call:call()) -> api_binary().
set_amqp_listener(Pid, Call) -> whapps_call:kvs_store(<<"amqp_listener">>, Pid, Call).
get_amqp_listener(Call) -> whapps_call:kvs_fetch(<<"amqp_listener">>, Call).

-spec set_gather_pidref(pid_ref() | 'undefined', whapps_call:call()) ->
                               whapps_call:call().
-spec get_gather_pidref(whapps_call:call()) ->
                               pid_ref() | 'undefined'.
set_gather_pidref('undefined', Call) ->
    whapps_call:kvs_store(<<"gather_pidref">>, 'undefined', Call);
set_gather_pidref({_, _}=PidRef, Call) ->
    gen_listener:cast(get_amqp_listener(Call), {'add_event_handler', PidRef}),
    whapps_call:kvs_store(<<"gather_pidref">>, PidRef, Call).
get_gather_pidref(Call) -> whapps_call:kvs_fetch(<<"gather_pidref">>, Call).

-spec set_conference_profile(wh_json:object(), whapps_call:call()) ->
                                    whapps_call:call().
-spec get_conference_profile(whapps_call:call()) ->
                                    wh_json:object().
set_conference_profile(JObj, Call) ->
    whapps_call:kvs_store(<<"conference_profile">>, JObj, Call).
get_conference_profile(Call) ->
    whapps_call:kvs_fetch(<<"conference_profile">>, Call).

-spec set_caller_controls(wh_json:object(), whapps_call:call()) ->
                                 whapps_call:call().
-spec get_caller_controls(whapps_call:call()) ->
                                 wh_json:object().
set_caller_controls(JObj, Call) ->
    whapps_call:kvs_store(<<"caller_controls">>, JObj, Call).
get_caller_controls(Call) ->
    whapps_call:kvs_fetch(<<"caller_controls">>, Call).

-spec set_advertise(wh_json:object(), whapps_call:call()) ->
                           whapps_call:call().
-spec get_advertise(whapps_call:call()) ->
                           wh_json:object().
set_advertise(JObj, Call) ->
    whapps_call:kvs_store(<<"advertise">>, JObj, Call).
get_advertise(Call) ->
    whapps_call:kvs_fetch(<<"advertise">>, Call).

-spec set_chat_permissions(wh_json:object(), whapps_call:call()) ->
                                  whapps_call:call().
-spec get_chat_permissions(whapps_call:call()) -> wh_json:object().
set_chat_permissions(JObj, Call) ->
    whapps_call:kvs_store(<<"chat_permissions">>, JObj, Call).
get_chat_permissions(Call) ->
    whapps_call:kvs_fetch(<<"chat_permissions">>, Call).

-spec get_request_vars(whapps_call:call()) -> wh_json:object().
get_request_vars(Call) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Digits">>, get_digits_collected(Call)}
         ,{<<"RecordingUrl">>, get_recording_url(Call)}
         ,{<<"RecordingDuration">>, get_recording_duration(Call)}
         ,{<<"DialCallStatus">>, get_dial_call_status(Call)}
         ,{<<"DialCallSid">>, get_dial_call_sid(Call)}
         ,{<<"DialCallDuration">>, get_dial_call_duration(Call)}
         ,{<<"QueueSid">>, get_queue_sid(Call)}
         ,{<<"CallStatus">>, get_call_status(Call)}
        ]
       )
     ).
