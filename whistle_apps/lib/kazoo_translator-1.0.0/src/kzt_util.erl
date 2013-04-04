%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
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
         ,set_call_timeout/2, get_call_timeout/1
         ,set_call_time_limit/2, get_call_time_limit/1
         ,set_record_call/2, get_record_call/1
         ,set_voice_uri/2, get_voice_uri/1
         ,set_voice_uri_method/2, get_voice_uri_method/1
         ,set_digits_collected/2, get_digits_collected/1
         ,add_digit_collected/2, clear_digits_collected/1
         ,attributes_to_proplist/1
         ,set_recording_url/2, get_recording_url/1
         ,set_recording_duration/2, get_recording_duration/1
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
        ]).

-include("kzt.hrl").

-define(SUPPORTED_METHODS, [get, post]).

-spec http_method(api_binary() | list()) -> 'get' | 'post'.
http_method(L) when is_list(L) ->
    http_method(props:get_value(method, L));
http_method(Method) ->
    case wh_util:to_atom(Method) of
        get -> get;
        post -> post;
        'GET' -> get;
        'POST' -> post;
        undefined -> post
    end.

-spec resolve_uri(nonempty_string() | ne_binary(), nonempty_string() | api_binary()) -> ne_binary().
resolve_uri(Raw, undefined) -> wh_util:to_binary(Raw);
resolve_uri(_Raw, [$h,$t,$t,$p|_]=Abs) -> wh_util:to_binary(Abs);
resolve_uri(_Raw, <<"http", _/binary>> = Abs) -> Abs;
resolve_uri(RawPath, Relative) ->
    lager:debug("taking url ~s and applying path ~s", [RawPath, Relative]),
    PathTokensRev = lists:reverse(binary:split(wh_util:to_binary(RawPath), <<"/">>, [global])),
    UrlTokens = binary:split(wh_util:to_binary(Relative), <<"/">>),

    wh_util:join_binary(
      lists:reverse(
        lists:foldl(fun(<<"..">>, []) -> [];
                       (<<"..">>, [_ | PathTokens]) -> PathTokens;
                       (<<".">>, PathTokens) -> PathTokens;
                       (<<>>, PathTokens) -> PathTokens;
                       (Segment, [LastToken|DirTokens]=PathTokens) ->
                            case filename:extension(LastToken) of
                                <<>> ->
                                    %% no extension, append Segment to Tokens
                                    [Segment | PathTokens];
                                _Ext ->
                                    %% Extension found, append Segment to DirTokens
                                    [Segment|DirTokens]
                            end
                    end, PathTokensRev, UrlTokens)
       ), <<"/">>).

%% see cf_offnet.erl
-spec offnet_req(wh_json:object(), whapps_call:call()) -> 'ok'.
offnet_req(Data, Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    CIDNum = case whapps_call:kvs_fetch(dynamic_cid, Call) of
                 undefined -> CIDNumber;
                 DynamicCID -> DynamicCID
             end,
    Req = [{<<"Call-ID">>, whapps_call:call_id(Call)}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"To-DID">>, whapps_call:request_user(Call)}
           ,{<<"Account-ID">>, whapps_call:account_id(Call)}
           ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
           ,{<<"Control-Queue">>, whapps_call:control_queue(Call)}
           ,{<<"Application-Name">>, <<"bridge">>}
           ,{<<"Flags">>, wh_json:get_value(<<"flags">>, Data)}
           ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, Data)}
           ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, Data)}
           ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
           ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
           ,{<<"Outbound-Caller-ID-Name">>, CIDName}
           ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
           ,{<<"Presence-ID">>, cf_attributes:presence_id(Call)}
           ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, Data)}
           ,{<<"Media">>, wh_json:get_value(<<"Media">>, Data)}
           | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)],
    wapi_offnet_resource:publish_req(Req).

-spec update_call_status(ne_binary(), whapps_call:call()) -> whapps_call:call().
-spec get_call_status(whapps_call:call()) -> api_binary().
update_call_status(Status, Call) ->
    whapps_call:kvs_store(<<"call_status">>, Status, Call).
get_call_status(Call) ->
    whapps_call:kvs_fetch(<<"call_status">>, Call).

-spec add_error(whapps_call:call(), ne_binary(), term()) -> whapps_call:call().
add_error(Call, K, V) ->
    case whapps_call:kvs_fetch(<<"response_errors">>, Call) of
        'undefined' ->
            whapps_call:kvs_store(<<"response_errors">>, [{K, V}], Call);
        Vs ->
            whapps_call:kvs_append_list(<<"response_errors">>, [{K, V}|Vs], Call)
    end.

get_errors(Call) -> whapps_call:kvs_fetch(<<"response_errors">>, Call).

set_hangup_dtmf(DTMF, Call) -> whapps_call:kvs_store(<<"hangup_dtmf">>, DTMF, Call).
get_hangup_dtmf(Call) -> whapps_call:kvs_fetch(<<"hangup_dtmf">>, Call).

set_record_call(R, Call) -> whapps_call:kvs_store(<<"record_call">>, R, Call).
get_record_call(Call) -> whapps_call:kvs_fetch(<<"record_call">>, Call).

set_call_timeout(T, Call) -> whapps_call:kvs_store(<<"call_timeout">>, T, Call).
get_call_timeout(Call) -> whapps_call:kvs_fetch(<<"call_timeout">>, Call).

set_call_time_limit(T, Call) -> whapps_call:kvs_store(<<"call_time_limit">>, T, Call).
get_call_time_limit(Call) -> whapps_call:kvs_fetch(<<"call_time_limit">>, Call).

set_voice_uri(Uri, Call) -> whapps_call:kvs_store(<<"voice_uri">>, Uri, Call).
get_voice_uri(Call) -> whapps_call:kvs_fetch(<<"voice_uri">>, Call).

set_voice_uri_method(M, Call) -> whapps_call:kvs_store(<<"voice_uri_method">>, M, Call).
get_voice_uri_method(Call) -> whapps_call:kvs_fetch(<<"voice_uri_method">>, Call).

set_digits_collected(Ds, Call) -> whapps_call:kvs_store(<<"digits_collected">>, Ds, Call).
get_digits_collected(Call) -> whapps_call:kvs_fetch(<<"digits_collected">>, Call).
clear_digits_collected(Call) -> whapps_call:kvs_store(<<"digits_collected">>, <<>>, Call).
add_digit_collected(D, Call) ->
    whapps_call:kvs_update(<<"digits_collected">>, fun(Ds) -> <<Ds/binary, D/binary>> end, D, Call).

set_recording_url(RU, Call) -> whapps_call:kvs_store(<<"recording_url">>, RU, Call).
get_recording_url(Call) -> whapps_call:kvs_fetch(<<"recording_url">>, Call).

set_recording_duration(RD, Call) -> whapps_call:kvs_store(<<"recording_duration">>, RD, Call).
get_recording_duration(Call) -> whapps_call:kvs_fetch(<<"recording_duration">>, Call).

set_dial_call_status(DCS, Call) -> whapps_call:kvs_store(<<"dial_call_status">>, DCS, Call).
get_dial_call_status(Call) -> whapps_call:kvs_fetch(<<"dial_call_status">>, Call).

set_dial_call_sid(DCS, Call) -> whapps_call:kvs_store(<<"dial_call_sid">>, DCS, Call).
get_dial_call_sid(Call) -> whapps_call:kvs_fetch(<<"dial_call_sid">>, Call).

set_dial_call_duration(DCS, Call) -> whapps_call:kvs_store(<<"dial_call_duration">>, DCS, Call).
get_dial_call_duration(Call) -> whapps_call:kvs_fetch(<<"dial_call_duration">>, Call).

set_queue_sid(DCS, Call) -> whapps_call:kvs_store(<<"queue_sid">>, DCS, Call).
get_queue_sid(Call) -> whapps_call:kvs_fetch(<<"queue_sid">>, Call).

set_dequeue_result(DCS, Call) -> whapps_call:kvs_store(<<"dequeue_result">>, DCS, Call).
get_dequeue_result(Call) -> whapps_call:kvs_fetch(<<"dequeue_result">>, Call).

set_dequeued_call_sid(DCS, Call) -> whapps_call:kvs_store(<<"dequeued_call_sid">>, DCS, Call).
get_dequeued_call_sid(Call) -> whapps_call:kvs_fetch(<<"dequeued_call_sid">>, Call).

set_dequeued_call_queue_time(DCS, Call) -> whapps_call:kvs_store(<<"dequeued_call_queue_time">>, DCS, Call).
get_dequeued_call_queue_time(Call) -> whapps_call:kvs_fetch(<<"dequeued_call_queue_time">>, Call).

set_dequeued_call_duration(DCS, Call) -> whapps_call:kvs_store(<<"dequeued_call_duration">>, DCS, Call).
get_dequeued_call_duration(Call) -> whapps_call:kvs_fetch(<<"dequeued_call_duration">>, Call).

set_media_meta(DCS, Call) -> whapps_call:kvs_store(<<"media_meta">>, DCS, Call).
get_media_meta(Call) -> whapps_call:kvs_fetch(<<"media_meta">>, Call).

set_amqp_listener(Pid, Call) -> whapps_call:kvs_store(<<"amqp_listener">>, Pid, Call).
get_amqp_listener(Call) -> whapps_call:kvs_fetch(<<"amqp_listener">>, Call).

get_request_vars(Call) ->
    wh_json:from_list(
      props:filter_empty(
        [{<<"Digits">>, get_digits_collected(Call)}
         ,{<<"RecordingUrl">>, get_recording_url(Call)}
         ,{<<"RecordingDuration">>, get_recording_duration(Call)}
         ,{<<"DialCallStatus">>, get_dial_call_status(Call)}
         ,{<<"DialCallSid">>, get_dial_call_sid(Call)}
         ,{<<"DialCallDuration">>, get_dial_call_duration(Call)}
         ,{<<"QueueSid">>, get_queue_sid(Call)}
         ,{<<"CallStatus">>, get_call_status(Call)}
        ])).

attributes_to_proplist(L) ->
    [{K, V} || #xmlAttribute{name=K, value=V} <- L].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_path_test() ->
    RawPath = <<"http://pivot/script.php">>,
    Relative = <<"script2.php">>,
    RawPath1 = <<"http://pivot/script2.php">>,

    ?assertEqual(RawPath1, resolve_uri(RawPath, Relative)),
    ?assertEqual(RawPath1, resolve_uri(RawPath, RawPath1)),
    ?assertEqual(RawPath1, resolve_uri(RawPath, <<"/", Relative/binary>>)).
-endif.
