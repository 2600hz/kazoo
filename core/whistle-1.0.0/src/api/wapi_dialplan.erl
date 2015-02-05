%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Dialplan API commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_dialplan).

-compile({'no_auto_import', [error/1]}).

-export([v/1]).

-export([optional_bridge_req_headers/0]).
-export([optional_bridge_req_endpoint_headers/0]).

-export([bridge/1, bridge_v/1, bridge_endpoint/1, bridge_endpoint_v/1
         ,unbridge/1, unbridge_v/1
         ,page/1, page_v/1
         ,store/1, store_v/1, store_amqp_resp/1, store_amqp_resp_v/1
         ,store_http_resp/1, store_http_resp_v/1
         ,noop/1, noop_v/1
         ,fetch/1, fetch_v/1
         ,respond/1, respond_v/1
         ,redirect/1, redirect_v/1
         ,progress/1, progress_v/1
         ,ring/1, ring_v/1
         ,receive_fax/1, receive_fax_v/1
         ,store_fax/1, store_fax_v/1
         ,execute_extension/1, execute_extension_v/1
         ,play/1, play_v/1, playstop/1, playstop_v/1
         ,tts/1, tts_v/1
         ,record/1, record_v/1
         ,record_call/1, record_call_v/1
         ,answer/1, answer_v/1
         ,echo/1, echo_v/1
         ,privacy/1, privacy_v/1
         ,hold/1, hold_v/1
         ,park/1, park_v/1
         ,play_and_collect_digits/1, play_and_collect_digits_v/1
         ,call_pickup/1, call_pickup_v/1
         ,connect_leg/1, connect_leg_v/1
         ,eavesdrop/1, eavesdrop_v/1
         ,hangup/1, hangup_v/1
         ,say/1, say_v/1
         ,sleep/1, sleep_v/1
         ,tone_detect/1, tone_detect_v/1
         ,set/1, set_v/1
         ,set_terminators/1, set_terminators_v/1
         ,send_dtmf/1, send_dtmf_v/1
         ,tones/1, tones_req_tone/1, tones_v/1, tones_req_tone_v/1
         ,tones_req_tone_headers/1
         ,conference/1, conference_v/1
         ,originate_ready/1, originate_ready_v/1
         ,originate_execute/1, originate_execute_v/1
         ,metaflow/1, metaflow_v/1
         ,fax_detection/1, fax_detection_v/1
        ]).

-export([queue/1, queue_v/1
         ,error/1, error_v/1
        ]).

%% API Helpers
-export([dial_method_single/0
         ,dial_method_simultaneous/0
         ,terminators/1, terminators_v/1
         ,local_store_url/2, offsite_store_url/2
        ]).

-export([bind_q/2
         ,unbind_q/2
        ]).

-export([declare_exchanges/0]).

-export([publish_action/2, publish_action/3
         ,publish_error/2, publish_error/3
         ,publish_command/2, publish_command/3
         ,publish_metaflow/1, publish_metaflow/2
         ,publish_originate_ready/2, publish_originate_ready/3
         ,publish_originate_execute/2, publish_originate_execute/3
        ]).

-include_lib("whistle/include/wh_api.hrl").
-include_lib("whistle/include/wh_log.hrl").

-include("wapi_dialplan.hrl").

-spec optional_bridge_req_headers() -> ne_binaries().
optional_bridge_req_headers() ->
    ?OPTIONAL_BRIDGE_REQ_HEADERS.

-spec optional_bridge_req_endpoint_headers() -> ne_binaries().
optional_bridge_req_endpoint_headers() ->
    ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS.

-spec b_leg_events_v(ne_binaries()) -> boolean().
b_leg_events_v(Events) ->
    lists:all(fun(ApiEvent) ->
                      lists:member(ApiEvent, ?CALL_EVENTS)
              end, Events).

%% Takes a generic API JObj, determines what type it is, and calls the appropriate validator
-spec v(api_terms()) -> boolean().
v(Prop) when is_list(Prop) ->
    v(Prop, props:get_value(<<"Application-Name">>, Prop));
v(JObj) ->
    v(wh_json:to_proplist(JObj)).

-spec v(api_terms(), binary()) -> boolean().
v(Prop, DPApp) ->
    try
        VFun = wh_util:to_atom(<<DPApp/binary, "_v">>),
        case lists:keyfind(VFun, 1, ?MODULE:module_info(exports)) of
            'false' -> throw({invalid_dialplan_object, Prop});
            {_, 1} -> ?MODULE:VFun(Prop)
        end
    catch
        _:R ->
            throw({R, Prop})
    end.

%%--------------------------------------------------------------------
%% @doc Bridge a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec bridge(api_terms()) -> api_formatter_return().
bridge(Prop) when is_list(Prop) ->
    EPs = [begin
               {'ok', EPProps} = bridge_endpoint_headers(EP),
               wh_json:from_list(EPProps)
           end
           || EP <- props:get_value(<<"Endpoints">>, Prop, []),
              bridge_endpoint_v(EP)],
    Prop1 = [ {<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    case bridge_v(Prop1) of
        'true' -> wh_api:build_message(Prop1, ?BRIDGE_REQ_HEADERS, ?OPTIONAL_BRIDGE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for bridge_req"}
    end;
bridge(JObj) ->
    bridge(wh_json:to_proplist(JObj)).

-spec bridge_v(api_terms()) -> boolean().
bridge_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?BRIDGE_REQ_HEADERS, ?BRIDGE_REQ_VALUES, ?BRIDGE_REQ_TYPES);
bridge_v(JObj) ->
    bridge_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Unbridge a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec unbridge(api_terms()) -> api_formatter_return().
unbridge(Prop) when is_list(Prop) ->
    case unbridge_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UNBRIDGE_REQ_HEADERS, ?OPTIONAL_UNBRIDGE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for unbridge_req"}
    end;
unbridge(JObj) ->
    unbridge(wh_json:to_proplist(JObj)).

-spec unbridge_v(api_terms()) -> boolean().
unbridge_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UNBRIDGE_REQ_HEADERS, ?UNBRIDGE_REQ_VALUES, ?UNBRIDGE_REQ_TYPES);
unbridge_v(JObj) ->
    unbridge_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Endpoints for bridging a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec bridge_endpoint(api_terms()) ->
                             {'ok', wh_proplist()} |
                             {'error', string()}.
bridge_endpoint(Prop) when is_list(Prop) ->
    case bridge_endpoint_v(Prop) of
        'true' -> wh_api:build_message_specific(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
        'false' -> {'error', "Proplist failed validation for bridge_req_endpoint"}
    end;
bridge_endpoint(JObj) ->
    bridge_endpoint(wh_json:to_proplist(JObj)).

-spec bridge_endpoint_headers(api_terms()) ->
                                     {'ok', wh_proplist()} |
                                     {'error', string()}.
bridge_endpoint_headers(Prop) when is_list(Prop) ->
    wh_api:build_message_specific_headers(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
bridge_endpoint_headers(JObj) ->
    bridge_endpoint_headers(wh_json:to_proplist(JObj)).

-spec bridge_endpoint_v(api_terms()) -> boolean().
bridge_endpoint_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?BRIDGE_REQ_ENDPOINT_VALUES, ?BRIDGE_REQ_ENDPOINT_TYPES);
bridge_endpoint_v(JObj) ->
    bridge_endpoint_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Page a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec page(api_terms()) -> api_formatter_return().
page(Prop) when is_list(Prop) ->
    EPs = [begin
               {'ok', EPProps} = bridge_endpoint_headers(EP),
               wh_json:from_list(EPProps)
           end
           || EP <- props:get_value(<<"Endpoints">>, Prop, []),
              bridge_endpoint_v(EP)
          ],
    Prop1 = [ {<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    case page_v(Prop1) of
        'true' -> wh_api:build_message(Prop1, ?PAGE_REQ_HEADERS, ?OPTIONAL_PAGE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for page_req"}
    end;
page(JObj) -> page(wh_json:to_proplist(JObj)).

-spec page_v(api_terms()) -> boolean().
page_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PAGE_REQ_HEADERS, ?PAGE_REQ_VALUES, ?PAGE_REQ_TYPES);
page_v(JObj) -> page_v(wh_json:to_proplist(JObj)).

-spec store(api_terms()) ->
                   {'ok', wh_proplist()} |
                   {'error', string()}.
store(Prop) when is_list(Prop) ->
    case store_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?STORE_REQ_HEADERS, ?OPTIONAL_STORE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for store"}
    end;
store(JObj) -> store(wh_json:to_proplist(JObj)).

-spec store_v(api_terms()) -> boolean().
store_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STORE_REQ_HEADERS, ?STORE_REQ_VALUES, ?STORE_REQ_TYPES);
store_v(JObj) -> store_v(wh_json:to_proplist(JObj)).

-spec store_amqp_resp(api_terms()) -> api_formatter_return().
store_amqp_resp(Prop) when is_list(Prop) ->
    case store_amqp_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?STORE_AMQP_RESP_HEADERS, ?OPTIONAL_STORE_AMQP_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validate for store_amqp_resp"}
    end;
store_amqp_resp(JObj) -> store_amqp_resp(wh_json:to_proplist(JObj)).

-spec store_amqp_resp_v(api_terms()) -> boolean().
store_amqp_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STORE_AMQP_RESP_HEADERS, ?STORE_AMQP_RESP_VALUES, ?STORE_AMQP_RESP_TYPES);
store_amqp_resp_v(JObj) -> store_amqp_resp_v(wh_json:to_proplist(JObj)).

-spec store_http_resp(api_terms()) -> api_formatter_return().
store_http_resp(Prop) when is_list(Prop) ->
    case store_http_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?STORE_HTTP_RESP_HEADERS, ?OPTIONAL_STORE_HTTP_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validate for store_http_resp"}
    end;
store_http_resp(JObj) -> store_http_resp(wh_json:to_proplist(JObj)).

-spec store_http_resp_v(api_terms()) -> boolean().
store_http_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STORE_HTTP_RESP_HEADERS, ?STORE_HTTP_RESP_VALUES, ?STORE_HTTP_RESP_TYPES);
store_http_resp_v(JObj) -> store_http_resp_v(wh_json:to_proplist(JObj)).

-spec store_media_content_v(binary() | 'eof') -> boolean().
store_media_content_v(V) ->
    is_binary(V) orelse V =:= 'eof'.

%%--------------------------------------------------------------------
%% @doc Create a DTMF (or DTMFs) on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec send_dtmf(api_terms()) -> api_formatter_return() .
send_dtmf(Prop) when is_list(Prop) ->
    case send_dtmf_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SEND_DTMF_HEADERS, ?OPTIONAL_SEND_DTMF_HEADERS);
        'false' -> {'error', "Prop failed validation for send_dtmf"}
    end;
send_dtmf(JObj) -> send_dtmf(wh_json:to_proplist(JObj)).

-spec send_dtmf_v(api_terms()) -> boolean().
send_dtmf_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SEND_DTMF_HEADERS, ?SEND_DTMF_VALUES, ?SEND_DTMF_TYPES);
send_dtmf_v(JObj) -> send_dtmf_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Create a tone on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec tones(api_terms()) -> api_formatter_return() .
tones(Prop) when is_list(Prop) ->
    Tones = [begin
                 {'ok', TonesProp} = tones_req_tone_headers(Tone),
                 wh_json:from_list(TonesProp)
             end
             || Tone <- props:get_value(<<"Tones">>, Prop, []),
                tones_req_tone_v(Tone)],
    Prop1 = [ {<<"Tones">>, Tones} | props:delete(<<"Tones">>, Prop)],
    case tones_v(Prop1) of
        'true' -> wh_api:build_message(Prop1, ?TONES_REQ_HEADERS, ?OPTIONAL_TONES_REQ_HEADERS);
        'false' -> {'error', "Prop failed validation for tones_req"}
    end;
tones(JObj) -> tones(wh_json:to_proplist(JObj)).

-spec tones_v(api_terms()) -> boolean().
tones_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?TONES_REQ_HEADERS, ?TONES_REQ_VALUES, ?TONES_REQ_TYPES);
tones_v(JObj) -> tones_v(wh_json:to_proplist(JObj)).

-spec tone_timeout_v(term()) -> boolean().
tone_timeout_v(Timeout) ->
    %% <<"+123">> converts to 123, so yay!
    try wh_util:to_integer(Timeout) of
        T when T < 0 -> 'false';
        _ -> 'true'
    catch
        _:_ -> 'false'
    end.

%%--------------------------------------------------------------------
%% @doc A Tone within a Tones request - see wiki
%% Takes proplist and returns a proplist
%% @end
%%--------------------------------------------------------------------
-spec tones_req_tone(api_terms()) -> api_formatter_return().
tones_req_tone(Prop) when is_list(Prop) ->
    case tones_req_tone_v(Prop) of
        'true' -> wh_api:build_message_specific(Prop, ?TONES_REQ_TONE_HEADERS, ?OPTIONAL_TONES_REQ_TONE_HEADERS);
        'false' -> {'error', "Proplist failed validation for tones_req_tone"}
    end;
tones_req_tone(JObj) -> tones_req_tone(wh_json:to_proplist(JObj)).

-spec tones_req_tone_v(api_terms()) -> boolean().
tones_req_tone_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?TONES_REQ_TONE_HEADERS, ?TONES_REQ_TONE_VALUES, ?TONES_REQ_TONE_TYPES);
tones_req_tone_v(JObj) -> tones_req_tone_v(wh_json:to_proplist(JObj)).

-spec tones_req_tone_headers(api_terms()) ->
                                    {'ok', wh_proplist()} |
                                    {'error', string()}.
tones_req_tone_headers(Prop) when is_list(Prop) ->
    wh_api:build_message_specific_headers(Prop, ?TONES_REQ_TONE_HEADERS, ?OPTIONAL_TONES_REQ_TONE_HEADERS);
tones_req_tone_headers(JObj) -> tones_req_tone_headers(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Detect tones on the line
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec tone_detect(api_terms()) -> api_formatter_return().
tone_detect(Prop) when is_list(Prop) ->
    case tone_detect_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?TONE_DETECT_REQ_HEADERS, ?OPTIONAL_TONE_DETECT_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for tone_detect"}
    end;
tone_detect(JObj) -> tone_detect(wh_json:to_proplist(JObj)).

-spec tone_detect_v(api_terms()) -> boolean().
tone_detect_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?TONE_DETECT_REQ_HEADERS, ?TONE_DETECT_REQ_VALUES, ?TONE_DETECT_REQ_TYPES);
tone_detect_v(JObj) -> tone_detect_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Send a list of dialplan applications in bulk - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec queue(api_terms()) -> api_formatter_return().
queue(Prop) when is_list(Prop) ->
    case queue_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUEUE_REQ_HEADERS, ?OPTIONAL_QUEUE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for queue_req"}
    end;
queue(JObj) -> queue(wh_json:to_proplist(JObj)).

-spec queue_v(api_terms()) -> boolean().
queue_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUEUE_REQ_HEADERS, ?QUEUE_REQ_VALUES, ?QUEUE_REQ_TYPES);
queue_v(JObj) -> queue_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Play media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec play(api_terms()) -> api_formatter_return().
play(Prop) when is_list(Prop) ->
    case play_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PLAY_REQ_HEADERS, ?OPTIONAL_PLAY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for play"}
    end;
play(JObj) -> play(wh_json:to_proplist(JObj)).

-spec play_v(api_terms()) -> boolean().
play_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PLAY_REQ_HEADERS, ?PLAY_REQ_VALUES, ?PLAY_REQ_TYPES);
play_v(JObj) -> play_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Stop media from playing - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec playstop(api_terms()) -> api_formatter_return().
playstop(Prop) when is_list(Prop) ->
    case playstop_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PLAY_STOP_REQ_HEADERS, ?OPTIONAL_PLAY_STOP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for playstop"}
    end;
playstop(JObj) -> playstop(wh_json:to_proplist(JObj)).

-spec playstop_v(api_terms()) -> boolean().
playstop_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PLAY_STOP_REQ_HEADERS, ?PLAY_STOP_REQ_VALUES, ?PLAY_STOP_REQ_TYPES);
playstop_v(JObj) -> playstop_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc TTS - Text-to-speech - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec tts(api_terms()) -> api_formatter_return().
tts(Prop) when is_list(Prop) ->
    case tts_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?TTS_REQ_HEADERS, ?OPTIONAL_TTS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for tts"}
    end;
tts(JObj) -> tts(wh_json:to_proplist(JObj)).

-spec tts_v(api_terms()) -> boolean().
tts_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?TTS_REQ_HEADERS, ?TTS_REQ_VALUES, ?TTS_REQ_TYPES);
tts_v(JObj) -> tts_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Record media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec record(api_terms()) -> api_formatter_return().
record(Prop) when is_list(Prop) ->
    case record_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RECORD_REQ_HEADERS, ?OPTIONAL_RECORD_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for record_req"}
    end;
record(JObj) -> record(wh_json:to_proplist(JObj)).

-spec record_v(api_terms()) -> boolean().
record_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RECORD_REQ_HEADERS, ?RECORD_REQ_VALUES, ?RECORD_REQ_TYPES);
record_v(JObj) -> record_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Record call media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec record_call(api_terms()) -> api_formatter_return().
record_call(Prop) when is_list(Prop) ->
    case record_call_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RECORD_CALL_REQ_HEADERS, ?OPTIONAL_RECORD_CALL_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for record_call_req"}
    end;
record_call(JObj) -> record_call(wh_json:to_proplist(JObj)).

-spec record_call_v(api_terms()) -> boolean().
record_call_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RECORD_CALL_REQ_HEADERS, ?RECORD_CALL_REQ_VALUES, ?RECORD_CALL_REQ_TYPES);
record_call_v(JObj) -> record_call_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Answer a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec answer(api_terms()) -> api_formatter_return().
answer(Prop) when is_list(Prop) ->
    case answer_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?ANSWER_REQ_HEADERS, ?OPTIONAL_ANSWER_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for answer_req"}
    end;
answer(JObj) -> answer(wh_json:to_proplist(JObj)).

-spec answer_v(api_terms()) -> boolean().
answer_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ANSWER_REQ_HEADERS, ?ANSWER_REQ_VALUES, ?ANSWER_REQ_TYPES);
answer_v(JObj) -> answer_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Echo a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec echo(api_terms()) -> api_formatter_return().
echo(Prop) when is_list(Prop) ->
    case echo_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?ECHO_REQ_HEADERS, ?OPTIONAL_ECHO_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for echo"}
    end;
echo(JObj) -> echo(wh_json:to_proplist(JObj)).

-spec echo_v(api_terms()) -> boolean().
echo_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ECHO_REQ_HEADERS, ?ECHO_REQ_VALUES, ?ECHO_REQ_TYPES);
echo_v(JObj) -> echo_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Privacy
%%
%% @end
%%--------------------------------------------------------------------
-spec privacy(api_terms()) -> api_formatter_return().
privacy(Prop) when is_list(Prop) ->
    case privacy_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PRIVACY_REQ_HEADERS, ?OPTIONAL_PRIVACY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for privacy"}
    end;
privacy(JObj) -> privacy(wh_json:to_proplist(JObj)).

-spec privacy_v(api_terms()) -> boolean().
privacy_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PRIVACY_REQ_HEADERS, ?PRIVACY_REQ_VALUES, ?PRIVACY_REQ_TYPES);
privacy_v(JObj) -> privacy_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Progress a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec progress(api_terms()) -> api_formatter_return().
progress(Prop) when is_list(Prop) ->
    case progress_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PROGRESS_REQ_HEADERS, ?OPTIONAL_PROGRESS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for progress_req"}
    end;
progress(JObj) -> progress(wh_json:to_proplist(JObj)).

-spec progress_v(api_terms()) -> boolean().
progress_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PROGRESS_REQ_HEADERS, ?PROGRESS_REQ_VALUES, ?PROGRESS_REQ_TYPES);
progress_v(JObj) -> progress_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Ring a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec ring(api_terms()) -> api_formatter_return().
ring(Prop) when is_list(Prop) ->
    case ring_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RING_REQ_HEADERS, ?OPTIONAL_RING_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for ring_req"}
    end;
ring(JObj) -> ring(wh_json:to_proplist(JObj)).

-spec ring_v(api_terms()) -> boolean().
ring_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RING_REQ_HEADERS, ?RING_REQ_VALUES, ?RING_REQ_TYPES);
ring_v(JObj) -> ring_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Receive a fax, storing it to local disk - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec receive_fax(api_terms()) -> api_formatter_return().
receive_fax(Prop) when is_list(Prop) ->
    case receive_fax_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RECV_FAX_HEADERS, ?OPTIONAL_RECV_FAX_HEADERS);
        'false' -> {'error', "Proplist failed validation for receive_fax"}
    end;
receive_fax(JObj) -> receive_fax(wh_json:to_proplist(JObj)).

-spec receive_fax_v(api_terms()) -> boolean().
receive_fax_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RECV_FAX_HEADERS, ?RECV_FAX_VALUES, ?RECV_FAX_TYPES);
receive_fax_v(JObj) -> receive_fax_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Store a fax, storing it to the DB - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec store_fax(api_terms()) -> api_formatter_return().
store_fax(Prop) when is_list(Prop) ->
    case store_fax_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?STORE_FAX_HEADERS, ?OPTIONAL_STORE_FAX_HEADERS);
        'false' -> {'error', "Proplist failed validation for store_fax"}
    end;
store_fax(JObj) ->
    store_fax(wh_json:to_proplist(JObj)).

-spec store_fax_v(api_terms()) -> boolean().
store_fax_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STORE_FAX_HEADERS, ?STORE_FAX_VALUES, ?STORE_FAX_TYPES);
store_fax_v(JObj) ->
    store_fax_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Hangup a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec hangup(api_terms()) -> api_formatter_return().
hangup(Prop) when is_list(Prop) ->
    case hangup_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?HANGUP_REQ_HEADERS, ?OPTIONAL_HANGUP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for hangup_req"}
    end;
hangup(JObj) ->
    hangup(wh_json:to_proplist(JObj)).

-spec hangup_v(api_terms()) -> boolean().
hangup_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?HANGUP_REQ_HEADERS, ?HANGUP_REQ_VALUES, ?HANGUP_REQ_TYPES);
hangup_v(JObj) ->
    hangup_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Hold a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec hold(api_terms()) -> api_formatter_return().
hold(Prop) when is_list(Prop) ->
    case hold_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?HOLD_REQ_HEADERS, ?OPTIONAL_HOLD_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for hold_req"}
    end;
hold(JObj) ->
    hold(wh_json:to_proplist(JObj)).

-spec hold_v(api_terms()) -> boolean().
hold_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?HOLD_REQ_HEADERS, ?HOLD_REQ_VALUES, ?HOLD_REQ_TYPES);
hold_v(JObj) ->
    hold_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Park a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec park(api_terms()) -> api_formatter_return().
park(Prop) when is_list(Prop) ->
    case park_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PARK_REQ_HEADERS, ?OPTIONAL_PARK_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for park_req"}
    end;
park(JObj) ->
    park(wh_json:to_proplist(JObj)).

-spec park_v(api_terms()) -> boolean().
park_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARK_REQ_HEADERS, ?PARK_REQ_VALUES, ?PARK_REQ_TYPES);
park_v(JObj) ->
    park_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Set Custom Channel variables - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec set(api_terms()) -> api_formatter_return().
set(Prop) when is_list(Prop) ->
    case set_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SET_REQ_HEADERS, ?OPTIONAL_SET_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for set_req"}
    end;
set(JObj) ->
    set(wh_json:to_proplist(JObj)).

-spec set_v(api_terms()) -> boolean() .
set_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SET_REQ_HEADERS, ?SET_REQ_VALUES, ?SET_REQ_TYPES);
set_v(JObj) ->
    set_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Set Terminators for playback/record
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec set_terminators(api_terms()) -> api_formatter_return().
set_terminators(Prop) when is_list(Prop) ->
    case set_terminators_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SET_TERM_HEADERS, ?OPTIONAL_SET_TERM_HEADERS);
        'false' -> {'error', "Proplist failed validation for set_terminators"}
    end;
set_terminators(JObj) ->
    set_terminators(wh_json:to_proplist(JObj)).

-spec set_terminators_v(api_terms()) -> boolean() .
set_terminators_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SET_TERM_HEADERS, ?SET_TERM_VALUES, ?SET_TERM_TYPES);
set_terminators_v(JObj) ->
    set_terminators_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Fetch Custom Channel variables - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fetch(api_terms()) -> api_formatter_return().
fetch(Prop) when is_list(Prop) ->
    case fetch_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?FETCH_REQ_HEADERS, ?OPTIONAL_FETCH_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for fetch_req"}
    end;
fetch(JObj) ->
    fetch(wh_json:to_proplist(JObj)).

-spec fetch_v(api_terms()) -> boolean().
fetch_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FETCH_REQ_HEADERS, ?FETCH_REQ_VALUES, ?FETCH_REQ_TYPES);
fetch_v(JObj) ->
    fetch_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Play media and record digits - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec play_and_collect_digits(api_terms()) -> api_formatter_return().
play_and_collect_digits(Prop) when is_list(Prop) ->
    case play_and_collect_digits_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PLAY_COLLECT_DIGITS_REQ_HEADERS, ?OPTIONAL_PLAY_COLLECT_DIGITS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for play_collect_digits_req"}
    end;
play_and_collect_digits(JObj) ->
    play_and_collect_digits(wh_json:to_proplist(JObj)).

-spec play_and_collect_digits_v(api_terms()) -> boolean().
play_and_collect_digits_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PLAY_COLLECT_DIGITS_REQ_HEADERS, ?PLAY_COLLECT_DIGITS_REQ_VALUES, ?PLAY_COLLECT_DIGITS_REQ_TYPES);
play_and_collect_digits_v(JObj) ->
    play_and_collect_digits_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Pickup a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_pickup(api_terms()) -> api_formatter_return().
call_pickup(Prop) when is_list(Prop) ->
    case call_pickup_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CALL_PICKUP_REQ_HEADERS, ?OPTIONAL_CALL_PICKUP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_pickup_req"}
    end;
call_pickup(JObj) ->
    call_pickup(wh_json:to_proplist(JObj)).

-spec call_pickup_v(api_terms()) -> boolean().
call_pickup_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_PICKUP_REQ_HEADERS, ?CALL_PICKUP_REQ_VALUES, ?CALL_PICKUP_REQ_TYPES);
call_pickup_v(JObj) ->
    call_pickup_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Connect a leg to the current leg - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec connect_leg(api_terms()) -> api_formatter_return().
connect_leg(Prop) when is_list(Prop) ->
    case connect_leg_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CONNECT_LEG_REQ_HEADERS, ?OPTIONAL_CONNECT_LEG_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for connect_leg_req"}
    end;
connect_leg(JObj) ->
    connect_leg(wh_json:to_proplist(JObj)).

-spec connect_leg_v(api_terms()) -> boolean().
connect_leg_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONNECT_LEG_REQ_HEADERS, ?CONNECT_LEG_REQ_VALUES, ?CONNECT_LEG_REQ_TYPES);
connect_leg_v(JObj) ->
    connect_leg_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Eavesdrop - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec eavesdrop(api_terms()) -> api_formatter_return().
eavesdrop(Prop) when is_list(Prop) ->
    case eavesdrop_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?EAVESDROP_REQ_HEADERS, ?OPTIONAL_EAVESDROP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for eavesdrop_req"}
    end;
eavesdrop(JObj) ->
    eavesdrop(wh_json:to_proplist(JObj)).

-spec eavesdrop_v(api_terms()) -> boolean().
eavesdrop_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?EAVESDROP_REQ_HEADERS, ?EAVESDROP_REQ_VALUES, ?EAVESDROP_REQ_TYPES);
eavesdrop_v(JObj) ->
    eavesdrop_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Say - convert text to speech - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec say(api_terms()) -> api_formatter_return().
say(Prop) when is_list(Prop) ->
    case say_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SAY_REQ_HEADERS, ?OPTIONAL_SAY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for say_req"}
    end;
say(JObj) ->
    say(wh_json:to_proplist(JObj)).

-spec say_v(api_terms()) -> boolean().
say_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SAY_REQ_HEADERS, ?SAY_REQ_VALUES, ?SAY_REQ_TYPES);
say_v(JObj) ->
    say_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Respond a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec respond(api_terms()) -> api_formatter_return().
respond(Prop) when is_list(Prop) ->
    case respond_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RESPOND_REQ_HEADERS, ?OPTIONAL_RESPOND_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for respond_req"}
    end;
respond(JObj) ->
    respond(wh_json:to_proplist(JObj)).

-spec respond_v(api_terms()) -> boolean().
respond_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESPOND_REQ_HEADERS, ?RESPOND_REQ_VALUES, ?RESPOND_REQ_TYPES);
respond_v(JObj) ->
    respond_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Redirect a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec redirect(api_terms()) -> api_formatter_return().
redirect(Prop) when is_list(Prop) ->
    case redirect_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?REDIRECT_REQ_HEADERS, ?OPTIONAL_REDIRECT_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for redirect_req"}
    end;
redirect(JObj) ->
    redirect(wh_json:to_proplist(JObj)).

-spec redirect_v(api_terms()) -> boolean().
redirect_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?REDIRECT_REQ_HEADERS, ?REDIRECT_REQ_VALUES, ?REDIRECT_REQ_TYPES);
redirect_v(JObj) ->
    redirect_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Execute_Extension a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec execute_extension(api_terms()) -> api_formatter_return().
execute_extension(Prop) when is_list(Prop) ->
    case execute_extension_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?EXECUTE_EXTENSION_REQ_HEADERS, ?OPTIONAL_EXECUTE_EXTENSION_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for execute_extension_req"}
    end;
execute_extension(JObj) ->
    execute_extension(wh_json:to_proplist(JObj)).

-spec execute_extension_v(api_terms()) -> boolean().
execute_extension_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?EXECUTE_EXTENSION_REQ_HEADERS, ?EXECUTE_EXTENSION_REQ_VALUES, ?EXECUTE_EXTENSION_REQ_TYPES);
execute_extension_v(JObj) ->
    execute_extension_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Sleep - Pauses execution - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec sleep(api_terms()) -> api_formatter_return().
sleep(Prop) when is_list(Prop) ->
    case sleep_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SLEEP_REQ_HEADERS, ?OPTIONAL_SLEEP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for sleep_req"}
    end;
sleep(JObj) ->
    sleep(wh_json:to_proplist(JObj)).

-spec sleep_v(api_terms()) -> boolean().
sleep_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SLEEP_REQ_HEADERS, ?SLEEP_REQ_VALUES, ?SLEEP_REQ_TYPES);
sleep_v(JObj) ->
    sleep_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:noop API call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec noop(api_terms()) -> api_formatter_return().
noop(Prop) when is_list(Prop) ->
    case noop_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?NOOP_REQ_HEADERS, ?OPTIONAL_NOOP_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for noop_req"}
    end;
noop(JObj) ->
    noop(wh_json:to_proplist(JObj)).

-spec noop_v(api_terms()) -> boolean().
noop_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?NOOP_REQ_HEADERS, ?NOOP_REQ_VALUES, ?NOOP_REQ_TYPES);
noop_v(JObj) ->
    noop_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Conference - Sends caller to a conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference(api_terms()) -> api_formatter_return().
conference(Prop) when is_list(Prop) ->
    case conference_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CONFERENCE_REQ_HEADERS, ?OPTIONAL_CONFERENCE_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for conference_req"}
    end;
conference(JObj) -> conference(wh_json:to_proplist(JObj)).

-spec conference_v(api_terms()) -> boolean().
conference_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONFERENCE_REQ_HEADERS, ?CONFERENCE_REQ_VALUES, ?CONFERENCE_REQ_TYPES);
conference_v(JObj) -> conference_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Originate Ready/Execute
%% Send Requestor a message that the originate is ready to execute and
%% wait for the Requestor to respond to execute the origination
%% @end
%%--------------------------------------------------------------------
-spec originate_ready(api_terms()) -> api_formatter_return().
originate_ready(Prop) when is_list(Prop) ->
    case originate_ready_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?ORIGINATE_READY_HEADERS, ?OPTIONAL_ORIGINATE_READY_HEADERS);
        'false' -> {'error', "Proplist failed validation for originate_ready"}
    end;
originate_ready(JObj) ->
    originate_ready(wh_json:to_proplist(JObj)).

-spec originate_ready_v(api_terms()) -> boolean().
originate_ready_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ORIGINATE_READY_HEADERS, ?ORIGINATE_READY_VALUES, ?ORIGINATE_READY_TYPES);
originate_ready_v(JObj) ->
    originate_ready_v(wh_json:to_proplist(JObj)).

-spec originate_execute(api_terms()) -> api_formatter_return().
originate_execute(Prop) when is_list(Prop) ->
    case originate_execute_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?ORIGINATE_EXECUTE_HEADERS, ?OPTIONAL_ORIGINATE_EXECUTE_HEADERS);
        'false' -> {'error', "Proplist failed validation for originate_execute"}
    end;
originate_execute(JObj) ->
    originate_execute(wh_json:to_proplist(JObj)).

-spec originate_execute_v(api_terms()) -> boolean().
originate_execute_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ORIGINATE_EXECUTE_HEADERS, ?ORIGINATE_EXECUTE_VALUES, ?ORIGINATE_EXECUTE_TYPES);
originate_execute_v(JObj) ->
    originate_execute_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Error - Sends error to Queue - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec error(api_terms()) -> api_formatter_return().
error(Prop) when is_list(Prop) ->
    case error_v(Prop) of
        'true' ->  wh_api:build_message(Prop, ?ERROR_RESP_HEADERS, ?OPTIONAL_ERROR_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for error_req"}
    end;
error(JObj) -> error(wh_json:to_proplist(JObj)).

-spec error_v(api_terms()) -> boolean().
error_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop
                    ,?ERROR_RESP_HEADERS
                    ,[{<<"Event-Name">>, <<"dialplan">>}
                      | ?ERROR_RESP_VALUES
                     ]
                    ,?ERROR_RESP_TYPES
                   );
error_v(JObj) -> error_v(wh_json:to_proplist(JObj)).

%% Takes a generic API JObj, determines what type it is, and calls the appropriate validator
-spec publish_command(ne_binary(), api_terms()) -> 'ok'.
-spec publish_command(ne_binary(), api_terms(), ne_binary()) -> 'ok'.

publish_command(CtrlQ, Prop) when is_list(Prop) ->
    publish_command(CtrlQ, Prop, props:get_value(<<"Application-Name">>, Prop));
publish_command(CtrlQ, JObj) ->
    publish_command(CtrlQ, wh_json:to_proplist(JObj)).

publish_command(CtrlQ, Prop, DPApp) ->
    try wh_util:to_atom(<<DPApp/binary>>) of
        BuildMsgFun ->
            case lists:keyfind(BuildMsgFun, 1, ?MODULE:module_info('exports')) of
                'false' ->
                    {'error', 'invalid_dialplan_object'};
                {_, 1} ->
                    {'ok', Payload} = ?MODULE:BuildMsgFun(wh_api:set_missing_values(Prop, ?DEFAULT_VALUES)),
                    amqp_util:callctl_publish(CtrlQ, Payload, ?DEFAULT_CONTENT_TYPE)
            end
    catch
        _:R -> throw({R, Prop})
    end.

-spec publish_metaflow(api_terms()) -> 'ok'.
-spec publish_metaflow(api_terms(), ne_binary()) -> 'ok'.
publish_metaflow(API) ->
    publish_metaflow(API, ?DEFAULT_CONTENT_TYPE).
publish_metaflow(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?METAFLOW_VALUES, fun ?MODULE:metaflow/1),
    CallId = metaflow_callid(API),
    amqp_util:whapps_publish(?METAFLOW_ROUTING_KEY(CallId), Payload, ContentType).

-spec metaflow_callid(api_terms()) -> api_binary().
metaflow_callid([_|_]=Props) ->
    wh_json:get_value(<<"Call-ID">>, props:get_value(<<"Call">>, Props));
metaflow_callid(JObj) ->
    wh_json:get_value([<<"Call">>, <<"Call-ID">>], JObj).

%% sending DP actions to CallControl Queue
publish_action(Queue, JSON) ->
    publish_action(Queue, JSON, ?DEFAULT_CONTENT_TYPE).
publish_action(Queue, Payload, ContentType) ->
    amqp_util:callctl_publish(Queue, Payload, ContentType).

-spec publish_error(ne_binary(), api_terms()) -> 'ok'.
-spec publish_error(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_error(CallID, JObj) ->
    publish_error(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_error(CallID, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, [{<<"Event-Name">>, <<"dialplan">>}
                                                       | ?ERROR_RESP_VALUES
                                                      ], fun ?MODULE:error/1),
    amqp_util:callevt_publish(wapi_call:event_routing_key(<<"diaplan">>, CallID), Payload, ContentType).

-spec publish_originate_ready(ne_binary(), api_terms()) -> 'ok'.
-spec publish_originate_ready(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_originate_ready(ServerId, JObj) ->
    publish_originate_ready(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_originate_ready(ServerId, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?ORIGINATE_READY_VALUES, fun ?MODULE:originate_ready/1),
    amqp_util:targeted_publish(ServerId, Payload, ContentType).

-spec publish_originate_execute(ne_binary(), api_terms()) -> 'ok'.
-spec publish_originate_execute(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_originate_execute(ServerId, JObj) ->
    publish_originate_execute(ServerId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_originate_execute(ServerId, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?ORIGINATE_EXECUTE_VALUES, fun ?MODULE:originate_execute/1),
    amqp_util:targeted_publish(ServerId, Payload, ContentType).

dial_method_single() -> ?DIAL_METHOD_SINGLE.

dial_method_simultaneous() -> ?DIAL_METHOD_SIMUL.

%%--------------------------------------------------------------------
%% @doc Asks for metaflows to be enabled for a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec metaflow(api_terms()) -> api_formatter_return().
metaflow(Prop) when is_list(Prop) ->
    case metaflow_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?METAFLOW_HEADERS, ?OPTIONAL_METAFLOW_HEADERS);
        'false' -> {'error', "Proplist failed validation for metaflow"}
    end;
metaflow(JObj) -> metaflow(wh_json:to_proplist(JObj)).

-spec metaflow_v(api_terms()) -> boolean().
metaflow_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?METAFLOW_HEADERS, ?METAFLOW_VALUES, ?METAFLOW_TYPES);
metaflow_v(JObj) -> metaflow_v(wh_json:to_proplist(JObj)).

-spec metaflow_digit_timeout_v(term()) -> boolean().
metaflow_digit_timeout_v(X) ->
    is_integer(wh_util:to_integer(X)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_q(Queue, Props, props:get_value('metaflow', Props)).

-spec bind_q(ne_binary(), wh_proplist(), 'undefined' | 'true') -> 'ok'.
bind_q(Queue, _Props, 'undefined') ->
    amqp_util:bind_q_to_callctl(Queue);
bind_q(Queue, Props, 'true') ->
    CallId = props:get_value('callid', Props, <<"*">>),
    amqp_util:bind_q_to_whapps(Queue, ?METAFLOW_ROUTING_KEY(CallId)).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q(Queue, Props, props:get_value('metaflow', Props)).

-spec unbind_q(ne_binary(), wh_proplist(), 'undefined' | 'true') -> 'ok'.
unbind_q(Queue, _Props, 'undefined') ->
    amqp_util:unbind_q_from_callctl(Queue);
unbind_q(Queue, Props, 'true') ->
    CallId = props:get_value('callid', Props, <<"*">>),
    amqp_util:unbind_q_from_whapps(Queue, ?METAFLOW_ROUTING_KEY(CallId)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callctl_exchange(),
    amqp_util:whapps_exchange().

-spec terminators(api_binary()) -> ne_binaries().
terminators(Bin) when is_binary(Bin) ->
    [<<B>> || <<B>> <= Bin, lists:member(<<B>>, ?ANY_DIGIT)];
terminators('undefined') -> ?ANY_DIGIT.

-spec terminators_v(api_binaries() | binary()) -> boolean().
-spec terminator_v(ne_binary()) -> boolean().
terminators_v(Ts) when is_list(Ts) ->
    lists:all(fun terminator_v/1, Ts);
terminators_v(<<>>) -> 'true';
terminators_v(<<"none">>) -> 'true';
terminators_v(_) -> 'false'.

terminator_v(T) -> lists:member(T, ?ANY_DIGIT).

-spec local_store_url(whapps_call:call(), wh_json:object()) -> ne_binary().
local_store_url(Call, JObj) ->
    AccountDb = whapps_call:account_db(Call),
    MediaId = wh_json:get_value(<<"_id">>, JObj),
    MediaName = wh_json:get_value(<<"name">>, JObj),

    Rev = wh_json:get_value(<<"_rev">>, JObj),
    list_to_binary([wh_couch_connections:get_url(), AccountDb
                    ,"/", MediaId
                    ,"/", MediaName
                    ,"?rev=", Rev
                   ]).

-spec offsite_store_url(api_binary(), ne_binary()) -> ne_binary().
offsite_store_url('undefined', _) -> throw({'error', <<"URL not defined">>});
offsite_store_url(Url, MediaName) ->
    iolist_to_binary([wh_util:strip_right_binary(Url, $/), "/", MediaName]).

%%--------------------------------------------------------------------
%% @doc Detect fax on the line
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fax_detection(api_terms()) -> api_formatter_return().
fax_detection(Prop) when is_list(Prop) ->
    case fax_detection_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?FAX_DETECTION_REQ_HEADERS, ?OPTIONAL_FAX_DETECTION_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for tone_detect"}
    end;
fax_detection(JObj) -> fax_detection(wh_json:to_proplist(JObj)).

-spec fax_detection_v(api_terms()) -> boolean().
fax_detection_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FAX_DETECTION_REQ_HEADERS, ?FAX_DETECTION_REQ_VALUES, ?FAX_DETECTION_REQ_TYPES);
fax_detection_v(JObj) -> fax_detection_v(wh_json:to_proplist(JObj)).
