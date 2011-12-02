%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Dialplan API commands
%%% @end
%%% Created : 19 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_dialplan).

-compile({no_auto_import, [error/1]}).

-export([v/1, bridge/1, bridge_v/1, bridge_endpoint/1, bridge_endpoint_v/1]).
-export([store/1, store_v/1, store_amqp_resp/1, store_amqp_resp_v/1
	 ,store_http_resp/1, store_http_resp_v/1]).

-export([conference/1, noop/1, fetch/1, respond/1, progress/1]).
-export([play/1, record/1, queue/1, answer/1, park/1, play_and_collect_digits/1
	 ,call_pickup/1, hangup/1, say/1, sleep/1, tone_detect/1, set/1
	 ,tones/1, tones_req_tone/1, tones_req_tone_headers/1, error/1
	]).

-export([play_v/1, record_v/1, noop_v/1, tones_v/1, tones_req_tone_v/1, queue_v/1
         ,answer_v/1, park_v/1, play_and_collect_digits_v/1, call_pickup_v/1, hangup_v/1
         ,say_v/1, sleep_v/1, tone_detect_v/1, set_v/1, respond_v/1, progress_v/1
        ]).

-export([conference_v/1, error_v/1]).

-export([publish_action/2, publish_action/3, publish_event/2, publish_event/3]).

-export([bind_q/2, unbind_q/1]).

-include("wapi_dialplan.hrl").
-include_lib("whistle/include/wh_log.hrl").

%% Takes a generic API JObj, determines what type it is, and calls the appropriate validator
-spec v/1 :: (proplist() | json_object()) -> boolean().
v(Prop) when is_list(Prop) ->
    v(Prop, props:get_value(<<"Application-Name">>, Prop));
v(JObj) ->
    v(wh_json:to_proplist(JObj)).

-spec v/2 :: (proplist() | json_object(), binary()) -> boolean().
v(Prop, DPApp) ->
    try
	VFun = wh_util:to_atom(<<DPApp/binary, "_v">>),
	?LOG("vfun: ~s", [VFun]),
	?LOG("keyfind: ~p", [lists:keyfind(VFun, 1, ?MODULE:module_info(exports))]),
	case lists:keyfind(VFun, 1, ?MODULE:module_info(exports)) of
	    false -> throw({invalid_dialplan_object, Prop});
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
-spec bridge/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
bridge(Prop) when is_list(Prop) ->
    EPs = [begin
	       {ok, EPProps} = bridge_endpoint_headers(EP),
	       wh_json:from_list(EPProps)
	   end
	   || EP <- props:get_value(<<"Endpoints">>, Prop, []),
	      bridge_endpoint_v(EP)],
    Prop1 = [ {<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    case bridge_v(Prop1) of
	true -> wh_api:build_message(Prop1, ?BRIDGE_REQ_HEADERS, ?OPTIONAL_BRIDGE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for bridge_req"}
    end;
bridge(JObj) ->
    bridge(wh_json:to_proplist(JObj)).

-spec bridge_v/1 :: (proplist() | json_object()) -> boolean().
bridge_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?BRIDGE_REQ_HEADERS, ?BRIDGE_REQ_VALUES, ?BRIDGE_REQ_TYPES);
bridge_v(JObj) ->
    bridge_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Endpoints for bridging a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec bridge_endpoint/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
bridge_endpoint(Prop) when is_list(Prop) ->
    case bridge_endpoint_v(Prop) of
	true -> wh_api:build_message_specific(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
	false -> {error, "Proplist failed validation for bridge_req_endpoint"}
    end;
bridge_endpoint(JObj) ->
    bridge_endpoint(wh_json:to_proplist(JObj)).

-spec bridge_endpoint_headers/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
bridge_endpoint_headers(Prop) when is_list(Prop) ->
    wh_api:build_message_specific_headers(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
bridge_endpoint_headers(JObj) ->
    bridge_endpoint_headers(wh_json:to_proplist(JObj)).

-spec bridge_endpoint_v/1 :: (proplist() | json_object()) -> boolean().
bridge_endpoint_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?BRIDGE_REQ_ENDPOINT_VALUES, ?BRIDGE_REQ_ENDPOINT_TYPES);
bridge_endpoint_v(JObj) ->
    bridge_endpoint_v(wh_json:to_proplist(JObj)).

-spec store/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
store(Prop) when is_list(Prop) ->
    case store_v(Prop) of
	true -> wh_api:build_message(Prop, ?STORE_REQ_HEADERS, ?OPTIONAL_STORE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for store"}
    end;
store(JObj) ->
    store(wh_json:to_proplist(JObj)).

-spec store_v/1 :: (proplist() | json_object()) -> boolean().
store_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STORE_REQ_HEADERS, ?STORE_REQ_VALUES, ?STORE_REQ_TYPES);
store_v(JObj) ->
    store_v(wh_json:to_proplist(JObj)).

-spec store_amqp_resp/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
store_amqp_resp(Prop) when is_list(Prop) ->
    case store_amqp_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?STORE_AMQP_RESP_HEADERS, ?OPTIONAL_STORE_AMQP_RESP_HEADERS);
	false -> {error, "Proplist failed validate for store_amqp_resp"}
    end;
store_amqp_resp(JObj) ->
    store_amqp_resp(wh_json:to_proplist(JObj)).

-spec store_amqp_resp_v/1 :: (proplist() | json_object()) -> boolean().
store_amqp_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STORE_AMQP_RESP_HEADERS, ?STORE_AMQP_RESP_VALUES, ?STORE_AMQP_RESP_TYPES);
store_amqp_resp_v(JObj) ->
    store_amqp_resp_v(wh_json:to_proplist(JObj)).

-spec store_http_resp/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
store_http_resp(Prop) when is_list(Prop) ->
    case store_http_resp_v(Prop) of
	true -> wh_api:build_message(Prop, ?STORE_HTTP_RESP_HEADERS, ?OPTIONAL_STORE_HTTP_RESP_HEADERS);
	false -> {error, "Proplist failed validate for store_http_resp"}
    end;
store_http_resp(JObj) ->
    store_http_resp(wh_json:to_proplist(JObj)).

-spec store_http_resp_v/1 :: (proplist() | json_object()) -> boolean().
store_http_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STORE_HTTP_RESP_HEADERS, ?STORE_HTTP_RESP_VALUES, ?STORE_HTTP_RESP_TYPES);
store_http_resp_v(JObj) ->
    store_http_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Create a tone on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec tones/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()} .
tones(Prop) when is_list(Prop) ->
    Tones = [begin
		 {ok, TonesProp} = tones_req_tone_headers(Tone),
		 wh_json:from_list(TonesProp)
	     end
	     || Tone <- props:get_value(<<"Tones">>, Prop, []),
		tones_req_tone_v(Tone)],
    Prop1 = [ {<<"Tones">>, Tones} | props:delete(<<"Tones">>, Prop)],
    case tones_v(Prop1) of
	true -> wh_api:build_message(Prop1, ?TONES_REQ_HEADERS, ?OPTIONAL_TONES_REQ_HEADERS);
	false -> {error, "Prop failed validation for tones_req"}
    end;
tones(JObj) ->
    tones(wh_json:to_proplist(JObj)).

-spec tones_v/1 :: (proplist() | json_object()) -> boolean().
tones_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?TONES_REQ_HEADERS, ?TONES_REQ_VALUES, ?TONES_REQ_TYPES);
tones_v(JObj) ->
    tones_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc A Tone within a Tones request - see wiki
%% Takes proplist and returns a proplist
%% @end
%%--------------------------------------------------------------------
-spec tones_req_tone/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
tones_req_tone(Prop) when is_list(Prop) ->
    case tones_req_tone_v(Prop) of
	true -> wh_api:build_message_specific(Prop, ?TONES_REQ_TONE_HEADERS, ?OPTIONAL_TONES_REQ_TONE_HEADERS);
	false -> {error, "Proplist failed validation for tones_req_tone"}
    end;
tones_req_tone(JObj) ->
    tones_req_tone(wh_json:to_proplist(JObj)).

-spec tones_req_tone_v/1 :: (proplist() | json_object()) -> boolean().
tones_req_tone_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?TONES_REQ_TONE_HEADERS, ?TONES_REQ_TONE_VALUES, ?TONES_REQ_TONE_TYPES);
tones_req_tone_v(JObj) ->
    tones_req_tone_v(wh_json:to_proplist(JObj)).

-spec tones_req_tone_headers/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
tones_req_tone_headers(Prop) when is_list(Prop) ->
    wh_api:build_message_specific_headers(Prop, ?TONES_REQ_TONE_HEADERS, ?OPTIONAL_TONES_REQ_TONE_HEADERS);
tones_req_tone_headers(JObj) ->
    tones_req_tone_headers(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Detect tones on the line
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec tone_detect/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
tone_detect(Prop) when is_list(Prop) ->
    case tone_detect_v(Prop) of
	true -> wh_api:build_message(Prop, ?TONE_DETECT_REQ_HEADERS, ?OPTIONAL_TONE_DETECT_REQ_HEADERS);
	false -> {error, "Proplist failed validation for tone_detect"}
    end;
tone_detect(JObj) ->
    tone_detect(wh_json:to_proplist(JObj)).

-spec tone_detect_v/1 :: (proplist() | json_object()) -> boolean().
tone_detect_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?TONE_DETECT_REQ_HEADERS, ?TONE_DETECT_REQ_VALUES, ?TONE_DETECT_REQ_TYPES);
tone_detect_v(JObj) ->
    tone_detect_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Send a list of dialplan applications in bulk - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec queue/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
queue(Prop) when is_list(Prop) ->
    case queue_v(Prop) of
	true -> wh_api:build_message(Prop, ?QUEUE_REQ_HEADERS, ?OPTIONAL_QUEUE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for queue_req"}
    end;
queue(JObj) ->
    queue(wh_json:to_proplist(JObj)).

-spec queue_v/1 :: (proplist() | json_object()) -> boolean().
queue_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUEUE_REQ_HEADERS, ?QUEUE_REQ_VALUES, ?QUEUE_REQ_TYPES);
queue_v(JObj) ->
    queue_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Play media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec play/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
play(Prop) when is_list(Prop) ->
    case play_v(Prop) of
	true -> wh_api:build_message(Prop, ?PLAY_REQ_HEADERS, ?OPTIONAL_PLAY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for play_req"}
    end;
play(JObj) ->
    play(wh_json:to_proplist(JObj)).

-spec play_v/1 :: (proplist() | json_object()) -> boolean().
play_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PLAY_REQ_HEADERS, ?PLAY_REQ_VALUES, ?PLAY_REQ_TYPES);
play_v(JObj) ->
    play_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Record media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec record/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
record(Prop) when is_list(Prop) ->
    case record_v(Prop) of
	true -> wh_api:build_message(Prop, ?RECORD_REQ_HEADERS, ?OPTIONAL_RECORD_REQ_HEADERS);
	false -> {error, "Proplist failed validation for record_req"}
    end;
record(JObj) ->
    record(wh_json:to_proplist(JObj)).

-spec record_v/1 :: (proplist() | json_object()) -> boolean().
record_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RECORD_REQ_HEADERS, ?RECORD_REQ_VALUES, ?RECORD_REQ_TYPES);
record_v(JObj) ->
    record_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Answer a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec answer/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
answer(Prop) when is_list(Prop) ->
    case answer_v(Prop) of
	true -> wh_api:build_message(Prop, ?ANSWER_REQ_HEADERS, ?OPTIONAL_ANSWER_REQ_HEADERS);
	false -> {error, "Proplist failed validation for answer_req"}
    end;
answer(JObj) ->
    answer(wh_json:to_proplist(JObj)).

-spec answer_v/1 :: (proplist() | json_object()) -> boolean().
answer_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ANSWER_REQ_HEADERS, ?ANSWER_REQ_VALUES, ?ANSWER_REQ_TYPES);
answer_v(JObj) ->
    answer_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Progress a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec progress/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
progress(Prop) when is_list(Prop) ->
    case progress_v(Prop) of
	true -> wh_api:build_message(Prop, ?PROGRESS_REQ_HEADERS, ?OPTIONAL_PROGRESS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for progress_req"}
    end;
progress(JObj) ->
    progress(wh_json:to_proplist(JObj)).

-spec progress_v/1 :: (proplist() | json_object()) -> boolean().
progress_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PROGRESS_REQ_HEADERS, ?PROGRESS_REQ_VALUES, ?PROGRESS_REQ_TYPES);
progress_v(JObj) ->
    progress_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Hangup a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec hangup/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
hangup(Prop) when is_list(Prop) ->
    case hangup_v(Prop) of
	true -> wh_api:build_message(Prop, ?HANGUP_REQ_HEADERS, ?OPTIONAL_HANGUP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for hangup_req"}
    end;
hangup(JObj) ->
    hangup(wh_json:to_proplist(JObj)).

-spec hangup_v/1 :: (proplist() | json_object()) -> boolean().
hangup_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?HANGUP_REQ_HEADERS, ?HANGUP_REQ_VALUES, ?HANGUP_REQ_TYPES);
hangup_v(JObj) ->
    hangup_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Park a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec park/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
park(Prop) when is_list(Prop) ->
    case park_v(Prop) of
	true -> wh_api:build_message(Prop, ?PARK_REQ_HEADERS, ?OPTIONAL_PARK_REQ_HEADERS);
	false -> {error, "Proplist failed validation for park_req"}
    end;
park(JObj) ->
    park(wh_json:to_proplist(JObj)).

-spec park_v/1 :: (proplist() | json_object()) -> boolean().
park_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARK_REQ_HEADERS, ?PARK_REQ_VALUES, ?PARK_REQ_TYPES);
park_v(JObj) ->
    park_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Set Custom Channel variables - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec set/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
set(Prop) when is_list(Prop) ->
    case set_v(Prop) of
	true -> wh_api:build_message(Prop, ?SET_REQ_HEADERS, ?OPTIONAL_SET_REQ_HEADERS);
	false -> {error, "Proplist failed validation for set_req"}
    end;
set(JObj) ->
    set(wh_json:to_proplist(JObj)).

-spec set_v/1 :: (proplist() | json_object()) -> boolean() .
set_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SET_REQ_HEADERS, ?SET_REQ_VALUES, ?SET_REQ_TYPES);
set_v(JObj) ->
    set_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Fetch Custom Channel variables - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fetch/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
fetch(Prop) when is_list(Prop) ->
    case fetch_v(Prop) of
	true -> wh_api:build_message(Prop, ?FETCH_REQ_HEADERS, ?OPTIONAL_FETCH_REQ_HEADERS);
	false -> {error, "Proplist failed validation for fetch_req"}
    end;
fetch(JObj) ->
    fetch(wh_json:to_proplist(JObj)).

-spec fetch_v/1 :: (proplist() | json_object()) -> boolean().
fetch_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FETCH_REQ_HEADERS, ?FETCH_REQ_VALUES, ?FETCH_REQ_TYPES);
fetch_v(JObj) ->
    fetch_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Play media and record digits - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec play_and_collect_digits/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
play_and_collect_digits(Prop) when is_list(Prop) ->
    case play_and_collect_digits_v(Prop) of
	true -> wh_api:build_message(Prop, ?PLAY_COLLECT_DIGITS_REQ_HEADERS, ?OPTIONAL_PLAY_COLLECT_DIGITS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for play_collect_digits_req"}
    end;
play_and_collect_digits(JObj) ->
    play_and_collect_digits(wh_json:to_proplist(JObj)).

-spec play_and_collect_digits_v/1 :: (proplist() | json_object()) -> boolean().
play_and_collect_digits_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PLAY_COLLECT_DIGITS_REQ_HEADERS, ?PLAY_COLLECT_DIGITS_REQ_VALUES, ?PLAY_COLLECT_DIGITS_REQ_TYPES);
play_and_collect_digits_v(JObj) ->
    play_and_collect_digits_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Pickup a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_pickup/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
call_pickup(Prop) when is_list(Prop) ->
    case call_pickup_v(Prop) of
	true -> wh_api:build_message(Prop, ?CALL_PICKUP_REQ_HEADERS, ?OPTIONAL_CALL_PICKUP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for call_pickup_req"}
    end;
call_pickup(JObj) ->
    call_pickup(wh_json:to_proplist(JObj)).

-spec call_pickup_v/1 :: (proplist() | json_object()) -> boolean().
call_pickup_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_PICKUP_REQ_HEADERS, ?CALL_PICKUP_REQ_VALUES, ?CALL_PICKUP_REQ_TYPES);
call_pickup_v(JObj) ->
    call_pickup_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Say - convert text to speech - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec say/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
say(Prop) when is_list(Prop) ->
    case say_v(Prop) of
	true -> wh_api:build_message(Prop, ?SAY_REQ_HEADERS, ?OPTIONAL_SAY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for say_req"}
    end;
say(JObj) ->
    say(wh_json:to_proplist(JObj)).

-spec say_v/1 :: (proplist() | json_object()) -> boolean().
say_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SAY_REQ_HEADERS, ?SAY_REQ_VALUES, ?SAY_REQ_TYPES);
say_v(JObj) ->
    say_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Respond a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec respond/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
respond(Prop) when is_list(Prop) ->
    case respond_v(Prop) of
	true -> wh_api:build_message(Prop, ?RESPOND_REQ_HEADERS, ?OPTIONAL_RESPOND_REQ_HEADERS);
	false -> {error, "Proplist failed validation for respond_req"}
    end;
respond(JObj) ->
    respond(wh_json:to_proplist(JObj)).

-spec respond_v/1 :: (proplist() | json_object()) -> boolean().
respond_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESPOND_REQ_HEADERS, ?RESPOND_REQ_VALUES, ?RESPOND_REQ_TYPES);
respond_v(JObj) ->
    respond_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Sleep - Pauses execution - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec sleep/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
sleep(Prop) when is_list(Prop) ->
    case sleep_v(Prop) of
	true -> wh_api:build_message(Prop, ?SLEEP_REQ_HEADERS, ?OPTIONAL_SLEEP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for sleep_req"}
    end;
sleep(JObj) ->
    sleep(wh_json:to_proplist(JObj)).

-spec sleep_v/1 :: (proplist() | json_object()) -> boolean().
sleep_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SLEEP_REQ_HEADERS, ?SLEEP_REQ_VALUES, ?SLEEP_REQ_TYPES);
sleep_v(JObj) ->
    sleep_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:noop API call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec noop/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
noop(Prop) when is_list(Prop) ->
    case noop_v(Prop) of
	true -> wh_api:build_message(Prop, ?NOOP_REQ_HEADERS, ?OPTIONAL_NOOP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for noop_req"}
    end;
noop(JObj) ->
    noop(wh_json:to_proplist(JObj)).

-spec noop_v/1 :: (proplist() | json_object()) -> boolean().
noop_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?NOOP_REQ_HEADERS, ?NOOP_REQ_VALUES, ?NOOP_REQ_TYPES);
noop_v(JObj) ->
    noop_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Conference - Sends caller to a conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
conference(Prop) when is_list(Prop) ->
    case conference_v(Prop) of
	true -> wh_api:build_message(Prop, ?CONFERENCE_REQ_HEADERS, ?OPTIONAL_CONFERENCE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_req"}
    end;
conference(JObj) ->
    conference(wh_json:to_proplist(JObj)).

-spec conference_v/1 :: (proplist() | json_object()) -> boolean().
conference_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONFERENCE_REQ_HEADERS, ?CONFERENCE_REQ_VALUES, ?CONFERENCE_REQ_TYPES);
conference_v(JObj) ->
    conference_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Error - Sends error to Queue - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec error/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
error(Prop) when is_list(Prop) ->
    case error_v(Prop) of
	true -> wh_api:build_message(Prop, ?ERROR_RESP_HEADERS, ?OPTIONAL_ERROR_RESP_HEADERS);
	false -> {error, "Proplist failed validation for error_req"}
    end;
error(JObj) ->
    error(wh_json:to_proplist(JObj)).

-spec error_v/1 :: (proplist() | json_object()) -> boolean().
error_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ERROR_RESP_HEADERS, [{<<"Event-Name">>, <<"dialplan">>} | ?ERROR_RESP_VALUES], ?ERROR_RESP_TYPES);
error_v(JObj) ->
    error_v(wh_json:to_proplist(JObj)).

%% sending DP actions to CallControl Queue
publish_action(Queue, JSON) ->
    publish_action(Queue, JSON, ?DEFAULT_CONTENT_TYPE).
publish_action(Queue, Payload, ContentType) ->
    amqp_util:callctl_publish(Queue, Payload, ContentType).

-spec publish_event/2 :: (ne_binary(), iolist()) -> 'ok'.
-spec publish_event/3 :: (ne_binary(), iolist(), ne_binary()) -> 'ok'.
publish_event(CallID, JObj) ->
    publish_event(CallID, JObj, ?DEFAULT_CONTENT_TYPE).
publish_event(CallID, Payload, ContentType) ->
    amqp_util:callevt_publish(CallID, Payload, event, ContentType).

bind_q(Queue, _Prop) ->
    _ = amqp_util:bind_q_to_callctl(Queue),
    'ok'.

unbind_q(Queue) ->
    amqp_util:unbind_q_from_callctl(Queue).
