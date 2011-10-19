%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%% Whistle API Helpers
%%%
%%% Most API functions take a proplist, filter it against required headers
%%% and optional headers, and return either the JSON string if all
%%% required headers (default AND API-call-specific) are present, or an
%%% error if some headers are missing.
%%%
%%% To only check the validity, use the api call's corresponding *_v/1 function.
%%% This will parse the proplist and return a boolean()if the proplist is valid
%%% for creating a JSON message.
%%%
%%% @end
%%% Created : 19 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_api).

%% API
-export([default_headers/5, extract_defaults/1]).

%% Resources
-export([offnet_resource_req/1, resource_req/1, resource_resp/1, resource_error/1
	 ,channel_query_req/1, channel_query_resp/1
	]).

%% In-Call
-export([call_event/1, error_resp/1, call_cdr/1, call_status_req/1, call_status_resp/1]).
-export([play_req/1, record_req/1, store_req/1, store_amqp_resp/1, store_http_resp/1, tones_req/1
	 ,tones_req_tone/1, queue_req/1, answer_req/1
	 ,park_req/1, play_collect_digits_req/1, call_pickup_req/1, hangup_req/1, say_req/1
	 ,sleep_req/1, tone_detect_req/1, set_req/1
         ,conference_req/1, noop_req/1, fetch_req/1, respond_req/1, progress_req/1
        ]).

%% Maintenance API calls
-export([mwi_update/1]).

%% Conference Members
-export([conference_participants_req/1, conference_participants_resp/1, conference_play_req/1, conference_deaf_req/1,
         conference_undeaf_req/1, conference_mute_req/1, conference_unmute_req/1, conference_kick_req/1,
         conference_move_req/1, conference_discovery_req/1
	]).

%% Configuration
-export([document_change/1, document_change_v/1]).

%% Validation functions
-export([offnet_resource_req_v/1, resource_req_v/1, resource_resp_v/1, resource_error_v/1
	,channel_query_req_v/1, channel_query_resp_v/1]).

-export([call_event_v/1, error_resp_v/1, call_cdr_v/1, call_status_req_v/1, call_status_resp_v/1]).
-export([play_req_v/1, record_req_v/1, store_req_v/1, store_amqp_resp_v/1, store_http_resp_v/1
         ,tones_req_v/1, tones_req_tone_v/1, queue_req_v/1
         ,answer_req_v/1, park_req_v/1, play_collect_digits_req_v/1, call_pickup_req_v/1, hangup_req_v/1
         ,say_req_v/1, sleep_req_v/1, tone_detect_req_v/1, set_req_v/1, dialplan_req_v/1, respond_req_v/1
         ,progress_req_v/1
        ]).

-export([conference_req_v/1]).

-export([conference_participants_req_v/1, conference_participants_resp_v/1, conference_play_req_v/1, conference_deaf_req_v/1
         ,conference_undeaf_req_v/1, conference_mute_req_v/1, conference_unmute_req_v/1, conference_kick_req_v/1
         ,conference_move_req_v/1, conference_discovery_req_v/1, noop_req_v/1, fetch_req_v/1, mwi_update_v/1
	]).

%% Other AMQP API validators can use these helpers
-export([build_message/3, build_message_specific/3, build_message_specific_headers/3
	 ,validate/4, validate_message/4]).

%% FS-specific routines
-export([convert_fs_evt_name/1, convert_whistle_app_name/1]).

-include("wh_api.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Default Headers in all messages - see wiki
%% Creates the seed proplist for the eventual message to be sent
%% All fields are required general headers.
%% @end
%%--------------------------------------------------------------------
-spec default_headers/5 :: (ServerID, EvtCat, EvtName, AppName, AppVsn) -> proplist() when
      ServerID :: binary(),
      EvtCat :: binary(),
      EvtName :: binary(),
      AppName :: binary(),
      AppVsn :: binary().
default_headers(ServerID, EvtCat, EvtName, AppName, AppVsn) ->
    [{<<"Server-ID">>, ServerID}
     ,{<<"Event-Category">>, EvtCat}
     ,{<<"Event-Name">>, EvtName}
     ,{<<"App-Name">>, AppName}
     ,{<<"App-Version">>, AppVsn}
    ].

%%--------------------------------------------------------------------
%% @doc Extract just the default headers from a message
%% @end
%%--------------------------------------------------------------------
-spec extract_defaults/1 :: (Prop) -> proplist() when
      Prop :: proplist() | json_object().
extract_defaults({struct, Prop}) ->
    extract_defaults(Prop);
extract_defaults(Prop) ->
    lists:foldl(fun(H, Acc) ->
			case props:get_value(H, Prop) of
			    undefined -> Acc;
			    V -> [{H, V} | Acc]
			end
		end, [], ?DEFAULT_HEADERS ++ ?OPTIONAL_DEFAULT_HEADERS).

%%--------------------------------------------------------------------

%% @doc Dialplan Request Validation
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec dialplan_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
dialplan_req_v({struct, Prop}) ->
    dialplan_req_v(Prop);
dialplan_req_v(Prop) ->
    dialplan_req_v(Prop, props:get_value(<<"Application-Name">>, Prop)).

dialplan_req_v(Prop, AppName) ->
    case lists:keyfind(AppName, 1, ?DIALPLAN_APPLICATIONS) of
	false ->
	    throw({invalid_dialplan_object, Prop});
	{_, VFun} ->
	    VFun(Prop)
    end.

%%--------------------------------------------------------------------
%% @doc Offnet resource request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec offnet_resource_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
offnet_resource_req({struct, Prop}) ->
    offnet_resource_req(Prop);
offnet_resource_req(Prop) ->
    case offnet_resource_req_v(Prop) of
	true -> build_message(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OPTIONAL_OFFNET_RESOURCE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for offnet_resource_req"}
    end.

-spec offnet_resource_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
offnet_resource_req_v({struct, Prop}) ->
    offnet_resource_req_v(Prop);
offnet_resource_req_v(Prop) ->
    validate(Prop, ?OFFNET_RESOURCE_REQ_HEADERS, ?OFFNET_RESOURCE_REQ_VALUES, ?OFFNET_RESOURCE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Resource Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resource_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
resource_req({struct, Prop}) ->
    resource_req(Prop);
resource_req(Prop) ->
    case resource_req_v(Prop) of
	true -> build_message(Prop, ?RESOURCE_REQ_HEADERS, ?OPTIONAL_RESOURCE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for resource_req"}
    end.

-spec resource_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
resource_req_v({struct, Prop}) ->
    resource_req_v(Prop);
resource_req_v(Prop) ->
    validate(Prop, ?RESOURCE_REQ_HEADERS, ?RESOURCE_REQ_VALUES, ?RESOURCE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Resource Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resource_resp/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
resource_resp({struct, Prop}) ->
    resource_resp(Prop);
resource_resp(Prop) ->
    case resource_resp_v(Prop) of
	true -> build_message(Prop, ?RESOURCE_RESP_HEADERS, ?OPTIONAL_RESOURCE_RESP_HEADERS);
	false -> {error, "Proplist failed validation for resource_resp"}
    end.

-spec resource_resp_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
resource_resp_v({struct, Prop}) ->
    resource_resp_v(Prop);
resource_resp_v(Prop) ->
    validate(Prop, ?RESOURCE_RESP_HEADERS, ?RESOURCE_RESP_VALUES, ?RESOURCE_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Resource Error - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resource_error/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
resource_error({struct, Prop}) ->
    resource_error(Prop);
resource_error(Prop) ->
    case resource_error_v(Prop) of
	true -> build_message(Prop, ?RESOURCE_ERROR_HEADERS, ?OPTIONAL_RESOURCE_ERROR_HEADERS);
	false -> {error, "Proplist failed validation for resource_error"}
    end.

-spec resource_error_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
resource_error_v({struct, Prop}) ->
    resource_error_v(Prop);
resource_error_v(Prop) ->
    validate(Prop, ?RESOURCE_ERROR_HEADERS, ?RESOURCE_ERROR_VALUES, ?RESOURCE_ERROR_TYPES).

%%--------------------------------------------------------------------
%% @doc Resource Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_query_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
channel_query_req([_|_]=Prop) ->
    case channel_query_req_v(Prop) of
	true -> build_message(Prop, ?CHANNEL_QUERY_REQ_HEADERS, ?OPTIONAL_CHANNEL_QUERY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for channel_query_req"}
    end;
channel_query_req(JObj) ->
    channel_query_req(wh_json:to_proplist(JObj)).

-spec channel_query_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
channel_query_req_v([_|_]=Prop) ->
    validate(Prop, ?CHANNEL_QUERY_REQ_HEADERS, ?CHANNEL_QUERY_REQ_VALUES, ?CHANNEL_QUERY_REQ_TYPES);
channel_query_req_v(JObj) ->
    channel_query_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Resource Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_query_resp/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
channel_query_resp([_|_]=Prop) ->
    case channel_query_resp_v(Prop) of
	true -> build_message(Prop, ?CHANNEL_QUERY_RESP_HEADERS, ?OPTIONAL_CHANNEL_QUERY_RESP_HEADERS);
	false -> {error, "Proplist failed validation for resource_resp"}
    end;
channel_query_resp(JObj) ->
    channel_query_resp(wh_json:to_proplist(JObj)).

-spec channel_query_resp_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
channel_query_resp_v([_|_]=Prop) ->
    validate(Prop, ?CHANNEL_QUERY_RESP_HEADERS, ?CHANNEL_QUERY_RESP_VALUES, ?CHANNEL_QUERY_RESP_TYPES);
channel_query_resp_v(JObj) ->
    channel_query_resp_v(wh_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_event/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
call_event({struct, Prop}) ->
    call_event(Prop);
call_event(Prop) ->
    case call_event_v(Prop) of
	true -> build_message(Prop, ?CALL_EVENT_HEADERS, ?OPTIONAL_CALL_EVENT_HEADERS);
	false -> {error, "Proplist failed validation for call_event"}
    end.

-spec call_event_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
call_event_v({struct, Prop}) ->
    call_event_v(Prop);
call_event_v(Prop) ->
    validate(Prop, ?CALL_EVENT_HEADERS, ?CALL_EVENT_VALUES, ?CALL_EVENT_TYPES).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_status_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
call_status_req({struct, Prop}) ->
    call_status_req(Prop);
call_status_req(Prop) ->
    case call_status_req_v(Prop) of
	true -> build_message(Prop, ?CALL_STATUS_REQ_HEADERS, ?OPTIONAL_CALL_STATUS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for call_status req"}
    end.

-spec call_status_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
call_status_req_v({struct, Prop}) ->
    call_status_req_v(Prop);
call_status_req_v(Prop) ->
    validate(Prop, ?CALL_STATUS_REQ_HEADERS, ?CALL_STATUS_REQ_VALUES, ?CALL_STATUS_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Respond with status of a call, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_status_resp/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
call_status_resp({struct, Prop}) ->
    call_status_resp(Prop);
call_status_resp(Prop) ->
    case call_status_resp_v(Prop) of
	true -> build_message(Prop, ?CALL_STATUS_RESP_HEADERS, ?OPTIONAL_CALL_STATUS_RESP_HEADERS);
	false -> {error, "Proplist failed validation for call_status_resp"}
    end.

-spec call_status_resp_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
call_status_resp_v({struct, Prop}) ->
    call_status_resp_v(Prop);
call_status_resp_v(Prop) ->
    validate(Prop, ?CALL_STATUS_RESP_HEADERS, ?CALL_STATUS_RESP_VALUES, ?CALL_STATUS_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a CDR for a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_cdr/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
call_cdr({struct, Prop}) ->
    call_cdr(Prop);
call_cdr(Prop) ->
    case call_cdr_v(Prop) of
	true -> build_message(Prop, ?CALL_CDR_HEADERS, ?OPTIONAL_CALL_CDR_HEADERS);
	false -> {error, "Proplist failed validation for call_cdr"}
    end.

-spec call_cdr_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
call_cdr_v({struct, Prop}) ->
    call_cdr_v(Prop);
call_cdr_v(Prop) ->
    validate(Prop, ?CALL_CDR_HEADERS, ?CALL_CDR_VALUES, ?CALL_CDR_TYPES).

%%--------------------------------------------------------------------
%% @doc Format an error event
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec error_resp/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
error_resp({struct, Prop}) ->
    error_resp(Prop);
error_resp(Prop) ->
    case error_resp_v(Prop) of
	true -> build_message(Prop, ?ERROR_RESP_HEADERS, ?OPTIONAL_ERROR_RESP_HEADERS);
	false -> {error, "Proplist failed validation for error_resp"}
    end.

-spec error_resp_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
error_resp_v({struct, Prop}) ->
    error_resp_v(Prop);
error_resp_v(Prop) ->
    validate(Prop, ?ERROR_RESP_HEADERS, ?ERROR_RESP_VALUES, ?ERROR_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec document_change/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
document_change({struct, Prop}) ->
    document_change(Prop);
document_change(Prop) ->
    case document_change_v(Prop) of
	true -> build_message(Prop, ?CONF_DOC_UPDATE_HEADERS, ?OPTIONAL_CONF_DOC_UPDATE_HEADERS);
	false -> {error, "Proplist failed validation for document_change"}
    end.

-spec document_change_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
document_change_v({struct, Prop}) ->
    document_change_v(Prop);
document_change_v(Prop) ->
    validate(Prop, ?CONF_DOC_UPDATE_HEADERS, ?CONF_DOC_UPDATE_VALUES, ?CONF_DOC_UPDATE_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store API call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec store_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
      Prop :: proplist().
store_req({struct, Prop}) ->
    store_req(Prop);
store_req(Prop) ->
    case store_req_v(Prop) of
	true -> build_message(Prop, ?STORE_REQ_HEADERS, ?OPTIONAL_STORE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for store_req"}
    end.

-spec store_req_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
store_req_v({struct, Prop}) ->
    store_req_v(Prop);
store_req_v(Prop) ->
    validate(Prop, ?STORE_REQ_HEADERS, ?STORE_REQ_VALUES, ?STORE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store response for amqp storage method
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec store_amqp_resp/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
store_amqp_resp({struct, Prop}) ->
    store_amqp_resp(Prop);
store_amqp_resp(Prop) ->
    case store_amqp_resp_v(Prop) of
	true -> build_message(Prop, ?STORE_AMQP_RESP_HEADERS, ?OPTIONAL_STORE_AMQP_RESP_HEADERS);
	false -> {error, "Proplist failed validation for store_amqp_resp"}
    end.

-spec store_amqp_resp_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
store_amqp_resp_v({struct, Prop}) ->
    store_amqp_resp_v(Prop);
store_amqp_resp_v(Prop) ->
    validate(Prop, ?STORE_AMQP_RESP_HEADERS, ?STORE_AMQP_RESP_VALUES, ?STORE_AMQP_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store response for http storage method
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec store_http_resp/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
store_http_resp({struct, Prop}) ->
    store_http_resp(Prop);
store_http_resp(Prop) ->
    case store_http_resp_v(Prop) of
	true -> build_message(Prop, ?STORE_HTTP_RESP_HEADERS, ?OPTIONAL_STORE_HTTP_RESP_HEADERS);
	false -> {error, "Prop failed validation for store_http_resp"}
    end.

-spec store_http_resp_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
store_http_resp_v({struct, Prop}) ->
    store_http_resp_v(Prop);
store_http_resp_v(Prop) ->
    validate(Prop, ?STORE_HTTP_RESP_HEADERS, ?STORE_HTTP_RESP_VALUES, ?STORE_HTTP_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Create a tone on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec tones_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
tones_req({struct, Prop}) ->
    tones_req(Prop);
tones_req(Prop) ->
    case tones_req_v(Prop) of
	true -> build_message(Prop, ?TONES_REQ_HEADERS, ?OPTIONAL_TONES_REQ_HEADERS);
	false -> {error, "Prop failed validation for tones_req"}
    end.

-spec tones_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
tones_req_v({struct, Prop}) ->
    tones_req_v(Prop);
tones_req_v(Prop) ->
    validate(Prop, ?TONES_REQ_HEADERS, ?TONES_REQ_VALUES, ?TONES_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc A Tone within a Tones request - see wiki
%% Takes proplist and returns a proplist
%% @end
%%--------------------------------------------------------------------
-spec tones_req_tone/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
tones_req_tone({struct, Prop}) ->
    tones_req_tone(Prop);
tones_req_tone(Prop) ->
    case tones_req_tone_v(Prop) of
	true -> build_message_specific(Prop, ?TONES_REQ_TONE_HEADERS, ?OPTIONAL_TONES_REQ_TONE_HEADERS);
	false -> {error, "Proplist failed validation for tones_req_tone"}
    end.

-spec tones_req_tone_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
tones_req_tone_v({struct, Prop}) ->
    tones_req_tone_v(Prop);
tones_req_tone_v(Prop) ->
    validate_message(Prop, ?TONES_REQ_TONE_HEADERS, ?TONES_REQ_TONE_VALUES, ?TONES_REQ_TONE_TYPES).

%%--------------------------------------------------------------------
%% @doc Detect tones on the line
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec tone_detect_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
tone_detect_req({struct, Prop}) ->
    tone_detect_req(Prop);
tone_detect_req(Prop) ->
    case tone_detect_req_v(Prop) of
	true -> build_message(Prop, ?TONE_DETECT_REQ_HEADERS, ?OPTIONAL_TONE_DETECT_REQ_HEADERS);
	false -> {error, "Proplist failed validation for tone_detect"}
    end.

-spec tone_detect_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
tone_detect_req_v({struct, Prop}) ->
    tone_detect_req_v(Prop);
tone_detect_req_v(Prop) ->
    validate(Prop, ?TONE_DETECT_REQ_HEADERS, ?TONE_DETECT_REQ_VALUES, ?TONE_DETECT_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Send a list of dialplan applications in bulk - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec queue_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
queue_req({struct, Prop}) ->
    queue_req(Prop);
queue_req(Prop) ->
    case queue_req_v(Prop) of
	true -> build_message(Prop, ?QUEUE_REQ_HEADERS, ?OPTIONAL_QUEUE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for queue_req"}
    end.

-spec queue_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
queue_req_v({struct, Prop}) ->
    queue_req_v(Prop);
queue_req_v(Prop) ->
    validate(Prop, ?QUEUE_REQ_HEADERS, ?QUEUE_REQ_VALUES, ?QUEUE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Play media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec play_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
play_req({struct, Prop}) ->
    play_req(Prop);
play_req(Prop) ->
    case play_req_v(Prop) of
	true -> build_message(Prop, ?PLAY_REQ_HEADERS, ?OPTIONAL_PLAY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for play_req"}
    end.

-spec play_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
play_req_v({struct, Prop}) ->
    play_req_v(Prop);
play_req_v(Prop) ->
    validate(Prop, ?PLAY_REQ_HEADERS, ?PLAY_REQ_VALUES, ?PLAY_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Record media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec record_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
record_req({struct, Prop}) ->
    record_req(Prop);
record_req(Prop) ->
    case record_req_v(Prop) of
	true -> build_message(Prop, ?RECORD_REQ_HEADERS, ?OPTIONAL_RECORD_REQ_HEADERS);
	false -> {error, "Proplist failed validation for record_req"}
    end.

-spec record_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
record_req_v({struct, Prop}) ->
    record_req_v(Prop);
record_req_v(Prop) ->
    validate(Prop, ?RECORD_REQ_HEADERS, ?RECORD_REQ_VALUES, ?RECORD_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Answer a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec answer_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
answer_req({struct, Prop}) ->
    answer_req(Prop);
answer_req(Prop) ->
    case answer_req_v(Prop) of
	true -> build_message(Prop, ?ANSWER_REQ_HEADERS, ?OPTIONAL_ANSWER_REQ_HEADERS);
	false -> {error, "Proplist failed validation for answer_req"}
    end.

-spec answer_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
answer_req_v({struct, Prop}) ->
    answer_req_v(Prop);
answer_req_v(Prop) ->
    validate(Prop, ?ANSWER_REQ_HEADERS, ?ANSWER_REQ_VALUES, ?ANSWER_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Progress a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec progress_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
progress_req({struct, Prop}) ->
    progress_req(Prop);
progress_req(Prop) ->
    case progress_req_v(Prop) of
	true -> build_message(Prop, ?PROGRESS_REQ_HEADERS, ?OPTIONAL_PROGRESS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for progress_req"}
    end.

-spec progress_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
progress_req_v({struct, Prop}) ->
    progress_req_v(Prop);
progress_req_v(Prop) ->
    validate(Prop, ?PROGRESS_REQ_HEADERS, ?PROGRESS_REQ_VALUES, ?PROGRESS_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Hangup a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec hangup_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
hangup_req({struct, Prop}) ->
    hangup_req(Prop);
hangup_req(Prop) ->
    case hangup_req_v(Prop) of
	true -> build_message(Prop, ?HANGUP_REQ_HEADERS, ?OPTIONAL_HANGUP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for hangup_req"}
    end.

-spec hangup_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
hangup_req_v({struct, Prop}) ->
    hangup_req_v(Prop);
hangup_req_v(Prop) ->
    validate(Prop, ?HANGUP_REQ_HEADERS, ?HANGUP_REQ_VALUES, ?HANGUP_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Park a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec park_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
park_req({struct, Prop}) ->
    park_req(Prop);
park_req(Prop) ->
    case park_req_v(Prop) of
	true -> build_message(Prop, ?PARK_REQ_HEADERS, ?OPTIONAL_PARK_REQ_HEADERS);
	false -> {error, "Proplist failed validation for park_req"}
    end.

-spec park_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
park_req_v({struct, Prop}) ->
    park_req_v(Prop);
park_req_v(Prop) ->
    validate(Prop, ?PARK_REQ_HEADERS, ?PARK_REQ_VALUES, ?PARK_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Set Custom Channel variables - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec set_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
set_req({struct, Prop}) ->
    set_req(Prop);
set_req(Prop) ->
    case set_req_v(Prop) of
	true -> build_message(Prop, ?SET_REQ_HEADERS, ?OPTIONAL_SET_REQ_HEADERS);
	false -> {error, "Proplist failed validation for set_req"}
    end.

-spec set_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
set_req_v({struct, Prop}) ->
    set_req_v(Prop);
set_req_v(Prop) ->
    validate(Prop, ?SET_REQ_HEADERS, ?SET_REQ_VALUES, ?SET_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Fetch Custom Channel variables - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fetch_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
fetch_req({struct, Prop}) ->
    fetch_req(Prop);
fetch_req(Prop) ->
    case fetch_req_v(Prop) of
	true -> build_message(Prop, ?FETCH_REQ_HEADERS, ?OPTIONAL_FETCH_REQ_HEADERS);
	false -> {error, "Proplist failed validation for fetch_req"}
    end.

-spec fetch_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
fetch_req_v({struct, Prop}) ->
    fetch_req_v(Prop);
fetch_req_v(Prop) ->
    validate(Prop, ?FETCH_REQ_HEADERS, ?FETCH_REQ_VALUES, ?FETCH_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Play media and record digits - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec play_collect_digits_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
play_collect_digits_req({struct, Prop}) ->
    play_collect_digits_req(Prop);
play_collect_digits_req(Prop) ->
    case play_collect_digits_req_v(Prop) of
	true -> build_message(Prop, ?PLAY_COLLECT_DIGITS_REQ_HEADERS, ?OPTIONAL_PLAY_COLLECT_DIGITS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for play_collect_digits_req"}
    end.

-spec play_collect_digits_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
play_collect_digits_req_v({struct, Prop}) ->
    play_collect_digits_req_v(Prop);
play_collect_digits_req_v(Prop) ->
    validate(Prop, ?PLAY_COLLECT_DIGITS_REQ_HEADERS, ?PLAY_COLLECT_DIGITS_REQ_VALUES, ?PLAY_COLLECT_DIGITS_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Pickup a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_pickup_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
call_pickup_req({struct, Prop}) ->
    call_pickup_req(Prop);
call_pickup_req(Prop) ->
    case call_pickup_req_v(Prop) of
	true -> build_message(Prop, ?CALL_PICKUP_REQ_HEADERS, ?OPTIONAL_CALL_PICKUP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for call_pickup_req"}
    end.

-spec call_pickup_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
call_pickup_req_v({struct, Prop}) ->
    call_pickup_req_v(Prop);
call_pickup_req_v(Prop) ->
    validate(Prop, ?CALL_PICKUP_REQ_HEADERS, ?CALL_PICKUP_REQ_VALUES, ?CALL_PICKUP_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Say - convert text to speech - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec say_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
say_req({struct, Prop}) ->
    say_req(Prop);
say_req(Prop) ->
    case say_req_v(Prop) of
	true -> build_message(Prop, ?SAY_REQ_HEADERS, ?OPTIONAL_SAY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for say_req"}
    end.

-spec say_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
say_req_v({struct, Prop}) ->
    say_req_v(Prop);
say_req_v(Prop) ->
    validate(Prop, ?SAY_REQ_HEADERS, ?SAY_REQ_VALUES, ?SAY_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Respond a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec respond_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
respond_req({struct, Prop}) ->
    respond_req(Prop);
respond_req(Prop) ->
    case respond_req_v(Prop) of
	true -> build_message(Prop, ?RESPOND_REQ_HEADERS, ?OPTIONAL_RESPOND_REQ_HEADERS);
	false -> {error, "Proplist failed validation for respond_req"}
    end.

-spec respond_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
respond_req_v({struct, Prop}) ->
    respond_req_v(Prop);
respond_req_v(Prop) ->
    validate(Prop, ?RESPOND_REQ_HEADERS, ?RESPOND_REQ_VALUES, ?RESPOND_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Sleep - Pauses execution - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec sleep_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
      Prop :: proplist() | json_object().
sleep_req({struct, Prop}) ->
    sleep_req(Prop);
sleep_req(Prop) ->
    case sleep_req_v(Prop) of
	true -> build_message(Prop, ?SLEEP_REQ_HEADERS, ?OPTIONAL_SLEEP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for sleep_req"}
    end.

-spec sleep_req_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
sleep_req_v({struct, Prop}) ->
    sleep_req_v(Prop);
sleep_req_v(Prop) ->
    validate(Prop, ?SLEEP_REQ_HEADERS, ?SLEEP_REQ_VALUES, ?SLEEP_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:noop API call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec noop_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
      Prop :: proplist() | json_object().
noop_req({struct, Prop}) ->
    noop_req(Prop);
noop_req(Prop) ->
    case noop_req_v(Prop) of
	true -> build_message(Prop, ?NOOP_REQ_HEADERS, ?OPTIONAL_NOOP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for noop_req"}
    end.

-spec noop_req_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
noop_req_v({struct, Prop}) ->
    noop_req_v(Prop);
noop_req_v(Prop) ->
    validate(Prop, ?NOOP_REQ_HEADERS, ?NOOP_REQ_VALUES, ?NOOP_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc MWI - Update the Message Waiting Indicator on a device - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec mwi_update/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
      Prop :: proplist() | json_object().
mwi_update({struct, Prop}) ->
    mwi_update(Prop);
mwi_update(Prop) ->
    case mwi_update_v(Prop) of
	true -> build_message(Prop, ?MWI_REQ_HEADERS, ?OPTIONAL_MWI_REQ_HEADERS);
	false -> {error, "Proplist failed validation for mwi_req"}
    end.

-spec mwi_update_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
mwi_update_v({struct, Prop}) ->
    mwi_update_v(Prop);
mwi_update_v(Prop) ->
    validate(Prop, ?MWI_REQ_HEADERS, ?MWI_REQ_VALUES, ?MWI_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference - Sends caller to a conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
      Prop :: proplist() | json_object().
conference_req({struct, Prop}) ->
    conference_req(Prop);
conference_req(Prop) ->
    case conference_req_v(Prop) of
	true -> build_message(Prop, ?CONFERENCE_REQ_HEADERS, ?OPTIONAL_CONFERENCE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_req"}
    end.

-spec conference_req_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
conference_req_v({struct, Prop}) ->
    conference_req_v(Prop);
conference_req_v(Prop) ->
    validate(Prop, ?CONFERENCE_REQ_HEADERS, ?CONFERENCE_REQ_VALUES, ?CONFERENCE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::discovery - Used to identify the conference ID
%% if not provided, locate the conference focus, and return sip url
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_discovery_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_discovery_req({struct, Prop}) ->
    conference_discovery_req(Prop);
conference_discovery_req(Prop) ->
    case conference_discovery_req_v(Prop) of
	true -> build_message(Prop, ?CONF_DISCOVERY_REQ_HEADERS, ?OPTIONAL_CONF_DISCOVERY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_discovery_req"}
    end.

-spec conference_discovery_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_discovery_req_v({struct, Prop}) ->
    conference_discovery_req_v(Prop);
conference_discovery_req_v(Prop) ->
    validate(Prop, ?CONF_DISCOVERY_REQ_HEADERS, ?CONF_DISCOVERY_REQ_VALUES, ?CONF_DISCOVERY_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::participants - Lists all participants of a conference
%%     or details about certain participants - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_participants_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_participants_req({struct, Prop}) ->
    conference_participants_req(Prop);
conference_participants_req(Prop) ->
    case conference_participants_req_v(Prop) of
	true -> build_message(Prop, ?CONF_PARTICIPANTS_REQ_HEADERS, ?OPTIONAL_CONF_PARTICIPANTS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_participants_req"}
    end.

-spec conference_participants_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_participants_req_v({struct, Prop}) ->
    conference_participants_req_v(Prop);
conference_participants_req_v(Prop) ->
    validate(Prop, ?CONF_PARTICIPANTS_REQ_HEADERS, ?CONF_PARTICIPANTS_REQ_VALUES, ?CONF_PARTICIPANTS_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::participants - The response to participants - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_participants_resp/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_participants_resp({struct, Prop}) ->
    conference_participants_resp(Prop);
conference_participants_resp(Prop) ->
    case conference_participants_resp_v(Prop) of
	true -> build_message(Prop, ?CONF_PARTICIPANTS_RESP_HEADERS, ?OPTIONAL_CONF_PARTICIPANTS_RESP_HEADERS);
	false -> {error, "Proplist failed validation for conference_participants_resp"}
    end.

-spec conference_participants_resp_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_participants_resp_v({struct, Prop}) ->
    conference_participants_resp_v(Prop);
conference_participants_resp_v(Prop) ->
    validate(Prop, ?CONF_PARTICIPANTS_RESP_HEADERS, ?CONF_PARTICIPANTS_RESP_VALUES, ?CONF_PARTICIPANTS_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::play - Play audio to all or a single
%%     conference participant - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_play_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_play_req({struct, Prop}) ->
    conference_play_req(Prop);
conference_play_req(Prop) ->
    case conference_play_req_v(Prop) of
	true -> build_message(Prop, ?CONF_PLAY_REQ_HEADERS, ?OPTIONAL_CONF_PLAY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_play_req"}
    end.

-spec conference_play_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_play_req_v({struct, Prop}) ->
    conference_play_req_v(Prop);
conference_play_req_v(Prop) ->
    validate(Prop, ?CONF_PLAY_REQ_HEADERS, ?CONF_PLAY_REQ_VALUES, ?CONF_PLAY_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::deaf - Make a conference participant deaf - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_deaf_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_deaf_req({struct, Prop}) ->
    conference_deaf_req(Prop);
conference_deaf_req(Prop) ->
    case conference_deaf_req_v(Prop) of
	true -> build_message(Prop, ?CONF_DEAF_REQ_HEADERS, ?OPTIONAL_CONF_DEAF_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_deaf_req"}
    end.

-spec conference_deaf_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_deaf_req_v({struct, Prop}) ->
    conference_deaf_req_v(Prop);
conference_deaf_req_v(Prop) ->
    validate(Prop, ?CONF_DEAF_REQ_HEADERS, ?CONF_DEAF_REQ_VALUES, ?CONF_DEAF_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::undeaf - Allows a specific conference participant to
%%     hear if they where marked deaf - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_undeaf_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_undeaf_req({struct, Prop}) ->
    conference_undeaf_req(Prop);
conference_undeaf_req(Prop) ->
    case conference_undeaf_req_v(Prop) of
	true -> build_message(Prop, ?CONF_UNDEAF_REQ_HEADERS, ?OPTIONAL_CONF_UNDEAF_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_undeaf_req"}
    end.

-spec conference_undeaf_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_undeaf_req_v({struct, Prop}) ->
    conference_undeaf_req_v(Prop);
conference_undeaf_req_v(Prop) ->
    validate(Prop, ?CONF_UNDEAF_REQ_HEADERS, ?CONF_UNDEAF_REQ_VALUES, ?CONF_UNDEAF_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::mute - Mutes a specific participant in a
%%     conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_mute_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_mute_req({struct, Prop}) ->
    conference_mute_req(Prop);
conference_mute_req(Prop) ->
    case conference_mute_req_v(Prop) of
	true -> build_message(Prop, ?CONF_MUTE_REQ_HEADERS, ?OPTIONAL_CONF_MUTE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_mute_req"}
    end.

-spec conference_mute_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_mute_req_v({struct, Prop}) ->
    conference_mute_req_v(Prop);
conference_mute_req_v(Prop) ->
    validate(Prop, ?CONF_MUTE_REQ_HEADERS, ?CONF_MUTE_REQ_VALUES, ?CONF_MUTE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::unmute - Unmutes a specific participant in a
%%     conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_unmute_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_unmute_req({struct, Prop}) ->
    conference_unmute_req(Prop);
conference_unmute_req(Prop) ->
    case conference_unmute_req_v(Prop) of
	true -> build_message(Prop, ?CONF_UNMUTE_REQ_HEADERS, ?OPTIONAL_CONF_UNMUTE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_unmute_req"}
    end.

-spec conference_unmute_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_unmute_req_v({struct, Prop}) ->
    conference_unmute_req_v(Prop);
conference_unmute_req_v(Prop) ->
    validate(Prop, ?CONF_UNMUTE_REQ_HEADERS, ?CONF_UNMUTE_REQ_VALUES, ?CONF_UNMUTE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::kick - Removes a participant from a conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_kick_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_kick_req({struct, Prop}) ->
    conference_kick_req(Prop);
conference_kick_req(Prop) ->
    case conference_kick_req_v(Prop) of
	true -> build_message(Prop, ?CONF_KICK_REQ_HEADERS, ?OPTIONAL_CONF_KICK_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_kick_req"}
    end.

-spec conference_kick_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_kick_req_v({struct, Prop}) ->
    conference_kick_req_v(Prop);
conference_kick_req_v(Prop) ->
    validate(Prop, ?CONF_KICK_REQ_HEADERS, ?CONF_KICK_REQ_VALUES, ?CONF_KICK_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::move - Transfers a participant between to
%%     conferences - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_move_req/1 :: (Prop) -> {'ok', iolist()} | {'error', string()} when
    Prop :: proplist() | json_object().
conference_move_req({struct, Prop}) ->
    conference_move_req(Prop);
conference_move_req(Prop) ->
    case conference_move_req_v(Prop) of
	true -> build_message(Prop, ?CONF_MOVE_REQ_HEADERS, ?OPTIONAL_CONF_MOVE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_move_req"}
    end.

-spec conference_move_req_v/1 :: (Prop) -> boolean() when
    Prop :: proplist() | json_object().
conference_move_req_v({struct, Prop}) ->
    conference_move_req_v(Prop);
conference_move_req_v(Prop) ->
    validate(Prop, ?CONF_MOVE_REQ_HEADERS, ?CONF_MOVE_REQ_VALUES, ?CONF_MOVE_REQ_TYPES).

%% given a proplist of a FS event, return the Whistle-equivalent app name(s).
%% a FS event could have multiple Whistle equivalents
-spec convert_fs_evt_name/1 :: (EvtName) -> [binary(),...] | [] when
      EvtName :: binary().
convert_fs_evt_name(EvtName) ->
    [ WhAppEvt || {FSEvt, WhAppEvt} <- ?SUPPORTED_APPLICATIONS, FSEvt =:= EvtName].

%% given a Whistle Dialplan Application name, return the FS-equivalent event name
%% A Whistle Dialplan Application name is 1-to-1 with the FS-equivalent
-spec convert_whistle_app_name/1 :: (App) -> [binary(),...] | [] when
      App :: binary().
convert_whistle_app_name(App) ->
    [EvtName || {EvtName, AppName} <- ?SUPPORTED_APPLICATIONS, App =:= AppName].

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec validate/4 :: (Prop, ReqHeaders, DefValues, DefTypes) -> boolean() when
      Prop :: proplist(),
      ReqHeaders :: [binary(),...] | [],
      DefValues :: proplist(),
      DefTypes :: proplist().
validate(Prop, ReqH, Vals, Types) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso
	validate_message(Prop, ReqH, Vals, Types).

-spec validate_message/4 :: (Prop, ReqHeaders, DefValues, DefTypes) -> boolean() when
      Prop :: proplist(),
      ReqHeaders :: [binary(),...] | [],
      DefValues :: proplist(),
      DefTypes :: proplist().
validate_message(Prop, ReqH, Vals, Types) ->
    has_all(Prop, ReqH) andalso
	values_check(Prop, Vals) andalso
	type_check(Prop, Types).

-spec build_message/3 :: (Prop, ReqHeaders, OptHeaders) -> {'ok', iolist()} | {'error', string()} when
      Prop :: proplist(),
      ReqHeaders :: [binary(),...] | [],
      OptHeaders:: [binary(),...] | [].
build_message(Prop, ReqH, OptH) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("Build Error: ~p~nDefHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	HeadAndProp ->
	    case build_message_specific_headers(HeadAndProp, ReqH, OptH) of
		{ok, FinalHeaders} -> headers_to_json(FinalHeaders);
		Err -> Err
	    end
    end.

-spec build_message_specific_headers/3 :: (Msg, ReqHeaders, OptHeaders) -> {'ok', proplist()} | {'error', string()} when
      Msg :: proplist() | {[binary(),...] | [], proplist()},
      ReqHeaders :: [binary(),...] | [],
      OptHeaders :: [binary(),...] | [].
build_message_specific_headers({Headers, Prop}, ReqH, OptH) ->
    case update_required_headers(Prop, ReqH, Headers) of
	{error, _Reason} = Error ->
	    io:format("Build Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ReqH, Prop]),
	    Error;
	{Headers1, Prop1} ->
	    {Headers2, _Prop2} = update_optional_headers(Prop1, OptH, Headers1),
	    {ok, Headers2}
    end;
build_message_specific_headers(Prop, ReqH, OptH) ->
    build_message_specific_headers({[], Prop}, ReqH, OptH).

-spec build_message_specific/3 :: (Msg, ReqHeaders, OptHeaders) -> {'ok', iolist()} | {'error', string()} when
      Msg :: proplist() | {[binary(),...] | [], proplist()},
      ReqHeaders :: [binary(),...] | [],
      OptHeaders :: [binary(),...] | [].
build_message_specific({Headers, Prop}, ReqH, OptH) ->
    case update_required_headers(Prop, ReqH, Headers) of
	{error, _Reason} = Error ->
	    io:format("Build Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ReqH, Prop]),
	    Error;
	{Headers1, Prop1} ->
	    {Headers2, _Prop2} = update_optional_headers(Prop1, OptH, Headers1),
	    headers_to_json(Headers2)
    end;
build_message_specific(Prop, ReqH, OptH) ->
    build_message_specific({[], Prop}, ReqH, OptH).

-spec headers_to_json/1 :: (HeadersProp) -> {'ok', iolist()} | {'error', string()} when
      HeadersProp :: proplist().
headers_to_json(HeadersProp) ->
    try
	{ok, mochijson2:encode({struct, HeadersProp})}
    catch
	_What:_Why -> {error, io_lib:format("WHISTLE TO_JSON ERROR(~p): ~p~n~p", [_What, _Why, HeadersProp])}
    end.

%% Checks Prop for all default headers, throws error if one is missing
%% defaults(PassedProps) -> { Headers, NewPropList } | {error, Reason}
-spec defaults/1 :: (Prop) -> {proplist(), proplist()} | {'error', string()} when
      Prop :: proplist() | json_object().
defaults(Prop) ->
    defaults(Prop, []).
defaults(Prop, Headers) ->
    case update_required_headers(Prop, ?DEFAULT_HEADERS, Headers) of
	{error, _Reason} = Error ->
	    Error;
	{Headers1, Prop1} ->
	    update_optional_headers(Prop1, ?OPTIONAL_DEFAULT_HEADERS, Headers1)
    end.

-spec update_required_headers/3 :: (Prop, Fields, Headers) -> {proplist(), proplist()} | {'error', string()} when
      Prop :: proplist(),
      Fields :: [binary(),...] | [],
      Headers :: proplist().
update_required_headers(Prop, Fields, Headers) ->
    case has_all(Prop, Fields) of
	true ->
	    add_headers(Prop, Fields, Headers);
	false ->
	    {error, "All required headers not defined"}
    end.

-spec update_optional_headers/3 :: (Prop, Fields, Headers) -> {proplist(), proplist()} when
      Prop :: proplist(),
      Fields :: [binary(),...] | [],
      Headers :: proplist().
update_optional_headers(Prop, Fields, Headers) ->
    case has_any(Prop, Fields) of
	true ->
	    add_optional_headers(Prop, Fields, Headers);
	false ->
	    {Headers, Prop}
    end.

%% add [Header] from Prop to HeadProp
-spec add_headers/3 :: (Prop, Fields, Headers) -> {proplist(), proplist()} when
      Prop :: proplist(),
      Fields :: [binary(),...] | [],
      Headers :: proplist().
add_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) ->
			{[{K, props:get_value(K, KVs)} | Headers1], props:delete(K, KVs)}
		end, {Headers, Prop}, Fields).

-spec add_optional_headers/3 :: (Prop, Fields, Headers) -> {proplist(), proplist()} when
      Prop :: proplist(),
      Fields :: [binary(),...] | [],
      Headers :: proplist().
add_optional_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) ->
			case props:get_value(K, KVs) of
			    undefined ->
				{Headers1, KVs};
			    V ->
				{[{K, V} | Headers1], props:delete(K, KVs)}
			end
		end, {Headers, Prop}, Fields).

%% Checks Prop against a list of required headers, returns true | false
-spec has_all/2 :: (Prop, Headers) -> boolean() when
      Prop :: proplist(),
      Headers :: [binary(),...] | [].
has_all(Prop, Headers) ->
    lists:all(fun(Header) ->
		      case props:is_defined(Header, Prop) of
			  true -> true;
			  false ->
			      io:format("WHISTLE_API.has_all: Failed to find ~p~nProp: ~p~n", [Header, Prop]),
			      false
		      end
	      end, Headers).

%% Checks Prop against a list of optional headers, returns true | false if at least one if found
-spec has_any/2 :: (Prop, Headers) -> boolean() when
      Prop :: proplist(),
      Headers :: [binary(),...] | [].
has_any(Prop, Headers) ->
    lists:any(fun(Header) -> props:is_defined(Header, Prop) end, Headers).

%% checks Prop against a list of values to ensure known key/value pairs are correct (like Event-Category
%% and Event-Name). We don't care if a key is defined in Values and not in Prop; that is handled by has_all/1
values_check(Prop, Values) ->
    lists:all(fun({Key, Vs}) when is_list(Vs) ->
		      case props:get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  V -> case lists:member(V, Vs) of
				   true -> true;
				   false ->
				       io:format("WHISTLE_API.values_check: K: ~p V: ~p not in ~p~n", [Key, V, Vs]),
				       false
			       end
		      end;
		 ({Key, V}) ->
		      case props:get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  V -> true;
			  _Val ->
			      io:format("WHISTLE_API.values_check: Key: ~p Set: ~p Expected: ~p~n", [Key, _Val, V]),
			      false
		      end
	      end, Values).

%% checks Prop against a list of {Key, Fun}, running the value of Key through Fun, which returns a
%% boolean.
type_check(Prop, Types) ->
    lists:all(fun({Key, Fun}) ->
		      case props:get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  Value -> try case Fun(Value) of % returns boolean
					   true -> true;
					   false ->
					       io:format("WHISTLE_API.type_check: K: ~p V: ~p failed fun~n", [Key, Value]),
					       false
				       end
				   catch _:_ -> io:format("WHISTLE_API.type_check: K: ~p V: ~p threw exception~n", [Key, Value]),
						false
				   end
		      end
	      end, Types).

%% EUNIT TESTING

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

has_all_test() ->
    Prop = [{<<"k1">>, <<"v1">>}
	    ,{<<"k2">>, <<"v2">>}
	    ,{<<"k3">>, <<"v3">>}
	    ],
    Headers = [<<"k1">>, <<"k2">>, <<"k3">>],
    ?assertEqual(true, has_all(Prop, Headers)),
    ?assertEqual(false, has_all(Prop, [<<"k4">> | Headers])),
    ok.

has_any_test() ->
    Prop = [{<<"k1">>, <<"v1">>}
	    ,{<<"k2">>, <<"v2">>}
	    ,{<<"k3">>, <<"v3">>}
	   ],
    Headers = [<<"k1">>, <<"k2">>, <<"k3">>],
    ?assertEqual(true, has_any(Prop, Headers)),
    ?assertEqual(false, has_any(Prop, [<<"k4">>])),
    ok.

-endif.
