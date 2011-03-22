%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Whistle API Helpers
%%%
%%% Most API functions take a proplist, filter it against required headers
%%% and optional headers, and return either the JSON string if all
%%% required headers (default AND API-call-specific) are present, or an
%%% error if some headers are missing.
%%%
%%% To only check the validity, use the api call's corresponding *_v/1 function.
%%% This will parse the proplist and return a boolean if the proplist is valid
%%% for creating a JSON message.
%%%
%%% See http://corp.switchfreedom.com/mediawiki/index.php/API_Definition
%%% @end
%%% Created : 19 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(whistle_api).

%% API
-export([default_headers/5, extract_defaults/1]).

%% Authentication and Routing
-export([auth_req/1, auth_resp/1, reg_success/1, route_req/1, route_resp/1, route_resp_route/1, route_win/1]).
-export([reg_query/1, reg_query_resp/1]).

%% Resources
-export([resource_req/1, resource_resp/1, resource_error/1]).

%% In-Call
-export([call_event/1, error_resp/1, call_cdr/1, call_status_req/1, call_status_resp/1]).
-export([play_req/1, record_req/1, store_req/1, store_amqp_resp/1, store_http_resp/1, tones_req/1
	 ,tones_req_tone/1, queue_req/1, bridge_req/1, bridge_req_endpoint/1, answer_req/1
	 ,park_req/1, play_collect_digits_req/1, call_pickup_req/1, hangup_req/1, say_req/1
	 ,sleep_req/1, tone_detect_req/1, set_req/1, media_req/1, media_resp/1, media_error/1
         ,conference_req/1, noop_req/1
        ]).

% Conference Members
-export([conference_members_req/1, conference_members_resp/1, conference_play_req/1, conference_deaf_req/1, 
         conference_undeaf_req/1, conference_mute_req/1, conference_unmute_req/1, conference_kick_req/1, 
         conference_move_req/1
	]).

%% Validation functions
-export([auth_req_v/1, auth_resp_v/1, reg_success_v/1, route_req_v/1, route_resp_v/1, route_resp_route_v/1, route_win_v/1
	 ,call_event_v/1, error_resp_v/1, play_req_v/1, record_req_v/1, store_req_v/1, store_amqp_resp_v/1
	 ,store_http_resp_v/1, tones_req_v/1, tones_req_tone_v/1, queue_req_v/1, bridge_req_v/1
	 ,bridge_req_endpoint_v/1, answer_req_v/1, park_req_v/1, play_collect_digits_req_v/1
	 ,call_pickup_req_v/1, hangup_req_v/1, say_req_v/1, sleep_req_v/1, tone_detect_req_v/1
	 ,resource_req_v/1, resource_resp_v/1, call_cdr_v/1, resource_error_v/1, call_status_req_v/1
	 ,call_status_resp_v/1, set_req_v/1, reg_query_v/1, reg_query_resp_v/1, dialplan_req_v/1
	 ,media_req_v/1, media_resp_v/1, media_error_v/1, conference_req_v/1, conference_members_req_v/1
         ,conference_members_resp_v/1, conference_play_req_v/1, conference_deaf_req_v/1, conference_undeaf_req_v/1
         ,conference_mute_req_v/1, conference_unmute_req_v/1, conference_kick_req_v/1, conference_move_req_v/1
         ,noop_req_v/1
	]).

%% FS-specific routines
-export([convert_fs_evt_name/1, convert_whistle_app_name/1]).

-import(props, [get_value/2, get_value/3]).
-import(proplists, [delete/2, is_defined/2]).
-import(logger, [format_log/3]).

-include("whistle_api.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Default Headers in all messages - see wiki
%% Creates the seed proplist for the eventual message to be sent
%% All fields are required general headers.
%% @end
%%--------------------------------------------------------------------
-spec(default_headers/5 :: (ServerID :: binary(), EvtCat :: binary(), EvtName :: binary(), AppName :: binary(), AppVsn :: binary()) -> proplist()).
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
-spec(extract_defaults/1 :: (Prop :: proplist() | json_object()) -> proplist()).
extract_defaults({struct, Prop}) ->
    extract_defaults(Prop);
extract_defaults(Prop) ->
    ReqH = lists:foldl(fun(H, Acc) -> [{H, get_value(H, Prop)} | Acc] end, [], ?DEFAULT_HEADERS),
    lists:foldl(fun(H, Acc) ->
			case get_value(H, Prop) of
			    undefined -> Acc;
			    V -> [{H, V} | Acc]
			end
		end, ReqH, ?OPTIONAL_DEFAULT_HEADERS).

%%--------------------------------------------------------------------
%% @doc Authentication Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(auth_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
auth_req({struct, Prop}) ->
    auth_req(Prop);
auth_req(Prop) ->
    case auth_req_v(Prop) of
	true -> build_message(Prop, ?AUTH_REQ_HEADERS, ?OPTIONAL_AUTH_REQ_HEADERS);
	false -> {error, "Proplist failed validation for auth_req"}
    end.

-spec(auth_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
auth_req_v({struct, Prop}) ->
    auth_req_v(Prop);
auth_req_v(Prop) ->
    validate(Prop, ?AUTH_REQ_HEADERS, ?AUTH_REQ_VALUES, ?AUTH_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Authentication Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(auth_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
auth_resp({struct, Prop}) ->
    auth_resp(Prop);
auth_resp(Prop) ->
    case auth_resp_v(Prop) of
	true -> build_message(Prop, ?AUTH_RESP_HEADERS, ?OPTIONAL_AUTH_RESP_HEADERS);
	false -> {error, "Proplist failed validation for auth_resp"}
    end.

-spec(auth_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
auth_resp_v({struct, Prop}) ->
    auth_resp_v(Prop);
auth_resp_v(Prop) ->
    validate(Prop, ?AUTH_RESP_HEADERS, ?AUTH_RESP_VALUES, ?AUTH_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Registration Success - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(reg_success/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
reg_success({struct, Prop}) ->
    reg_success(Prop);
reg_success(Prop) ->
    case reg_success_v(Prop) of
	true -> build_message(Prop, ?REG_SUCCESS_HEADERS, ?OPTIONAL_REG_SUCCESS_HEADERS);
	false -> {error, "Proplist failed validation for reg_success"}
    end.

-spec(reg_success_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
reg_success_v({struct, Prop}) ->
    reg_success_v(Prop);
reg_success_v(Prop) ->
    validate(Prop, ?REG_SUCCESS_HEADERS, ?REG_SUCCESS_VALUES, ?REG_SUCCESS_TYPES).

%%--------------------------------------------------------------------
%% @doc Registration Query - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(reg_query/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
reg_query({struct, Prop}) ->
    reg_query(Prop);
reg_query(Prop) ->
    case reg_query_v(Prop) of
	true -> build_message(Prop, ?REG_QUERY_HEADERS, ?OPTIONAL_REG_QUERY_HEADERS);
	false -> {error, "Proplist failed validation for reg_query"}
    end.

-spec(reg_query_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
reg_query_v({struct, Prop}) ->
    reg_query_v(Prop);
reg_query_v(Prop) ->
    validate(Prop, ?REG_QUERY_HEADERS, ?REG_QUERY_VALUES, ?REG_QUERY_TYPES).

%%--------------------------------------------------------------------
%% @doc Registration Query Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(reg_query_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
reg_query_resp({struct, Prop}) ->
    reg_query_resp(Prop);
reg_query_resp(Prop) ->
    case reg_query_resp_v(Prop) of
	true -> build_message(Prop, ?REG_QUERY_RESP_HEADERS, ?OPTIONAL_REG_QUERY_RESP_HEADERS);
	false -> {error, "Proplist failed validation for reg_query_resp"}
    end.

-spec(reg_query_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
reg_query_resp_v({struct, Prop}) ->
    reg_query_resp_v(Prop);
reg_query_resp_v(Prop) ->
    validate(Prop, ?REG_QUERY_RESP_HEADERS, ?REG_QUERY_RESP_VALUES, ?REG_QUERY_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Dialplan Request Validation
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(dialplan_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
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
%% @doc Dialplan Route Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
route_req({struct, Prop}) ->
    route_req(Prop);
route_req(Prop) ->
    case route_req_v(Prop) of
	true -> build_message(Prop, ?ROUTE_REQ_HEADERS, ?OPTIONAL_ROUTE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for route_req"}
    end.

-spec(route_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
route_req_v({struct, Prop}) ->
    route_req_v(Prop);
route_req_v(Prop) ->
    validate(Prop, ?ROUTE_REQ_HEADERS, ?ROUTE_REQ_VALUES, ?ROUTE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Dialplan Route Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
route_resp({struct, Prop}) ->
    route_resp(Prop);
route_resp(Prop) ->
    case route_resp_v(Prop) of
	true -> build_message(Prop, ?ROUTE_RESP_HEADERS, ?OPTIONAL_ROUTE_RESP_HEADERS);
	false -> {error, "Proplist failed validation for route_resp"}
    end.

-spec(route_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
route_resp_v({struct, Prop}) ->
    route_resp_v(Prop);
route_resp_v(Prop) ->
    validate(Prop, ?ROUTE_RESP_HEADERS, ?ROUTE_RESP_VALUES, ?ROUTE_RESP_TYPES)
	andalso lists:all(fun({struct, R}) -> route_resp_route_v(R) end, get_value(<<"Routes">>, Prop)).

%%--------------------------------------------------------------------
%% @doc Route within a Dialplan Route Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_resp_route/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
route_resp_route({struct, Prop}) ->
    route_resp_route(Prop);
route_resp_route(Prop) ->
    case route_resp_route_v(Prop) of
	true -> build_message_specific(Prop, ?ROUTE_RESP_ROUTE_HEADERS, ?OPTIONAL_ROUTE_RESP_ROUTE_HEADERS);
	false -> {error, "Proplist failed validation for route_resp_route"}
    end.

-spec(route_resp_route_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
route_resp_route_v({struct, Prop}) ->
    route_resp_route_v(Prop);
route_resp_route_v(Prop) ->
    validate_message(Prop, ?ROUTE_RESP_ROUTE_HEADERS, ?ROUTE_RESP_ROUTE_VALUES, ?ROUTE_RESP_ROUTE_TYPES).

%%--------------------------------------------------------------------
%% @doc Winning Responder Message - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_win/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
route_win({struct, Prop}) ->
    route_win(Prop);
route_win(Prop) ->
    case route_win_v(Prop) of
	true -> build_message(Prop, ?ROUTE_WIN_HEADERS, ?OPTIONAL_ROUTE_WIN_HEADERS);
	false -> {error, "Proplist failed validation for route_win"}
    end.

-spec(route_win_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
route_win_v({struct, Prop}) ->
    route_win_v(Prop);
route_win_v(Prop) ->
    validate(Prop, ?ROUTE_WIN_HEADERS, ?ROUTE_WIN_VALUES, ?ROUTE_WIN_TYPES).

%%--------------------------------------------------------------------
%% @doc Resource Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(resource_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
resource_req({struct, Prop}) ->
    resource_req(Prop);
resource_req(Prop) ->
    case resource_req_v(Prop) of
	true -> build_message(Prop, ?RESOURCE_REQ_HEADERS, ?OPTIONAL_RESOURCE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for resource_req"}
    end.

-spec(resource_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
resource_req_v({struct, Prop}) ->
    resource_req_v(Prop);
resource_req_v(Prop) ->
    validate(Prop, ?RESOURCE_REQ_HEADERS, ?RESOURCE_REQ_VALUES, ?RESOURCE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Resource Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(resource_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
resource_resp({struct, Prop}) ->
    resource_resp(Prop);
resource_resp(Prop) ->
    case resource_resp_v(Prop) of
	true -> build_message(Prop, ?RESOURCE_RESP_HEADERS, ?OPTIONAL_RESOURCE_RESP_HEADERS);
	false -> {error, "Proplist failed validation for resource_resp"}
    end.

-spec(resource_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
resource_resp_v({struct, Prop}) ->
    resource_resp_v(Prop);
resource_resp_v(Prop) ->
    validate(Prop, ?RESOURCE_RESP_HEADERS, ?RESOURCE_RESP_VALUES, ?RESOURCE_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Resource Error - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(resource_error/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
resource_error({struct, Prop}) ->
    resource_error(Prop);
resource_error(Prop) ->
    case resource_error_v(Prop) of
	true -> build_message(Prop, ?RESOURCE_ERROR_HEADERS, ?OPTIONAL_RESOURCE_ERROR_HEADERS);
	false -> {error, "Proplist failed validation for resource_error"}
    end.

-spec(resource_error_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
resource_error_v({struct, Prop}) ->
    resource_error_v(Prop);
resource_error_v(Prop) ->
    validate(Prop, ?RESOURCE_ERROR_HEADERS, ?RESOURCE_ERROR_VALUES, ?RESOURCE_ERROR_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(call_event/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
call_event({struct, Prop}) ->
    call_event(Prop);
call_event(Prop) ->
    case call_event_v(Prop) of
	true -> build_message(Prop, ?CALL_EVENT_HEADERS, ?OPTIONAL_CALL_EVENT_HEADERS);
	false -> {error, "Proplist failed validation for call_event"}
    end.

-spec(call_event_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
call_event_v({struct, Prop}) ->
    call_event_v(Prop);
call_event_v(Prop) ->
    validate(Prop, ?CALL_EVENT_HEADERS, ?CALL_EVENT_VALUES, ?CALL_EVENT_TYPES).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(call_status_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
call_status_req({struct, Prop}) ->
    call_status_req(Prop);
call_status_req(Prop) ->
    case call_status_req_v(Prop) of
	true -> build_message(Prop, ?CALL_STATUS_REQ_HEADERS, ?OPTIONAL_CALL_STATUS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for call_status req"}
    end.

-spec(call_status_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
call_status_req_v({struct, Prop}) ->
    call_status_req_v(Prop);
call_status_req_v(Prop) ->
    validate(Prop, ?CALL_STATUS_REQ_HEADERS, ?CALL_STATUS_REQ_VALUES, ?CALL_STATUS_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Respond with status of a call, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(call_status_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
call_status_resp({struct, Prop}) ->
    call_status_resp(Prop);
call_status_resp(Prop) ->
    case call_status_resp_v(Prop) of
	true -> build_message(Prop, ?CALL_STATUS_RESP_HEADERS, ?OPTIONAL_CALL_STATUS_RESP_HEADERS);
	false -> {error, "Proplist failed validation for call_status_resp"}
    end.

-spec(call_status_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
call_status_resp_v({struct, Prop}) ->
    call_status_resp_v(Prop);
call_status_resp_v(Prop) ->
    validate(Prop, ?CALL_STATUS_RESP_HEADERS, ?CALL_STATUS_RESP_VALUES, ?CALL_STATUS_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a CDR for a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(call_cdr/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
call_cdr({struct, Prop}) ->
    call_cdr(Prop);
call_cdr(Prop) ->
    case call_cdr_v(Prop) of
	true -> build_message(Prop, ?CALL_CDR_HEADERS, ?OPTIONAL_CALL_CDR_HEADERS);
	false -> {error, "Proplist failed validation for call_cdr"}
    end.

-spec(call_cdr_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
call_cdr_v({struct, Prop}) ->
    call_cdr_v(Prop);
call_cdr_v(Prop) ->
    validate(Prop, ?CALL_CDR_HEADERS, ?CALL_CDR_VALUES, ?CALL_CDR_TYPES).

%%--------------------------------------------------------------------
%% @doc Format an error event
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(error_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
error_resp({struct, Prop}) ->
    error_resp(Prop);
error_resp(Prop) ->
    case error_resp_v(Prop) of
	true -> build_message(Prop, ?ERROR_RESP_HEADERS, ?OPTIONAL_ERROR_RESP_HEADERS);
	false -> {error, "Proplist failed validation for error_resp"}
    end.

-spec(error_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
error_resp_v({struct, Prop}) ->
    error_resp_v(Prop);
error_resp_v(Prop) ->
    validate(Prop, ?ERROR_RESP_HEADERS, ?ERROR_RESP_VALUES, ?ERROR_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store API call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(store_req/1 :: ( Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
store_req({struct, Prop}) ->
    store_req(Prop);
store_req(Prop) ->
    case store_req_v(Prop) of
	true -> build_message(Prop, ?STORE_REQ_HEADERS, ?OPTIONAL_STORE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for store_req"}
    end.

-spec(store_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
store_req_v({struct, Prop}) ->
    store_req_v(Prop);
store_req_v(Prop) ->
    validate(Prop, ?STORE_REQ_HEADERS, ?STORE_REQ_VALUES, ?STORE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store response for amqp storage method
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(store_amqp_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
store_amqp_resp({struct, Prop}) ->
    store_amqp_resp(Prop);
store_amqp_resp(Prop) ->
    case store_amqp_resp_v(Prop) of
	true -> build_message(Prop, ?STORE_AMQP_RESP_HEADERS, ?OPTIONAL_STORE_AMQP_RESP_HEADERS);
	false -> {error, "Proplist failed validation for store_amqp_resp"}
    end.

-spec(store_amqp_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
store_amqp_resp_v({struct, Prop}) ->
    store_amqp_resp_v(Prop);
store_amqp_resp_v(Prop) ->
    validate(Prop, ?STORE_AMQP_RESP_HEADERS, ?STORE_AMQP_RESP_VALUES, ?STORE_AMQP_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store response for http storage method
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(store_http_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
store_http_resp({struct, Prop}) ->
    store_http_resp(Prop);
store_http_resp(Prop) ->
    case store_http_resp_v(Prop) of
	true -> build_message(Prop, ?STORE_HTTP_RESP_HEADERS, ?OPTIONAL_STORE_HTTP_RESP_HEADERS);
	false -> {error, "Prop failed validation for store_http_resp"}
    end.

-spec(store_http_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
store_http_resp_v({struct, Prop}) ->
    store_http_resp_v(Prop);
store_http_resp_v(Prop) ->
    validate(Prop, ?STORE_HTTP_RESP_HEADERS, ?STORE_HTTP_RESP_VALUES, ?STORE_HTTP_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Create a tone on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(tones_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
tones_req({struct, Prop}) ->
    tones_req(Prop);
tones_req(Prop) ->
    case tones_req_v(Prop) of
	true -> build_message(Prop, ?TONES_REQ_HEADERS, ?OPTIONAL_TONES_REQ_HEADERS);
	false -> {error, "Prop failed validation for tones_req"}
    end.

-spec(tones_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
tones_req_v({struct, Prop}) ->
    tones_req_v(Prop);
tones_req_v(Prop) ->
    validate(Prop, ?TONES_REQ_HEADERS, ?TONES_REQ_VALUES, ?TONES_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc A Tone within a Tones request - see wiki
%% Takes proplist and returns a proplist
%% @end
%%--------------------------------------------------------------------
-spec(tones_req_tone/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
tones_req_tone({struct, Prop}) ->
    tones_req_tone(Prop);
tones_req_tone(Prop) ->
    case tones_req_tone_v(Prop) of
	true -> build_message_specific(Prop, ?TONES_REQ_TONE_HEADERS, ?OPTIONAL_TONES_REQ_TONE_HEADERS);
	false -> {error, "Proplist failed validation for tones_req_tone"}
    end.

-spec(tones_req_tone_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
tones_req_tone_v({struct, Prop}) ->
    tones_req_tone_v(Prop);
tones_req_tone_v(Prop) ->
    validate_message(Prop, ?TONES_REQ_TONE_HEADERS, ?TONES_REQ_TONE_VALUES, ?TONES_REQ_TONE_TYPES).

%%--------------------------------------------------------------------
%% @doc Detect tones on the line
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(tone_detect_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
tone_detect_req({struct, Prop}) ->
    tone_detect_req(Prop);
tone_detect_req(Prop) ->
    case tone_detect_req_v(Prop) of
	true -> build_message(Prop, ?TONE_DETECT_REQ_HEADERS, ?OPTIONAL_TONE_DETECT_REQ_HEADERS);
	false -> {error, "Proplist failed validation for tone_detect"}
    end.

-spec(tone_detect_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
tone_detect_req_v({struct, Prop}) ->
    tone_detect_req_v(Prop);
tone_detect_req_v(Prop) ->
    validate(Prop, ?TONE_DETECT_REQ_HEADERS, ?TONE_DETECT_REQ_VALUES, ?TONE_DETECT_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Send a list of dialplan applications in bulk - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(queue_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
queue_req({struct, Prop}) ->
    queue_req(Prop);
queue_req(Prop) ->
    case queue_req_v(Prop) of
	true -> build_message(Prop, ?QUEUE_REQ_HEADERS, ?OPTIONAL_QUEUE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for queue_req"}
    end.

-spec(queue_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
queue_req_v({struct, Prop}) ->
    queue_req_v(Prop);
queue_req_v(Prop) ->
    validate(Prop, ?QUEUE_REQ_HEADERS, ?QUEUE_REQ_VALUES, ?QUEUE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Play media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(play_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
play_req({struct, Prop}) ->
    play_req(Prop);
play_req(Prop) ->
    case play_req_v(Prop) of
	true -> build_message(Prop, ?PLAY_REQ_HEADERS, ?OPTIONAL_PLAY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for play_req"}
    end.

-spec(play_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
play_req_v({struct, Prop}) ->
    play_req_v(Prop);
play_req_v(Prop) ->
    validate(Prop, ?PLAY_REQ_HEADERS, ?PLAY_REQ_VALUES, ?PLAY_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Request media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
media_req({struct, Prop}) ->
    media_req(Prop);
media_req(Prop) ->
    case media_req_v(Prop) of
	true -> build_message(Prop, ?MEDIA_REQ_HEADERS, ?OPTIONAL_MEDIA_REQ_HEADERS);
	false -> {error, "Proplist failed validation for media_req"}
    end.

media_req_v({struct, Prop}) ->
    media_req_v(Prop);
media_req_v(Prop) ->
    validate(Prop, ?MEDIA_REQ_HEADERS, ?MEDIA_REQ_VALUES, ?MEDIA_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Response with media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
media_resp({struct, Prop}) ->
    media_resp(Prop);
media_resp(Prop) ->
    case media_resp_v(Prop) of
	true -> build_message(Prop, ?MEDIA_RESP_HEADERS, ?OPTIONAL_MEDIA_RESP_HEADERS);
	false -> {error, "Proplist failed validation for media_resp"}
    end.

media_resp_v({struct, Prop}) ->
    media_resp_v(Prop);
media_resp_v(Prop) ->
    validate(Prop, ?MEDIA_RESP_HEADERS, ?MEDIA_RESP_VALUES, ?MEDIA_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Media error - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
media_error({struct, Prop}) ->
    media_error(Prop);
media_error(Prop) ->
    case media_error_v(Prop) of
	true -> build_message(Prop, ?MEDIA_ERROR_HEADERS, ?OPTIONAL_MEDIA_ERROR_HEADERS);
	false -> {error, "Proplist failed validation for media_error"}
    end.

media_error_v({struct, Prop}) ->
    media_error_v(Prop);
media_error_v(Prop) ->
    validate(Prop, ?MEDIA_ERROR_HEADERS, ?MEDIA_ERROR_VALUES, ?MEDIA_ERROR_TYPES).

%%--------------------------------------------------------------------
%% @doc Record media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(record_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
record_req({struct, Prop}) ->
    record_req(Prop);
record_req(Prop) ->
    case record_req_v(Prop) of
	true -> build_message(Prop, ?RECORD_REQ_HEADERS, ?OPTIONAL_RECORD_REQ_HEADERS);
	false -> {error, "Proplist failed validation for record_req"}
    end.

-spec(record_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
record_req_v({struct, Prop}) ->
    record_req_v(Prop);
record_req_v(Prop) ->
    validate(Prop, ?RECORD_REQ_HEADERS, ?RECORD_REQ_VALUES, ?RECORD_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Bridge a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(bridge_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
bridge_req({struct, Prop}) ->
    bridge_req(Prop);
bridge_req(Prop) ->
    case bridge_req_v(Prop) of
	true -> build_message(Prop, ?BRIDGE_REQ_HEADERS, ?OPTIONAL_BRIDGE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for bridge_req"}
    end.

-spec(bridge_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
bridge_req_v({struct, Prop}) ->
    bridge_req_v(Prop);
bridge_req_v(Prop) ->
    validate(Prop, ?BRIDGE_REQ_HEADERS, ?BRIDGE_REQ_VALUES, ?BRIDGE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Endpoints for bridging a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(bridge_req_endpoint/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, proplist()) | tuple(error, string())).
bridge_req_endpoint({struct, Prop}) ->
    bridge_req_endpoint(Prop);
bridge_req_endpoint(Prop) ->
    case bridge_req_endpoint_v(Prop) of
	true -> build_message_specific(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
	false -> {error, "Proplist failed validation for bridge_req_endpoint"}
    end.

-spec(bridge_req_endpoint_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
bridge_req_endpoint_v({struct, Prop}) ->
    bridge_req_endpoint_v(Prop);
bridge_req_endpoint_v(Prop) ->
    validate_message(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?BRIDGE_REQ_ENDPOINT_VALUES, ?BRIDGE_REQ_ENDPOINT_TYPES).

%%--------------------------------------------------------------------
%% @doc Answer a session - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(answer_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
answer_req({struct, Prop}) ->
    answer_req(Prop);
answer_req(Prop) ->
    case answer_req_v(Prop) of
	true -> build_message(Prop, ?ANSWER_REQ_HEADERS, ?OPTIONAL_ANSWER_REQ_HEADERS);
	false -> {error, "Proplist failed validation for answer_req"}
    end.

-spec(answer_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
answer_req_v({struct, Prop}) ->
    answer_req_v(Prop);
answer_req_v(Prop) ->
    validate(Prop, ?ANSWER_REQ_HEADERS, ?ANSWER_REQ_VALUES, ?ANSWER_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Hangup a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(hangup_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
hangup_req({struct, Prop}) ->
    hangup_req(Prop);
hangup_req(Prop) ->
    case hangup_req_v(Prop) of
	true -> build_message(Prop, ?HANGUP_REQ_HEADERS, ?OPTIONAL_HANGUP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for hangup_req"}
    end.

-spec(hangup_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
hangup_req_v({struct, Prop}) ->
    hangup_req_v(Prop);
hangup_req_v(Prop) ->
    validate(Prop, ?HANGUP_REQ_HEADERS, ?HANGUP_REQ_VALUES, ?HANGUP_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Park a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(park_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
park_req({struct, Prop}) ->
    park_req(Prop);
park_req(Prop) ->
    case park_req_v(Prop) of
	true -> build_message(Prop, ?PARK_REQ_HEADERS, ?OPTIONAL_PARK_REQ_HEADERS);
	false -> {error, "Proplist failed validation for park_req"}
    end.

-spec(park_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
park_req_v({struct, Prop}) ->
    park_req_v(Prop);
park_req_v(Prop) ->
    validate(Prop, ?PARK_REQ_HEADERS, ?PARK_REQ_VALUES, ?PARK_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Set Custom Channel variables - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(set_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
set_req({struct, Prop}) ->
    set_req(Prop);
set_req(Prop) ->
    case set_req_v(Prop) of
	true -> build_message(Prop, ?SET_REQ_HEADERS, ?OPTIONAL_SET_REQ_HEADERS);
	false -> {error, "Proplist failed validation for set_req"}
    end.

-spec(set_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
set_req_v({struct, Prop}) ->
    set_req_v(Prop);
set_req_v(Prop) ->
    validate(Prop, ?SET_REQ_HEADERS, ?SET_REQ_VALUES, ?SET_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Play media and record digits - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(play_collect_digits_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
play_collect_digits_req({struct, Prop}) ->
    play_collect_digits_req(Prop);
play_collect_digits_req(Prop) ->
    case play_collect_digits_req_v(Prop) of
	true -> build_message(Prop, ?PLAY_COLLECT_DIGITS_REQ_HEADERS, ?OPTIONAL_PLAY_COLLECT_DIGITS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for play_collect_digits_req"}
    end.

-spec(play_collect_digits_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
play_collect_digits_req_v({struct, Prop}) ->
    play_collect_digits_req_v(Prop);
play_collect_digits_req_v(Prop) ->
    validate(Prop, ?PLAY_COLLECT_DIGITS_REQ_HEADERS, ?PLAY_COLLECT_DIGITS_REQ_VALUES, ?PLAY_COLLECT_DIGITS_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Pickup a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(call_pickup_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
call_pickup_req({struct, Prop}) ->
    call_pickup_req(Prop);
call_pickup_req(Prop) ->
    case call_pickup_req_v(Prop) of
	true -> build_message(Prop, ?CALL_PICKUP_REQ_HEADERS, ?OPTIONAL_CALL_PICKUP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for call_pickup_req"}
    end.

-spec(call_pickup_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
call_pickup_req_v({struct, Prop}) ->
    call_pickup_req_v(Prop);
call_pickup_req_v(Prop) ->
    validate(Prop, ?CALL_PICKUP_REQ_HEADERS, ?CALL_PICKUP_REQ_VALUES, ?CALL_PICKUP_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Say - convert text to speech - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(say_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
say_req({struct, Prop}) ->
    say_req(Prop);
say_req(Prop) ->
    case say_req_v(Prop) of
	true -> build_message(Prop, ?SAY_REQ_HEADERS, ?OPTIONAL_SAY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for say_req"}
    end.

-spec(say_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
say_req_v({struct, Prop}) ->
    say_req_v(Prop);
say_req_v(Prop) ->
    validate(Prop, ?SAY_REQ_HEADERS, ?SAY_REQ_VALUES, ?SAY_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Sleep - Pauses execution - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(sleep_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
sleep_req({struct, Prop}) ->
    sleep_req(Prop);
sleep_req(Prop) ->
    case sleep_req_v(Prop) of
	true -> build_message(Prop, ?SLEEP_REQ_HEADERS, ?OPTIONAL_SLEEP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for sleep_req"}
    end.

-spec(sleep_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
sleep_req_v({struct, Prop}) ->
    sleep_req_v(Prop);
sleep_req_v(Prop) ->
    validate(Prop, ?SLEEP_REQ_HEADERS, ?SLEEP_REQ_VALUES, ?SLEEP_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:noop API call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(noop_req/1 :: ( Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
noop_req({struct, Prop}) ->
    noop_req(Prop);
noop_req(Prop) ->
    case noop_req_v(Prop) of
	true -> build_message(Prop, ?NOOP_REQ_HEADERS, ?OPTIONAL_NOOP_REQ_HEADERS);
	false -> {error, "Proplist failed validation for noop_req"}
    end.

-spec(noop_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
noop_req_v({struct, Prop}) ->
    noop_req_v(Prop);
noop_req_v(Prop) ->
    validate(Prop, ?NOOP_REQ_HEADERS, ?NOOP_REQ_VALUES, ?NOOP_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference - Sends caller to a conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_req({struct, Prop}) ->
    conference_req(Prop);
conference_req(Prop) ->
    case conference_req_v(Prop) of
	true -> build_message(Prop, ?CONFERENCE_REQ_HEADERS, ?OPTIONAL_CONFERENCE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_req"}
    end.

-spec(conference_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_req_v({struct, Prop}) ->
    conference_req_v(Prop);
conference_req_v(Prop) ->
    validate(Prop, ?CONFERENCE_REQ_HEADERS, ?CONFERENCE_REQ_VALUES, ?CONFERENCE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::members - Lists all members of a conference
%%     or details about certain members - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_members_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_members_req({struct, Prop}) ->
    conference_members_req(Prop);
conference_members_req(Prop) ->
    case conference_members_req_v(Prop) of
	true -> build_message(Prop, ?CONF_MEMBERS_REQ_HEADERS, ?OPTIONAL_CONF_MEMBERS_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_members_req"}
    end.

-spec(conference_members_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_members_req_v({struct, Prop}) ->
    conference_members_req_v(Prop);
conference_members_req_v(Prop) ->
    validate(Prop, ?CONF_MEMBERS_REQ_HEADERS, ?CONF_MEMBERS_REQ_VALUES, ?CONF_MEMBERS_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::members - The response to members - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_members_resp/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_members_resp({struct, Prop}) ->
    conference_members_resp(Prop);
conference_members_resp(Prop) ->
    case conference_members_resp_v(Prop) of
	true -> build_message(Prop, ?CONF_MEMBERS_RESP_HEADERS, ?OPTIONAL_CONF_MEMBERS_RESP_HEADERS);
	false -> {error, "Proplist failed validation for conference_members_resp"}
    end.

-spec(conference_members_resp_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_members_resp_v({struct, Prop}) ->
    conference_members_resp_v(Prop);
conference_members_resp_v(Prop) ->
    validate(Prop, ?CONF_MEMBERS_RESP_HEADERS, ?CONF_MEMBERS_RESP_VALUES, ?CONF_MEMBERS_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::play - Play audio to all or a single 
%%     conference member - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_play_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_play_req({struct, Prop}) ->
    conference_play_req(Prop);
conference_play_req(Prop) ->
    case conference_play_req_v(Prop) of
	true -> build_message(Prop, ?CONF_PLAY_REQ_HEADERS, ?OPTIONAL_CONF_PLAY_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_play_req"}
    end.

-spec(conference_play_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_play_req_v({struct, Prop}) ->
    conference_play_req_v(Prop);
conference_play_req_v(Prop) ->
    validate(Prop, ?CONF_PLAY_REQ_HEADERS, ?CONF_PLAY_REQ_VALUES, ?CONF_PLAY_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::deaf - Make a conference member deaf - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_deaf_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_deaf_req({struct, Prop}) ->
    conference_deaf_req(Prop);
conference_deaf_req(Prop) ->
    case conference_deaf_req_v(Prop) of
	true -> build_message(Prop, ?CONF_DEAF_REQ_HEADERS, ?OPTIONAL_CONF_DEAF_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_deaf_req"}
    end.

-spec(conference_deaf_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_deaf_req_v({struct, Prop}) ->
    conference_deaf_req_v(Prop);
conference_deaf_req_v(Prop) ->
    validate(Prop, ?CONF_DEAF_REQ_HEADERS, ?CONF_DEAF_REQ_VALUES, ?CONF_DEAF_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::undeaf - Allows a specific conference member to 
%%     hear if they where marked deaf - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_undeaf_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_undeaf_req({struct, Prop}) ->
    conference_undeaf_req(Prop);
conference_undeaf_req(Prop) ->
    case conference_undeaf_req_v(Prop) of
	true -> build_message(Prop, ?CONF_UNDEAF_REQ_HEADERS, ?OPTIONAL_CONF_UNDEAF_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_undeaf_req"}
    end.

-spec(conference_undeaf_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_undeaf_req_v({struct, Prop}) ->
    conference_undeaf_req_v(Prop);
conference_undeaf_req_v(Prop) ->
    validate(Prop, ?CONF_UNDEAF_REQ_HEADERS, ?CONF_UNDEAF_REQ_VALUES, ?CONF_UNDEAF_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::mute - Mutes a specific member in a 
%%     conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_mute_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_mute_req({struct, Prop}) ->
    conference_mute_req(Prop);
conference_mute_req(Prop) ->
    case conference_mute_req_v(Prop) of
	true -> build_message(Prop, ?CONF_MUTE_REQ_HEADERS, ?OPTIONAL_CONF_MUTE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_mute_req"}
    end.

-spec(conference_mute_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_mute_req_v({struct, Prop}) ->
    conference_mute_req_v(Prop);
conference_mute_req_v(Prop) ->
    validate(Prop, ?CONF_MUTE_REQ_HEADERS, ?CONF_MUTE_REQ_VALUES, ?CONF_MUTE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::unmute - Unmutes a specific member in a 
%%     conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_unmute_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_unmute_req({struct, Prop}) ->
    conference_unmute_req(Prop);
conference_unmute_req(Prop) ->
    case conference_unmute_req_v(Prop) of
	true -> build_message(Prop, ?CONF_UNMUTE_REQ_HEADERS, ?OPTIONAL_CONF_UNMUTE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_unmute_req"}
    end.

-spec(conference_unmute_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_unmute_req_v({struct, Prop}) ->
    conference_unmute_req_v(Prop);
conference_unmute_req_v(Prop) ->
    validate(Prop, ?CONF_UNMUTE_REQ_HEADERS, ?CONF_UNMUTE_REQ_VALUES, ?CONF_UNMUTE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::kick - Removes a member from a conference - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_kick_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_kick_req({struct, Prop}) ->
    conference_kick_req(Prop);
conference_kick_req(Prop) ->
    case conference_kick_req_v(Prop) of
	true -> build_message(Prop, ?CONF_KICK_REQ_HEADERS, ?OPTIONAL_CONF_KICK_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_kick_req"}
    end.

-spec(conference_kick_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_kick_req_v({struct, Prop}) ->
    conference_kick_req_v(Prop);
conference_kick_req_v(Prop) ->
    validate(Prop, ?CONF_KICK_REQ_HEADERS, ?CONF_KICK_REQ_VALUES, ?CONF_KICK_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Conference::move - Transfers a member between to 
%%     conferences - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(conference_move_req/1 :: (Prop :: proplist() | json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
conference_move_req({struct, Prop}) ->
    conference_move_req(Prop);
conference_move_req(Prop) ->
    case conference_move_req_v(Prop) of
	true -> build_message(Prop, ?CONF_MOVE_REQ_HEADERS, ?OPTIONAL_CONF_MOVE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for conference_move_req"}
    end.

-spec(conference_move_req_v/1 :: (Prop :: proplist() | json_object()) -> boolean()).
conference_move_req_v({struct, Prop}) ->
    conference_move_req_v(Prop);
conference_move_req_v(Prop) ->
    validate(Prop, ?CONF_MOVE_REQ_HEADERS, ?CONF_MOVE_REQ_VALUES, ?CONF_MOVE_REQ_TYPES).

%% given a proplist of a FS event, return the Whistle-equivalent app name(s)
%% a FS event could have multiple Whistle equivalents
-spec(convert_fs_evt_name/1 :: (EvtName :: binary()) -> binary() | list(binary())).
convert_fs_evt_name(EvtName) ->
    find_all_apps(EvtName, ?SUPPORTED_APPLICATIONS, []).

find_all_apps(_, [], []) -> <<>>;
find_all_apps(_, [], [App]) -> App;
find_all_apps(_, [], Apps) -> Apps;
find_all_apps(EvtName, [{EvtName, App} | SAs], Apps) ->
    find_all_apps(EvtName, SAs, [App | Apps]);
find_all_apps(EvtName, [_ | SAs], Apps) ->
    find_all_apps(EvtName, SAs, Apps).

%% given a Whistle Dialplan Application name, return the FS-equivalent event name
%% A Whistle Dialplan Application name is 1-to-1 with the FS-equivalent
-spec(convert_whistle_app_name/1 :: (AppName :: binary()) -> binary()).
convert_whistle_app_name(AppName) ->
    case lists:keyfind(AppName, 2, ?SUPPORTED_APPLICATIONS) of
	false -> <<>>;
	{EvtName, AppName} -> EvtName
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(validate/4 :: (Prop :: proplist(), ReqH :: list(binary()), Vals :: proplist(), Types :: proplist()) -> boolean()).
validate(Prop, ReqH, Vals, Types) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso
	validate_message(Prop, ReqH, Vals, Types).

-spec(validate_message/4 :: (Prop :: proplist(), ReqH :: list(binary()), Vals :: proplist(), Types :: proplist()) -> boolean()).
validate_message(Prop, ReqH, Vals, Types) ->
    has_all(Prop, ReqH) andalso
	values_check(Prop, Vals) andalso
	type_check(Prop, Types).

-spec(build_message/3 :: (Prop :: proplist(), ReqH :: list(binary()), OptH :: list(binary())) -> tuple(ok, iolist()) | tuple(error, string())).
build_message(Prop, ReqH, OptH) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    format_log(error,"Build Error: ~p~nDefHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	HeadAndProp ->
	    build_message_specific(HeadAndProp, ReqH, OptH)
    end.

-spec(build_message_specific/3 :: (proplist() | tuple(list(binary()), proplist()), list(binary()), list(binary())) -> tuple(ok, iolist()) | tuple(error, string())).
build_message_specific({Headers, Prop}, ReqH, OptH) ->
    case update_required_headers(Prop, ReqH, Headers) of
	{error, _Reason} = Error ->
	    format_log(error,"Build Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ReqH, Prop]),
	    Error;
	{Headers1, Prop1} ->
	    {Headers2, _Prop2} = update_optional_headers(Prop1, OptH, Headers1),
	    headers_to_json(Headers2)
    end;
build_message_specific(Prop, ReqH, OptH) ->
    build_message_specific({[], Prop}, ReqH, OptH).

-spec(headers_to_json/1 :: (HeadersProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
headers_to_json(HeadersProp) ->
    try
	{ok, mochijson2:encode({struct, HeadersProp})}
    catch
	_What:_Why -> {error, io_lib:format("WHISTLE TO_JSON ERROR(~p): ~p~n~p", [_What, _Why, HeadersProp])}
    end.

%% Checks Prop for all default headers, throws error if one is missing
%% defaults(PassedProps) -> { Headers, NewPropList } | {error, Reason}
-spec(defaults/1 :: (Prop :: proplist() | json_object()) -> {proplist(), proplist()} | {error, string()}).
defaults(Prop) ->
    defaults(Prop, []).
defaults(Prop, Headers) ->
    case update_required_headers(Prop, ?DEFAULT_HEADERS, Headers) of
	{error, _Reason} = Error ->
	    Error;
	{Headers1, Prop1} ->
	    update_optional_headers(Prop1, ?OPTIONAL_DEFAULT_HEADERS, Headers1)
    end.

-spec(update_required_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist()) -> {proplist(), proplist()} | {error, string()}).
update_required_headers(Prop, Fields, Headers) ->
    case has_all(Prop, Fields) of 
	true ->
	    add_headers(Prop, Fields, Headers);
	false ->
	    {error, "All required headers not defined"}
    end.

-spec(update_optional_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist()) -> {proplist(), proplist()}).
update_optional_headers(Prop, Fields, Headers) ->
    case has_any(Prop, Fields) of
	true ->
	    add_optional_headers(Prop, Fields, Headers);
	false ->
	    {Headers, Prop}
    end.

%% add [Header] from Prop to HeadProp
-spec(add_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist()) -> {proplist(), proplist()}).
add_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) ->
			{[{K, get_value(K, KVs)} | Headers1], delete(K, KVs)}
		end, {Headers, Prop}, Fields).

-spec(add_optional_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist()) -> {proplist(), proplist()}).
add_optional_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) ->
			case get_value(K, KVs) of
			    undefined ->
				{Headers1, KVs};
			    V ->
				{[{K, V} | Headers1], delete(K, KVs)}
			end
		end, {Headers, Prop}, Fields).

%% Checks Prop against a list of required headers, returns true | false
-spec(has_all/2 :: (Prop :: proplist(), Headers :: list(binary())) -> boolean()).
has_all(Prop, Headers) ->
    lists:all(fun(Header) ->
		      case is_defined(Header, Prop) of
			  true -> true;
			  false ->
			      format_log(error,"WHISTLE_API.has_all: Failed to find ~p~nProp: ~p~n", [Header, Prop]),
			      false
		      end
	      end, Headers).

%% Checks Prop against a list of optional headers, returns true | false if at least one if found
-spec(has_any/2 :: (Prop :: proplist(), Headers :: list(binary())) -> boolean()).
has_any(Prop, Headers) ->
    lists:any(fun(Header) -> is_defined(Header, Prop) end, Headers).

%% checks Prop against a list of values to ensure known key/value pairs are correct (like Event-Category
%% and Event-Name). We don't care if a key is defined in Values and not in Prop; that is handled by has_all/1
values_check(Prop, Values) ->
    lists:all(fun({Key, Vs}) when is_list(Vs) ->
		      case get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  V -> case lists:member(V, Vs) of
				   true -> true;
				   false ->
				       format_log(error,"WHISTLE_API.values_check: K: ~p V: ~p not in ~p~n", [Key, V, Vs]),
				       false
			       end
		      end;
		 ({Key, V}) ->
		      case get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  V -> true;
			  _Val ->
			      format_log(error,"WHISTLE_API.values_check: Key: ~p Set: ~p Expected: ~p~n", [Key, _Val, V]),
			      false
		      end
	      end, Values).

%% checks Prop against a list of {Key, Fun}, running the value of Key through Fun, which returns a
%% boolean.
type_check(Prop, Types) ->
    lists:all(fun({Key, Fun}) ->
		      case get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  Value -> case Fun(Value) of % returns boolean
				       true -> true;
				       false ->
					   format_log(error,"WHISTLE_API.type_check: K: ~p V: ~p failed fun~n", [Key, Value]),
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
