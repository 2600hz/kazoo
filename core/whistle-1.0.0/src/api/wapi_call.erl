%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%% Call-related messages, like switch events, status requests, etc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_call).

-export([optional_call_event_headers/0]).

-export([event/1, event_v/1]).

-export([channel_status_req/1, channel_status_req_v/1]).
-export([channel_status_resp/1, channel_status_resp_v/1]).

-export([query_auth_id_req/1, query_auth_id_req_v/1]).
-export([query_auth_id_resp/1, query_auth_id_resp_v/1]).

-export([query_user_channels_req/1, query_user_channels_req_v/1]).
-export([query_user_channels_resp/1, query_user_channels_resp_v/1]).

-export([query_account_channels_req/1, query_account_channels_req_v/1]).
-export([query_account_channels_resp/1, query_account_channels_resp_v/1]).

-export([query_channels_req/1, query_channels_req_v/1]).
-export([query_channels_resp/1, query_channels_resp_v/1]).

-export([usurp_control/1, usurp_control_v/1]).
-export([usurp_publisher/1, usurp_publisher_v/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_event/1, publish_event/2]).

-export([publish_channel_status_req/1 ,publish_channel_status_req/2, publish_channel_status_req/3]).
-export([publish_channel_status_resp/2, publish_channel_status_resp/3]).

-export([publish_query_auth_id_req/1 ,publish_query_auth_id_req/2, publish_query_auth_id_req/3]).
-export([publish_query_auth_id_resp/2, publish_query_auth_id_resp/3]).

-export([publish_query_user_channels_req/1 ,publish_query_user_channels_req/4]).
-export([publish_query_user_channels_resp/2 ,publish_query_user_channels_resp/3]).

-export([publish_query_account_channels_req/1 ,publish_query_account_channels_req/3]).
-export([publish_query_account_channels_resp/2 ,publish_query_account_channels_resp/3]).

-export([publish_query_channels_req/1 ,publish_query_channels_req/2]).
-export([publish_query_channels_resp/2 ,publish_query_channels_resp/3]).

-export([publish_usurp_control/2, publish_usurp_control/3]).
-export([publish_usurp_publisher/2, publish_usurp_publisher/3]).

-export([get_status/1]).
-export([event_routing_key/2]).

-include_lib("whistle/include/wh_api.hrl").

%% Routing key prefix for rating
-define(KEY_RATING_REQ, <<"call.rating">>).

%% Call Events
-define(CALL_EVENT_ROUTING_KEY(Event, CallId), <<"call."
                                                 ,(wh_util:to_binary(Event))/binary
                                                 ,"."
                                                 ,(amqp_util:encode(CallId))/binary
                                               >>).
-define(CALL_EVENT_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Response">>
                                      ,<<"Application-Event">>, <<"Application-Data">>
                                      ,<<"Custom-Channel-Vars">>, <<"Timestamp">>
                                      ,<<"Channel-State">>, <<"Channel-Call-State">>, <<"Channel-Name">>
                                      ,<<"Call-Direction">>, <<"Transfer-History">>
                                      ,<<"Other-Leg-Direction">>, <<"Other-Leg-Caller-ID-Name">>
                                      ,<<"Other-Leg-Caller-ID-Number">>, <<"Other-Leg-Destination-Number">>
                                      ,<<"Other-Leg-Call-ID">> %% BRIDGE
                                      ,<<"Target-Call-ID">> %% TRANSFEREE
                                      ,<<"Detected-Tone">>, <<"DTMF-Duration">>, <<"DTMF-Digit">> %% DTMF and Tones
                                      ,<<"Terminator">>, <<"Disposition">>
                                      ,<<"Hangup-Cause">>, <<"Hangup-Code">> %% Hangup
                                      ,<<"Raw-Application-Name">>, <<"Raw-Application-Data">>
                                      ,<<"Length">>, <<"Silence-Terminated">> %% Record-related
                                      ,<<"User-Agent">>
                                      ,<<"Switch-Hostname">>, <<"Group-ID">>
                                      ,<<"Control-Queue">>, <<"Channel-Moving">>
                                      ,<<"Conference-Name">>, <<"Conference-Config">>
                                      ,<<"Replaced-By">>, <<"Remote-SDP">>, <<"Local-SDP">>
                                      ,<<"Duration-Seconds">>, <<"Billing-Seconds">>, <<"Ringing-Seconds">>
                                      ,<<"To-Uri">>, <<"From-Uri">>, <<"To">>, <<"From">>, <<"Request">>
                                      ,<<"Digits-Dialed">>, <<"Presence-ID">>, <<"Media-Server">>
                                      ,<<"Caller-ID-Number">>, <<"Caller-ID-Name">>
                                      ,<<"Callee-ID-Number">>, <<"Callee-ID-Name">>
                                      ,<<"Custom-SIP-Headers">>, <<"Fax-Info">>
                                      ,<<"From-Tag">>, <<"To-Tag">>
                                      ,<<"Intercepted-By">>
                                      ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                      ,<<"Switch-URL">>, <<"Switch-URI">>, <<"Switch-Node">>
                                      ,<<"Parking-Slot">>
                                     ]).
-define(CALL_EVENT_VALUES, [{<<"Event-Category">>, <<"call_event">>}]).
-define(CALL_EVENT_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                           ,{<<"Custom-SIP-Headers">>, fun wh_json:is_json_object/1}
                           ,{<<"Fax-Info">>, fun wh_json:is_json_object/1}
                          ]).

%% Channel Status Request
-define(CHANNEL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CHANNEL_STATUS_REQ_HEADERS, [<<"Active-Only">>]).
-define(CHANNEL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"channel_status_req">>}
                                   ]).
-define(CHANNEL_STATUS_REQ_TYPES, []).

%% Channel Status Response
-define(CHANNEL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CHANNEL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>
                                               ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                               ,<<"Switch-URL">>, <<"Other-Leg-Call-ID">>
                                               ,<<"Realm">>, <<"Username">>
                                               ,<<"From-Tag">>, <<"To-Tag">>
                                              ]).
-define(CHANNEL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                     ,{<<"Event-Name">>, <<"channel_status_resp">>}
                                     ,{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                                    ]).
-define(CHANNEL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%% Query Auth ID Req
-define(QUERY_AUTH_ID_REQ_HEADERS, [<<"Auth-ID">>]).
-define(OPTIONAL_QUERY_AUTH_ID_REQ_HEADERS, []).
-define(QUERY_AUTH_ID_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                   ,{<<"Event-Name">>, <<"query_auth_id_req">>}
                                  ]).
-define(QUERY_AUTH_ID_REQ_TYPES, []).

%% Query Auth ID Resp
-define(QUERY_AUTH_ID_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_AUTH_ID_RESP_HEADERS, [<<"Channels">>]).
-define(QUERY_AUTH_ID_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"query_auth_id_resp">>}
                                   ]).
-define(QUERY_AUTH_ID_RESP_TYPES, []).

%% Query User Channels Req
-define(QUERY_USER_CHANNELS_REQ_HEADERS, []).
-define(OPTIONAL_QUERY_USER_CHANNELS_REQ_HEADERS, [<<"Usernames">>, <<"Username">>
                                                   ,<<"Realm">>, <<"Authorizing-IDs">>
                                                   ,<<"Active-Only">>
                                                  ]).
-define(QUERY_USER_CHANNELS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                         ,{<<"Event-Name">>, <<"query_user_channels_req">>}
                                        ]).
-define(QUERY_USER_CHANNELS_REQ_TYPES, [{<<"Usernames">>, fun erlang:is_list/1}
                                        ,{<<"Username">>, fun erlang:is_binary/1}
                                        ,{<<"Authorizing-IDs">>, fun erlang:is_list/1}
                                        ,{<<"Active-Only">>, fun wh_util:is_boolean/1}
                                       ]).

%% Query User Channels Resp
-define(QUERY_USER_CHANNELS_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_USER_CHANNELS_RESP_HEADERS, [<<"Channels">>]).
-define(QUERY_USER_CHANNELS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                          ,{<<"Event-Name">>, <<"query_user_channels_resp">>}
                                         ]).
-define(QUERY_USER_CHANNELS_RESP_TYPES, []).

%% Query Account Channels Req
-define(QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, [<<"Active-Only">>, <<"Username">>, <<"Usernames">>]).
-define(QUERY_ACCOUNT_CHANNELS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                            ,{<<"Event-Name">>, <<"query_account_channels_req">>}
                                           ]).
-define(QUERY_ACCOUNT_CHANNELS_REQ_TYPES, [{<<"Usernames">>, fun erlang:is_list/1}
                                           ,{<<"Username">>, fun erlang:is_binary/1}
                                           ,{<<"Active-Only">>, fun wh_util:is_boolean/1}
                                          ]).

%% Query Account Channels Resp
-define(QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, [<<"Channels">>]).
-define(QUERY_ACCOUNT_CHANNELS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                             ,{<<"Event-Name">>, <<"query_account_channels_resp">>}
                                            ]).
-define(QUERY_ACCOUNT_CHANNELS_RESP_TYPES, []).

%% Query Channels Req
-define(QUERY_CHANNELS_REQ_HEADERS, []).
-define(OPTIONAL_QUERY_CHANNELS_REQ_HEADERS, [<<"Fields">>, <<"Call-ID">>
                                              ,<<"Active-Only">>
                                             ]).
-define(QUERY_CHANNELS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"query_channels_req">>}
                                   ]).
-define(QUERY_CHANNELS_REQ_TYPES, [{<<"Active-Only">>, fun wh_util:is_boolean/1}]).

%% Query Channels Resp
-define(QUERY_CHANNELS_RESP_HEADERS, [<<"Channels">>]).
-define(OPTIONAL_QUERY_CHANNELS_RESP_HEADERS, []).
-define(QUERY_CHANNELS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                     ,{<<"Event-Name">>, <<"query_channels_resp">>}
                                    ]).
-define(QUERY_CHANNELS_RESP_TYPES, [{<<"Channels">>, fun wh_json:is_json_object/1}]).

%% Call Usurp Control
-define(CALL_USURP_CONTROL_HEADERS, [<<"Call-ID">>, <<"Fetch-ID">>]).
-define(OPTIONAL_CALL_USURP_CONTROL_HEADERS, [<<"Reason">>, <<"Media-Node">>]).
-define(CALL_USURP_CONTROL_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"usurp_control">>}
                                   ]).
-define(CALL_USURP_CONTROL_TYPES, []).

%% Usurp Call Event Publisher
-define(PUBLISHER_USURP_CONTROL_HEADERS, [<<"Call-ID">>, <<"Reference">>]).
-define(OPTIONAL_PUBLISHER_USURP_CONTROL_HEADERS, [<<"Reason">>, <<"Media-Node">>]).
-define(PUBLISHER_USURP_CONTROL_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                         ,{<<"Event-Name">>, <<"usurp_publisher">>}
                                        ]).
-define(PUBLISHER_USURP_CONTROL_TYPES, []).

-spec optional_call_event_headers() -> ne_binaries().
optional_call_event_headers() ->
    ?OPTIONAL_CALL_EVENT_HEADERS.

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec event(api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CALL_EVENT_HEADERS, ?OPTIONAL_CALL_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_event"}
    end;
event(JObj) -> event(wh_json:to_proplist(JObj)).

-spec event_v(api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_EVENT_HEADERS, ?CALL_EVENT_VALUES, ?CALL_EVENT_TYPES);
event_v(JObj) -> event_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a channel
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_status_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_status_req(Prop) when is_list(Prop) ->
    case channel_status_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CHANNEL_STATUS_REQ_HEADERS, ?OPTIONAL_CHANNEL_STATUS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for channel status req"}
    end;
channel_status_req(JObj) -> channel_status_req(wh_json:to_proplist(JObj)).

-spec channel_status_req_v(api_terms()) -> boolean().
channel_status_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_STATUS_REQ_HEADERS, ?CHANNEL_STATUS_REQ_VALUES, ?CHANNEL_STATUS_REQ_TYPES);
channel_status_req_v(JObj) -> channel_status_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Respond with status of a channel, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec channel_status_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
channel_status_resp(Prop) when is_list(Prop) ->
    case channel_status_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CHANNEL_STATUS_RESP_HEADERS, ?OPTIONAL_CHANNEL_STATUS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for channel status resp"}
    end;
channel_status_resp(JObj) -> channel_status_resp(wh_json:to_proplist(JObj)).

-spec channel_status_resp_v(api_terms()) -> boolean().
channel_status_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CHANNEL_STATUS_RESP_HEADERS, ?CHANNEL_STATUS_RESP_VALUES, ?CHANNEL_STATUS_RESP_TYPES);
channel_status_resp_v(JObj) -> channel_status_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_auth_id_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_auth_id_req(Prop) when is_list(Prop) ->
    case query_auth_id_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_AUTH_ID_REQ_HEADERS, ?OPTIONAL_QUERY_AUTH_ID_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for auth id query req"}
    end;
query_auth_id_req(JObj) -> query_auth_id_req(wh_json:to_proplist(JObj)).

-spec query_auth_id_req_v(api_terms()) -> boolean().
query_auth_id_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_AUTH_ID_REQ_HEADERS, ?QUERY_AUTH_ID_REQ_VALUES, ?QUERY_AUTH_ID_REQ_TYPES);
query_auth_id_req_v(JObj) -> query_auth_id_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_auth_id_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_auth_id_resp(Prop) when is_list(Prop) ->
    case query_auth_id_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_AUTH_ID_RESP_HEADERS, ?OPTIONAL_QUERY_AUTH_ID_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for auth id query resp"}
    end;
query_auth_id_resp(JObj) -> query_auth_id_resp(wh_json:to_proplist(JObj)).

-spec query_auth_id_resp_v(api_terms()) -> boolean().
query_auth_id_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_AUTH_ID_RESP_HEADERS, ?QUERY_AUTH_ID_RESP_VALUES, ?QUERY_AUTH_ID_RESP_TYPES);
query_auth_id_resp_v(JObj) -> query_auth_id_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_user_channels_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_user_channels_req(Prop) when is_list(Prop) ->
    case query_user_channels_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_USER_CHANNELS_REQ_HEADERS, ?OPTIONAL_QUERY_USER_CHANNELS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for users channels query req"}
    end;
query_user_channels_req(JObj) -> query_user_channels_req(wh_json:to_proplist(JObj)).

-spec query_user_channels_req_v(api_terms()) -> boolean().
query_user_channels_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_USER_CHANNELS_REQ_HEADERS, ?QUERY_USER_CHANNELS_REQ_VALUES, ?QUERY_USER_CHANNELS_REQ_TYPES);
query_user_channels_req_v(JObj) -> query_user_channels_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_user_channels_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_user_channels_resp(Prop) when is_list(Prop) ->
    case query_user_channels_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_USER_CHANNELS_RESP_HEADERS, ?OPTIONAL_QUERY_USER_CHANNELS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for users channels query resp"}
    end;
query_user_channels_resp(JObj) -> query_user_channels_resp(wh_json:to_proplist(JObj)).

-spec query_user_channels_resp_v(api_terms()) -> boolean().
query_user_channels_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_USER_CHANNELS_RESP_HEADERS, ?QUERY_USER_CHANNELS_RESP_VALUES, ?QUERY_USER_CHANNELS_RESP_TYPES);
query_user_channels_resp_v(JObj) -> query_user_channels_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_account_channels_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_account_channels_req(Prop) when is_list(Prop) ->
    case query_account_channels_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, ?OPTIONAL_QUERY_ACCOUNT_CHANNELS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for account channels query req"}
    end;
query_account_channels_req(JObj) -> query_account_channels_req(wh_json:to_proplist(JObj)).

-spec query_account_channels_req_v(api_terms()) -> boolean().
query_account_channels_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, ?QUERY_ACCOUNT_CHANNELS_REQ_VALUES, ?QUERY_ACCOUNT_CHANNELS_REQ_TYPES);
query_account_channels_req_v(JObj) -> query_account_channels_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_account_channels_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_account_channels_resp(Prop) when is_list(Prop) ->
    case query_account_channels_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, ?OPTIONAL_QUERY_ACCOUNT_CHANNELS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for accounts channels query resp"}
    end;
query_account_channels_resp(JObj) -> query_account_channels_resp(wh_json:to_proplist(JObj)).

-spec query_account_channels_resp_v(api_terms()) -> boolean().
query_account_channels_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, ?QUERY_ACCOUNT_CHANNELS_RESP_VALUES, ?QUERY_ACCOUNT_CHANNELS_RESP_TYPES);
query_account_channels_resp_v(JObj) -> query_account_channels_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_channels_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_channels_req(Prop) when is_list(Prop) ->
    case query_channels_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_CHANNELS_REQ_HEADERS, ?OPTIONAL_QUERY_CHANNELS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for channels query req"}
    end;
query_channels_req(JObj) -> query_channels_req(wh_json:to_proplist(JObj)).

-spec query_channels_req_v(api_terms()) -> boolean().
query_channels_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_CHANNELS_REQ_HEADERS, ?QUERY_CHANNELS_REQ_VALUES, ?QUERY_CHANNELS_REQ_TYPES);
query_channels_req_v(JObj) -> query_channels_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Inquire into the status of a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec query_channels_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
query_channels_resp(Prop) when is_list(Prop) ->
    case query_channels_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?QUERY_CHANNELS_RESP_HEADERS, ?OPTIONAL_QUERY_CHANNELS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for channels query resp"}
    end;
query_channels_resp(JObj) -> query_channels_resp(wh_json:to_proplist(JObj)).

-spec query_channels_resp_v(api_terms()) -> boolean().
query_channels_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?QUERY_CHANNELS_RESP_HEADERS, ?QUERY_CHANNELS_RESP_VALUES, ?QUERY_CHANNELS_RESP_TYPES);
query_channels_resp_v(JObj) -> query_channels_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec usurp_control(api_terms()) -> {'ok', iolist()} | {'error', string()}.
usurp_control(Prop) when is_list(Prop) ->
    case usurp_control_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CALL_USURP_CONTROL_HEADERS, ?OPTIONAL_CALL_USURP_CONTROL_HEADERS);
        'false' -> {'error', "Proplist failed validation for usurp_control"}
    end;
usurp_control(JObj) -> usurp_control(wh_json:to_proplist(JObj)).

-spec usurp_control_v(api_terms()) -> boolean().
usurp_control_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_USURP_CONTROL_HEADERS, ?CALL_USURP_CONTROL_VALUES, ?CALL_USURP_CONTROL_TYPES);
usurp_control_v(JObj) -> usurp_control_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call id update from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec usurp_publisher(api_terms()) -> {'ok', iolist()} | {'error', string()}.
usurp_publisher(Prop) when is_list(Prop) ->
    case usurp_publisher_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PUBLISHER_USURP_CONTROL_HEADERS, ?OPTIONAL_PUBLISHER_USURP_CONTROL_HEADERS);
        'false' -> {'error', "Proplist failed validation for usurp_publisher"}
    end;
usurp_publisher(JObj) -> usurp_publisher(wh_json:to_proplist(JObj)).

-spec usurp_publisher_v(api_terms()) -> boolean().
usurp_publisher_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PUBLISHER_USURP_CONTROL_HEADERS, ?PUBLISHER_USURP_CONTROL_VALUES, ?PUBLISHER_USURP_CONTROL_TYPES);
usurp_publisher_v(JObj) -> usurp_publisher_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, CallId).

bind_q(Q, [Event|T], CallId) ->
    _ = amqp_util:bind_q_to_callevt(Q, ?CALL_EVENT_ROUTING_KEY(Event, CallId)),
    bind_q(Q, T, CallId);
bind_q(_Q, [], _CallId) -> 'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, CallId).

unbind_q(Q, [Event|T], CallId) ->
    _ = amqp_util:unbind_q_from_callevt(Q, ?CALL_EVENT_ROUTING_KEY(Event, CallId)),
    unbind_q(Q, T, CallId);
unbind_q(_Q, [], _CallId) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callevt_exchange(),
    amqp_util:callmgr_exchange().

-spec publish_event(api_terms()) -> 'ok'.
-spec publish_event(api_terms(), ne_binary()) -> 'ok'.
publish_event(Event) -> publish_event(Event, ?DEFAULT_CONTENT_TYPE).
publish_event(Event, ContentType) when is_list(Event) ->
    CallId = props:get_first_defined([<<"Call-ID">>, <<"Unique-ID">>], Event),
    EventName = props:get_value(<<"Event-Name">>, Event),
    {'ok', Payload} = wh_api:prepare_api_payload(Event, ?CALL_EVENT_VALUES, fun ?MODULE:event/1),
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY(EventName, CallId), Payload, ContentType);
publish_event(Event, ContentType) ->
    publish_event(wh_json:to_proplist(Event), ContentType).

-spec publish_channel_status_req(api_terms()) -> 'ok'.
-spec publish_channel_status_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_status_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_status_req(API) ->
    case is_list(API) of
        'true' -> publish_channel_status_req(props:get_value(<<"Call-ID">>, API), API);
        'false' -> publish_channel_status_req(wh_json:get_value(<<"Call-ID">>, API), API)
    end.
publish_channel_status_req(CallId, JObj) ->
    publish_channel_status_req(CallId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_status_req(CallId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?CHANNEL_STATUS_REQ_VALUES, fun ?MODULE:channel_status_req/1),
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', CallId), Payload, ContentType).

-spec publish_channel_status_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_status_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_status_resp(RespQ, JObj) ->
    publish_channel_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_status_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?CHANNEL_STATUS_RESP_VALUES, fun ?MODULE:channel_status_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_query_auth_id_req(api_terms()) -> 'ok'.
-spec publish_query_auth_id_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_auth_id_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_auth_id_req(API) ->
    case is_list(API) of
        'true' -> publish_query_auth_id_req(props:get_value(<<"Auth-ID">>, API), API);
        'false' -> publish_query_auth_id_req(wh_json:get_value(<<"Auth-ID">>, API), API)
    end.
publish_query_auth_id_req(AuthId, JObj) ->
    publish_query_auth_id_req(AuthId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_auth_id_req(AuthId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?QUERY_AUTH_ID_REQ_VALUES, fun ?MODULE:query_auth_id_req/1),
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', AuthId), Payload, ContentType).

-spec publish_query_auth_id_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_auth_id_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_auth_id_resp(RespQ, JObj) ->
    publish_query_auth_id_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_auth_id_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?QUERY_AUTH_ID_RESP_VALUES, fun ?MODULE:query_auth_id_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

publish_query_user_channels_req(Props) when is_list(Props) ->
    publish_query_user_channels_req(Props
                                    ,props:get_value(<<"Username">>, Props)
                                    ,props:get_value(<<"Realm">>, Props)
                                    ,?DEFAULT_CONTENT_TYPE
                                   );
publish_query_user_channels_req(JObj) ->
    publish_query_user_channels_req(JObj
                                    ,wh_json:get_value(<<"Username">>, JObj)
                                    ,wh_json:get_value(<<"Realm">>, JObj)
                                    ,?DEFAULT_CONTENT_TYPE
                                   ).

publish_query_user_channels_req(Req, 'undefined', 'undefined', ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?QUERY_USER_CHANNELS_REQ_VALUES, fun ?MODULE:query_user_channels_req/1),
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', <<>>), Payload, ContentType);
publish_query_user_channels_req(Req, 'undefined', Realm, ContentType) ->
    Username = first_username(Req),
    publish_query_user_channels_req(Req, Username, Realm, ContentType);
publish_query_user_channels_req(Req, Username, Realm, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?QUERY_USER_CHANNELS_REQ_VALUES, fun ?MODULE:query_user_channels_req/1),
    User = <<Username/binary, ":", Realm/binary>>,
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', User), Payload, ContentType).

-spec publish_query_user_channels_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_user_channels_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_user_channels_resp(RespQ, JObj) ->
    publish_query_user_channels_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_user_channels_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?QUERY_USER_CHANNELS_RESP_VALUES, fun ?MODULE:query_user_channels_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_query_account_channels_req(api_terms()) -> 'ok'.
-spec publish_query_account_channels_req(api_terms(), ne_binary(), ne_binary()) -> 'ok'.
publish_query_account_channels_req(Props) when is_list(Props) ->
    publish_query_account_channels_req(Props
                                       ,props:get_value(<<"Account-ID">>, Props)
                                       ,?DEFAULT_CONTENT_TYPE
                                      );
publish_query_account_channels_req(JObj) ->
    publish_query_account_channels_req(JObj
                                       ,wh_json:get_value(<<"Account-ID">>, JObj)
                                       ,?DEFAULT_CONTENT_TYPE
                                      ).

publish_query_account_channels_req(Req, AccountId, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?QUERY_ACCOUNT_CHANNELS_REQ_VALUES, fun ?MODULE:query_account_channels_req/1),
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', AccountId), Payload, ContentType).

-spec publish_query_account_channels_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_account_channels_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_account_channels_resp(RespQ, JObj) ->
    publish_query_account_channels_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_account_channels_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?QUERY_ACCOUNT_CHANNELS_RESP_VALUES, fun ?MODULE:query_account_channels_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_query_channels_req(api_terms()) -> 'ok'.
-spec publish_query_channels_req(api_terms(), ne_binary()) -> 'ok'.
publish_query_channels_req(ApiProps) -> publish_query_channels_req(ApiProps, ?DEFAULT_CONTENT_TYPE).
publish_query_channels_req(ApiProps, ContentType) when is_list(ApiProps) ->
    {'ok', Payload} = wh_api:prepare_api_payload(ApiProps, ?QUERY_CHANNELS_REQ_VALUES, fun ?MODULE:query_channels_req/1),
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('status_req', <<"channels">>), Payload, ContentType);
publish_query_channels_req(JObj, ContentType) ->
    publish_query_channels_req(wh_json:to_proplist(JObj), ContentType).

-spec publish_query_channels_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_channels_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_channels_resp(RespQ, ApiProps) -> publish_query_channels_resp(RespQ, ApiProps, ?DEFAULT_CONTENT_TYPE).
publish_query_channels_resp(RespQ, ApiProps, ContentType) when is_list(ApiProps) ->
    {'ok', Payload} = wh_api:prepare_api_payload(ApiProps, ?QUERY_CHANNELS_RESP_VALUES, fun ?MODULE:query_channels_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType);
publish_query_channels_resp(RespQ, JObj, ContentType) ->
    publish_query_channels_resp(RespQ, wh_json:to_proplist(JObj), ContentType).

-spec publish_usurp_control(ne_binary(), api_terms()) -> 'ok'.
-spec publish_usurp_control(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_usurp_control(CallId, JObj) ->
    publish_usurp_control(CallId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_usurp_control(CallId, JObj, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(JObj, ?CALL_USURP_CONTROL_VALUES, fun ?MODULE:usurp_control/1),
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('usurp_control', CallId), Payload, ContentType).

-spec publish_usurp_publisher(ne_binary(), api_terms()) -> 'ok'.
-spec publish_usurp_publisher(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_usurp_publisher(CallId, JObj) ->
    publish_usurp_publisher(CallId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_usurp_publisher(CallId, JObj, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(JObj, ?PUBLISHER_USURP_CONTROL_VALUES, fun ?MODULE:usurp_publisher/1),
    amqp_util:callevt_publish(?CALL_EVENT_ROUTING_KEY('publisher_usurp', CallId), Payload, ContentType).

-spec get_status(api_terms()) -> ne_binary().
get_status(API) when is_list(API) -> props:get_value(<<"Status">>, API);
get_status(API) -> wh_json:get_value(<<"Status">>, API).

-spec first_username(api_terms()) -> ne_binary().
first_username(Props) when is_list(Props) ->
    [U|_] = props:get_value(<<"Usernames">>, Props),
    U;
first_username(JObj) ->
    [U|_] = wh_json:get_value(<<"Usernames">>, JObj),
    U.

-spec event_routing_key(ne_binary(), ne_binary()) -> ne_binary().
event_routing_key(EventName, CallId) ->
    ?CALL_EVENT_ROUTING_KEY(EventName, CallId).
