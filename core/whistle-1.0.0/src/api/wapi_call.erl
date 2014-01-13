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

-export([new_channel/1, new_channel_v/1]).
-export([destroy_channel/1, destroy_channel_v/1]).
-export([answered_channel/1, answered_channel_v/1]).
-export([event/1, event_v/1]).

-export([channel_status_req/1, channel_status_req_v/1]).
-export([channel_status_resp/1, channel_status_resp_v/1]).

-export([call_status_req/1, call_status_req_v/1]).
-export([call_status_resp/1, call_status_resp_v/1]).

-export([query_auth_id_req/1, query_auth_id_req_v/1]).
-export([query_auth_id_resp/1, query_auth_id_resp_v/1]).

-export([query_user_channels_req/1, query_user_channels_req_v/1]).
-export([query_user_channels_resp/1, query_user_channels_resp_v/1]).

-export([query_account_channels_req/1, query_account_channels_req_v/1]).
-export([query_account_channels_resp/1, query_account_channels_resp_v/1]).

-export([cdr/1, cdr_v/1]).
-export([usurp_control/1, usurp_control_v/1]).
-export([usurp_publisher/1, usurp_publisher_v/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_new_channel/1, publish_new_channel/2]).
-export([publish_destroy_channel/1, publish_destroy_channel/2]).
-export([publish_answered_channel/1, publish_answered_channel/2]).
-export([publish_event/2, publish_event/3]).

-export([publish_channel_status_req/1 ,publish_channel_status_req/2, publish_channel_status_req/3]).
-export([publish_channel_status_resp/2, publish_channel_status_resp/3]).

-export([publish_call_status_req/1 ,publish_call_status_req/2, publish_call_status_req/3]).
-export([publish_call_status_resp/2, publish_call_status_resp/3]).

-export([publish_query_auth_id_req/1 ,publish_query_auth_id_req/2, publish_query_auth_id_req/3]).
-export([publish_query_auth_id_resp/2, publish_query_auth_id_resp/3]).

-export([publish_query_user_channels_req/1 ,publish_query_user_channels_req/4]).
-export([publish_query_user_channels_resp/2 ,publish_query_user_channels_resp/3]).

-export([publish_query_account_channels_req/1 ,publish_query_account_channels_req/3]).
-export([publish_query_account_channels_resp/2 ,publish_query_account_channels_resp/3]).

-export([publish_cdr/2, publish_cdr/3]).
-export([publish_usurp_control/2, publish_usurp_control/3]).
-export([publish_usurp_publisher/2, publish_usurp_publisher/3]).

-export([get_status/1]).

-include_lib("whistle/include/wh_api.hrl").

%% Routing key prefix for rating
-define(KEY_RATING_REQ, <<"call.rating">>).

-define(NEW_CHANNEL_ROUTING_KEY, <<"call.new_channel">>).
-define(NEW_CHANNEL_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_NEW_CHANNEL_HEADERS, [<<"To">>, <<"From">>, <<"Request">>
                                       | ?OPTIONAL_CALL_EVENT_HEADERS
                                      ]).
-define(NEW_CHANNEL_VALUES, [{<<"Event-Category">>, <<"channel">>}
                             ,{<<"Event-Name">>, <<"new">>}
                            ]).
-define(NEW_CHANNEL_TYPES, []).

-define(DESTROY_CHANNEL_ROUTING_KEY(CALLID), <<"call.destroy_channel.", (amqp_util:encode(CALLID))/binary>>).
-define(DESTROY_CHANNEL_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_DESTROY_CHANNEL_HEADERS, [<<"To">>, <<"From">>, <<"Request">>
                                           | ?OPTIONAL_CALL_EVENT_HEADERS
                                          ]).
-define(DESTROY_CHANNEL_VALUES, [{<<"Event-Category">>, <<"channel">>}
                                 ,{<<"Event-Name">>, <<"destroy">>}
                                ]).
-define(DESTROY_CHANNEL_TYPES, []).

-define(ANSWERED_CHANNEL_ROUTING_KEY(CALLID), <<"call.answered_channel.", (amqp_util:encode(CALLID))/binary>>).
-define(ANSWERED_CHANNEL_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_ANSWERED_CHANNEL_HEADERS, [<<"To">>, <<"From">>, <<"Request">>
                                                | ?OPTIONAL_CALL_EVENT_HEADERS
                                           ]).
-define(ANSWERED_CHANNEL_VALUES, [{<<"Event-Category">>, <<"channel">>}
                                  ,{<<"Event-Name">>, <<"answered">>}
                                 ]).
-define(ANSWERED_CHANNEL_TYPES, []).

%% Call Events
-define(CALL_EVENT_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_EVENT_HEADERS, [<<"Application-Name">>, <<"Application-Response">>
                                      ,<<"Custom-Channel-Vars">>, <<"Timestamp">>, <<"Channel-State">>
                                      ,<<"Call-Direction">>, <<"Transfer-History">>
                                      ,<<"Other-Leg-Direction">>, <<"Other-Leg-Caller-ID-Name">>
                                      ,<<"Other-Leg-Caller-ID-Number">>, <<"Other-Leg-Destination-Number">>
                                      ,<<"Other-Leg-Unique-ID">> %% BRIDGE
                                      ,<<"Detected-Tone">>, <<"DTMF-Duration">>, <<"DTMF-Digit">> %% DTMF and Tones
                                      ,<<"Terminator">>, <<"Disposition">>
                                      ,<<"Hangup-Cause">>, <<"Hangup-Code">> %% Hangup
                                      ,<<"Raw-Application-Name">>, <<"Raw-Application-Data">>
                                      ,<<"Length">>, <<"Silence-Terminated">> %% Record-related
                                      ,<<"Channel-Call-State">>
                                      ,<<"Fax-Success">>, <<"Fax-Result-Code">>
                                      ,<<"Fax-Result-Text">>, <<"Fax-ECM-Used">>
                                      ,<<"Fax-Transferred-Pages">>, <<"Fax-Total-Pages">>
                                      ,<<"Fax-Bad-Rows">>, <<"Fax-Transfer-Rate">>
                                      ,<<"Switch-Hostname">>, <<"Group-ID">>
                                      ,<<"Control-Queue">>, <<"Channel-Moving">>
                                      ,<<"Conference-Name">>, <<"Conference-Config">>
                                      ,<<"Replaced-By">>
                                     ]).
-define(CALL_EVENT_VALUES, [{<<"Event-Category">>, <<"call_event">>}]).
-define(CALL_EVENT_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%% Channel Status Request
-define(CHANNEL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CHANNEL_STATUS_REQ_HEADERS, []).
-define(CHANNEL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"channel_status_req">>}
                                   ]).
-define(CHANNEL_STATUS_REQ_TYPES, []).

%% Channel Status Response
-define(CHANNEL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CHANNEL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>
                                                   ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                                   ,<<"Switch-URL">>
                                              ]).
-define(CHANNEL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                     ,{<<"Event-Name">>, <<"channel_status_resp">>}
                                     ,{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                                    ]).
-define(CHANNEL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%% Call Status Request
-define(CALL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_STATUS_REQ_HEADERS, []).
-define(CALL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                 ,{<<"Event-Name">>, <<"call_status_req">>}
                                ]).
-define(CALL_STATUS_REQ_TYPES, []).

%% Call Status Response
-define(CALL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CALL_STATUS_RESP_HEADERS, [<<"Custom-Channel-Vars">>, <<"Error-Msg">>, <<"Switch-Hostname">>
                                                ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                                ,<<"Destination-Number">>, <<"Other-Leg-Unique-ID">>
                                                ,<<"Other-Leg-Caller-ID-Name">>, <<"Other-Leg-Caller-ID-Number">>
                                                ,<<"Other-Leg-Destination-Number">>, <<"Presence-ID">>
                                           ]).
-define(CALL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                  ,{<<"Event-Name">>, <<"call_status_resp">>}
                                  ,{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                                 ]).
-define(CALL_STATUS_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

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
-define(QUERY_USER_CHANNELS_REQ_HEADERS, [<<"Realm">>]).
-define(OPTIONAL_QUERY_USER_CHANNELS_REQ_HEADERS, [<<"Usernames">>, <<"Username">>]).
-define(QUERY_USER_CHANNELS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                         ,{<<"Event-Name">>, <<"query_user_channels_req">>}
                                        ]).
-define(QUERY_USER_CHANNELS_REQ_TYPES, [{<<"Usernames">>, fun erlang:is_list/1}
                                        ,{<<"Username">>, fun erlang:is_binary/1}
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
-define(OPTIONAL_QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, []).
-define(QUERY_ACCOUNT_CHANNELS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                         ,{<<"Event-Name">>, <<"query_account_channels_req">>}
                                        ]).
-define(QUERY_ACCOUNT_CHANNELS_REQ_TYPES, [{<<"Usernames">>, fun erlang:is_list/1}
                                        ,{<<"Username">>, fun erlang:is_binary/1}
                                       ]).

%% Query Account Channels Resp
-define(QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, [<<"Channels">>]).
-define(QUERY_ACCOUNT_CHANNELS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                          ,{<<"Event-Name">>, <<"query_account_channels_resp">>}
                                         ]).
-define(QUERY_ACCOUNT_CHANNELS_RESP_TYPES, []).


%% Call CDR
-define(CALL_CDR_HEADERS, [ <<"Call-ID">>]).
-define(OPTIONAL_CALL_CDR_HEADERS, [<<"Hangup-Cause">>, <<"Handling-Server-Name">>, <<"Custom-Channel-Vars">>
                                        ,<<"Remote-SDP">>, <<"Local-SDP">>, <<"Caller-ID-Name">>
                                        ,<<"Caller-ID-Number">>, <<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                        ,<<"User-Agent">>, <<"Caller-ID-Type">>, <<"Other-Leg-Call-ID">>
                                        ,<<"Timestamp">>, <<"Duration-Seconds">>, <<"Billing-Seconds">>, <<"Ringing-Seconds">>
                                        ,<<"Call-Direction">>, <<"To-Uri">>, <<"From-Uri">>
                                        ,<<"Digits-Dialed">>, <<"To">>, <<"From">>, <<"Request">>
                                   ]).
-define(CALL_CDR_VALUES, [{<<"Event-Category">>, <<"call_detail">>}
                          ,{<<"Event-Name">>, <<"cdr">>}
                          ,{<<"Call-Direction">>, [<<"inbound">>, <<"outbound">>]}
                          ,{<<"Caller-ID-Type">>, [<<"pid">>, <<"rpid">>, <<"from">>]}
                         ]).
-define(CALL_CDR_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

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
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec new_channel(api_terms()) -> {'ok', iolist()} | {'error', string()}.
new_channel(Prop) when is_list(Prop) ->
    case new_channel_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?NEW_CHANNEL_HEADERS, ?OPTIONAL_NEW_CHANNEL_HEADERS);
        'false' -> {'error', "Proplist failed validation for new_channel"}
    end;
new_channel(JObj) -> new_channel(wh_json:to_proplist(JObj)).

-spec new_channel_v(api_terms()) -> boolean().
new_channel_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?NEW_CHANNEL_HEADERS, ?NEW_CHANNEL_VALUES, ?NEW_CHANNEL_TYPES);
new_channel_v(JObj) -> new_channel_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec destroy_channel(api_terms()) -> {'ok', iolist()} | {'error', string()}.
destroy_channel(Prop) when is_list(Prop) ->
    case destroy_channel_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?DESTROY_CHANNEL_HEADERS, ?OPTIONAL_DESTROY_CHANNEL_HEADERS);
        'false' -> {'error', "Proplist failed validation for destroy_channel"}
    end;
destroy_channel(JObj) -> destroy_channel(wh_json:to_proplist(JObj)).

-spec destroy_channel_v(api_terms()) -> boolean().
destroy_channel_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DESTROY_CHANNEL_HEADERS, ?DESTROY_CHANNEL_VALUES, ?DESTROY_CHANNEL_TYPES);
destroy_channel_v(JObj) -> destroy_channel_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec answered_channel(api_terms()) -> {'ok', iolist()} | {'error', string()}.
answered_channel(Prop) when is_list(Prop) ->
    case answered_channel_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?ANSWERED_CHANNEL_HEADERS, ?OPTIONAL_ANSWERED_CHANNEL_HEADERS);
        'false' -> {'error', "Proplist failed validation for answered_channel"}
    end;
answered_channel(JObj) -> answered_channel(wh_json:to_proplist(JObj)).

-spec answered_channel_v(api_terms()) -> boolean().
answered_channel_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ANSWERED_CHANNEL_HEADERS, ?ANSWERED_CHANNEL_VALUES, ?ANSWERED_CHANNEL_TYPES);
answered_channel_v(JObj) -> answered_channel_v(wh_json:to_proplist(JObj)).

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
-spec call_status_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
call_status_req(Prop) when is_list(Prop) ->
    case call_status_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CALL_STATUS_REQ_HEADERS, ?OPTIONAL_CALL_STATUS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for call status req"}
    end;
call_status_req(JObj) -> call_status_req(wh_json:to_proplist(JObj)).

-spec call_status_req_v(api_terms()) -> boolean().
call_status_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_STATUS_REQ_HEADERS, ?CALL_STATUS_REQ_VALUES, ?CALL_STATUS_REQ_TYPES);
call_status_req_v(JObj) -> call_status_req_v(wh_json:to_proplist(JObj)).

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
%% @doc Respond with status of a call, either active or non-existant
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec call_status_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
call_status_resp(Prop) when is_list(Prop) ->
    case call_status_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CALL_STATUS_RESP_HEADERS, ?OPTIONAL_CALL_STATUS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for call status resp"}
    end;
call_status_resp(JObj) -> call_status_resp(wh_json:to_proplist(JObj)).

-spec call_status_resp_v(api_terms()) -> boolean().
call_status_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_STATUS_RESP_HEADERS, ?CALL_STATUS_RESP_VALUES, ?CALL_STATUS_RESP_TYPES);
call_status_resp_v(JObj) -> call_status_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Format a CDR for a call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec cdr(api_terms()) -> {'ok', iolist()} | {'error', string()}.
cdr(Prop) when is_list(Prop) ->
    case cdr_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CALL_CDR_HEADERS, ?OPTIONAL_CALL_CDR_HEADERS);
        'false' -> {'error', "Proplist failed validation for call_cdr"}
    end;
cdr(JObj) -> cdr(wh_json:to_proplist(JObj)).

-spec cdr_v(api_terms()) -> boolean().
cdr_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CALL_CDR_HEADERS, ?CALL_CDR_VALUES, ?CALL_CDR_TYPES);
cdr_v(JObj) -> cdr_v(wh_json:to_proplist(JObj)).

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
    bind_q(Queue, props:get_value('restrict_to', Props), CallId).

bind_q(Q, 'undefined', CallId) ->
    'ok' = amqp_util:bind_q_to_callevt(Q, CallId),
    'ok' = amqp_util:bind_q_to_callevt(Q, CallId, 'cdr'),
    'ok' = amqp_util:bind_q_to_callevt(Q, CallId, 'publisher_usurp');

bind_q(Q, ['events'|T], CallId) ->
    _ = amqp_util:bind_q_to_callevt(Q, CallId),
    bind_q(Q, T, CallId);
bind_q(Q, ['cdr'|T], CallId) ->
    _ = amqp_util:bind_q_to_callevt(Q, CallId, 'cdr'),
    bind_q(Q, T, CallId);
bind_q(Q, ['status_req'|T], CallId) ->
    'ok' = amqp_util:bind_q_to_callevt(Q, CallId, 'status_req'),
    bind_q(Q, T, CallId);
bind_q(Q, ['new_channel'|T], CallId) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?NEW_CHANNEL_ROUTING_KEY),
    bind_q(Q, T, CallId);
bind_q(Q, ['destroy_channel'|T], CallId) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?DESTROY_CHANNEL_ROUTING_KEY(CallId)),
    bind_q(Q, T, CallId);
bind_q(Q, ['answered_channel'|T], CallId) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?ANSWERED_CHANNEL_ROUTING_KEY(CallId)),
    bind_q(Q, T, CallId);
bind_q(Q, ['publisher_usurp'|T], CallId) ->
    'ok' = amqp_util:bind_q_to_callevt(Q, CallId, 'publisher_usurp'),
    bind_q(Q, T, CallId);
bind_q(Q, [_|T], CallId) -> bind_q(Q, T, CallId);
bind_q(_Q, [], _CallId) -> 'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    CallId = props:get_value('callid', Props, <<"*">>),
    unbind_q(Queue, props:get_value('restrict_to', Props), CallId).

unbind_q(Q, 'undefined', CallId) ->
    'ok' = amqp_util:unbind_q_from_callevt(Q, CallId),
    'ok' = amqp_util:unbind_q_from_callevt(Q, CallId, 'cdr'),
    'ok' = amqp_util:unbind_q_from_callevt(Q, CallId, 'publisher_usurp');

unbind_q(Q, ['events'|T], CallId) ->
    'ok' = amqp_util:unbind_q_from_callevt(Q, CallId),
    unbind_q(Q, T, CallId);
unbind_q(Q, ['cdr'|T], CallId) ->
    'ok' = amqp_util:unbind_q_from_callevt(Q, CallId, 'cdr'),
    unbind_q(Q, T, CallId);
unbind_q(Q, ['status_req'|T], CallId) ->
    'ok' = amqp_util:unbind_q_from_callevt(Q, CallId, 'status_req'),
    unbind_q(Q, T, CallId);
unbind_q(Q, ['new_channel'|T], CallId) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?NEW_CHANNEL_ROUTING_KEY),
    unbind_q(Q, T, CallId);
unbind_q(Q, ['destroy_channel'|T], CallId) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?DESTROY_CHANNEL_ROUTING_KEY(CallId)),
    unbind_q(Q, T, CallId);
unbind_q(Q, ['answered_channel'|T], CallId) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?ANSWERED_CHANNEL_ROUTING_KEY(CallId)),
    unbind_q(Q, T, CallId);
unbind_q(Q, ['publisher_usurp'|T], CallId) ->
    'ok' = amqp_util:unbind_q_from_callevt(Q, CallId, 'publisher_usurp'),
    unbind_q(Q, T, CallId);
unbind_q(Q, [_|T], CallId) -> unbind_q(Q, T, CallId);
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

-spec publish_event(ne_binary(), api_terms()) -> 'ok'.
-spec publish_event(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_event(CallId, JObj) -> publish_event(CallId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_event(CallId, Event, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Event, ?CALL_EVENT_VALUES, fun ?MODULE:event/1),
    amqp_util:callevt_publish(CallId, Payload, 'event', ContentType).

-spec publish_new_channel(api_terms()) -> 'ok'.
-spec publish_new_channel(api_terms(), ne_binary()) -> 'ok'.
publish_new_channel(JObj) -> publish_new_channel(JObj, ?DEFAULT_CONTENT_TYPE).
publish_new_channel(Event, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Event, ?NEW_CHANNEL_VALUES, fun ?MODULE:new_channel/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?NEW_CHANNEL_ROUTING_KEY).

-spec publish_destroy_channel(api_terms()) -> 'ok'.
-spec publish_destroy_channel(api_terms(), ne_binary()) -> 'ok'.
publish_destroy_channel(JObj) -> publish_destroy_channel(JObj, ?DEFAULT_CONTENT_TYPE).
publish_destroy_channel(Event, ContentType) ->
    CallId = callid(Event),
    {'ok', Payload} = wh_api:prepare_api_payload(Event, ?DESTROY_CHANNEL_VALUES, fun ?MODULE:destroy_channel/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?DESTROY_CHANNEL_ROUTING_KEY(CallId)).

-spec publish_answered_channel(api_terms()) -> 'ok'.
-spec publish_answered_channel(api_terms(), ne_binary()) -> 'ok'.
publish_answered_channel(JObj) -> publish_answered_channel(JObj, ?DEFAULT_CONTENT_TYPE).
publish_answered_channel(Event, ContentType) ->
    CallId = callid(Event),
    {'ok', Payload} = wh_api:prepare_api_payload(Event, ?ANSWERED_CHANNEL_VALUES, fun ?MODULE:answered_channel/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?ANSWERED_CHANNEL_ROUTING_KEY(CallId)).

-spec callid(api_terms()) -> api_binary().
callid(Props) when is_list(Props) ->
    case props:get_value(<<"Call-ID">>, Props) of
        'undefined' -> props:get_value(<<"Unique-ID">>, Props);
        CallId -> CallId
    end;
callid(JObj) ->
    case wh_json:get_value(<<"Call-ID">>, JObj) of
        'undefined' -> wh_json:get_value(<<"Unique-ID">>, JObj);
        CallId -> CallId
    end.

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
    amqp_util:callevt_publish(CallId, Payload, 'status_req', ContentType).

-spec publish_channel_status_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_channel_status_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_channel_status_resp(RespQ, JObj) ->
    publish_channel_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_channel_status_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?CHANNEL_STATUS_RESP_VALUES, fun ?MODULE:channel_status_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_call_status_req(api_terms()) -> 'ok'.
-spec publish_call_status_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_call_status_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_call_status_req(API) ->
    case is_list(API) of
        'true' -> publish_call_status_req(props:get_value(<<"Call-ID">>, API), API);
        'false' -> publish_call_status_req(wh_json:get_value(<<"Call-ID">>, API), API)
    end.
publish_call_status_req(CallId, JObj) ->
    publish_call_status_req(CallId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_call_status_req(CallId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?CALL_STATUS_REQ_VALUES, fun ?MODULE:call_status_req/1),
    amqp_util:callevt_publish(CallId, Payload, 'status_req', ContentType).

-spec publish_call_status_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_call_status_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_call_status_resp(RespQ, JObj) ->
    publish_call_status_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_call_status_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?CALL_STATUS_RESP_VALUES, fun ?MODULE:call_status_resp/1),
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
    amqp_util:callevt_publish(AuthId, Payload, 'status_req', ContentType).

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

publish_query_user_channels_req(Req, 'undefined', Realm, ContentType) ->
    Username = first_username(Req),
    publish_query_user_channels_req(Req, Username, Realm, ContentType);
publish_query_user_channels_req(Req, Username, Realm, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?QUERY_USER_CHANNELS_REQ_VALUES, fun ?MODULE:query_user_channels_req/1),
    amqp_util:callevt_publish(<<Username/binary, ":", Realm/binary>>, Payload, 'status_req', ContentType).

-spec publish_query_user_channels_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_user_channels_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_user_channels_resp(RespQ, JObj) ->
    publish_query_user_channels_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_user_channels_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?QUERY_USER_CHANNELS_RESP_VALUES, fun ?MODULE:query_user_channels_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

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
    amqp_util:callevt_publish(<<AccountId/binary>>, Payload, 'status_req', ContentType).

-spec publish_query_account_channels_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_query_account_channels_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_query_account_channels_resp(RespQ, JObj) ->
    publish_query_account_channels_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_query_account_channels_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?QUERY_ACCOUNT_CHANNELS_RESP_VALUES, fun ?MODULE:query_account_channels_resp/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_cdr(ne_binary(), api_terms()) -> 'ok'.
-spec publish_cdr(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_cdr(CallId, JObj) ->
    publish_cdr(CallId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_cdr(CallId, CDR, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(CDR, ?CALL_CDR_VALUES, fun ?MODULE:cdr/1),
    amqp_util:callevt_publish(CallId, Payload, 'cdr', ContentType).

-spec publish_usurp_control(ne_binary(), api_terms()) -> 'ok'.
-spec publish_usurp_control(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_usurp_control(CallId, JObj) ->
    publish_usurp_control(CallId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_usurp_control(CallId, JObj, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(JObj, ?CALL_USURP_CONTROL_VALUES, fun ?MODULE:usurp_control/1),
    amqp_util:callevt_publish(CallId, Payload, 'event', ContentType).

-spec publish_usurp_publisher(ne_binary(), api_terms()) -> 'ok'.
-spec publish_usurp_publisher(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_usurp_publisher(CallId, JObj) ->
    publish_usurp_publisher(CallId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_usurp_publisher(CallId, JObj, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(JObj, ?PUBLISHER_USURP_CONTROL_VALUES, fun ?MODULE:usurp_publisher/1),
    amqp_util:callevt_publish(CallId, Payload, 'publisher_usurp', ContentType).

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
