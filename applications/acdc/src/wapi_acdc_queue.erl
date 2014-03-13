%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_acdc_queue).

%% Convert JObj or Prop to iolist json
-export([member_call/1, member_call_v/1
         ,member_call_failure/1, member_call_failure_v/1
         ,member_call_success/1, member_call_success_v/1
         ,member_call_cancel/1, member_call_cancel_v/1
         ,member_connect_req/1, member_connect_req_v/1
         ,member_connect_resp/1, member_connect_resp_v/1
         ,member_connect_win/1, member_connect_win_v/1
         ,agent_timeout/1, agent_timeout_v/1
         ,member_connect_retry/1, member_connect_retry_v/1
         ,member_connect_accepted/1, member_connect_accepted_v/1
         ,member_hungup/1, member_hungup_v/1
         ,sync_req/1, sync_req_v/1
         ,sync_resp/1, sync_resp_v/1
         ,agent_change/1, agent_change_v/1
        ]).

-export([agent_change_available/0
         ,agent_change_ringing/0
         ,agent_change_unavailable/0
        ]).

-export([bind_q/2
         ,unbind_q/2
        ]).
-export([declare_exchanges/0]).

-export([publish_member_call/1, publish_member_call/2
         ,publish_shared_member_call/1, publish_shared_member_call/3, publish_shared_member_call/4
         ,publish_member_call_failure/2, publish_member_call_failure/3
         ,publish_member_call_success/2, publish_member_call_success/3
         ,publish_member_call_cancel/1, publish_member_call_cancel/2
         ,publish_member_connect_req/1, publish_member_connect_req/2
         ,publish_member_connect_resp/2, publish_member_connect_resp/3
         ,publish_member_connect_win/2, publish_member_connect_win/3
         ,publish_agent_timeout/2, publish_agent_timeout/3
         ,publish_member_connect_retry/2, publish_member_connect_retry/3
         ,publish_member_connect_accepted/2, publish_member_connect_accepted/3
         ,publish_member_hungup/2, publish_member_hungup/3
         ,publish_sync_req/1, publish_sync_req/2
         ,publish_sync_resp/2, publish_sync_resp/3
         ,publish_agent_change/1, publish_agent_change/2
        ]).

-export([queue_size/2, shared_queue_name/2]).

-include("acdc.hrl").

%%------------------------------------------------------------------------------
%% Member Call
%%------------------------------------------------------------------------------
-define(MEMBER_CALL_HEADERS, [<<"Account-ID">>, <<"Queue-ID">>, <<"Call">>]).
-define(OPTIONAL_MEMBER_CALL_HEADERS, []).
-define(MEMBER_CALL_VALUES, [{<<"Event-Category">>, <<"member">>}
                             ,{<<"Event-Name">>, <<"call">>}
                            ]).
-define(MEMBER_CALL_TYPES, [{<<"Queue-ID">>, fun erlang:is_binary/1}]).

-spec member_call(api_terms()) ->
                         {'ok', iolist()} |
                         {'error', string()}.
member_call(Props) when is_list(Props) ->
    case member_call_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CALL_HEADERS, ?OPTIONAL_MEMBER_CALL_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_call"}
    end;
member_call(JObj) ->
    member_call(wh_json:to_proplist(JObj)).

-spec member_call_v(api_terms()) -> boolean().
member_call_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CALL_HEADERS, ?MEMBER_CALL_VALUES, ?MEMBER_CALL_TYPES);
member_call_v(JObj) ->
    member_call_v(wh_json:to_proplist(JObj)).

-spec member_call_routing_key(api_terms()) -> ne_binary().
-spec member_call_routing_key(ne_binary(), ne_binary()) -> ne_binary().
member_call_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Queue-ID">>, Props, <<"*">>),
    AcctId = props:get_value(<<"Account-ID">>, Props),
    member_call_routing_key(AcctId, Id);
member_call_routing_key(JObj) ->
    Id = wh_json:get_value(<<"Queue-ID">>, JObj, <<"*">>),
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    member_call_routing_key(AcctId, Id).

member_call_routing_key(AcctId, QueueId) ->
    <<"acdc.member.call.", AcctId/binary, ".", QueueId/binary>>.

%%------------------------------------------------------------------------------
%% Member Call Fail - if the queue is unable to properly handle the call
%%  (queue is full, empty, wait timeout expires, etc)
%%------------------------------------------------------------------------------
-define(MEMBER_CALL_FAIL_HEADERS, [<<"Account-ID">>, <<"Queue-ID">>, <<"Call-ID">>]).
-define(OPTIONAL_MEMBER_CALL_FAIL_HEADERS, [<<"Failure-Reason">>, <<"Process-ID">>, <<"Agent-ID">>]).
-define(MEMBER_CALL_FAIL_VALUES, [{<<"Event-Category">>, <<"member">>}
                                  ,{<<"Event-Name">>, <<"call_fail">>}
                                 ]).
-define(MEMBER_CALL_FAIL_TYPES, []).

-spec member_call_failure(api_terms()) ->
                                 {'ok', iolist()} |
                                 {'error', string()}.
member_call_failure(Props) when is_list(Props) ->
    case member_call_failure_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CALL_FAIL_HEADERS, ?OPTIONAL_MEMBER_CALL_FAIL_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_call_fail"}
    end;
member_call_failure(JObj) ->
    member_call_failure(wh_json:to_proplist(JObj)).

-spec member_call_failure_v(api_terms()) -> boolean().
member_call_failure_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CALL_FAIL_HEADERS, ?MEMBER_CALL_FAIL_VALUES, ?MEMBER_CALL_FAIL_TYPES);
member_call_failure_v(JObj) ->
    member_call_failure_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Member Call Success - if an agent is handling the call
%%------------------------------------------------------------------------------
-define(MEMBER_CALL_SUCCESS_HEADERS, [<<"Account-ID">>, <<"Queue-ID">>]).
-define(OPTIONAL_MEMBER_CALL_SUCCESS_HEADERS, [<<"Process-ID">>, <<"Agent-ID">>, <<"Call-ID">>]).
-define(MEMBER_CALL_SUCCESS_VALUES, [{<<"Event-Category">>, <<"member">>}
                                     ,{<<"Event-Name">>, <<"call_success">>}
                                    ]).
-define(MEMBER_CALL_SUCCESS_TYPES, []).

-spec member_call_success(api_terms()) ->
                                 {'ok', iolist()} |
                                 {'error', string()}.
member_call_success(Props) when is_list(Props) ->
    case member_call_success_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CALL_SUCCESS_HEADERS, ?OPTIONAL_MEMBER_CALL_SUCCESS_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_call_success"}
    end;
member_call_success(JObj) ->
    member_call_success(wh_json:to_proplist(JObj)).

-spec member_call_success_v(api_terms()) -> boolean().
member_call_success_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CALL_SUCCESS_HEADERS, ?MEMBER_CALL_SUCCESS_VALUES, ?MEMBER_CALL_SUCCESS_TYPES);
member_call_success_v(JObj) ->
    member_call_success_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Member Call Fail - if the queue is unable to properly handle the call
%%  (queue is full, empty, wait timeout expires, etc)
%%------------------------------------------------------------------------------
-define(MEMBER_CALL_CANCEL_HEADERS, [<<"Call-ID">>, <<"Account-ID">>, <<"Queue-ID">>]).
-define(OPTIONAL_MEMBER_CALL_CANCEL_HEADERS, [<<"Reason">>]).
-define(MEMBER_CALL_CANCEL_VALUES, [{<<"Event-Category">>, <<"member">>}
                                    ,{<<"Event-Name">>, <<"call_cancel">>}
                                   ]).
-define(MEMBER_CALL_CANCEL_TYPES, []).

-spec member_call_cancel(api_terms()) ->
                                {'ok', iolist()} |
                                {'error', string()}.
member_call_cancel(Props) when is_list(Props) ->
    case member_call_cancel_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CALL_CANCEL_HEADERS, ?OPTIONAL_MEMBER_CALL_CANCEL_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_call_cancel"}
    end;
member_call_cancel(JObj) ->
    member_call_cancel(wh_json:to_proplist(JObj)).

-spec member_call_cancel_v(api_terms()) -> boolean().
member_call_cancel_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CALL_CANCEL_HEADERS, ?MEMBER_CALL_CANCEL_VALUES, ?MEMBER_CALL_CANCEL_TYPES);
member_call_cancel_v(JObj) ->
    member_call_cancel_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Member Connect Request
%%------------------------------------------------------------------------------
-define(MEMBER_CONNECT_REQ_HEADERS, [<<"Account-ID">>, <<"Queue-ID">>, <<"Call-ID">>]).
-define(OPTIONAL_MEMBER_CONNECT_REQ_HEADERS, [<<"Process-ID">>]).
-define(MEMBER_CONNECT_REQ_VALUES, [{<<"Event-Category">>, <<"member">>}
                                    ,{<<"Event-Name">>, <<"connect_req">>}
                                   ]).
-define(MEMBER_CONNECT_REQ_TYPES, []).

-spec member_connect_req(api_terms()) ->
                                {'ok', iolist()} |
                                {'error', string()}.
member_connect_req(Props) when is_list(Props) ->
    case member_connect_req_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CONNECT_REQ_HEADERS, ?OPTIONAL_MEMBER_CONNECT_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_connect_req"}
    end;
member_connect_req(JObj) ->
    member_connect_req(wh_json:to_proplist(JObj)).

-spec member_connect_req_v(api_terms()) -> boolean().
member_connect_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CONNECT_REQ_HEADERS, ?MEMBER_CONNECT_REQ_VALUES, ?MEMBER_CONNECT_REQ_TYPES);
member_connect_req_v(JObj) ->
    member_connect_req_v(wh_json:to_proplist(JObj)).

-spec member_connect_req_routing_key(api_terms()) -> ne_binary().
-spec member_connect_req_routing_key(ne_binary(), ne_binary()) -> ne_binary().
member_connect_req_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Queue-ID">>, Props, <<"*">>),
    AcctId = props:get_value(<<"Account-ID">>, Props),
    member_connect_req_routing_key(AcctId, Id);
member_connect_req_routing_key(JObj) ->
    Id = wh_json:get_value(<<"Queue-ID">>, JObj, <<"*">>),
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    member_connect_req_routing_key(AcctId, Id).
member_connect_req_routing_key(AcctId, QID) ->
    <<"acdc.member.connect_req.", AcctId/binary, ".", QID/binary>>.

%%------------------------------------------------------------------------------
%% Member Connect Response
%%------------------------------------------------------------------------------
-define(MEMBER_CONNECT_RESP_HEADERS, [<<"Agent-ID">>]).
-define(OPTIONAL_MEMBER_CONNECT_RESP_HEADERS, [<<"Idle-Time">>, <<"Process-ID">>]).
-define(MEMBER_CONNECT_RESP_VALUES, [{<<"Event-Category">>, <<"member">>}
                                     ,{<<"Event-Name">>, <<"connect_resp">>}
                                    ]).
-define(MEMBER_CONNECT_RESP_TYPES, []).

-spec member_connect_resp(api_terms()) ->
                                 {'ok', iolist()} |
                                 {'error', string()}.
member_connect_resp(Props) when is_list(Props) ->
    case member_connect_resp_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CONNECT_RESP_HEADERS, ?OPTIONAL_MEMBER_CONNECT_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_connect_resp"}
    end;
member_connect_resp(JObj) ->
    member_connect_resp(wh_json:to_proplist(JObj)).

-spec member_connect_resp_v(api_terms()) -> boolean().
member_connect_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CONNECT_RESP_HEADERS, ?MEMBER_CONNECT_RESP_VALUES, ?MEMBER_CONNECT_RESP_TYPES);
member_connect_resp_v(JObj) ->
    member_connect_resp_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Member Connect Win
%%------------------------------------------------------------------------------
-define(MEMBER_CONNECT_WIN_HEADERS, [<<"Queue-ID">>, <<"Call">>]).
-define(OPTIONAL_MEMBER_CONNECT_WIN_HEADERS, [<<"Ring-Timeout">>, <<"Caller-Exit-Key">>
                                              ,<<"Wrapup-Timeout">>, <<"CDR-Url">>
                                              ,<<"Process-ID">>, <<"Agent-Process-ID">>
                                              ,<<"Record-Caller">>, <<"Recording-URL">>
                                              ,<<"Notifications">>
                                             ]).
-define(MEMBER_CONNECT_WIN_VALUES, [{<<"Event-Category">>, <<"member">>}
                                    ,{<<"Event-Name">>, <<"connect_win">>}
                                   ]).
-define(MEMBER_CONNECT_WIN_TYPES, [{<<"Record-Caller">>, fun wh_util:is_boolean/1}]).

-spec member_connect_win(api_terms()) ->
                                {'ok', iolist()} |
                                {'error', string()}.
member_connect_win(Props) when is_list(Props) ->
    case member_connect_win_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CONNECT_WIN_HEADERS, ?OPTIONAL_MEMBER_CONNECT_WIN_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_connect_win"}
    end;
member_connect_win(JObj) ->
    member_connect_win(wh_json:to_proplist(JObj)).

-spec member_connect_win_v(api_terms()) -> boolean().
member_connect_win_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CONNECT_WIN_HEADERS, ?MEMBER_CONNECT_WIN_VALUES, ?MEMBER_CONNECT_WIN_TYPES);
member_connect_win_v(JObj) ->
    member_connect_win_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Agent Timeout
%%------------------------------------------------------------------------------
-define(AGENT_TIMEOUT_HEADERS, [<<"Queue-ID">>, <<"Call-ID">>]).
-define(OPTIONAL_AGENT_TIMEOUT_HEADERS, [<<"Agent-Process-ID">>]).
-define(AGENT_TIMEOUT_VALUES, [{<<"Event-Category">>, <<"agent">>}
                               ,{<<"Event-Name">>, <<"connect_timeout">>}
                              ]).
-define(AGENT_TIMEOUT_TYPES, []).

-spec agent_timeout(api_terms()) ->
                           {'ok', iolist()} |
                           {'error', string()}.
agent_timeout(Props) when is_list(Props) ->
    case agent_timeout_v(Props) of
        'true' -> wh_api:build_message(Props, ?AGENT_TIMEOUT_HEADERS, ?OPTIONAL_AGENT_TIMEOUT_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_timeout"}
    end;
agent_timeout(JObj) -> agent_timeout(wh_json:to_proplist(JObj)).

-spec agent_timeout_v(api_terms()) -> boolean().
agent_timeout_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AGENT_TIMEOUT_HEADERS, ?AGENT_TIMEOUT_VALUES, ?AGENT_TIMEOUT_TYPES);
agent_timeout_v(JObj) ->
    agent_timeout_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Member Connect Accepted
%%------------------------------------------------------------------------------
-define(MEMBER_CONNECT_ACCEPTED_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_MEMBER_CONNECT_ACCEPTED_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>, <<"Process-ID">>]).
-define(MEMBER_CONNECT_ACCEPTED_VALUES, [{<<"Event-Category">>, <<"member">>}
                                         ,{<<"Event-Name">>, <<"connect_accepted">>}
                                        ]).
-define(MEMBER_CONNECT_ACCEPTED_TYPES, []).

-spec member_connect_accepted(api_terms()) ->
                                     {'ok', iolist()} |
                                     {'error', string()}.
member_connect_accepted(Props) when is_list(Props) ->
    case member_connect_accepted_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CONNECT_ACCEPTED_HEADERS, ?OPTIONAL_MEMBER_CONNECT_ACCEPTED_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_connect_accepted"}
    end;
member_connect_accepted(JObj) ->
    member_connect_accepted(wh_json:to_proplist(JObj)).

-spec member_connect_accepted_v(api_terms()) -> boolean().
member_connect_accepted_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CONNECT_ACCEPTED_HEADERS, ?MEMBER_CONNECT_ACCEPTED_VALUES, ?MEMBER_CONNECT_ACCEPTED_TYPES);
member_connect_accepted_v(JObj) ->
    member_connect_accepted_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Member Connect Retry
%%   Sent by the agent process that dialed its agent endpoints when the agent
%%   fails to respond. Informs the queue to try a member_connect_req again
%%------------------------------------------------------------------------------
-define(MEMBER_CONNECT_RETRY_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_MEMBER_CONNECT_RETRY_HEADERS, [<<"Process-ID">>, <<"Agent-ID">>]).
-define(MEMBER_CONNECT_RETRY_VALUES, [{<<"Event-Category">>, <<"member">>}
                                      ,{<<"Event-Name">>, <<"connect_retry">>}
                                     ]).
-define(MEMBER_CONNECT_RETRY_TYPES, []).

-spec member_connect_retry(api_terms()) ->
                                  {'ok', iolist()} |
                                  {'error', string()}.
member_connect_retry(Props) when is_list(Props) ->
    case member_connect_retry_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_CONNECT_RETRY_HEADERS, ?OPTIONAL_MEMBER_CONNECT_RETRY_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_connect_retry"}
    end;
member_connect_retry(JObj) ->
    member_connect_retry(wh_json:to_proplist(JObj)).

-spec member_connect_retry_v(api_terms()) -> boolean().
member_connect_retry_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_CONNECT_RETRY_HEADERS, ?MEMBER_CONNECT_RETRY_VALUES, ?MEMBER_CONNECT_RETRY_TYPES);
member_connect_retry_v(JObj) ->
    member_connect_retry_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Member Hungup
%%   When a bridge ends, the agent processes controlling/monitoring the call
%%   will send the Queue process this hangup event, so the queue knows the call
%%   has finished.
%%------------------------------------------------------------------------------
-define(MEMBER_HUNGUP_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_MEMBER_HUNGUP_HEADERS, [<<"Process-ID">>]).
-define(MEMBER_HUNGUP_VALUES, [{<<"Event-Category">>, <<"member">>}
                               ,{<<"Event-Name">>, <<"hungup">>}
                              ]).
-define(MEMBER_HUNGUP_TYPES, []).

-spec member_hungup(api_terms()) ->
                           {'ok', iolist()} |
                           {'error', string()}.
member_hungup(Props) when is_list(Props) ->
    case member_hungup_v(Props) of
        'true' -> wh_api:build_message(Props, ?MEMBER_HUNGUP_HEADERS, ?OPTIONAL_MEMBER_HUNGUP_HEADERS);
        'false' -> {'error', "Proplist failed validation for member_hungup"}
    end;
member_hungup(JObj) ->
    member_hungup(wh_json:to_proplist(JObj)).

-spec member_hungup_v(api_terms()) -> boolean().
member_hungup_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEMBER_HUNGUP_HEADERS, ?MEMBER_HUNGUP_VALUES, ?MEMBER_HUNGUP_TYPES);
member_hungup_v(JObj) ->
    member_hungup_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Sync Req/Resp
%%   Depending on the queue strategy, get the other queue's strategy state
%%------------------------------------------------------------------------------
-spec sync_req_routing_key(api_terms()) -> ne_binary().
-spec sync_req_routing_key(ne_binary(), ne_binary()) -> ne_binary().
sync_req_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Queue-ID">>, Props, <<"*">>),
    AcctId = props:get_value(<<"Account-ID">>, Props),
    sync_req_routing_key(AcctId, Id);
sync_req_routing_key(JObj) ->
    Id = wh_json:get_value(<<"Queue-ID">>, JObj, <<"*">>),
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    sync_req_routing_key(AcctId, Id).

sync_req_routing_key(AcctId, QID) ->
    <<"acdc.queue.sync_req.", AcctId/binary, ".", QID/binary>>.

-define(SYNC_REQ_HEADERS, [<<"Account-ID">>, <<"Queue-ID">>]).
-define(OPTIONAL_SYNC_REQ_HEADERS, [<<"Process-ID">>]).
-define(SYNC_REQ_VALUES, [{<<"Event-Category">>, <<"queue">>}
                          ,{<<"Event-Name">>, <<"sync_req">>}
                         ]).
-define(SYNC_REQ_TYPES, []).

-spec sync_req(api_terms()) ->
                      {'ok', iolist()} |
                      {'error', string()}.
sync_req(Props) when is_list(Props) ->
    case sync_req_v(Props) of
        'true' -> wh_api:build_message(Props, ?SYNC_REQ_HEADERS, ?OPTIONAL_SYNC_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for sync_req"}
    end;
sync_req(JObj) ->
    sync_req(wh_json:to_proplist(JObj)).

-spec sync_req_v(api_terms()) -> boolean().
sync_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYNC_REQ_HEADERS, ?SYNC_REQ_VALUES, ?SYNC_REQ_TYPES);
sync_req_v(JObj) ->
    sync_req_v(wh_json:to_proplist(JObj)).

-define(SYNC_RESP_HEADERS, [<<"Account-ID">>, <<"Queue-ID">>
                            ,<<"Current-Strategy">>
                           ]).
-define(OPTIONAL_SYNC_RESP_HEADERS, [<<"Process-ID">>, <<"Strategy-State">>]).
-define(SYNC_RESP_VALUES, [{<<"Event-Category">>, <<"queue">>}
                           ,{<<"Event-Name">>, <<"sync_resp">>}
                          ]).
-define(SYNC_RESP_TYPES, []).

-spec sync_resp(api_terms()) ->
                       {'ok', iolist()} |
                       {'error', string()}.
sync_resp(Props) when is_list(Props) ->
    case sync_resp_v(Props) of
        'true' -> wh_api:build_message(Props, ?SYNC_RESP_HEADERS, ?OPTIONAL_SYNC_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for sync_resp"}
    end;
sync_resp(JObj) ->
    sync_resp(wh_json:to_proplist(JObj)).

-spec sync_resp_v(api_terms()) -> boolean().
sync_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYNC_RESP_HEADERS, ?SYNC_RESP_VALUES, ?SYNC_RESP_TYPES);
sync_resp_v(JObj) ->
    sync_resp_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Agent Change
%%   available: when an agent logs in, tell its configured queues
%%   ringing: when an agent is being run, forward queues' round robin
%%------------------------------------------------------------------------------
agent_change_publish_key(Prop) when is_list(Prop) ->
    agent_change_routing_key(props:get_value(<<"Account-ID">>, Prop)
                             ,props:get_value(<<"Queue-ID">>, Prop)
                            );
agent_change_publish_key(JObj) ->
    agent_change_routing_key(wh_json:get_value(<<"Account-ID">>, JObj)
                             ,wh_json:get_value(<<"Queue-ID">>, JObj)
                            ).

agent_change_routing_key(AcctId, QueueId) ->
    <<"acdc.queue.agent_change.", AcctId/binary, ".", QueueId/binary>>.

-define(AGENT_CHANGE_AVAILABLE, <<"available">>).
-define(AGENT_CHANGE_RINGING, <<"ringing">>).
-define(AGENT_CHANGE_UNAVAILABLE, <<"unavailable">>).
-define(AGENT_CHANGES, [?AGENT_CHANGE_AVAILABLE
                        ,?AGENT_CHANGE_RINGING
                        ,?AGENT_CHANGE_UNAVAILABLE
                       ]).

agent_change_available() -> ?AGENT_CHANGE_AVAILABLE.
agent_change_ringing() -> ?AGENT_CHANGE_RINGING.
agent_change_unavailable() -> ?AGENT_CHANGE_UNAVAILABLE.

-define(AGENT_CHANGE_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>, <<"Queue-ID">>, <<"Change">>]).
-define(OPTIONAL_AGENT_CHANGE_HEADERS, [<<"Process-ID">>]).
-define(AGENT_CHANGE_VALUES, [{<<"Event-Category">>, <<"queue">>}
                              ,{<<"Event-Name">>, <<"agent_change">>}
                              ,{<<"Change">>, ?AGENT_CHANGES}
                             ]).
-define(AGENT_CHANGE_TYPES, []).

-spec agent_change(api_terms()) ->
                          {'ok', iolist()} |
                          {'error', string()}.
agent_change(Prop) when is_list(Prop) ->
    case agent_change_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?AGENT_CHANGE_HEADERS, ?OPTIONAL_AGENT_CHANGE_HEADERS);
        'false' -> {'error', "proplist failed validation for agent_change"}
    end;
agent_change(JObj) -> agent_change(wh_json:to_proplist(JObj)).

-spec agent_change_v(api_terms()) -> boolean().
agent_change_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AGENT_CHANGE_HEADERS, ?AGENT_CHANGE_VALUES, ?AGENT_CHANGE_TYPES);
agent_change_v(JObj) -> agent_change_v(wh_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Bind/Unbind the queue as appropriate
%%------------------------------------------------------------------------------
-spec shared_queue_name(ne_binary(), ne_binary()) -> ne_binary().
shared_queue_name(AcctId, QueueId) ->
    <<"acdc.queue.", AcctId/binary, ".", QueueId/binary>>.

-spec queue_size(ne_binary(), ne_binary()) -> integer() | 'undefined'.
queue_size(AcctId, QueueId) ->
    Q = shared_queue_name(AcctId, QueueId),
    try amqp_util:new_queue(Q, [{'return_field', 'message_count'}
                                 ,{'exclusive', 'false'}
                                 ,{'passive', 'true'}
                                ])
    of
        {'error', {'server_initiated_close', 404, _Msg}} ->
            lager:debug("failed to query queue size: ~s", [_Msg]),
            'undefined';
        N -> N
    catch
        _E:_R ->
            lager:debug("failed to query queue size: ~s: ~p", [_E, _R]),
            'undefined'
    end.

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    QID = props:get_value('queue_id', Props, <<"*">>),
    AcctId = props:get_value('account_id', Props),
    bind_q(Q, AcctId, QID, props:get_value('restrict_to', Props)).

bind_q(Q, AcctId, QID, 'undefined') ->
    amqp_util:bind_q_to_whapps(Q, sync_req_routing_key(AcctId, QID)),
    amqp_util:bind_q_to_whapps(Q, agent_change_routing_key(AcctId, QID)),
    amqp_util:bind_q_to_callmgr(Q, member_call_routing_key(AcctId, QID)),
    amqp_util:bind_q_to_callmgr(Q, member_connect_req_routing_key(AcctId, QID));
bind_q(Q, AcctId, QID, ['member_call'|T]) ->
    amqp_util:bind_q_to_callmgr(Q, member_call_routing_key(AcctId, QID)),
    bind_q(Q, AcctId, QID, T);
bind_q(Q, AcctId, QID, ['member_connect_req'|T]) ->
    amqp_util:bind_q_to_callmgr(Q, member_connect_req_routing_key(AcctId, QID)),
    bind_q(Q, AcctId, QID, T);
bind_q(Q, AcctId, QID, ['sync_req'|T]) ->
    amqp_util:bind_q_to_whapps(Q, sync_req_routing_key(AcctId, QID)),
    bind_q(Q, AcctId, QID, T);
bind_q(Q, AcctId, QID, ['agent_change'|T]) ->
    amqp_util:bind_q_to_whapps(Q, agent_change_routing_key(AcctId, QID)),
    bind_q(Q, AcctId, QID, T);
bind_q(Q, AcctId, QID, [_|T]) -> bind_q(Q, AcctId, QID, T);
bind_q(_, _, _, []) -> 'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    QID = props:get_value('queue_id', Props, <<"*">>),
    AcctId = props:get_value('account_id', Props),

    unbind_q(Q, AcctId, QID, props:get_value('restrict_to', Props)).

unbind_q(Q, AcctId, QID, 'undefined') ->
    _ = amqp_util:unbind_q_from_whapps(Q, sync_req_routing_key(AcctId, QID)),
    _ = amqp_util:unbind_q_from_whapps(Q, agent_change_routing_key(AcctId, QID)),
    _ = amqp_util:unbind_q_from_callmgr(Q, member_call_routing_key(AcctId, QID)),
    _ = amqp_util:unbind_q_from_callmgr(Q, member_connect_req_routing_key(AcctId, QID));
unbind_q(Q, AcctId, QID, ['member_call'|T]) ->
    _ = amqp_util:unbind_q_from_callmgr(Q, member_call_routing_key(AcctId, QID)),
    unbind_q(Q, AcctId, QID, T);
unbind_q(Q, AcctId, QID, ['member_connect_req'|T]) ->
    _ = amqp_util:unbind_q_from_callmgr(Q, member_connect_req_routing_key(AcctId, QID)),
    unbind_q(Q, AcctId, QID, T);
unbind_q(Q, AcctId, QID, ['sync_req'|T]) ->
    _ = amqp_util:unbind_q_from_whapps(Q, sync_req_routing_key(AcctId, QID)),
    unbind_q(Q, AcctId, QID, T);
unbind_q(Q, AcctId, QID, ['agent_change'|T]) ->
    _ = amqp_util:unbind_q_from_whapps(Q, agent_change_routing_key(AcctId, QID)),
    unbind_q(Q, AcctId, QID, T);
unbind_q(Q, AcctId, QID, [_|T]) ->
    unbind_q(Q, AcctId, QID, T);
unbind_q(_, _, _, []) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange(),
    amqp_util:whapps_exchange().

%%------------------------------------------------------------------------------
%% Publishers for convenience
%%------------------------------------------------------------------------------
-spec publish_member_call(api_terms()) -> 'ok'.
-spec publish_member_call(api_terms(), ne_binary()) -> 'ok'.
publish_member_call(JObj) ->
    publish_member_call(JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_call(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CALL_VALUES, fun member_call/1),
    amqp_util:callmgr_publish(Payload, ContentType, member_call_routing_key(API)).

-spec publish_member_call_cancel(api_terms()) -> 'ok'.
-spec publish_member_call_cancel(api_terms(), ne_binary()) -> 'ok'.
publish_member_call_cancel(JObj) ->
    publish_member_call_cancel(JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_call_cancel(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CALL_CANCEL_VALUES, fun member_call_cancel/1),
    amqp_util:callmgr_publish(Payload, ContentType, member_call_routing_key(API)).

publish_shared_member_call(JObj) ->
    publish_shared_member_call(wh_json:get_value(<<"Account-ID">>, JObj)
                               ,wh_json:get_value(<<"Queue-ID">>, JObj)
                               ,JObj
                              ).
publish_shared_member_call(AcctId, QueueId, JObj) ->
    publish_shared_member_call(AcctId, QueueId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_shared_member_call(AcctId, QueueId, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CALL_VALUES, fun member_call/1),
    amqp_util:targeted_publish(shared_queue_name(AcctId, QueueId), Payload, ContentType).

-spec publish_member_call_failure(ne_binary(), api_terms()) -> 'ok'.
-spec publish_member_call_failure(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_member_call_failure(Q, JObj) ->
    publish_member_call_failure(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_call_failure(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CALL_FAIL_VALUES, fun member_call_failure/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_member_call_success(ne_binary(), api_terms()) -> 'ok'.
-spec publish_member_call_success(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_member_call_success(Q, JObj) ->
    publish_member_call_success(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_call_success(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CALL_SUCCESS_VALUES, fun member_call_success/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_member_connect_req(api_terms()) -> 'ok'.
-spec publish_member_connect_req(api_terms(), ne_binary()) -> 'ok'.
publish_member_connect_req(JObj) ->
    publish_member_connect_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_connect_req(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CONNECT_REQ_VALUES, fun member_connect_req/1),
    amqp_util:callmgr_publish(Payload, ContentType, member_connect_req_routing_key(API)).

-spec publish_member_connect_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_member_connect_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_member_connect_resp(Q, JObj) ->
    publish_member_connect_resp(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_connect_resp(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CONNECT_RESP_VALUES, fun member_connect_resp/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_member_connect_win(ne_binary(), api_terms()) -> 'ok'.
-spec publish_member_connect_win(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_member_connect_win(Q, JObj) ->
    publish_member_connect_win(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_connect_win(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CONNECT_WIN_VALUES, fun member_connect_win/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_agent_timeout(ne_binary(), api_terms()) -> 'ok'.
-spec publish_agent_timeout(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_agent_timeout(Q, JObj) ->
    publish_agent_timeout(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_agent_timeout(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?AGENT_TIMEOUT_VALUES, fun agent_timeout/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_member_connect_accepted(ne_binary(), api_terms()) -> 'ok'.
-spec publish_member_connect_accepted(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_member_connect_accepted(Q, JObj) ->
    publish_member_connect_accepted(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_connect_accepted(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CONNECT_ACCEPTED_VALUES, fun member_connect_accepted/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_member_connect_retry(ne_binary(), api_terms()) -> 'ok'.
-spec publish_member_connect_retry(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_member_connect_retry(Q, JObj) ->
    publish_member_connect_retry(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_connect_retry(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_CONNECT_RETRY_VALUES, fun member_connect_retry/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_member_hungup(ne_binary(), api_terms()) -> 'ok'.
-spec publish_member_hungup(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_member_hungup(Q, JObj) ->
    publish_member_hungup(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_member_hungup(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?MEMBER_HUNGUP_VALUES, fun member_hungup/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_sync_req(api_terms()) -> 'ok'.
-spec publish_sync_req(api_terms(), ne_binary()) -> 'ok'.
publish_sync_req(JObj) ->
    publish_sync_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_sync_req(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?SYNC_REQ_VALUES, fun sync_req/1),
    amqp_util:whapps_publish(sync_req_routing_key(API), Payload, ContentType).

-spec publish_sync_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_sync_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_sync_resp(Q, JObj) ->
    publish_sync_resp(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_sync_resp(Q, API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?SYNC_RESP_VALUES, fun sync_resp/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_agent_change(api_terms()) -> 'ok'.
-spec publish_agent_change(api_terms(), ne_binary()) -> 'ok'.
publish_agent_change(JObj) ->
    publish_agent_change(JObj, ?DEFAULT_CONTENT_TYPE).
publish_agent_change(API, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(API, ?AGENT_CHANGE_VALUES, fun agent_change/1),
    amqp_util:whapps_publish(agent_change_publish_key(API), Payload, ContentType).
