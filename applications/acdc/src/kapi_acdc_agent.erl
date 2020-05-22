%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Bindings and JSON APIs for dealing with agents, as part of ACDc
%%% @author James Aimonetti
%%%
%%% @author James Aimonetti
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_acdc_agent).

-export([sync_req/1, sync_req_v/1
        ,sync_resp/1, sync_resp_v/1
        ,stats_req/1, stats_req_v/1
        ,stats_resp/1, stats_resp_v/1
        ,login/1, login_v/1
        ,logout/1, logout_v/1
        ,pause/1, pause_v/1
        ,resume/1, resume_v/1
        ,end_wrapup/1, end_wrapup_v/1
        ,login_queue/1, login_queue_v/1
        ,logout_queue/1, logout_queue_v/1
        ,restart/1, restart_v/1

        ,login_resp/1, login_resp_v/1

        ,shared_originate_failure/1, shared_originate_failure_v/1
        ,shared_call_id/1, shared_call_id_v/1
        ]).

-export([bind_q/2
        ,unbind_q/2
        ]).
-export([declare_exchanges/0]).

-export([publish_sync_req/1, publish_sync_req/2
        ,publish_sync_resp/2, publish_sync_resp/3
        ,publish_stats_req/1, publish_stats_req/2
        ,publish_stats_resp/2, publish_stats_resp/3
        ,publish_login/1, publish_login/2
        ,publish_logout/1, publish_logout/2
        ,publish_pause/1, publish_pause/2
        ,publish_resume/1, publish_resume/2
        ,publish_end_wrapup/1, publish_end_wrapup/2
        ,publish_login_queue/1, publish_login_queue/2
        ,publish_logout_queue/1, publish_logout_queue/2
        ,publish_restart/1, publish_restart/2

        ,publish_login_resp/2, publish_login_resp/3

        ,publish_shared_originate_failure/1, publish_shared_originate_failure/2
        ,publish_shared_call_id/1, publish_shared_call_id/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_amqp/include/kz_api.hrl").

%%------------------------------------------------------------------------------
%% Agent Sync Req
%%   When an agent process starts up, it will ask for other agent processes
%%   handling the same agent what state they are in. 0 or more will respond, and
%%   the calling agent process will join them in that state.
%%   After a timeout period of no responses, the agent will assume its alone and
%%   will transition to the ready state.
%%------------------------------------------------------------------------------
-define(SYNC_REQ_KEY, "agent.sync_req.").

-define(SYNC_REQ_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>]).
-define(OPTIONAL_SYNC_REQ_HEADERS, [<<"Process-ID">>]).
-define(SYNC_REQ_VALUES, [{<<"Event-Category">>, <<"agent">>}
                         ,{<<"Event-Name">>, <<"sync_req">>}
                         ]).
-define(SYNC_REQ_TYPES, []).

-spec sync_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
sync_req(Props) when is_list(Props) ->
    case sync_req_v(Props) of
        'true' -> kz_api:build_message(Props, ?SYNC_REQ_HEADERS, ?OPTIONAL_SYNC_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for sync_req"}
    end;
sync_req(JObj) ->
    sync_req(kz_json:to_proplist(JObj)).

-spec sync_req_v(kz_term:api_terms()) -> boolean().
sync_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYNC_REQ_HEADERS, ?SYNC_REQ_VALUES, ?SYNC_REQ_TYPES);
sync_req_v(JObj) ->
    sync_req_v(kz_json:to_proplist(JObj)).

-spec sync_req_routing_key(kz_json:object() | kz_term:proplist()) -> kz_term:ne_binary().
sync_req_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Agent-ID">>, Props, <<"*">>),
    AccountId = props:get_value(<<"Account-ID">>, Props, <<"*">>),
    sync_req_routing_key(AccountId, Id);
sync_req_routing_key(JObj) ->
    Id = kz_json:get_value(<<"Agent-ID">>, JObj, <<"*">>),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj, <<"*">>),
    sync_req_routing_key(AccountId, Id).

-spec sync_req_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
sync_req_routing_key(AccountId, Id) ->
    <<?SYNC_REQ_KEY, AccountId/binary, ".", Id/binary>>.

%% And the response
-define(SYNC_RESP_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>, <<"Status">>]).
-define(OPTIONAL_SYNC_RESP_HEADERS, [<<"Call-ID">>, <<"Time-Left">>, <<"Process-ID">>]).
-define(SYNC_RESP_VALUES, [{<<"Event-Category">>, <<"agent">>}
                          ,{<<"Event-Name">>, <<"sync_resp">>}
                          ,{<<"Status">>, [<<"sync">>
                                          ,<<"ready">>
                                          ,<<"ringing">>
                                          ,<<"ringing_callback">>
                                          ,<<"awaiting_callback">>
                                          ,<<"answered">>
                                          ,<<"wrapup">>
                                          ,<<"paused">>
                                          ,<<"outbound">>
                                          ]}
                          ]).
-define(SYNC_RESP_TYPES, []).

-spec sync_resp(kz_term:api_terms()) -> {'ok', iolist()} |
          {'error', string()}.
sync_resp(Props) when is_list(Props) ->
    case sync_resp_v(Props) of
        'true' -> kz_api:build_message(Props, ?SYNC_RESP_HEADERS, ?OPTIONAL_SYNC_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for sync_resp"}
    end;
sync_resp(JObj) ->
    sync_resp(kz_json:to_proplist(JObj)).

-spec sync_resp_v(kz_term:api_terms()) -> boolean().
sync_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYNC_RESP_HEADERS, ?SYNC_RESP_VALUES, ?SYNC_RESP_TYPES);
sync_resp_v(JObj) ->
    sync_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Agent Stats Req
%%   Let's others on the message bus request stats about all agents, or a given
%%   agent, within an account
%%------------------------------------------------------------------------------

%% agent.stats_req.AccountId.AGENT_ID
-define(STATS_REQ_KEY, "acdc.agent.stats_req.").

-define(STATS_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_STATS_REQ_HEADERS, [<<"Agent-ID">>, <<"Call-ID">>]).
-define(STATS_REQ_VALUES, [{<<"Event-Category">>, <<"agent">>}
                          ,{<<"Event-Name">>, <<"stats_req">>}
                          ]).
-define(STATS_REQ_TYPES, []).

-spec stats_req(kz_term:api_terms()) -> {'ok', iolist()} |
          {'error', string()}.
stats_req(Props) when is_list(Props) ->
    case stats_req_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATS_REQ_HEADERS, ?OPTIONAL_STATS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for stats_req"}
    end;
stats_req(JObj) ->
    stats_req(kz_json:to_proplist(JObj)).

-spec stats_req_v(kz_term:api_terms()) -> boolean().
stats_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATS_REQ_HEADERS, ?STATS_REQ_VALUES, ?STATS_REQ_TYPES);
stats_req_v(JObj) ->
    stats_req_v(kz_json:to_proplist(JObj)).

-spec stats_req_routing_key(kz_json:object() | kz_term:proplist() | kz_term:ne_binary()) -> kz_term:ne_binary().
stats_req_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Account-ID">>, Props, <<"*">>),
    AgentId = props:get_value(<<"Agent-ID">>, Props, <<"*">>),
    CallId = props:get_value(<<"Call-ID">>, Props, <<"*">>),
    stats_req_routing_key(Id, AgentId, CallId);
stats_req_routing_key(Id) when is_binary(Id) ->
    <<?STATS_REQ_KEY, Id/binary>>;
stats_req_routing_key(JObj) ->
    Id = kz_json:get_value(<<"Account-ID">>, JObj, <<"*">>),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj, <<"*">>),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj, <<"*">>),
    stats_req_routing_key(Id, AgentId, CallId).

-spec stats_req_publish_key(kz_json:object() | kz_term:proplist() | kz_term:ne_binary()) -> kz_term:ne_binary().
stats_req_publish_key(Props) when is_list(Props) ->
    stats_req_routing_key(props:get_value(<<"Account-ID">>, Props)
                         ,props:get_value(<<"Agent-ID">>, Props)
                         ,props:get_value(<<"Call-ID">>, Props)
                         );
stats_req_publish_key(JObj) ->
    stats_req_routing_key(kz_json:get_value(<<"Account-ID">>, JObj)
                         ,kz_json:get_value(<<"Agent-ID">>, JObj)
                         ,kz_json:get_value(<<"Call-ID">>, JObj)
                         ).

-spec stats_req_routing_key(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
stats_req_routing_key(Id, 'undefined') ->
    stats_req_routing_key(Id, 'undefined', 'undefined');
stats_req_routing_key(Id, AgentId) ->
    stats_req_routing_key(Id, AgentId, 'undefined').

stats_req_routing_key(Id, 'undefined', 'undefined') ->
    stats_req_routing_key(Id);
stats_req_routing_key(Id, AgentId, 'undefined') ->
    <<?STATS_REQ_KEY, Id/binary, ".", AgentId/binary>>;
stats_req_routing_key('undefined', 'undefined', CallId) ->
    <<?STATS_REQ_KEY, "*.*.", CallId/binary>>;
stats_req_routing_key(Id, AgentId, CallId) ->
    <<?STATS_REQ_KEY, Id/binary, ".", AgentId/binary, ".", CallId/binary>>.

%% And the response
-define(STATS_RESP_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_STATS_RESP_HEADERS, [<<"Current-Calls">>, <<"Current-Stats">>, <<"Current-Statuses">>, <<"Agent-Call-IDs">>]).
-define(STATS_RESP_VALUES, [{<<"Event-Category">>, <<"agent">>}
                           ,{<<"Event-Name">>, <<"stats_resp">>}
                           ]).
-define(STATS_RESP_TYPES, [{<<"Stats">>, fun kz_json:is_json_object/1}]).

-spec stats_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
stats_resp(Props) when is_list(Props) ->
    case stats_resp_v(Props) of
        'true' -> kz_api:build_message(Props, ?STATS_RESP_HEADERS, ?OPTIONAL_STATS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for stats_resp"}
    end;
stats_resp(JObj) ->
    stats_resp(kz_json:to_proplist(JObj)).

-spec stats_resp_v(kz_term:api_terms()) -> boolean().
stats_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STATS_RESP_HEADERS, ?STATS_RESP_VALUES, ?STATS_RESP_TYPES);
stats_resp_v(JObj) ->
    stats_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Agent Login/Logout/Pause/Resume
%%------------------------------------------------------------------------------
-define(AGENT_KEY, "acdc.agent.action."). % append queue ID

-define(AGENT_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>]).
-define(OPTIONAL_AGENT_HEADERS, [<<"Time-Limit">>, <<"Queue-ID">>
                                ,<<"Presence-ID">>, <<"Presence-State">>
                                ]).
-define(AGENT_VALUES, [{<<"Event-Category">>, <<"agent">>}
                      ,{<<"Presence-State">>, kapi_presence:presence_states()}
                      ]).
-define(AGENT_TYPES, []).

-define(OPTIONAL_PAUSE_HEADERS, [<<"Alias">>]).

-define(LOGIN_VALUES, [{<<"Event-Name">>, <<"login">>} | ?AGENT_VALUES]).
-define(LOGOUT_VALUES, [{<<"Event-Name">>, <<"logout">>} | ?AGENT_VALUES]).
-define(PAUSE_VALUES, [{<<"Event-Name">>, <<"pause">>} | ?AGENT_VALUES]).
-define(RESUME_VALUES, [{<<"Event-Name">>, <<"resume">>} | ?AGENT_VALUES]).
-define(END_WRAPUP_VALUES, [{<<"Event-Name">>, <<"end_wrapup">>} | ?AGENT_VALUES]).
-define(LOGIN_QUEUE_VALUES, [{<<"Event-Name">>, <<"login_queue">>} | ?AGENT_VALUES]).
-define(LOGOUT_QUEUE_VALUES, [{<<"Event-Name">>, <<"logout_queue">>} | ?AGENT_VALUES]).
-define(RESTART_VALUES, [{<<"Event-Name">>, <<"restart">>} | ?AGENT_VALUES]).

-spec login(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
login(Props) when is_list(Props) ->
    case login_v(Props) of
        'true' -> kz_api:build_message(Props, ?AGENT_HEADERS, ?OPTIONAL_AGENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_login"}
    end;
login(JObj) ->
    login(kz_json:to_proplist(JObj)).

-spec login_v(kz_term:api_terms()) -> boolean().
login_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AGENT_HEADERS, ?LOGIN_VALUES, ?AGENT_TYPES);
login_v(JObj) ->
    login_v(kz_json:to_proplist(JObj)).

-spec login_queue(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
login_queue(Props) when is_list(Props) ->
    case login_queue_v(Props) of
        'true' -> kz_api:build_message(Props, ?AGENT_HEADERS, ?OPTIONAL_AGENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_login_queue"}
    end;
login_queue(JObj) ->
    login_queue(kz_json:to_proplist(JObj)).

-spec login_queue_v(kz_term:api_terms()) -> boolean().
login_queue_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AGENT_HEADERS, ?LOGIN_QUEUE_VALUES, ?AGENT_TYPES);
login_queue_v(JObj) ->
    login_queue_v(kz_json:to_proplist(JObj)).


-spec logout(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
logout(Props) when is_list(Props) ->
    case logout_v(Props) of
        'true' -> kz_api:build_message(Props, ?AGENT_HEADERS, ?OPTIONAL_AGENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_logout"}
    end;
logout(JObj) ->
    logout(kz_json:to_proplist(JObj)).

-spec logout_v(kz_term:api_terms()) -> boolean().
logout_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AGENT_HEADERS, ?LOGOUT_VALUES, ?AGENT_TYPES);
logout_v(JObj) ->
    logout_v(kz_json:to_proplist(JObj)).

-spec logout_queue(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
logout_queue(Props) when is_list(Props) ->
    case logout_queue_v(Props) of
        'true' -> kz_api:build_message(Props, ?AGENT_HEADERS, ?OPTIONAL_AGENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_logout_queue"}
    end;
logout_queue(JObj) ->
    logout_queue(kz_json:to_proplist(JObj)).

-spec logout_queue_v(kz_term:api_terms()) -> boolean().
logout_queue_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AGENT_HEADERS, ?LOGOUT_QUEUE_VALUES, ?AGENT_TYPES);
logout_queue_v(JObj) ->
    logout_queue_v(kz_json:to_proplist(JObj)).

-spec pause(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
pause(Props) when is_list(Props) ->
    case pause_v(Props) of
        'true' -> kz_api:build_message(Props, ?AGENT_HEADERS, ?OPTIONAL_AGENT_HEADERS ++ ?OPTIONAL_PAUSE_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_pause"}
    end;
pause(JObj) ->
    pause(kz_json:to_proplist(JObj)).

-spec pause_v(kz_term:api_terms()) -> boolean().
pause_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AGENT_HEADERS, ?PAUSE_VALUES, ?AGENT_TYPES);
pause_v(JObj) ->
    pause_v(kz_json:to_proplist(JObj)).

-spec resume(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
resume(Props) when is_list(Props) ->
    case resume_v(Props) of
        'true' -> kz_api:build_message(Props, ?AGENT_HEADERS, ?OPTIONAL_AGENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_resume"}
    end;
resume(JObj) -> resume(kz_json:to_proplist(JObj)).

-spec resume_v(kz_term:api_terms()) -> boolean().
resume_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AGENT_HEADERS, ?RESUME_VALUES, ?AGENT_TYPES);
resume_v(JObj) -> resume_v(kz_json:to_proplist(JObj)).

-spec end_wrapup(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
end_wrapup(Props) when is_list(Props) ->
    case end_wrapup_v(Props) of
        'true' -> kz_api:build_message(Props, ?AGENT_HEADERS, ?OPTIONAL_AGENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_end_wrapup"}
    end;
end_wrapup(JObj) -> end_wrapup(kz_json:to_proplist(JObj)).

-spec end_wrapup_v(kz_term:api_terms()) -> boolean().
end_wrapup_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AGENT_HEADERS, ?END_WRAPUP_VALUES, ?AGENT_TYPES);
end_wrapup_v(JObj) -> end_wrapup_v(kz_json:to_proplist(JObj)).

-spec restart(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
restart(Props) when is_list(Props) ->
    case restart_v(Props) of
        'true' -> kz_api:build_message(Props, ?AGENT_HEADERS, ?OPTIONAL_AGENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for agent_restart"}
    end;
restart(JObj) -> restart(kz_json:to_proplist(JObj)).

-spec restart_v(kz_term:api_terms()) -> boolean().
restart_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?AGENT_HEADERS, ?RESTART_VALUES, ?AGENT_TYPES);
restart_v(JObj) -> restart_v(kz_json:to_proplist(JObj)).

-spec agent_status_routing_key(kz_term:proplist()) -> kz_term:ne_binary().
agent_status_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Agent-ID">>, Props, <<"*">>),
    AccountId = props:get_value(<<"Account-ID">>, Props, <<"*">>),
    Status = props:get_value(<<"Event-Name">>, Props, <<"*">>),
    agent_status_routing_key(AccountId, Id, Status).

-spec agent_status_routing_key(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
agent_status_routing_key(AccountId, AgentId, Status) ->
    <<?AGENT_KEY, Status/binary, ".", AccountId/binary, ".", AgentId/binary>>.

-define(LOGIN_RESP_HEADERS, [<<"Status">>]).
-define(OPTIONAL_LOGIN_RESP_HEADERS, []).
-define(LOGIN_RESP_VALUES, [{<<"Event-Category">>, <<"agent">>}
                           ,{<<"Event-Name">>, <<"login_resp">>}
                           ,{<<"Status">>, [<<"success">>, <<"failed">>]}
                           ]).
-define(LOGIN_RESP_TYPES, []).

-spec login_resp(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
login_resp(Props) when is_list(Props) ->
    case login_resp_v(Props) of
        'true' -> kz_api:build_message(Props, ?LOGIN_RESP_HEADERS, ?OPTIONAL_LOGIN_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for login_resp"}
    end;
login_resp(JObj) ->
    login_resp(kz_json:to_proplist(JObj)).

-spec login_resp_v(kz_term:api_terms()) -> boolean().
login_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?LOGIN_RESP_HEADERS, ?LOGIN_RESP_VALUES, ?LOGIN_RESP_TYPES);
login_resp_v(JObj) ->
    login_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Sharing of originate_failure to all agent FSMs
%%------------------------------------------------------------------------------
-define(FSM_SHARED_KEY, "acdc.agent.fsm_shared.").

-spec fsm_shared_routing_key(kz_term:proplist()) -> kz_term:ne_binary().
fsm_shared_routing_key(Props) when is_list(Props) ->
    Id = props:get_value(<<"Agent-ID">>, Props, <<"*">>),
    AccountId = props:get_value(<<"Account-ID">>, Props, <<"*">>),
    fsm_shared_routing_key(AccountId, Id).

-spec fsm_shared_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
fsm_shared_routing_key(AccountId, AgentId) ->
    <<?FSM_SHARED_KEY, AccountId/binary, ".", AgentId/binary>>.

-define(SHARED_FAILURE_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>]).
-define(OPTIONAL_SHARED_FAILURE_HEADERS, [<<"Blame">>]).
-define(SHARED_FAILURE_VALUES, [{<<"Event-Category">>, <<"agent">>}
                               ,{<<"Event-Name">>, <<"shared_failure">>}
                               ,{<<"Blame">>, [<<"member">>]}
                               ]).
-define(SHARED_FAILURE_TYPES, []).

-spec shared_originate_failure(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
shared_originate_failure(Props) when is_list(Props) ->
    case shared_originate_failure_v(Props) of
        'true' -> kz_api:build_message(Props, ?SHARED_FAILURE_HEADERS, ?OPTIONAL_SHARED_FAILURE_HEADERS);
        'false' -> {'error', "Proplist failed validation for shared_originate_failure"}
    end;
shared_originate_failure(JObj) -> shared_originate_failure(kz_json:to_proplist(JObj)).

-spec shared_originate_failure_v(kz_term:api_terms()) -> boolean().
shared_originate_failure_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SHARED_FAILURE_HEADERS, ?SHARED_FAILURE_VALUES, ?SHARED_FAILURE_TYPES);
shared_originate_failure_v(JObj) -> shared_originate_failure_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Sharing of answered call id to all agent FSMs
%%------------------------------------------------------------------------------
-define(SHARED_CALL_ID_HEADERS, [<<"Account-ID">>, <<"Agent-ID">>]).
-define(OPTIONAL_SHARED_CALL_ID_HEADERS, [<<"Agent-Call-ID">>, <<"Member-Call-ID">>]).
-define(SHARED_CALL_ID_VALUES, [{<<"Event-Category">>, <<"agent">>}
                               ,{<<"Event-Name">>, <<"shared_call_id">>}
                               ]).
-define(SHARED_CALL_ID_TYPES, []).

-spec shared_call_id(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
shared_call_id(Props) when is_list(Props) ->
    case shared_call_id_v(Props) of
        'true' -> kz_api:build_message(Props, ?SHARED_CALL_ID_HEADERS, ?OPTIONAL_SHARED_CALL_ID_HEADERS);
        'false' -> {'error', "Proplist failed validation for shared_call_id"}
    end;
shared_call_id(JObj) -> shared_call_id(kz_json:to_proplist(JObj)).

-spec shared_call_id_v(kz_term:api_terms()) -> boolean().
shared_call_id_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SHARED_CALL_ID_HEADERS, ?SHARED_CALL_ID_VALUES, ?SHARED_CALL_ID_TYPES);
shared_call_id_v(JObj) -> shared_call_id_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% Shared routing key for member_connect_satisfied
%%------------------------------------------------------------------------------
-spec member_connect_satisfied_routing_key(kz_term:api_terms() | kz_term:ne_binary()) -> kz_term:ne_binary().
member_connect_satisfied_routing_key(Props) when is_list(Props) ->
    AgentId = props:get_value(<<"Agent-ID">>, Props),
    member_connect_satisfied_routing_key(AgentId);
member_connect_satisfied_routing_key(AgentId) when is_binary(AgentId) ->
    <<"acdc.member.connect_satisfied.", AgentId/binary>>;
member_connect_satisfied_routing_key(JObj) ->
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    member_connect_satisfied_routing_key(AgentId).

%%------------------------------------------------------------------------------
%% Shared routing key for member_connect_win
%%------------------------------------------------------------------------------
-spec member_connect_win_routing_key(kz_term:api_terms() | kz_term:ne_binary()) -> kz_term:ne_binary().
member_connect_win_routing_key(Props) when is_list(Props) ->
    AgentId = props:get_value(<<"Agent-ID">>, Props),
    member_connect_win_routing_key(AgentId);
member_connect_win_routing_key(AgentId) when is_binary(AgentId) ->
    <<"acdc.member.connect_win.", AgentId/binary>>;
member_connect_win_routing_key(JObj) ->
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    member_connect_win_routing_key(AgentId).

%%------------------------------------------------------------------------------
%% Bind/Unbind the queue as appropriate
%%------------------------------------------------------------------------------

-spec bind_q(binary(), kz_term:proplist()) -> 'ok'.
bind_q(Q, Props) ->
    AgentId = props:get_value('agent_id', Props, <<"*">>),
    AccountId = props:get_value('account_id', Props, <<"*">>),
    CallId = props:get_value('callid', Props, <<"*">>),
    Status = props:get_value('status', Props, <<"*">>),
    bind_q(Q, {AccountId, AgentId, CallId, Status}, props:get_value('restrict_to', Props)).

-spec bind_q(binary(), {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}, 'undefined' | list()) -> 'ok'.
bind_q(Q, {AccountId, AgentId, _, Status}, 'undefined') ->
    kz_amqp_util:bind_q_to_kapps(Q, agent_status_routing_key(AccountId, AgentId, Status)),
    kz_amqp_util:bind_q_to_kapps(Q, fsm_shared_routing_key(AccountId, AgentId)),
    kz_amqp_util:bind_q_to_kapps(Q, sync_req_routing_key(AccountId, AgentId)),
    kz_amqp_util:bind_q_to_kapps(Q, stats_req_routing_key(AccountId)),
    kz_amqp_util:bind_q_to_kapps(Q, stats_req_routing_key(AccountId, AgentId));
bind_q(Q, {_, AgentId, _, _}=Ids, ['member_connect_win'|T]) ->
    kz_amqp_util:bind_q_to_callmgr(Q, member_connect_win_routing_key(AgentId)),
    bind_q(Q, Ids, T);
bind_q(Q, {_, AgentId, _, _}=Ids, ['member_connect_satisfied'|T]) ->
    kz_amqp_util:bind_q_to_callmgr(Q, member_connect_satisfied_routing_key(AgentId)),
    bind_q(Q, Ids, T);
bind_q(Q, {AccountId, AgentId, _, Status}=Ids, ['status'|T]) ->
    kz_amqp_util:bind_q_to_kapps(Q, agent_status_routing_key(AccountId, AgentId, Status)),
    bind_q(Q, Ids, T);
bind_q(Q, {AccountId, AgentId, _, _}=Ids, ['fsm_shared'|T]) ->
    kz_amqp_util:bind_q_to_kapps(Q, fsm_shared_routing_key(AccountId, AgentId)),
    bind_q(Q, Ids, T);
bind_q(Q, {AccountId, AgentId, _, _}=Ids, ['sync'|T]) ->
    kz_amqp_util:bind_q_to_kapps(Q, sync_req_routing_key(AccountId, AgentId)),
    bind_q(Q, Ids, T);
bind_q(Q, {<<"*">>, <<"*">>, CallId, _}=Ids, ['stats_req'|T]) ->
    kz_amqp_util:bind_q_to_kapps(Q, stats_req_routing_key('undefined', 'undefined', CallId)),
    bind_q(Q, Ids, T);
bind_q(Q, {AccountId, <<"*">>, <<"*">>, _}=Ids, ['stats_req'|T]) ->
    kz_amqp_util:bind_q_to_kapps(Q, stats_req_routing_key(AccountId)),
    bind_q(Q, Ids, T);
bind_q(Q, {AccountId, AgentId, <<"*">>, _}=Ids, ['stats_req'|T]) ->
    kz_amqp_util:bind_q_to_kapps(Q, stats_req_routing_key(AccountId, AgentId)),
    bind_q(Q, Ids, T);
bind_q(Q, Ids, [_|T]) -> bind_q(Q, Ids, T);
bind_q(_, _, []) -> 'ok'.

-spec unbind_q(binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    AgentId = props:get_value('agent_id', Props, <<"*">>),
    AccountId = props:get_value('account_id', Props, <<"*">>),
    CallId = props:get_value('callid', Props, <<"*">>),
    Status = props:get_value('status', Props, <<"*">>),

    unbind_q(Q, {AccountId, AgentId, CallId, Status}, props:get_value('restrict_to', Props)).

-spec unbind_q(binary(), {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}, 'undefined' | list()) -> 'ok'.
unbind_q(Q, {AccountId, AgentId, _, Status}, 'undefined') ->
    _ = kz_amqp_util:unbind_q_from_kapps(Q, agent_status_routing_key(AccountId, AgentId, Status)),
    _ = kz_amqp_util:unbind_q_from_kapps(Q, fsm_shared_routing_key(AccountId, AgentId)),
    _ = kz_amqp_util:unbind_q_from_kapps(Q, sync_req_routing_key(AccountId, AgentId)),
    kz_amqp_util:unbind_q_from_kapps(Q, stats_req_routing_key(AccountId));
unbind_q(Q, {_, AgentId, _, _}=Ids, ['member_connect_win'|T]) ->
    kz_amqp_util:unbind_q_from_callmgr(Q, member_connect_win_routing_key(AgentId)),
    unbind_q(Q, Ids, T);
unbind_q(Q, {_, AgentId, _, _}=Ids, ['member_connect_satisfied'|T]) ->
    kz_amqp_util:unbind_q_from_callmgr(Q, member_connect_satisfied_routing_key(AgentId)),
    unbind_q(Q, Ids, T);
unbind_q(Q, {AccountId, AgentId, _, Status}=Ids, ['status'|T]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(Q, agent_status_routing_key(AccountId, AgentId, Status)),
    unbind_q(Q, Ids, T);
unbind_q(Q, {AccountId, AgentId, _, _}=Ids, ['fsm_shared'|T]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(Q, fsm_shared_routing_key(AccountId, AgentId)),
    unbind_q(Q, Ids, T);
unbind_q(Q, {AccountId, AgentId, _, _}=Ids, ['sync'|T]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(Q, sync_req_routing_key(AccountId, AgentId)),
    unbind_q(Q, Ids, T);
unbind_q(Q, {<<"*">>, <<"*">>, CallId, _}=Ids, ['stats_req'|T]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(Q, stats_req_routing_key('undefined', 'undefined', CallId)),
    unbind_q(Q, Ids, T);
unbind_q(Q, {AccountId, <<"*">>, <<"*">>, _}=Ids, ['stats'|T]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(Q, stats_req_routing_key(AccountId)),
    unbind_q(Q, Ids, T);
unbind_q(Q, {AccountId, AgentId, <<"*">>, _}=Ids, ['stats'|T]) ->
    _ = kz_amqp_util:unbind_q_from_kapps(Q, stats_req_routing_key(AccountId, AgentId)),
    unbind_q(Q, Ids, T);
unbind_q(Q, Ids, [_|T]) -> unbind_q(Q, Ids, T);
unbind_q(_, _, []) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc declare the exchanges used by this API
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

%%------------------------------------------------------------------------------
%% Publishers for convenience
%%------------------------------------------------------------------------------
-spec publish_sync_req(kz_term:api_terms()) -> 'ok'.
publish_sync_req(JObj) ->
    publish_sync_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_sync_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_sync_req(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?SYNC_REQ_VALUES, fun sync_req/1),
    kz_amqp_util:kapps_publish(sync_req_routing_key(API), Payload, ContentType).

-spec publish_sync_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_sync_resp(Q, JObj) ->
    publish_sync_resp(Q, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_sync_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_sync_resp('undefined', _, _) -> {'error', 'no_destination'};
publish_sync_resp(Q, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?SYNC_RESP_VALUES, fun sync_resp/1),
    kz_amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_stats_req(kz_term:api_terms()) -> 'ok'.
publish_stats_req(JObj) ->
    publish_stats_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_stats_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_stats_req(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATS_REQ_VALUES, fun stats_req/1),
    kz_amqp_util:kapps_publish(stats_req_publish_key(API), Payload, ContentType).

-spec publish_stats_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_stats_resp(Q, JObj) ->
    publish_stats_resp(Q, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_stats_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_stats_resp(Q, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?STATS_RESP_VALUES, fun stats_resp/1),
    kz_amqp_util:targeted_publish(Q, Payload, ContentType).

-spec publish_login(kz_term:api_terms()) -> 'ok'.
publish_login(JObj) ->
    publish_login(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_login(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_login(API, ContentType) ->
    {'ok', Payload} = login((API1 = kz_api:prepare_api_payload(API, ?LOGIN_VALUES))),
    kz_amqp_util:kapps_publish(agent_status_routing_key(API1), Payload, ContentType).

-spec publish_logout(kz_term:api_terms()) -> 'ok'.
publish_logout(JObj) ->
    publish_logout(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_logout(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_logout(API, ContentType) ->
    {'ok', Payload} = logout((API1 = kz_api:prepare_api_payload(API, ?LOGOUT_VALUES))),
    kz_amqp_util:kapps_publish(agent_status_routing_key(API1), Payload, ContentType).

-spec publish_login_queue(kz_term:api_terms()) -> 'ok'.
publish_login_queue(JObj) ->
    publish_login_queue(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_login_queue(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_login_queue(API, ContentType) ->
    {'ok', Payload} = login_queue((API1 = kz_api:prepare_api_payload(API, ?LOGIN_QUEUE_VALUES))),
    kz_amqp_util:kapps_publish(agent_status_routing_key(API1), Payload, ContentType).

-spec publish_logout_queue(kz_term:api_terms()) -> 'ok'.
publish_logout_queue(JObj) ->
    publish_logout_queue(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_logout_queue(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_logout_queue(API, ContentType) ->
    {'ok', Payload} = logout_queue((API1 = kz_api:prepare_api_payload(API, ?LOGOUT_QUEUE_VALUES))),
    kz_amqp_util:kapps_publish(agent_status_routing_key(API1), Payload, ContentType).

-spec publish_pause(kz_term:api_terms()) -> 'ok'.
publish_pause(JObj) ->
    publish_pause(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_pause(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_pause(API, ContentType) ->
    {'ok', Payload} = pause((API1 = kz_api:prepare_api_payload(API, ?PAUSE_VALUES))),
    kz_amqp_util:kapps_publish(agent_status_routing_key(API1), Payload, ContentType).

-spec publish_resume(kz_term:api_terms()) -> 'ok'.
publish_resume(JObj) ->
    publish_resume(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resume(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_resume(API, ContentType) ->
    {'ok', Payload} = resume((API1 = kz_api:prepare_api_payload(API, ?RESUME_VALUES))),
    kz_amqp_util:kapps_publish(agent_status_routing_key(API1), Payload, ContentType).

-spec publish_end_wrapup(kz_term:api_terms()) -> 'ok'.
publish_end_wrapup(JObj) ->
    publish_end_wrapup(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_end_wrapup(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_end_wrapup(API, ContentType) ->
    {'ok', Payload} = end_wrapup((API1 = kz_api:prepare_api_payload(API, ?END_WRAPUP_VALUES))),
    kz_amqp_util:kapps_publish(agent_status_routing_key(API1), Payload, ContentType).

-spec publish_restart(kz_term:api_terms()) -> 'ok'.
publish_restart(JObj) ->
    publish_restart(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_restart(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_restart(API, ContentType) ->
    {'ok', Payload} = restart((API1 = kz_api:prepare_api_payload(API, ?RESTART_VALUES))),
    kz_amqp_util:kapps_publish(agent_status_routing_key(API1), Payload, ContentType).

-spec publish_login_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_login_resp(RespQ, JObj) ->
    publish_login_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_login_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_login_resp(RespQ, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?LOGIN_RESP_VALUES, fun login_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_shared_originate_failure(kz_term:api_terms()) -> 'ok'.
publish_shared_originate_failure(JObj) ->
    publish_shared_originate_failure(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_shared_originate_failure(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_shared_originate_failure(API, ContentType) ->
    {'ok', Payload} = shared_originate_failure((API1 = kz_api:prepare_api_payload(API, ?SHARED_FAILURE_VALUES))),
    kz_amqp_util:kapps_publish(fsm_shared_routing_key(API1), Payload, ContentType).

-spec publish_shared_call_id(kz_term:api_terms()) -> 'ok'.
publish_shared_call_id(JObj) ->
    publish_shared_call_id(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_shared_call_id(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_shared_call_id(API, ContentType) ->
    {'ok', Payload} = shared_call_id((API1 = kz_api:prepare_api_payload(API, ?SHARED_CALL_ID_VALUES))),
    kz_amqp_util:kapps_publish(fsm_shared_routing_key(API1), Payload, ContentType).
