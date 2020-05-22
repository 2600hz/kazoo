%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_util).

-export([get_endpoints/2
        ,bind_to_call_events/1, bind_to_call_events/2
        ,b_bind_to_call_events/2
        ,unbind_from_call_events/1
        ,unbind_from_call_events/2
        ,agents_in_queue/2
        ,agent_devices/2
        ,proc_id/0, proc_id/1, proc_id/2
        ,queue_presence_update/2
        ,agent_presence_update/2
        ,presence_update/3, presence_update/4
        ,send_cdr/2
        ,caller_id/1
        ,hangup_cause/1
        ,max_priority/2
        ]).

-include("acdc.hrl").

-define(CALL_EVENT_RESTRICTIONS, ['CHANNEL_CREATE'
                                 ,'CHANNEL_ANSWER'
                                 ,'CHANNEL_BRIDGE', 'CHANNEL_UNBRIDGE'
                                 ,'LEG_CREATED', 'LEG_DESTROYED'
                                 ,'CHANNEL_DESTROY'
                                 ,'DTMF'
                                 ,'CHANNEL_EXECUTE_COMPLETE'
                                 ,'PLAYBACK_STOP'
                                 ,'usurp_control'
                                 ]).

-spec queue_presence_update(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
queue_presence_update(AccountId, QueueId) ->
    case kapi_acdc_queue:queue_size(AccountId, QueueId) of
        0 -> presence_update(AccountId, QueueId, ?PRESENCE_GREEN);
        N when is_integer(N), N > 0 -> presence_update(AccountId, QueueId, ?PRESENCE_RED_FLASH);
        _N -> lager:debug("queue size for ~s(~s): ~p", [QueueId, AccountId, _N])
    end.

-spec agent_presence_update(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_presence_update(AccountId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> presence_update(AccountId, AgentId, ?PRESENCE_RED_SOLID);
        P when is_pid(P) -> presence_update(AccountId, AgentId, ?PRESENCE_GREEN)
    end.

-spec presence_update(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
presence_update(AccountId, PresenceId, State) ->
    presence_update(AccountId, PresenceId, State, kz_term:to_hex_binary(crypto:hash('md5', PresenceId))).

-spec presence_update(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
presence_update(AccountId, PresenceId, State, CallId) ->
    {'ok', AcctDoc} = kzd_accounts:fetch(AccountId),
    To = <<PresenceId/binary, "@", (kz_json:get_value(<<"realm">>, AcctDoc))/binary>>,

    lager:debug("sending presence update '~s' to '~s'", [State, To]),
    kapps_call_command:presence(State, To, CallId).

-spec send_cdr(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_cdr(Url, JObj) ->
    send_cdr(Url, JObj, 3).
send_cdr('undefined', _JObj, _Retries) ->
    lager:debug("no cdr URI defined");
send_cdr(Url, _JObj, 0) ->
    lager:debug("trying to send cdr to ~s failed retry count", [Url]);
send_cdr(Url, JObj, Retries) ->
    case kz_http:post(kz_term:to_list(Url)
                     ,[{"Content-Type", "application/json"}]
                     , kz_json:encode(JObj)
                     ,[{'timeout', 1000}]
                     ) of
        {'ok', _StatusCode, _RespHeaders, _RespBody} ->
            lager:debug("cdr server at ~s responded with a ~p: ~s", [Url, _StatusCode, _RespBody]);
        _Else ->
            lager:debug("sending cdr to server at ~s caused error: ~p", [Url, _Else]),
            send_cdr(Url, JObj, Retries-1)
    end.

%% Returns the list of agents configured for the queue
-spec agents_in_queue(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:path().
agents_in_queue(AcctDb, QueueId) ->
    case kz_datamgr:get_results(AcctDb, <<"queues/agents_listing">>, [{'key', QueueId}]) of
        {'ok', []} -> [];
        {'error', _E} -> lager:debug("failed to lookup agents for ~s: ~p", [QueueId, _E]), [];
        {'ok', As} -> [kz_json:get_value(<<"value">>, A) || A <- As]
    end.

-spec agent_devices(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:objects().
agent_devices(AcctDb, AgentId) ->
    case kz_datamgr:get_results(AcctDb, <<"attributes/owned">>, [{'key', [AgentId, <<"device">>]}
                                                                ,'include_docs'
                                                                ])
    of
        {'ok', Devices} -> [kz_json:get_value(<<"doc">>, Dev) || Dev <- Devices];
        {'error', _} -> []
    end.

-spec get_endpoints(kapps_call:call(), kz_term:ne_binary() | kazoo_data:get_results_return()) ->
          kz_json:objects().
get_endpoints(Call, ?NE_BINARY = AgentId) ->
    Params = kz_json:from_list([{<<"source">>, kz_term:to_binary(?MODULE)}
                               ,{<<"can_call_self">>, 'true'}
                               ]),
    kz_endpoints:by_owner_id(AgentId, Params, Call).

%% Handles subscribing/unsubscribing from call events
-spec bind_to_call_events(kz_term:api_binary() | {kz_term:api_binary(), any()} | kapps_call:call()) -> 'ok'.
bind_to_call_events(Call) ->
    bind_to_call_events(Call, self()).

-spec bind_to_call_events(kz_term:api_binary() | {kz_term:api_binary(), any()} | kapps_call:call(), pid()) -> 'ok'.
bind_to_call_events('undefined', _) -> 'ok';
bind_to_call_events(?NE_BINARY = CallId, Pid) ->
    gen_listener:add_binding(Pid, 'call', [{'callid', CallId}]);
bind_to_call_events({CallId, _}, Pid) -> bind_to_call_events(CallId, Pid);
bind_to_call_events(Call, Pid) -> bind_to_call_events(kapps_call:call_id(Call), Pid).

-spec b_bind_to_call_events(kz_term:api_binary(), pid()) -> 'ok'.
b_bind_to_call_events('undefined', _) -> 'ok';
b_bind_to_call_events(CallId, Pid) ->
    gen_listener:b_add_binding(Pid, 'call', [{'callid', CallId}]).

-spec unbind_from_call_events(kz_term:api_binary() | {kz_term:api_binary(), any()} | kapps_call:call()) -> 'ok'.
unbind_from_call_events(Call) ->
    unbind_from_call_events(Call, self()).

-spec unbind_from_call_events(kz_term:api_binary() | {kz_term:api_binary(), any()} | kapps_call:call(), pid()) -> 'ok'.
unbind_from_call_events('undefined', _Pid) -> 'ok';
unbind_from_call_events(?NE_BINARY = CallId, Pid) ->
    gen_listener:rm_binding(Pid, 'call', [{'callid', CallId}]),
    gen_listener:rm_binding(Pid, 'acdc_agent', [{'callid', CallId}
                                               ,{'restrict_to', ['stats_req']}
                                               ]);
unbind_from_call_events({CallId, _}, Pid) -> unbind_from_call_events(CallId, Pid);
unbind_from_call_events(Call, Pid) -> unbind_from_call_events(kapps_call:call_id(Call), Pid).

-spec proc_id() -> kz_term:ne_binary().
proc_id() -> proc_id(self()).

-spec proc_id(pid()) -> kz_term:ne_binary().
proc_id(Pid) -> proc_id(Pid, node()).

-spec proc_id(pid(), atom() | kz_term:ne_binary()) -> kz_term:ne_binary().
proc_id(Pid, Node) -> list_to_binary([kz_term:to_binary(Node), "-", pid_to_list(Pid)]).

-spec caller_id(kapps_call:call()) -> {kz_term:api_binary(), kz_term:api_binary()}.
caller_id(Call) ->
    CallerIdType = case kapps_call:inception(Call) of
                       'undefined' -> <<"internal">>;
                       _Else -> <<"external">>
                   end,
    kz_attributes:caller_id(CallerIdType, Call).

-spec hangup_cause(kz_json:object()) -> kz_term:ne_binary().
hangup_cause(JObj) ->
    case kz_json:get_value(<<"Hangup-Cause">>, JObj) of
        'undefined' -> <<"unknown">>;
        Cause -> Cause
    end.

-spec max_priority(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_integer().
max_priority(AccountDb, QueueId) ->
    case kz_datamgr:open_cache_doc(AccountDb, QueueId) of
        {'ok', QueueJObj} -> max_priority(QueueJObj);
        _ -> kapps_config:get_integer(?CONFIG_CAT, <<"default_queue_max_priority">>)
    end.

-spec max_priority(kz_json:object()) -> kz_term:api_integer().
max_priority(QueueJObj) ->
    case kz_json:get_integer_value(<<"max_priority">>, QueueJObj) of
        'undefined' -> kapps_config:get_integer(?CONFIG_CAT, <<"default_queue_max_priority">>);
        Priority -> Priority
    end.
