%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_util).

-export([get_endpoints/2
         ,bind_to_call_events/1, bind_to_call_events/2
         ,unbind_from_call_events/1
         ,unbind_from_call_events/2
         ,agents_in_queue/2
         ,agent_devices/2
         ,proc_id/0, proc_id/1, proc_id/2
         ,queue_presence_update/2
         ,agent_presence_update/2
         ,presence_update/3, presence_update/4
         ,send_cdr/2
        ]).

-include("acdc.hrl").

-define(CALL_EVENT_RESTRICTIONS, ['CHANNEL_CREATE'
                                  ,'CHANNEL_ANSWER'
                                  ,'CHANNEL_BRIDGE', 'CHANNEL_UNBRIDGE'
                                  ,'LEG_CREATED', 'LEG_DESTROYED'
                                  ,'CHANNEL_DESTROY'
                                  ,'DTMF'
                                  ,'CHANNEL_EXECUTE_COMPLETE'
                                  ,'usurp_control'
                                 ]).

-spec queue_presence_update(ne_binary(), ne_binary()) -> 'ok'.
queue_presence_update(AcctId, QueueId) ->
    case wapi_acdc_queue:queue_size(AcctId, QueueId) of
        0 -> presence_update(AcctId, QueueId, ?PRESENCE_GREEN);
        N when is_integer(N), N > 0 -> presence_update(AcctId, QueueId, ?PRESENCE_RED_FLASH);
        _N -> lager:debug("queue size for ~s(~s): ~p", [QueueId, AcctId, _N])
    end.

-spec agent_presence_update(ne_binary(), ne_binary()) -> 'ok'.
agent_presence_update(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> presence_update(AcctId, AgentId, ?PRESENCE_RED_SOLID);
        P when is_pid(P) -> presence_update(AcctId, AgentId, ?PRESENCE_GREEN)
    end.

-spec presence_update(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
-spec presence_update(ne_binary(), ne_binary(), ne_binary(), api_binary()) -> 'ok'.
presence_update(AcctId, PresenceId, State) ->
    presence_update(AcctId, PresenceId, State, 'undefined').
presence_update(AcctId, PresenceId, State, CallId) ->
    {'ok', AcctDoc} = kz_account:fetch(AcctId),
    To = <<PresenceId/binary, "@", (wh_json:get_value(<<"realm">>, AcctDoc))/binary>>,

    lager:debug("sending presence update '~s' to '~s'", [State, To]),
    whapps_call_command:presence(State, To, CallId).

-spec send_cdr(ne_binary(), wh_json:object()) -> 'ok'.
send_cdr(Url, JObj) ->
    send_cdr(Url, JObj, 3).
send_cdr('undefined', _JObj, _Retries) ->
    lager:debug("no cdr URI defined");
send_cdr(Url, _JObj, 0) ->
    lager:debug("trying to send cdr to ~s failed retry count", [Url]);
send_cdr(Url, JObj, Retries) ->
    case ibrowse:send_req(wh_util:to_list(Url)
                          ,[{"Content-Type", "application/json"}]
                          ,'post', wh_json:encode(JObj)
                          ,1000
                         ) of
        {'ok', _StatusCode, _RespHeaders, _RespBody} ->
            lager:debug("cdr server at ~s responded with a ~s: ~s", [Url, _StatusCode, _RespBody]);
        _Else ->
            lager:debug("sending cdr to server at ~s caused error: ~p", [Url, _Else]),
            send_cdr(Url, JObj, Retries-1)
    end.

%% Returns the list of agents configured for the queue
-spec agents_in_queue(ne_binary(), ne_binary()) -> wh_json:keys().
agents_in_queue(AcctDb, QueueId) ->
    case couch_mgr:get_results(AcctDb, <<"queues/agents_listing">>, [{'key', QueueId}]) of
        {'ok', []} -> [];
        {'error', _E} -> lager:debug("failed to lookup agents for ~s: ~p", [QueueId, _E]), [];
        {'ok', As} -> [wh_json:get_value(<<"value">>, A) || A <- As]
    end.

-spec agent_devices(ne_binary(), ne_binary()) -> wh_json:objects().
agent_devices(AcctDb, AgentId) ->
    case couch_mgr:get_results(AcctDb, <<"cf_attributes/owned">>, [{'key', [AgentId, <<"device">>]}
                                                                   ,'include_docs'
                                                                  ])
    of
        {'ok', Devices} -> [wh_json:get_value(<<"doc">>, Dev) || Dev <- Devices];
        {'error', _} -> []
    end.

-spec get_endpoints(whapps_call:call(), ne_binary() | couch_mgr:get_results_return()) ->
                           wh_json:objects().
get_endpoints(Call, ?NE_BINARY = AgentId) ->
    cf_user:get_endpoints(AgentId, [], Call).

%% Handles subscribing/unsubscribing from call events
-spec bind_to_call_events(api_binary() | {api_binary(), _} | whapps_call:call()) -> 'ok'.
bind_to_call_events(Call) ->
    bind_to_call_events(Call, self()).

-spec bind_to_call_events(api_binary() | {api_binary(), _} | whapps_call:call(), pid()) -> 'ok'.
bind_to_call_events('undefined', _) -> 'ok';
bind_to_call_events(?NE_BINARY = CallId, Pid) ->
    gen_listener:add_binding(Pid, 'call', [{'callid', CallId}]);
bind_to_call_events({CallId, _}, Pid) -> bind_to_call_events(CallId, Pid);
bind_to_call_events(Call, Pid) -> bind_to_call_events(whapps_call:call_id(Call), Pid).

-spec unbind_from_call_events(api_binary() | {api_binary(), _} | whapps_call:call()) -> 'ok'.
unbind_from_call_events(Call) ->
    unbind_from_call_events(Call, self()).

-spec unbind_from_call_events(api_binary() | {api_binary(), _} | whapps_call:call(), pid()) -> 'ok'.
unbind_from_call_events('undefined', _Pid) -> 'ok';
unbind_from_call_events(?NE_BINARY = CallId, Pid) ->
    gen_listener:rm_binding(Pid, 'call', [{'callid', CallId}]);
unbind_from_call_events({CallId, _}, Pid) -> unbind_from_call_events(CallId, Pid);
unbind_from_call_events(Call, Pid) -> unbind_from_call_events(whapps_call:call_id(Call), Pid).

-spec proc_id() -> ne_binary().
-spec proc_id(pid()) -> ne_binary().
-spec proc_id(pid(), atom() | ne_binary()) -> ne_binary().
proc_id() -> proc_id(self()).
proc_id(Pid) -> proc_id(Pid, node()).
proc_id(Pid, Node) -> list_to_binary([wh_util:to_binary(Node), "-", pid_to_list(Pid)]).
