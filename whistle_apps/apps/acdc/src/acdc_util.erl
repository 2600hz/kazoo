%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_util).

-export([get_endpoints/2
         ,bind_to_call_events/1, bind_to_call_events/2
         ,unbind_from_call_events/1, unbind_from_call_events/2
         ,unbind_from_cdr/1
         ,agents_in_queue/2
         ,agent_status/2, agent_status/3
         ,update_agent_status/3, update_agent_status/4
         ,agent_devices/2
         ,proc_id/0, proc_id/1, proc_id/2
         ,queue_presence_update/2
         ,agent_presence_update/2
         ,presence_update/3
         ,send_cdr/2
        ]).

-include("acdc.hrl").

queue_presence_update(AcctId, QueueId) ->
    case wapi_acdc_queue:queue_size(AcctId, QueueId) of
        0 -> presence_update(AcctId, QueueId, ?PRESENCE_GREEN);
        N when is_integer(N), N > 0 -> presence_update(AcctId, QueueId, ?PRESENCE_RED_FLASH);
        _N -> lager:debug("queue size for ~s(~s): ~p", [QueueId, AcctId, _N])
    end.

agent_presence_update(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> presence_update(AcctId, AgentId, ?PRESENCE_RED_SOLID);
        P when is_pid(P) -> presence_update(AcctId, AgentId, ?PRESENCE_GREEN)
    end.

presence_update(AcctId, QueueId, State) ->
    AcctDb = wh_util:format_account_id(AcctId, 'encoded'),
    {'ok', AcctDoc} = couch_mgr:open_cache_doc(AcctDb, AcctId),
    To = <<QueueId/binary, "@", (wh_json:get_value(<<"realm">>, AcctDoc))/binary>>,

    lager:debug("sending presence update '~s' to '~s'", [State, To]),
    whapps_call_command:presence(State, To).

-spec send_cdr(ne_binary(), wh_json:object()) -> 'ok'.
send_cdr(Url, JObj) ->
    case ibrowse:send_req(wh_util:to_list(Url)
                          ,[{"Content-Type", "application/json"}]
                          ,'post', wh_json:encode(JObj)
                         ) of
        {'ok', _StatusCode, _RespHeaders, _RespBody} ->
            lager:debug("cdr server at ~s responded with a ~s: ~s", [Url, _StatusCode, _RespBody]);
        _Else ->
            lager:debug("sending cdr to server at ~s caused error: ~p", [Url, _Else])
    end.

%% Returns the list of agents configured for the queue
-spec agents_in_queue(ne_binary(), ne_binary()) -> wh_json:json_strings().
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
    AcctDb = whapps_call:account_db(Call),
    get_endpoints(Call
                  ,couch_mgr:get_results(AcctDb
                                         ,<<"cf_attributes/owned">>
                                         ,[{'key', [AgentId, <<"device">>]}]
                                        )
                 );
get_endpoints(_Call, {'ok', []}) -> [];
get_endpoints(_Call, {'error', _E}) -> [];
get_endpoints(Call, {'ok', Devices}) ->
    EPDocs = [EPDoc
              || Device <- Devices,
                 (EPDoc = get_endpoint(Call, wh_json:get_value(<<"id">>, Device))) =/= 'undefined',
                 wh_json:is_true(<<"enabled">>, EPDoc, 'false')
             ],

    lists:foldl(fun(EPDoc, Acc) ->
                        case cf_endpoint:build(EPDoc, Call) of
                            {'ok', EP} -> EP ++ Acc;
                            {'error', _} -> Acc
                        end
                end, [], EPDocs).

-spec get_endpoint(whapps_call:call(), ne_binary()) -> api_object().
get_endpoint(Call, ?NE_BINARY = EndpointId) ->
    case couch_mgr:open_doc(whapps_call:account_db(Call), EndpointId) of
        {'ok', JObj} -> JObj;
        {'error', _R} -> 'undefined'
    end.

%% Handles subscribing/unsubscribing from call events
-spec bind_to_call_events(api_binary() | whapps_call:call()) -> 'ok'.
-spec bind_to_call_events(api_binary() | whapps_call:call(), api_binary()) -> 'ok'.

-spec unbind_from_call_events(api_binary() | whapps_call:call()) -> 'ok'.
-spec unbind_from_call_events(api_binary() | whapps_call:call(), api_binary()) -> 'ok'.
-spec unbind_from_cdr(api_binary()) -> 'ok'.

bind_to_call_events('undefined') -> 'ok';
bind_to_call_events(?NE_BINARY = CallId) -> bind(CallId, ['events', 'error']);
bind_to_call_events(Call) -> bind_to_call_events(whapps_call:call_id(Call)).

bind_to_call_events('undefined', _) -> 'ok';
bind_to_call_events(Call, 'undefined') -> bind_to_call_events(Call);
bind_to_call_events(?NE_BINARY = CallId, _Url) -> bind(CallId, ['events', 'error', 'cdr']);
bind_to_call_events(Call, Url) -> bind_to_call_events(whapps_call:call_id(Call), Url).

bind(CallId, Restrict) ->
    gen_listener:add_binding(self(), 'call', [{'callid', CallId}
                                              ,{'restrict_to', Restrict}
                                             ]).

unbind_from_call_events('undefined') -> 'ok';
unbind_from_call_events(?NE_BINARY = CallId) -> unbind(CallId, ['events', 'error']);
unbind_from_call_events(Call) -> unbind_from_call_events(whapps_call:call_id(Call)).

unbind_from_call_events('undefined', _) -> 'ok';
unbind_from_call_events(Call, 'undefined') -> unbind_from_call_events(Call);
unbind_from_call_events(?NE_BINARY = CallId, _Url) -> unbind(CallId, ['events', 'error', 'cdr']);
unbind_from_call_events(Call, Url) -> unbind_from_call_events(whapps_call:call_id(Call), Url).

unbind_from_cdr('undefined') -> 'ok';
unbind_from_cdr(CallId) -> unbind(CallId, ['cdr']).

unbind(CallId, Restrict) ->
    gen_listener:rm_binding(self(), 'call', [{'callid', CallId}
                                             ,{'restrict_to', Restrict}
                                            ]).

-spec agent_status(ne_binary(), ne_binary()) -> ne_binary().
-spec agent_status(ne_binary(), ne_binary(), boolean()) -> ne_binary() | wh_json:object().
agent_status(?NE_BINARY = AcctId, AgentId) ->
    agent_status(AcctId, AgentId, 'false').
agent_status(?NE_BINARY = AcctId, AgentId, ReturnDoc) ->
    Opts = [{'endkey', [AgentId, 0]}
            ,{'startkey', [AgentId, wh_json:new()]}
            ,{'limit', 1}
            ,{'r', 2}
            ,'descending'
            | case ReturnDoc of 'true' -> ['include_docs']; 'false' -> [] end
           ],

    Key = case ReturnDoc of 'true' -> <<"doc">>; 'false' -> <<"value">> end,

    case couch_mgr:get_results(acdc_stats:db_name(AcctId), <<"agent_stats/status_log">>, Opts) of
        {'ok', []} -> <<"logout">>;
        {'error', _E} -> <<"logout">>;
        {'ok', [StatusJObj|_]} -> wh_json:get_value(Key, StatusJObj)
    end.

update_agent_status(AcctId, AgentId, Status) ->
    update_agent_status(AcctId, AgentId, Status, []).
update_agent_status(?NE_BINARY = AcctId, AgentId, Status, Options) ->
    Doc =
        wh_doc:update_pvt_parameters(
          wh_json:from_list(
            props:filter_undefined(
              [{<<"agent_id">>, AgentId}
               ,{<<"status">>, Status}
               ,{<<"pvt_type">>, <<"agent_partial">>}
               ,{<<"timestamp">>, wh_util:current_tstamp()}
               | Options
              ]))
          ,AcctId),
    couch_mgr:save_doc(acdc_stats:db_name(AcctId), Doc).

-spec proc_id() -> ne_binary().
-spec proc_id(pid()) -> ne_binary().
-spec proc_id(pid(), atom() | ne_binary()) -> ne_binary().
proc_id() -> proc_id(self()).
proc_id(Pid) -> proc_id(Pid, node()).
proc_id(Pid, Node) -> list_to_binary([wh_util:to_binary(Node), "-", pid_to_list(Pid)]).
