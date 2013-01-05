%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various call events, acdc events, etc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_handler).

%% Listener callbacks
-export([handle_status_update/2
         ,handle_sync_req/2
         ,handle_sync_resp/2
         ,handle_call_event/2
         ,handle_originate_resp/2
         ,handle_member_message/2
         ,handle_config_change/2
         ,handle_presence_probe/2
         ,handle_route_req/2
        ]).

-include("acdc.hrl").

-spec handle_status_update/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_status_update(JObj, _Props) ->
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    Timeout = wh_json:get_integer_value(<<"Time-Limit">>, JObj, whapps_config:get(<<"acdc">>, <<"default_agent_pause_timeout">>, 600)),

    case wh_json:get_value(<<"Event-Name">>, JObj) of
        <<"login">> ->
            true = wapi_acdc_agent:login_v(JObj),
            maybe_start_agent(AcctId, AgentId);
        <<"logout">> ->
            true = wapi_acdc_agent:logout_v(JObj),
            maybe_stop_agent(AcctId, AgentId);
        <<"pause">> ->
            true = wapi_acdc_agent:pause_v(JObj),
            maybe_pause_agent(AcctId, AgentId, Timeout);
        <<"resume">> ->
            true = wapi_acdc_agent:resume_v(JObj),
            maybe_resume_agent(AcctId, AgentId);
        Event -> maybe_agent_queue_change(AcctId, AgentId, Event
                                          ,wh_json:get_value(<<"Queue-ID">>, JObj)
                                         )
    end.

maybe_agent_queue_change(AcctId, AgentId, <<"login_queue">>, QueueId) ->
    update_agent(acdc_agents_sup:find_agent_supervisor(AcctId, AgentId)
                 ,QueueId
                 ,fun acdc_agent:add_acdc_queue/2
                );
maybe_agent_queue_change(AcctId, AgentId, <<"logout_queue">>, QueueId) ->
    update_agent(acdc_agents_sup:find_agent_supervisor(AcctId, AgentId)
                 ,QueueId
                 ,fun acdc_agent:rm_acdc_queue/2
                );
maybe_agent_queue_change(_AcctId, _AgentId, _Evt, _QueueId) ->
    lager:debug("unhandled evt: ~s for ~s", [_Evt, _QueueId]).

update_agent(undefined, _, _) -> ok;
update_agent(Super, Q, F) when is_pid(Super) -> 
    F(acdc_agent_sup:agent(Super), Q).

maybe_start_agent(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined ->
            lager:debug("agent ~s (~s) not found, starting", [AgentId, AcctId]),
            case couch_mgr:open_doc(wh_util:format_account_id(AcctId, encoded), AgentId) of
                {ok, AgentJObj} ->
                    {ok, _APid} = acdc_agents_sup:new(AgentJObj),
                    acdc_stats:agent_active(AcctId, AgentId),
                    lager:debug("started agent at ~p", [_APid]);
                {error, _E} ->
                    lager:debug("error opening agent doc: ~p", [_E])
            end;
        P when is_pid(P) ->
            lager:debug("agent ~s (~s) already running: supervisor ~p", [AgentId, AcctId, P])
    end.

maybe_stop_agent(AcctId, AgentId) ->
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_RED_SOLID),
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined ->
            lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AcctId]);
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) is logging out, stopping ~p", [AcctId, AgentId, P]),
            _ = acdc_agent_sup:stop(P),
            acdc_stats:agent_inactive(AcctId, AgentId)
    end.

maybe_pause_agent(AcctId, AgentId, Timeout) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined -> lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AcctId]);
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) is pausing for ~p", [AcctId, AgentId, Timeout]),
            acdc_agent_fsm:pause(acdc_agent_sup:fsm(P), Timeout)
    end.

maybe_resume_agent(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined -> lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AcctId]);
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) is resuming: ~p", [AcctId, AgentId, P]),
            acdc_agent_fsm:resume(acdc_agent_sup:fsm(P))
    end.

-spec handle_sync_req/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_sync_req(JObj, Props) ->
    true = wapi_acdc_agent:sync_req_v(JObj),
    acdc_agent_fsm:sync_req(props:get_value(fsm_pid, Props), JObj).

-spec handle_sync_resp/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_sync_resp(JObj, Props) ->
    true = wapi_acdc_agent:sync_resp_v(JObj),
    acdc_agent_fsm:sync_resp(props:get_value(fsm_pid, Props), JObj).

-spec handle_call_event/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    FSM = props:get_value(fsm_pid, Props),
    case wapi_call:event_v(JObj) of
        true ->
            {Cat, Name} = wh_util:get_event_type(JObj),
            acdc_agent_fsm:call_event(FSM, Cat, Name, JObj);
        false ->
            true = wh_api:error_resp_v(JObj),

            case wh_json:get_value([<<"Request">>, <<"Event-Name">>], JObj) of
                <<"originate_req">> ->
                    acdc_agent_fsm:originate_failed(FSM, JObj);
                _ -> ok
            end
    end.

handle_originate_resp(JObj, Props) ->
    true = wapi_resource:originate_resp_v(JObj) orelse wapi_resource:originate_started_v(JObj),
    acdc_agent_fsm:originate_resp(props:get_value(fsm_pid, Props), JObj).

-spec handle_member_message/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
-spec handle_member_message/3 :: (wh_json:object(), wh_proplist(), ne_binary()) -> 'ok'.
handle_member_message(JObj, Props) ->
    handle_member_message(JObj, Props, wh_json:get_value(<<"Event-Name">>, JObj)).

handle_member_message(JObj, Props, <<"connect_req">>) ->
    true = wapi_acdc_queue:member_connect_req_v(JObj),
    acdc_agent_fsm:member_connect_req(props:get_value(fsm_pid, Props), JObj);
handle_member_message(JObj, Props, <<"connect_win">>) ->
    true = wapi_acdc_queue:member_connect_win_v(JObj),
    acdc_agent_fsm:member_connect_win(props:get_value(fsm_pid, Props), JObj);
handle_member_message(_, _, EvtName) ->
    lager:debug("not handling member event ~s", [EvtName]).

handle_config_change(JObj, _Props) ->
    true = wapi_conf:doc_update_v(JObj),
    handle_agent_change(wh_json:get_value(<<"Doc">>, JObj)
                        ,wh_json:get_value(<<"pvt_account_id">>, JObj)
                        ,wh_json:get_value(<<"_id">>, JObj)
                        ,wh_json:get_value(<<"Event-Name">>, JObj)
                       ).
handle_agent_change(JObj, AcctId, AgentId, <<"doc_created">>) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined -> acdc_agents_sup:new(JObj);
        P when is_pid(P) -> ok
    end;
handle_agent_change(JObj, AcctId, AgentId, <<"doc_edited">>) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined -> acdc_agents_sup:new(JObj);
        P when is_pid(P) -> acdc_agent_fsm:refresh(acdc_agent_sup:fsm(P), JObj)
    end;
handle_agent_change(_JObj, AcctId, AgentId, <<"doc_deleted">>) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined -> ok;
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) has been deleted, stopping ~p", [AcctId, AgentId, P]),
            _ = acdc_agent_sup:stop(P),
            acdc_stats:agent_inactive(AcctId, AgentId)
    end.

handle_presence_probe(JObj, _Props) ->
    true = wapi_notifications:presence_probe_v(JObj),

    FromRealm = wh_json:get_value(<<"From-Realm">>, JObj),
    case whapps_util:get_account_by_realm(FromRealm) of
        {ok, AcctDb} -> maybe_respond_to_presence_probe(JObj, AcctDb);
        _ -> ok
    end.

maybe_respond_to_presence_probe(JObj, AcctDb) ->
    case wh_json:get_value(<<"To-User">>, JObj) of
        undefined -> lager:debug("no to-user found on json: ~p", [JObj]);
        AgentId ->
            case couch_mgr:open_doc(AcctDb, AgentId) of
                {ok, Doc} ->
                    AcctId = wh_util:format_account_id(AcctDb, raw),
                    lager:debug("maybe looking for probe for agent ~s(~s)", [AgentId, AcctId]),
                    maybe_update_probe(JObj, AcctId, AgentId, wh_json:get_value(<<"pvt_type">>, Doc));
                _ -> ok
            end
    end.

maybe_update_probe(JObj, AcctId, AgentId, <<"user">>) ->
    update_probe(JObj, acdc_agents_sup:find_agent_supervisor(AcctId, AgentId));
maybe_update_probe(_, _, _, _) ->
    ok.

update_probe(JObj, undefined) ->
    lager:debug("no agent present, redify!"),
    send_probe(JObj, ?PRESENCE_RED_SOLID);
update_probe(JObj, P) when is_pid(P) ->
    lager:debug("agent is active with super: ~p", [P]),
    send_probe(JObj, ?PRESENCE_GREEN).

send_probe(JObj, State) ->
    To = wh_json:get_value(<<"To">>, JObj),
    PresenceUpdate =
        [{<<"State">>, State}
         ,{<<"Presence-ID">>, To}
         ,{<<"Call-ID">>, wh_util:to_hex_binary(crypto:md5(To))}
         ,{<<"Subscription-Call-ID">>, wh_json:get_ne_value(<<"Subscription-Call-ID">>, JObj)}
         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    wapi_notifications:publish_presence_update(PresenceUpdate).

handle_route_req(JObj, Props) ->
    _ = wh_util:put_callid(JObj),
    Call = whapps_call:from_route_req(JObj),

    Owner = whapps_call:owner_id(Call),
    Agent = props:get_value(agent_id, Props),

    case Owner =:= Agent of
        true -> acdc_agent_fsm:route_req(props:get_value(fsm_pid, Props), Call);
        false -> ok
    end.
