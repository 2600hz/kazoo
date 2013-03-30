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
         ,handle_new_channel/2
         ,handle_cdr/2
         ,handle_originate_resp/2
         ,handle_member_message/2
         ,handle_agent_message/2
         ,handle_config_change/2
         ,handle_presence_probe/2
         ,handle_route_req/2

         ,maybe_stop_agent/3
        ]).

-include("acdc.hrl").

-spec handle_status_update(wh_json:object(), wh_proplist()) -> 'ok'.
handle_status_update(JObj, _Props) ->
    _ = wh_util:put_callid(JObj),
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    Timeout = wh_json:get_integer_value(<<"Time-Limit">>, JObj
                                        ,whapps_config:get(<<"acdc">>, <<"default_agent_pause_timeout">>, 600)
                                       ),

    lager:debug("status update recv for ~s (~s)", [AgentId, AcctId]),

    case wh_json:get_value(<<"Event-Name">>, JObj) of
        <<"login">> ->
            'true' = wapi_acdc_agent:login_v(JObj),
            maybe_start_agent(AcctId, AgentId);
        <<"logout">> ->
            'true' = wapi_acdc_agent:logout_v(JObj),
            maybe_stop_agent(AcctId, AgentId);
        <<"pause">> ->
            'true' = wapi_acdc_agent:pause_v(JObj),
            maybe_pause_agent(AcctId, AgentId, Timeout);
        <<"resume">> ->
            'true' = wapi_acdc_agent:resume_v(JObj),
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

update_agent('undefined', _, _) -> 'ok';
update_agent(Super, Q, F) when is_pid(Super) ->
    F(acdc_agent_sup:agent(Super), Q).

maybe_start_agent(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' ->
            lager:debug("agent ~s (~s) not found, starting", [AgentId, AcctId]),
            case couch_mgr:open_doc(wh_util:format_account_id(AcctId, 'encoded'), AgentId) of
                {'ok', AgentJObj} ->
                    {'ok', _APid} = acdc_agents_sup:new(AgentJObj),
                    acdc_stats:agent_active(AcctId, AgentId),
                    lager:debug("started agent at ~p", [_APid]);
                {'error', _E} ->
                    lager:debug("error opening agent doc: ~p", [_E])
            end;
        P when is_pid(P) ->
            lager:debug("agent ~s (~s) already running: supervisor ~p", [AgentId, AcctId, P])
    end.

maybe_stop_agent(LPid, AcctId, AgentId) ->
    _ = wh_amqp_channel:consumer_pid(LPid),
    maybe_stop_agent(AcctId, AgentId).
maybe_stop_agent(AcctId, AgentId) ->
    catch acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_RED_SOLID),
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' ->
            lager:debug("agent ~s(~s) not found, nothing to do", [AgentId, AcctId]);
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) is logging out, stopping ~p", [AgentId, AgentId, P]),
            acdc_stats:agent_inactive(AcctId, AgentId),

            case catch acdc_agent_sup:agent(P) of
                APid when is_pid(APid) -> acdc_agent:logout_agent(APid);
                _P -> lager:debug("failed to find agent listener for ~s: ~p", [AgentId, _P])
            end,

            _Stop = acdc_agent_sup:stop(P),
            lager:debug("supervisor ~p stopping agent: ~p", [P, _Stop])
    end.

maybe_pause_agent(AcctId, AgentId, Timeout) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AcctId]);
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) is pausing for ~p", [AcctId, AgentId, Timeout]),
            acdc_agent_fsm:pause(acdc_agent_sup:fsm(P), Timeout)
    end.

maybe_resume_agent(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AcctId]);
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) is resuming: ~p", [AcctId, AgentId, P]),
            acdc_agent_fsm:resume(acdc_agent_sup:fsm(P))
    end.

-spec handle_sync_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_sync_req(JObj, Props) ->
    'true' = wapi_acdc_agent:sync_req_v(JObj),
    acdc_agent_fsm:sync_req(props:get_value('fsm_pid', Props), JObj).

-spec handle_sync_resp(wh_json:object(), wh_proplist()) -> 'ok'.
handle_sync_resp(JObj, Props) ->
    'true' = wapi_acdc_agent:sync_resp_v(JObj),
    acdc_agent_fsm:sync_resp(props:get_value('fsm_pid', Props), JObj).

-spec handle_call_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    FSM = props:get_value('fsm_pid', Props),
    case wapi_call:event_v(JObj) of
        'true' ->
            {Cat, Name} = wh_util:get_event_type(JObj),
            acdc_agent_fsm:call_event(FSM, Cat, Name, JObj);
        'false' ->
            'true' = wh_api:error_resp_v(JObj),

            case wh_json:get_value([<<"Request">>, <<"Event-Name">>], JObj) of
                <<"originate_req">> -> acdc_agent_fsm:originate_failed(FSM, JObj);
                _ -> 'ok'
            end
    end.

handle_new_channel(JObj, _Props) ->
    'true' = wapi_call:new_channel_v(JObj),
    _ = wh_util:put_callid(JObj),
    handle_new_channel_acct(JObj, wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj)).

handle_new_channel_acct(_, 'undefined') -> 'ok';
handle_new_channel_acct(JObj, AcctId) ->
    [FromUser, _FromHost] = binary:split(wh_json:get_value(<<"From">>, JObj), <<"@">>),
    [ToUser, _ToHost] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
    [ReqUser, _ReqHost] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),

    CallId = wh_json:get_value(<<"Call-ID">>, JObj),

    lager:debug("new channel in acct ~s: from ~s to ~s(~s)", [AcctId, FromUser, ToUser, ReqUser]),

    gproc:send(?NEW_CHANNEL_REG(AcctId, FromUser), ?NEW_CHANNEL_FROM(CallId)),
    gproc:send(?NEW_CHANNEL_REG(AcctId, ToUser), ?NEW_CHANNEL_TO(CallId)),
    gproc:send(?NEW_CHANNEL_REG(AcctId, ReqUser), ?NEW_CHANNEL_TO(CallId)).

handle_cdr(JObj, Props) ->
    'true' = wapi_call:cdr_v(JObj),
    _ = wh_util:put_callid(JObj),

    Urls = props:get_value('cdr_urls', Props),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),

    case catch dict:fetch(CallId, Urls) of
        {'EXIT', _} -> lager:debug("no cdr url for call ~s", [CallId]);
        Url -> acdc_util:send_cdr(Url, JObj)
    end,
    acdc_agent:unbind_from_cdr(props:get_value('server', Props), CallId).

handle_originate_resp(JObj, Props) ->
    case wh_json:get_value(<<"Event-Name">>, JObj) of
        <<"originate_resp">> ->
            'true' = wapi_resource:originate_resp_v(JObj),
            acdc_agent_fsm:originate_resp(props:get_value('fsm_pid', Props), JObj);
        <<"originate_started">> ->
            'true' = wapi_resource:originate_started_v(JObj),
            acdc_agent_fsm:originate_started(props:get_value('fsm_pid', Props), JObj);
        <<"originate_uuid">> ->
            'true' = wapi_resource:originate_uuid_v(JObj),
            acdc_agent_fsm:originate_uuid(props:get_value('fsm_pid', Props), JObj)
    end.

-spec handle_member_message(wh_json:object(), wh_proplist()) -> 'ok'.
-spec handle_member_message(wh_json:object(), wh_proplist(), ne_binary()) -> 'ok'.
handle_member_message(JObj, Props) ->
    handle_member_message(JObj, Props, wh_json:get_value(<<"Event-Name">>, JObj)).

handle_member_message(JObj, Props, <<"connect_req">>) ->
    'true' = wapi_acdc_queue:member_connect_req_v(JObj),
    acdc_agent_fsm:member_connect_req(props:get_value('fsm_pid', Props), JObj);
handle_member_message(JObj, Props, <<"connect_win">>) ->
    'true' = wapi_acdc_queue:member_connect_win_v(JObj),
    acdc_agent_fsm:member_connect_win(props:get_value('fsm_pid', Props), JObj);
handle_member_message(_, _, EvtName) ->
    lager:debug("not handling member event ~s", [EvtName]).

-spec handle_agent_message(wh_json:object(), wh_proplist()) -> 'ok'.
-spec handle_agent_message(wh_json:object(), wh_proplist(), ne_binary()) -> 'ok'.
handle_agent_message(JObj, Props) ->
    handle_agent_message(JObj, Props, wh_json:get_value(<<"Event-Name">>, JObj)).

handle_agent_message(JObj, Props, <<"connect_timeout">>) ->
    'true' = wapi_acdc_queue:agent_timeout_v(JObj),
    acdc_agent_fsm:agent_timeout(props:get_value('fsm_pid', Props), JObj);
handle_agent_message(_, _, _EvtName) ->
    lager:debug("not handling agent event ~s", [_EvtName]).

handle_config_change(JObj, _Props) ->
    'true' = wapi_conf:doc_update_v(JObj),

    handle_change(JObj, wh_json:get_value(<<"Type">>, JObj)).

handle_change(JObj, <<"user">>) ->
    handle_agent_change(wh_json:get_value(<<"Database">>, JObj)
                        ,wh_json:get_value(<<"Account-ID">>, JObj)
                        ,wh_json:get_value(<<"ID">>, JObj)
                        ,wh_json:get_value(<<"Event-Name">>, JObj)
                       );
handle_change(JObj, <<"device">>) ->
    handle_device_change(wh_json:get_value(<<"Database">>, JObj)
                         ,wh_json:get_value(<<"Account-ID">>, JObj)
                         ,wh_json:get_value(<<"ID">>, JObj)
                         ,wh_json:get_value(<<"Rev">>, JObj)
                         ,wh_json:get_value(<<"Event-Name">>, JObj)
                        ).

handle_device_change(AccountDb, AccountId, DeviceId, Rev, Type) ->
    handle_device_change(AccountDb, AccountId, DeviceId, Rev, Type, 0).

handle_device_change(_AccountDb, _AccountId, DeviceId, Rev, _Type, Cnt) when Cnt > 3 ->
    lager:debug("retried ~p times to refresh endpoint ~s(~s), giving up", [Cnt, DeviceId, Rev]);
handle_device_change(AccountDb, AccountId, DeviceId, Rev, <<"doc_created">>, Cnt) ->
    case cf_endpoint:get(DeviceId, AccountDb) of
        {'ok', EP} ->
            case wh_json:get_value(<<"_rev">>, EP) of
                Rev ->
                    gproc:send(?ENDPOINT_UPDATE_REG(AccountId, DeviceId), ?ENDPOINT_CREATED(EP)),
                    gproc:send(?OWNER_UPDATE_REG(AccountId, wh_json:get_value(<<"owner_id">>, EP)), ?ENDPOINT_CREATED(EP));
                _OldRev ->
                    timer:sleep(250),
                    handle_device_change(AccountDb, AccountId, DeviceId, Rev, <<"doc_created">>, Cnt+1)
            end;
        _ -> lager:debug("ignoring the fact that device ~s was created", [DeviceId])
    end;
handle_device_change(AccountDb, AccountId, DeviceId, Rev, <<"doc_edited">>, Cnt) ->
    case cf_endpoint:get(DeviceId, AccountDb) of
        {'ok', EP} ->
            case wh_json:get_value(<<"_rev">>, EP) of
                Rev ->
                    gproc:send(?ENDPOINT_UPDATE_REG(AccountId, DeviceId), ?ENDPOINT_EDITED(EP)),
                    gproc:send(?OWNER_UPDATE_REG(AccountId, wh_json:get_value(<<"owner_id">>, EP)), ?ENDPOINT_EDITED(EP));
                _OldRev ->
                    timer:sleep(250),
                    handle_device_change(AccountDb, AccountId, DeviceId, Rev, <<"doc_edited">>, Cnt+1)
            end;
        _ -> lager:debug("ignoring the fact that device ~s was edited", [DeviceId])
    end;
handle_device_change(AccountDb, AccountId, DeviceId, Rev, <<"doc_deleted">>, Cnt) ->
    case cf_endpoint:get(DeviceId, AccountDb) of
        {'ok', EP} ->
            case wh_json:get_value(<<"_rev">>, EP) of
                Rev ->
                    gproc:send(?ENDPOINT_UPDATE_REG(AccountId, DeviceId), ?ENDPOINT_DELETED(EP)),
                    gproc:send(?OWNER_UPDATE_REG(AccountId, wh_json:get_value(<<"owner_id">>, EP)), ?ENDPOINT_DELETED(EP));
                _OldRev ->
                    timer:sleep(250),
                    handle_device_change(AccountDb, AccountId, DeviceId, Rev, <<"doc_deleted">>, Cnt+1)
            end;
        _ -> lager:debug("ignoring the fact that device ~s was edited", [DeviceId])
    end.

handle_agent_change(AccountDb, AccountId, AgentId, <<"doc_created">>) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' ->
            {'ok', JObj} = couch_mgr:open_doc(AccountDb, AgentId),
            acdc_agents_sup:new(JObj);
        P when is_pid(P) -> 'ok'
    end;
handle_agent_change(AccountDb, AccountId, AgentId, <<"doc_edited">>) ->
    {'ok', JObj} = couch_mgr:open_doc(AccountDb, AgentId),
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> acdc_agents_sup:new(JObj);
        P when is_pid(P) -> acdc_agent_fsm:refresh(acdc_agent_sup:fsm(P), JObj)
    end;
handle_agent_change(_, AccountId, AgentId, <<"doc_deleted">>) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> 'ok';
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) has been deleted, stopping ~p", [AccountId, AgentId, P]),
            _ = acdc_agent_sup:stop(P),
            acdc_stats:agent_inactive(AccountId, AgentId)
    end.

handle_presence_probe(JObj, _Props) ->
    'true' = wapi_notifications:presence_probe_v(JObj),

    FromRealm = wh_json:get_value(<<"From-Realm">>, JObj),
    case whapps_util:get_account_by_realm(FromRealm) of
        {'ok', AcctDb} -> maybe_respond_to_presence_probe(JObj, wh_util:format_account_id(AcctDb, 'raw'));
        _ -> lager:debug("ignoring presence probe from realm ~s", [FromRealm])
    end.

maybe_respond_to_presence_probe(JObj, AcctId) ->
    case wh_json:get_value(<<"To-User">>, JObj) of
        'undefined' -> lager:debug("no user on presence probe for ~s", [AcctId]);
        AgentId ->
            lager:debug("maybe updating presence probe for ~s(~s)", [AgentId, AcctId]),
            update_probe(JObj, acdc_agents_sup:find_agent_supervisor(AcctId, AgentId))
    end.

update_probe(_JObj, 'undefined') -> 'ok';
update_probe(JObj, P) when is_pid(P) ->
    lager:debug("agent is active with supervisor: ~p", [P]),
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
    Agent = props:get_value('agent_id', Props),

    case Owner =:= Agent of
        'true' -> acdc_agent_fsm:route_req(props:get_value('fsm_pid', Props), Call);
        'false' -> 'ok'
    end.
