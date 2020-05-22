%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Handlers for various call events, acdc events, etc
%%% @author James Aimonetti
%%% @author Daniel Finke
%%%
%%% @author James Aimonetti
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_handler).

%% Listener callbacks
-export([handle_status_update/2
        ,handle_sync_req/2
        ,handle_sync_resp/2
        ,handle_call_event/2
        ,handle_new_channel/2
        ,handle_destroyed_channel/2
        ,handle_originate_resp/2
        ,handle_member_message/2
        ,handle_agent_message/2
        ,handle_config_change/2
        ,handle_presence_probe/2
        ]).

-include("acdc.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-define(DEFAULT_PAUSE, kapps_config:get(?CONFIG_CAT, <<"default_agent_pause_timeout">>, 600)).

-spec handle_status_update(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_status_update(JObj, _Props) ->
    _ = kz_log:put_callid(JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),

    lager:debug("status update recv for ~s (~s)", [AgentId, AccountId]),

    case kz_json:get_value(<<"Event-Name">>, JObj) of
        <<"login">> ->
            'true' = kapi_acdc_agent:login_v(JObj),
            login(AccountId, AgentId, JObj);
        <<"logout">> ->
            'true' = kapi_acdc_agent:logout_v(JObj),
            maybe_stop_agent(AccountId, AgentId, JObj);
        <<"pause">> ->
            'true' = kapi_acdc_agent:pause_v(JObj),
            Timeout = kz_json:get_value(<<"Time-Limit">>, JObj, ?DEFAULT_PAUSE),
            Alias = kz_json:get_value(<<"Alias">>, JObj),
            maybe_pause_agent(AccountId, AgentId, Timeout, Alias, JObj);
        <<"resume">> ->
            'true' = kapi_acdc_agent:resume_v(JObj),
            maybe_resume_agent(AccountId, AgentId, JObj);
        <<"end_wrapup">> ->
            'true' = kapi_acdc_agent:end_wrapup_v(JObj),
            maybe_end_wrapup_agent(AccountId, AgentId, JObj);
        <<"restart">> ->
            'true' = kapi_acdc_agent:restart_v(JObj),
            _ = acdc_agents_sup:restart_agent(AccountId, AgentId),
            'ok';
        Event -> maybe_agent_queue_change(AccountId, AgentId, Event
                                         ,kz_json:get_value(<<"Queue-ID">>, JObj)
                                         ,JObj
                                         )
    end.

-spec login(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
login(AccountId, AgentId, JObj) ->
    case maybe_start_agent(AccountId, AgentId, JObj) of
        'fail' -> login_fail(JObj);
        _ -> login_success(JObj)
    end.

-spec maybe_agent_queue_change(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_agent_queue_change(AccountId, AgentId, <<"login_queue">>, QueueId, JObj) ->
    lager:debug("queue login for agent ~s into ~s", [AgentId, QueueId]),
    case maybe_start_agent(AccountId, AgentId, JObj) of
        'fail' -> lager:error("could not start agent process for ~s", [AgentId]);
        Sup -> acdc_agent_fsm:add_acdc_queue(acdc_agent_sup:fsm(Sup), QueueId)
    end;
maybe_agent_queue_change(AccountId, AgentId, <<"logout_queue">>, QueueId, JObj) ->
    lager:debug("queue logout for agent ~s into ~s", [AgentId, QueueId]),
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> lager:debug("agent process for ~s already stopped");
        Sup ->
            maybe_update_presence(Sup, JObj),
            acdc_agent_fsm:rm_acdc_queue(acdc_agent_sup:fsm(Sup), QueueId)
    end.

-spec maybe_start_agent(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pid() | 'fail'.
maybe_start_agent(AccountId, AgentId, JObj) ->
    try maybe_start_agent(AccountId, AgentId) of
        {'ok', Sup} ->
            timer:sleep(100),
            case erlang:is_process_alive(Sup) of
                'true' ->
                    maybe_update_presence(Sup, JObj),
                    acdc_agent_stats:agent_logged_in(AccountId, AgentId),
                    Sup;
                'false' ->
                    acdc_agent_stats:agent_logged_out(AccountId, AgentId),
                    'fail'
            end;
        {'exists', Sup} ->
            FSM = acdc_agent_sup:fsm(Sup),
            acdc_agent_stats:agent_logged_in(AccountId, AgentId),
            case presence_state(JObj, 'undefined') of
                'undefined' -> 'ok';
                PresenceState -> acdc_agent_fsm:update_presence(FSM, presence_id(JObj), PresenceState)
            end,
            Sup;
        {'error', _E} ->
            acdc_agent_stats:agent_logged_out(AccountId, AgentId),
            'fail'
    catch
        _E:_R ->
            acdc_agent_stats:agent_logged_out(AccountId, AgentId),
            'fail'
    end.

login_fail(JObj) ->
    login_resp(JObj, <<"failed">>).
login_success(JObj) ->
    login_resp(JObj, <<"success">>).

login_resp(JObj, Status) ->
    case {kz_json:get_value(<<"Server-ID">>, JObj), kz_json:get_value(<<"Msg-ID">>, JObj)} of
        {'undefined', _} -> lager:debug("not publishing a login resp: no server_id");
        {<<>>, _} -> lager:debug("not publishing a login resp: no server_id");
        {_, 'undefined'} -> lager:debug("not publishing a login resp: no msg_id");
        {ServerID, MsgId} ->
            Prop = [{<<"Status">>, Status}
                   ,{<<"Msg-ID">>, MsgId}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_acdc_agent:publish_login_resp(ServerID, Prop)
    end.

-spec maybe_start_agent(kz_term:api_binary(), kz_term:api_binary()) ->
          {'ok', pid()} |
          {'exists', pid()} |
          {'error', any()}.
maybe_start_agent(AccountId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' ->
            lager:debug("agent ~s (~s) not found, starting", [AgentId, AccountId]),
            case kz_datamgr:open_doc(kzs_util:format_account_db(AccountId), AgentId) of
                {'ok', AgentJObj} -> acdc_agents_sup:new(AgentJObj);
                {'error', _E}=E ->
                    lager:debug("error opening agent doc: ~p", [_E]),
                    E
            end;
        Sup when is_pid(Sup) ->
            lager:debug("agent ~s (~s) already running: supervisor ~p", [AgentId, AccountId, Sup]),
            {'exists', Sup}
    end.

maybe_stop_agent(AccountId, AgentId, JObj) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' ->
            lager:debug("agent ~s(~s) not found, nothing to do", [AgentId, AccountId]),
            catch acdc_util:presence_update(AccountId, presence_id(JObj, AgentId), ?PRESENCE_RED_SOLID),
            acdc_agent_stats:agent_logged_out(AccountId, AgentId);
        Sup when is_pid(Sup) ->
            lager:debug("agent ~s(~s) is logging out, stopping ~p", [AgentId, AgentId, Sup]),
            case catch acdc_agent_sup:fsm(Sup) of
                APid when is_pid(APid) ->
                    acdc_agent_fsm:update_presence(APid, presence_id(JObj), presence_state(JObj, ?PRESENCE_RED_SOLID)),
                    acdc_agent_fsm:agent_logout(APid);
                _P -> lager:debug("failed to find agent fsm for ~s: ~p", [AgentId, _P])
            end

    end.

maybe_pause_agent(AccountId, AgentId, Timeout, Alias, JObj) when is_integer(Timeout) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AccountId]);
        Sup when is_pid(Sup) ->
            lager:debug("agent ~s(~s) is pausing (~p) for ~p", [AccountId, AgentId, Alias, Timeout]),
            FSM = acdc_agent_sup:fsm(Sup),
            acdc_agent_fsm:update_presence(FSM,  presence_id(JObj), presence_state(JObj, 'undefined')),
            acdc_agent_fsm:pause(FSM, Timeout, Alias)
    end;
maybe_pause_agent(AccountId, AgentId, Timeout, _, _) ->
    lager:error("not pausing agent ~s(~s) invalid Timeout: ~p", [AccountId, AgentId, Timeout]),
    ok.

maybe_resume_agent(AccountId, AgentId, JObj) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AccountId]);
        Sup when is_pid(Sup) ->
            lager:debug("agent ~s(~s) is resuming: ~p", [AccountId, AgentId, Sup]),
            FSM = acdc_agent_sup:fsm(Sup),
            acdc_agent_fsm:update_presence(FSM,  presence_id(JObj), presence_state(JObj, 'undefined')),
            acdc_agent_fsm:resume(FSM)
    end.

-spec maybe_end_wrapup_agent(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_end_wrapup_agent(AccountId, AgentId, JObj) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AccountId]);
        Sup when is_pid(Sup) ->
            lager:debug("agent ~s(~s) is ending wrapup: ~p", [AccountId, AgentId, Sup]),
            FSM = acdc_agent_sup:fsm(Sup),
            acdc_agent_fsm:update_presence(FSM,  presence_id(JObj), presence_state(JObj, 'undefined')),
            acdc_agent_fsm:end_wrapup(FSM)
    end.

-spec handle_sync_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_sync_req(JObj, Props) ->
    'true' = kapi_acdc_agent:sync_req_v(JObj),
    acdc_agent_fsm:sync_req(props:get_value('fsm_pid', Props), JObj).

-spec handle_sync_resp(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_sync_resp(JObj, Props) ->
    'true' = kapi_acdc_agent:sync_resp_v(JObj),
    acdc_agent_fsm:sync_resp(props:get_value('fsm_pid', Props), JObj).

-spec handle_call_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    _ = kz_log:put_callid(JObj),
    FSM = props:get_value('fsm_pid', Props),
    case kapi_call:event_v(JObj) of
        'true' ->
            {Category, Name} = kz_util:get_event_type(JObj),
            handle_call_event(Category, Name, FSM, JObj, Props);
        'false' ->
            'true' = kz_api:error_resp_v(JObj),

            case kz_json:get_value([<<"Request">>, <<"Event-Name">>], JObj) of
                <<"originate_req">> -> acdc_agent_fsm:originate_failed(FSM, JObj);
                _ -> 'ok'
            end
    end.

-spec handle_call_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:server_ref(), kz_json:object(), kz_term:proplist()) -> any().
handle_call_event(Category, <<"CHANNEL_DESTROY">> = Name, FSM, JObj, Props) ->
    Urls = props:get_value('cdr_urls', Props),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case catch dict:fetch(CallId, Urls) of
        {'EXIT', _} -> lager:debug("no cdr url for call ~s", [CallId]);
        Url -> acdc_util:send_cdr(Url, JObj)
    end,
    Srv = props:get_value('server', Props),
    acdc_agent_fsm:call_event(FSM, Category, Name, JObj),
    _ = acdc_agent_listener:remove_cdr_urls(Srv, CallId),
    acdc_util:unbind_from_call_events(CallId, Srv);
handle_call_event(Category, Name, FSM, JObj, _) ->
    acdc_agent_fsm:call_event(FSM, Category, Name, JObj).

-spec handle_new_channel(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
handle_new_channel(JObj, AccountId) ->
    'true' = kapi_call:event_v(JObj),
    _ = kz_log:put_callid(JObj),
    handle_new_channel_acct(JObj, AccountId).

-spec handle_new_channel_acct(kz_json:object(), kz_term:api_binary()) -> 'ok'.
handle_new_channel_acct(_, 'undefined') -> 'ok';
handle_new_channel_acct(JObj, AccountId) ->
    FromUser =
        case kz_json:is_defined(<<"From-Uri">>, JObj) of
            false -> hd(binary:split(kz_json:get_value(<<"From">>, JObj), <<"@">>));
            true -> hd(binary:split(kz_json:get_value(<<"From-Uri">>, JObj), <<"@">>))
        end,
    ToUser = get_to_user(JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    MemberCallId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Member-Call-ID">>], JObj),
    lager:debug("new channel in acct ~s: from ~s to ~s", [AccountId, FromUser, ToUser]),
    case kz_call_event:call_direction(JObj) of
        <<"inbound">> ->
            gproc:send(?NEW_CHANNEL_REG(AccountId, FromUser), ?NEW_CHANNEL_TO(CallId, ToUser, <<"unknown">>));
        <<"outbound">> ->
            CR_IDNumber  =  kz_json:get_value(<<"Caller-ID-Number">>, JObj),
            CR_IDName  =  kz_json:get_value(<<"Caller-ID-Name">>, JObj),
            gproc:send(?NEW_CHANNEL_REG(AccountId, ToUser), ?NEW_CHANNEL_FROM(CallId, CR_IDNumber, CR_IDName, MemberCallId));
        _ -> lager:debug("invalid call direction for call ~s", [CallId])
    end.

%%------------------------------------------------------------------------------
%% @doc Send event to agent FSM when channels are destroyed. This occurs in
%% addition to the above handle_call_event/2. Though this is redundant
%% in most cases, it will keep the agent from becoming stuck in the
%% outbound state if a channel is created and destroyed before the
%% acdc_agent_listener gen_listener can bind to it.
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_destroyed_channel(kz_json:object(), kz_term:api_binary()) -> 'ok'.
handle_destroyed_channel(JObj, AccountId) ->
    FromUser =
        case kz_json:is_defined(<<"From-Uri">>, JObj) of
            false -> hd(binary:split(kz_json:get_value(<<"From">>, JObj), <<"@">>));
            true -> hd(binary:split(kz_json:get_value(<<"From-Uri">>, JObj), <<"@">>))
        end,

    ToUser = get_to_user(JObj),

    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    HangupCause = acdc_util:hangup_cause(JObj),

    lager:debug("destroyed channel in acct ~s: from ~s to ~s", [AccountId, FromUser, ToUser]),

    case kz_call_event:call_direction(JObj) of
        <<"inbound">> -> gproc:send(?DESTROYED_CHANNEL_REG(AccountId, FromUser)
                                   ,?DESTROYED_CHANNEL(CallId, HangupCause));
        <<"outbound">> ->
            gproc:send(?DESTROYED_CHANNEL_REG(AccountId, FromUser)
                      ,?DESTROYED_CHANNEL(CallId, HangupCause)),
            gproc:send(?DESTROYED_CHANNEL_REG(AccountId, ToUser)
                      ,?DESTROYED_CHANNEL(CallId, HangupCause));
        _ -> 'ok'
    end.

-spec get_to_user(kz_json:object()) -> kz_term:api_binary().
get_to_user(JObj) ->
    case kz_json:is_defined(<<"To-Uri">>, JObj) of
        true -> hd(binary:split(kz_json:get_value(<<"To-Uri">>, JObj), <<"@">>));
        false -> get_to_or_destination_number(JObj)
    end.

-spec get_to_or_destination_number(kz_json:object()) -> kz_term:api_binary().
get_to_or_destination_number(JObj) ->
    case kz_json:is_defined(<<"To">>, JObj) of
        true -> hd(binary:split(kz_json:get_value(<<"To">>, JObj), <<"@">>));
        false -> kz_json:get_value(<<"Caller-Destination-Number">>, JObj)
    end.

-spec handle_originate_resp(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_originate_resp(JObj, Props) ->
    case kz_json:get_value(<<"Event-Name">>, JObj) of
        <<"originate_resp">> ->
            'true' = kapi_resource:originate_resp_v(JObj),
            acdc_agent_fsm:originate_resp(props:get_value('fsm_pid', Props), JObj);
        <<"originate_started">> ->
            'true' = kapi_resource:originate_started_v(JObj),
            acdc_agent_fsm:originate_started(props:get_value('fsm_pid', Props), JObj);
        <<"originate_uuid">> ->
            'true' = kapi_resource:originate_uuid_v(JObj),
            acdc_agent_fsm:originate_uuid(props:get_value('fsm_pid', Props), JObj)
    end.


-spec handle_member_message(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_member_message(JObj, Props) ->
    handle_member_message(JObj, Props, kz_json:get_value(<<"Event-Name">>, JObj)).

-spec handle_member_message(kz_json:object(), kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
handle_member_message(JObj, Props, <<"connect_req">>) ->
    'true' = kapi_acdc_queue:member_connect_req_v(JObj),
    acdc_agent_fsm:member_connect_req(props:get_value('fsm_pid', Props), JObj);
handle_member_message(JObj, Props, <<"connect_win">>) ->
    'true' = kapi_acdc_queue:member_connect_win_v(JObj),
    MyId = acdc_util:proc_id(props:get_value('fsm_pid', Props)),
    lager:debug("myid ~p", [MyId]),
    lager:debug("procids ~p", [kz_json:get_value(<<"Agent-Process-IDs">>, JObj)]),
    case lists:member(MyId, kz_json:get_value(<<"Agent-Process-IDs">>, JObj)) of
        true -> acdc_agent_fsm:member_connect_win(props:get_value('fsm_pid', Props), JObj, 'same_node');
        false -> acdc_agent_fsm:member_connect_win(props:get_value('fsm_pid', Props), JObj, 'different_node')
    end;
handle_member_message(JObj, Props, <<"connect_satisfied">>) ->
    'true' = kapi_acdc_queue:member_connect_satisfied_v(JObj),
    MyId = acdc_util:proc_id(props:get_value('fsm_pid', Props)),
    lager:debug("myid ~p", [MyId]),
    lager:debug("procids ~p", [kz_json:get_value(<<"Agent-Process-IDs">>, JObj)]),
    case lists:member(MyId, kz_json:get_value(<<"Agent-Process-IDs">>, JObj)) of
        true -> acdc_agent_fsm:member_connect_satisfied(props:get_value('fsm_pid', Props), JObj, 'same_node');
        false -> acdc_agent_fsm:member_connect_satisfied(props:get_value('fsm_pid', Props), JObj, 'different_node')
    end;
handle_member_message(_, _, EvtName) ->
    lager:debug("not handling member event ~s", [EvtName]).

-spec handle_agent_message(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_agent_message(JObj, Props) ->
    handle_agent_message(JObj, Props, kz_json:get_value(<<"Event-Name">>, JObj)).

-spec handle_agent_message(kz_json:object(), kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
handle_agent_message(JObj, Props, <<"connect_timeout">>) ->
    'true' = kapi_acdc_queue:agent_timeout_v(JObj),
    acdc_agent_fsm:agent_timeout(props:get_value('fsm_pid', Props), JObj);
handle_agent_message(JObj, Props, <<"shared_failure">>) ->
    'true' = kapi_acdc_agent:shared_originate_failure_v(JObj),
    acdc_agent_fsm:shared_failure(props:get_value('fsm_pid', Props), JObj);
handle_agent_message(JObj, Props, <<"shared_call_id">>) ->
    'true' = kapi_acdc_agent:shared_call_id_v(JObj),
    acdc_agent_fsm:shared_call_id(props:get_value('fsm_pid', Props), JObj);
handle_agent_message(_, _, _EvtName) ->
    lager:debug("not handling agent event ~s", [_EvtName]).

-spec handle_config_change(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_config_change(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    handle_change(JObj, kz_json:get_value(<<"Type">>, JObj)).

-spec handle_change(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
handle_change(JObj, <<"user">>) ->
    handle_agent_change(kz_json:get_value(<<"Database">>, JObj)
                       ,kz_json:get_value(<<"Account-ID">>, JObj)
                       ,kz_json:get_value(<<"ID">>, JObj)
                       ,kz_json:get_value(<<"Event-Name">>, JObj)
                       );
handle_change(JObj, <<"device">>) ->
    handle_device_change(kz_json:get_value(<<"Database">>, JObj)
                        ,kz_json:get_value(<<"Account-ID">>, JObj)
                        ,kz_json:get_value(<<"ID">>, JObj)
                        ,kz_json:get_value(<<"Rev">>, JObj)
                        ,kz_json:get_value(<<"Event-Name">>, JObj)
                        );
handle_change(JObj, <<"undefined">>) ->
    lager:debug("undefined type for change"),
    case kz_datamgr:open_cache_doc(kz_json:get_value(<<"Database">>, JObj)
                                  ,kz_json:get_value(<<"ID">>, JObj)
                                  )
    of
        {'ok', Doc} ->
            Type = kz_doc:type(Doc),
            lager:debug("found doc of type ~s", [Type]),
            handle_change(JObj, Type);
        {'error', _E} ->
            lager:debug("failed to find doc")
    end.

handle_device_change(AccountDb, AccountId, DeviceId, Rev, Type) ->
    handle_device_change(AccountDb, AccountId, DeviceId, Rev, Type, 0).

handle_device_change(_AccountDb, _AccountId, DeviceId, Rev, _Type, Cnt) when Cnt > 3 ->
    lager:debug("retried ~p times to refresh endpoint ~s(~s), giving up", [Cnt, DeviceId, Rev]);
handle_device_change(AccountDb, AccountId, DeviceId, Rev, ?DOC_CREATED, Cnt) ->
    case kz_endpoint:get(DeviceId, AccountDb) of
        {'ok', EP} ->
            case kz_doc:revision(EP) of
                Rev ->
                    gproc:send(?ENDPOINT_UPDATE_REG(AccountId, DeviceId), ?ENDPOINT_CREATED(EP)),
                    gproc:send(?OWNER_UPDATE_REG(AccountId, kz_json:get_value(<<"owner_id">>, EP)), ?ENDPOINT_CREATED(EP));
                _OldRev ->
                    timer:sleep(250),
                    handle_device_change(AccountDb, AccountId, DeviceId, Rev, ?DOC_CREATED, Cnt+1)
            end;
        _ -> lager:debug("ignoring the fact that device ~s was created", [DeviceId])
    end;
handle_device_change(AccountDb, AccountId, DeviceId, Rev, ?DOC_EDITED, Cnt) ->
    case kz_endpoint:get(DeviceId, AccountDb) of
        {'ok', EP} ->
            case kz_doc:revision(EP) of
                Rev ->
                    gproc:send(?ENDPOINT_UPDATE_REG(AccountId, DeviceId), ?ENDPOINT_EDITED(EP)),
                    gproc:send(?OWNER_UPDATE_REG(AccountId, kz_json:get_value(<<"owner_id">>, EP)), ?ENDPOINT_EDITED(EP));
                _OldRev ->
                    timer:sleep(250),
                    handle_device_change(AccountDb, AccountId, DeviceId, Rev, ?DOC_EDITED, Cnt+1)
            end;
        _ -> lager:debug("ignoring the fact that device ~s was edited", [DeviceId])
    end;
handle_device_change(AccountDb, AccountId, DeviceId, Rev, ?DOC_DELETED, Cnt) ->
    case kz_endpoint:get(DeviceId, AccountDb) of
        {'ok', EP} ->
            case kz_doc:revision(EP) of
                Rev ->
                    gproc:send(?ENDPOINT_UPDATE_REG(AccountId, DeviceId), ?ENDPOINT_DELETED(EP)),
                    gproc:send(?OWNER_UPDATE_REG(AccountId, kz_json:get_value(<<"owner_id">>, EP)), ?ENDPOINT_DELETED(EP));
                _OldRev ->
                    timer:sleep(250),
                    handle_device_change(AccountDb, AccountId, DeviceId, Rev, ?DOC_DELETED, Cnt+1)
            end;
        _ -> lager:debug("ignoring the fact that device ~s was edited", [DeviceId])
    end.

handle_agent_change(_AccountDb, AccountId, AgentId, ?DOC_CREATED) ->
    lager:debug("new agent ~s(~s) created, hope they log in soon!", [AgentId, AccountId]);
handle_agent_change(AccountDb, AccountId, AgentId, ?DOC_EDITED) ->
    {'ok', JObj} = kz_datamgr:open_doc(AccountDb, AgentId),
    lager:debug("agent ~s edited", [AgentId]),
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> lager:debug("failed to find agent ~s", [AgentId]);
        P when is_pid(P) -> acdc_agent_fsm:refresh(acdc_agent_sup:fsm(P), JObj)
    end;
handle_agent_change(_, AccountId, AgentId, ?DOC_DELETED) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> lager:debug("user ~s has left us, but wasn't started", [AgentId]);
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) has been deleted, stopping ~p", [AccountId, AgentId, P]),
            _ = acdc_agent_sup:stop(P),
            acdc_agent_stats:agent_logged_out(AccountId, AgentId)
    end.

-spec handle_presence_probe(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_presence_probe(JObj, _Props) ->
    'true' = kapi_presence:probe_v(JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AcctDb} ->
            AccountId = kzs_util:format_account_id(AcctDb),
            maybe_respond_to_presence_probe(JObj, AccountId);
        _ -> lager:debug("ignoring presence probe from realm ~s", [Realm])
    end.

maybe_respond_to_presence_probe(JObj, AccountId) ->
    case kz_json:get_value(<<"Username">>, JObj) of
        'undefined' -> lager:debug("no user on presence probe for ~s", [AccountId]);
        AgentId ->
            update_probe(JObj, acdc_agents_sup:find_agent_supervisor(AccountId, AgentId))
    end.

update_probe(_JObj, 'undefined') -> 'ok';
update_probe(JObj, P) when is_pid(P) ->
    lager:debug("agent is active with supervisor: ~p", [P]),
    send_probe(JObj, ?PRESENCE_GREEN).

send_probe(JObj, State) ->
    To = <<(kz_json:get_value(<<"Username">>, JObj))/binary
          ,"@"
          ,(kz_json:get_value(<<"Realm">>, JObj))/binary>>,
    PresenceUpdate =
        [{<<"State">>, State}
        ,{<<"Presence-ID">>, To}
        ,{<<"Call-ID">>, kz_term:to_hex_binary(crypto:hash(md5, To))}
         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    kapi_presence:publish_update(PresenceUpdate).

presence_id(JObj) ->
    presence_id(JObj, 'undefined').
presence_id(JObj, AgentId) ->
    lager:debug("find presence in ~p", [JObj]),
    kz_json:get_value(<<"Presence-ID">>, JObj, AgentId).

-spec presence_state(kz_json:object(), kz_term:api_binary()) -> kz_term:api_binary().
presence_state(JObj, State) ->
    kz_json:get_value(<<"Presence-State">>, JObj, State).

maybe_update_presence(Sup, JObj) ->
    maybe_update_presence(Sup, JObj, 'undefined').
maybe_update_presence(Sup, JObj, PresenceState) ->
    APid = acdc_agent_sup:listener(Sup),
    acdc_agent_listener:maybe_update_presence_id(APid, presence_id(JObj)),
    acdc_agent_listener:presence_update(APid, presence_state(JObj, PresenceState)).
