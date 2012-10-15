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
         ,handle_stats_req/2
         ,handle_call_event/2
         ,handle_member_message/2
         ,handle_config_change/2
        ]).

-include("acdc.hrl").

-spec handle_status_update/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_status_update(JObj, _Props) ->
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    Timeout = wh_json:get_integer_value(<<"Time-Limit">>, JObj, whapps_config:get(<<"acdc">>, <<"default_agent_pause_timeout">>, 600)),

    case wh_json:get_value(<<"Event-Name">>, JObj) of
        <<"login">> -> maybe_start_agent(AcctId, AgentId);
        <<"logout">> -> maybe_stop_agent(AcctId, AgentId);
        <<"pause">> -> maybe_pause_agent(AcctId, AgentId, Timeout);
        <<"resume">> -> maybe_resume_agent(AcctId, AgentId);
        Event -> maybe_agent_queue_change(AcctId, AgentId, Event
                                          ,wh_json:get_value(<<"AgentId-ID">>, JObj)
                                         )
    end.

maybe_agent_queue_change(AcctId, AgentId, <<"login_agent">>, AgentId) ->
    update_agent(acdc_agents_sup:find_agent_supervisor(AcctId, AgentId), AgentId, add_acdc_agent);
maybe_agent_queue_change(AcctId, AgentId, <<"logout_agent">>, AgentId) ->
    update_agent(acdc_agents_sup:find_agent_supervisor(AcctId, AgentId), AgentId, rm_acdc_agent).

update_agent(undefined, _, _) -> ok;
update_agent(Super, Q, F) -> 
    acdc_agent:F(acdc_agent_sup:agent(Super), Q).

maybe_start_agent(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined ->
            lager:debug("agent ~s (~s) not found, starting", [AgentId, AcctId]),
            case couch_mgr:open_doc(wh_util:format_account_id(AcctId, encoded), AgentId) of
                {ok, AgentJObj} ->
                    _R = acdc_agents_sup:new(AgentJObj),
                    acdc_stats:agent_active(AcctId, AgentId),
                    lager:debug("started agent at ~p", [_R]);
                {error, _E} ->
                    lager:debug("error opening agent doc: ~p", [_E])
            end;
        P when is_pid(P) ->
            lager:debug("agent ~s (~s) already running: supervisor ~p", [AgentId, AcctId, P])
    end.

maybe_stop_agent(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined -> lager:debug("agent ~s (~s) not found, nothing to do", [AgentId, AcctId]);
        P when is_pid(P) ->
            lager:debug("agent ~s(~s) is logging out, stopping ~p", [AcctId, AgentId, P]),
            acdc_agent_sup:stop(P),
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

-spec handle_sync_req/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_sync_req(JObj, Props) ->
    acdc_agent_fsm:sync_req(props:get_value(fsm_pid, Props), JObj).

-spec handle_sync_resp/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_sync_resp(JObj, Props) ->
    acdc_agent_fsm:sync_resp(props:get_value(fsm_pid, Props), JObj).

-spec handle_call_event/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    {Cat, Name} = wh_util:get_event_type(JObj),
    acdc_agent_fsm:call_event(props:get_value(fsm_pid, Props), Cat, Name, JObj).    

-spec handle_member_message/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
-spec handle_member_message/3 :: (wh_json:json_object(), wh_proplist(), ne_binary()) -> 'ok'.
handle_member_message(JObj, Props) ->
    handle_member_message(JObj, Props, wh_json:get_value(<<"Event-Name">>, JObj)).

handle_member_message(JObj, Props, <<"connect_req">>) ->
    acdc_agent_fsm:member_connect_req(props:get_value(fsm_pid, Props), JObj);
handle_member_message(JObj, Props, <<"connect_win">>) ->
    acdc_agent_fsm:member_connect_win(props:get_value(fsm_pid, Props), JObj);
handle_member_message(JObj, Props, <<"connect_monitor">>) ->
    acdc_agent_fsm:member_connect_monitor(props:get_value(fsm_pid, Props), JObj);
handle_member_message(_, _, EvtName) ->
    lager:debug("not handling member event ~s", [EvtName]).

-spec handle_stats_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_stats_req(JObj, _Props) ->
    true = wapi_acdc_agent:stats_req_v(JObj),
    handle_stats_req(wh_json:get_value(<<"Account-ID">>, JObj)
                     ,wh_json:get_value(<<"Agent-ID">>, JObj)
                     ,wh_json:get_value(<<"Server-ID">>, JObj)
                     ,wh_json:get_value(<<"Msg-ID">>, JObj)
                    ).

-spec handle_stats_req/4 :: (api_binary(), api_binary(), api_binary(), api_binary()) -> any().
handle_stats_req(AcctId, undefined, RespQ, MsgId) ->
    build_stats_resp(AcctId, RespQ, MsgId, acdc_agents_sup:find_acct_supervisors(AcctId));
handle_stats_req(AcctId, AgentId, RespQ, MsgId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        undefined -> lager:debug("agent ~s in acct ~s isn't running", [AgentId, AcctId]);
        P when is_pid(P) -> build_stats_resp(AcctId, RespQ, MsgId, [P])
    end.

-spec build_stats_resp/4 :: (api_binary(), api_binary(), api_binary(), [pid()] | []) -> any().
-spec build_stats_resp/6 :: (api_binary(), api_binary(), api_binary(), [pid()] | []
                             ,wh_proplist(), wh_proplist()
                            ) -> any().
build_stats_resp(AcctId, RespQ, MsgId, Ps) ->
    build_stats_resp(AcctId, RespQ, MsgId, Ps, [], []).

build_stats_resp(AcctId, RespQ, MsgId, [], CurrCalls, CurrAgents) ->
    Resp = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Current-Calls">>, wh_json:from_list(props:filter_undefined(CurrCalls))}
              ,{<<"Current-Stats">>, acdc_stats:agent_stats(AcctId)}
              ,{<<"Current-Statuses">>, wh_json:from_list(props:filter_undefined(CurrAgents))}
              ,{<<"Msg-ID">>, MsgId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    wapi_acdc_agent:publish_stats_resp(RespQ, Resp);
build_stats_resp(AcctId, RespQ, MsgId, [P|Ps], CurrCalls, CurrAgents) ->
    A = acdc_agent_sup:agent(P),
    {AcctId, AgentId} = acdc_agent:config(A),

    FSM = acdc_agent_sup:fsm(P),

    build_stats_resp(AcctId, RespQ, MsgId, Ps
                     ,[{AgentId, acdc_agent_fsm:current_call(FSM)} | CurrCalls]
                     ,[{AgentId, acdc_agent_fsm:status(FSM)} | CurrAgents]
                    ).

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
            acdc_agent_sup:stop(P),
            acdc_stats:agent_inactive(AcctId, AgentId)
    end.
