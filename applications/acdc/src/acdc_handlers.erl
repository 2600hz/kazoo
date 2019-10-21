%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_handlers).

-export([handle_route_req/2
        ]).

-include("acdc.hrl").

-spec handle_route_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_route_req(JObj, Props) ->
    'true' = kapi_route:req_v(JObj),
    _ = kz_log:put_callid(JObj),

    Call = kapps_call:set_controller_queue(props:get_value('queue', Props)
                                          ,kapps_call:from_route_req(JObj)
                                          ),
    AcctId = kapps_call:account_id(Call),
    Id = kapps_call:request_user(Call),
    maybe_route_respond(JObj, Call, AcctId, Id).

maybe_route_respond(_JObj, _Call, 'undefined', _Id) -> 'ok';
maybe_route_respond(ReqJObj, Call, AcctId, Id) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), Id) of
        {'error', _} -> 'ok';
        {'ok', Doc} ->
            maybe_route_respond(ReqJObj, Call, AcctId, Id, kz_doc:type(Doc))
    end.

maybe_route_respond(ReqJObj, Call, AccountId, QueueId, <<"queue">> = T) ->
    send_route_response(ReqJObj, Call, AccountId, QueueId, T);
maybe_route_respond(ReqJObj, Call, AccountId, AgentId, <<"user">> = T) ->
    send_route_response(ReqJObj, Call, AccountId, AgentId, T);
maybe_route_respond(_ReqJObj, _Call, _AccountId, _Id, _) -> 'ok'.

send_route_response(ReqJObj, Call, AccountId, Id, Type) ->
    lager:debug("sending route response to park the call for ~s(~s)", [Id, AccountId]),
    CCVs = [{<<"ACDc-ID">>, Id}
           ,{<<"ACDc-Type">>, Type}
           ],
    Resp = [{?KEY_MSG_ID, kz_api:msg_id(ReqJObj)}
           ,{?KEY_MSG_REPLY_ID, kapps_call:call_id_direct(Call)}
           ,{<<"Routes">>, []}
           ,{<<"Method">>, <<"park">>}
           ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
           ,{<<"From-Realm">>, kzd_accounts:fetch_realm(AccountId)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = kz_api:server_id(ReqJObj),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,
    case kz_amqp_worker:call(Resp
                            ,Publisher
                            ,fun kapi_route:win_v/1
                            )
    of
        {'ok', RouteWin} ->
            lager:info("acdc has received a route win, updating agent"),
            update_agent(kapps_call:from_route_win(RouteWin, Call));
        {'error', _E} ->
            lager:info("callflow didn't received a route win, exiting : ~p", [_E])
    end.

-spec update_agent(kapps_call:call()) -> 'ok'.
update_agent(Call) ->
    kapps_call_command:answer(Call),
    try update_acdc_actor(Call) of
        {'ok', P} when is_pid(P) -> 'ok';
        'ok' ->
            kapps_call_command:hangup(Call)
    catch
        _E:_R ->
            lager:debug("crash starting agent: ~s: ~p", [_E, _R]),
            kapps_call_command:hangup(Call)
    end.

-spec update_acdc_actor(kapps_call:call()) -> any().
update_acdc_actor(Call) ->
    update_acdc_actor(Call
                     ,kapps_call:custom_channel_var(<<"ACDc-ID">>, Call)
                     ,kapps_call:custom_channel_var(<<"ACDc-Type">>, Call)
                     ).

-spec update_acdc_actor(kapps_call:call(), kz_term:api_binary(), kz_term:api_binary()) -> any().
update_acdc_actor(_Call, 'undefined', _) -> 'ok';
update_acdc_actor(_Call, _, 'undefined') -> 'ok';
update_acdc_actor(Call, QueueId, <<"queue">>) ->
    lager:debug("caller ~s wants a call in queue ~s", [kapps_call:caller_id_name(Call), QueueId]),

    {'ok', _P}=OK = acdc_agents_sup:new_thief(Call, QueueId),
    lager:debug("started thief at ~p", [_P]),
    OK;
update_acdc_actor(Call, AgentId, <<"user">>) ->
    AcctId = kapps_call:account_id(Call),

    case acdc_agent_util:most_recent_status(AcctId, AgentId) of
        {'ok', <<"logged_out">>} ->
            update_acdc_agent(Call, AcctId, AgentId, <<"login">>, fun kapi_acdc_agent:publish_login/1);
        {'ok', <<"pause">>} ->
            update_acdc_agent(Call, AcctId, AgentId, <<"resume">>, fun kapi_acdc_agent:publish_resume/1);
        {'ok', _S} ->
            update_acdc_agent(Call, AcctId, AgentId, <<"logout">>, fun kapi_acdc_agent:publish_logout/1)
    end.

-spec update_acdc_agent(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), fun()) -> kz_term:ne_binary().
update_acdc_agent(Call, AcctId, AgentId, Status, PubFun) ->
    lager:debug("agent ~s going to new status ~s", [AgentId, Status]),

    try update_agent_device(Call, AgentId, Status) of
        {'ok', _D} ->
            lager:debug("updated device with new owner"),
            'ok' = save_status(Call, AgentId, Status),
            send_new_status(AcctId, AgentId, PubFun),
            play_status_prompt(Call, Status);
        {'error', 'not_owner'} -> play_failed_update(Call)
    catch
        _E:_R ->
            lager:debug("failed to update agent device: ~s: ~p", [_E, _R]),
            play_failed_update(Call)
    end.

-spec save_status(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
save_status(Call, AgentId, Status) ->
    acdc_agent_util:update_status(kapps_call:account_id(Call)
                                 ,AgentId
                                 ,Status
                                 ,[{<<"call_id">>, kapps_call:call_id(Call)}
                                  ,{<<"method">>, <<"acdc_blf">>}
                                  ]).

update_agent_device(Call, AgentId, <<"login">>) ->
    lager:debug("need to set owner_id to ~s on device ~s", [AgentId, kapps_call:authorizing_id(Call)]),
    {'ok', Device} = kz_datamgr:open_doc(kapps_call:account_db(Call), kapps_call:authorizing_id(Call)),
    lager:debug("setting owner_id from ~s to ~s", [kz_json:get_value(<<"owner_id">>, Device), AgentId]),

    move_agent_device(Call, AgentId, Device);
update_agent_device(Call, AgentId, <<"logout">>) ->
    {'ok', Device} = kz_datamgr:open_doc(kapps_call:account_db(Call), kapps_call:authorizing_id(Call)),

    case kz_json:get_value(<<"owner_id">>, Device) of
        AgentId ->
            lager:debug("unsetting owner_id from ~s", [kz_json:get_value(<<"owner_id">>, Device)]),
            {'ok', _} = kz_datamgr:save_doc(kapps_call:account_db(Call), kz_json:delete_key(<<"owner_id">>, Device));
        _Other ->
            lager:debug("owner is ~s, not ~s, leaving as-is: ~p", [_Other, AgentId, Device]),
            {'error', 'not_owner'}
    end;
update_agent_device(_, _, _) -> {'ok', 'ok'}.

-spec move_agent_device(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
                               {'ok', kz_json:object()}.
move_agent_device(Call, AgentId, Device) ->
    DeviceId = kz_doc:id(Device),

    _ = case [kz_json:delete_key(<<"owner_id">>, D)
              || D <- acdc_util:agent_devices(kapps_call:account_db(Call), AgentId),
                 kz_doc:id(D) =/= DeviceId
             ]
        of
            [] -> 'ok';
            OldDevices -> kz_datamgr:save_docs(kapps_call:account_db(Call), OldDevices)
        end,
    {'ok', _} = kz_datamgr:save_doc(kapps_call:account_db(Call), kz_json:set_value(<<"owner_id">>, AgentId, Device)).

-spec send_new_status(kz_term:ne_binary(), kz_term:ne_binary(), kz_amqp_worker:publish_fun()) -> 'ok'.
send_new_status(AcctId, AgentId, PubFun) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
               ,{<<"Agent-ID">>, AgentId}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    PubFun(Update).

-spec play_status_prompt(kapps_call:call(), kz_term:ne_binary()) -> kz_term:ne_binary().
play_status_prompt(Call, <<"login">>) ->
    kapps_call_command:prompt(<<"agent-logged_in">>, Call);
play_status_prompt(Call, <<"logout">>) ->
    kapps_call_command:prompt(<<"agent-logged_out">>, Call);
play_status_prompt(Call, <<"resume">>) ->
    kapps_call_command:prompt(<<"agent-resume">>, Call).

-spec play_failed_update(kapps_call:call()) -> kz_term:ne_binary().
play_failed_update(Call) ->
    kapps_call_command:prompt(<<"agent-invalid_choice">>, Call).
