%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_handlers).

-export([handle_route_req/2
         ,handle_route_win/2
        ]).

-include("acdc.hrl").

handle_route_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    _ = wh_util:put_callid(JObj),

    Call = whapps_call:set_controller_queue(props:get_value('queue', Props)
                                            ,whapps_call:from_route_req(JObj)
                                           ),
    AcctId = whapps_call:account_id(Call),
    Id = whapps_call:request_user(Call),
    maybe_route_respond(JObj, Call, AcctId, Id).

maybe_route_respond(_JObj, _Call, 'undefined', _Id) -> 'ok';
maybe_route_respond(ReqJObj, Call, AcctId, Id) ->
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call), Id) of
        {'error', _} -> 'ok';
        {'ok', Doc} ->
            maybe_route_respond(ReqJObj, Call, AcctId, Id, wh_doc:type(Doc))
    end.

maybe_route_respond(ReqJObj, Call, AccountId, QueueId, <<"queue">> = T) ->
    send_route_response(ReqJObj, Call, AccountId, QueueId, T);
maybe_route_respond(ReqJObj, Call, AccountId, AgentId, <<"user">> = T) ->
    send_route_response(ReqJObj, Call, AccountId, AgentId, T);
maybe_route_respond(_ReqJObj, _Call, _AccountId, _Id, _) -> 'ok'.

send_route_response(ReqJObj, Call, AccountId, Id, Type) ->
    CCVs = [{<<"ACDc-ID">>, Id}
            ,{<<"ACDc-Type">>, Type}
           ],
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, ReqJObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
            ,{<<"From-Realm">>, wh_util:get_account_realm(AccountId)}
            | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
           ],
    wapi_route:publish_resp(wh_json:get_value(<<"Server-ID">>, ReqJObj), Resp),
    _ = whapps_call:cache(Call, ?APP_NAME),
    lager:debug("sent route response to park the call for ~s(~s)", [Id, AccountId]).

-spec handle_route_win(wh_json:object(), wh_proplist()) -> any().
handle_route_win(JObj, _Props) ->
    'true' = wapi_route:win_v(JObj),
    _ = wh_util:put_callid(JObj),

    case whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME) of
        {'ok', Call} ->
            lager:debug("won the call, updating the agent"),
            Call1 = whapps_call:from_route_win(JObj, Call),
            whapps_call_command:answer(Call1),

            try update_acdc_actor(Call1) of
                {'ok', P} when is_pid(P) -> 'ok';
                'ok' ->
                    whapps_call_command:hangup(Call1)
            catch
                _E:_R ->
                    lager:debug("crash starting agent: ~s: ~p", [_E, _R]),
                    whapps_call_command:hangup(Call1)
            end;
        {'error', _R} ->
            lager:debug("failed to retrieve cached call: ~p", [_R])
    end.

-spec update_acdc_actor(whapps_call:call()) -> any().
-spec update_acdc_actor(whapps_call:call(), api_binary(), api_binary()) -> any().
update_acdc_actor(Call) ->
    update_acdc_actor(Call
                      ,whapps_call:custom_channel_var(<<"ACDc-ID">>, Call)
                      ,whapps_call:custom_channel_var(<<"ACDc-Type">>, Call)
                     ).
update_acdc_actor(_Call, 'undefined', _) -> 'ok';
update_acdc_actor(_Call, _, 'undefined') -> 'ok';
update_acdc_actor(Call, QueueId, <<"queue">>) ->
    lager:debug("caller ~s wants a call in queue ~s", [whapps_call:caller_id_name(Call), QueueId]),

    {'ok', _P}=OK = acdc_agents_sup:new_thief(Call, QueueId),
    lager:debug("started thief at ~p", [_P]),
    OK;
update_acdc_actor(Call, AgentId, <<"user">>) ->
    AcctId = whapps_call:account_id(Call),

    case acdc_agent_util:most_recent_status(AcctId, AgentId) of
        {'ok', <<"logout">>} ->
            update_acdc_agent(Call, AcctId, AgentId, <<"login">>, fun wapi_acdc_agent:publish_login/1);
        {'ok', <<"pause">>} ->
            update_acdc_agent(Call, AcctId, AgentId, <<"resume">>, fun wapi_acdc_agent:publish_resume/1);
        {'ok', _S} ->
            update_acdc_agent(Call, AcctId, AgentId, <<"logout">>, fun wapi_acdc_agent:publish_logout/1)
    end.

-spec update_acdc_agent(whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), fun()) -> ne_binary().
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

-spec save_status(whapps_call:call(), ne_binary(), ne_binary()) -> 'ok'.
save_status(Call, AgentId, Status) ->
    acdc_agent_util:update_status(whapps_call:account_id(Call)
                                  ,AgentId
                                  ,Status
                                  ,[{<<"call_id">>, whapps_call:call_id(Call)}
                                    ,{<<"method">>, <<"acdc_blf">>}
                                   ]).

update_agent_device(Call, AgentId, <<"login">>) ->
    lager:debug("need to set owner_id to ~s on device ~s", [AgentId, whapps_call:authorizing_id(Call)]),
    {'ok', Device} = couch_mgr:open_doc(whapps_call:account_db(Call), whapps_call:authorizing_id(Call)),
    lager:debug("setting owner_id from ~s to ~s", [wh_json:get_value(<<"owner_id">>, Device), AgentId]),

    move_agent_device(Call, AgentId, Device);
update_agent_device(Call, AgentId, <<"logout">>) ->
    {'ok', Device} = couch_mgr:open_doc(whapps_call:account_db(Call), whapps_call:authorizing_id(Call)),

    case wh_json:get_value(<<"owner_id">>, Device) of
        AgentId ->
            lager:debug("unsetting owner_id from ~s", [wh_json:get_value(<<"owner_id">>, Device)]),
            {'ok', _} = couch_mgr:save_doc(whapps_call:account_db(Call), wh_json:delete_key(<<"owner_id">>, Device));
        _Other ->
            lager:debug("owner is ~s, not ~s, leaving as-is: ~p", [_Other, AgentId, Device]),
            {'error', 'not_owner'}
    end;
update_agent_device(_, _, _) -> {'ok', 'ok'}.

-spec move_agent_device(whapps_call:call(), ne_binary(), wh_json:object()) ->
                               {'ok', wh_json:object()}.
move_agent_device(Call, AgentId, Device) ->
    DeviceId = wh_json:get_value(<<"_id">>, Device),

    _ = case [wh_json:delete_key(<<"owner_id">>, D)
              || D <- acdc_util:agent_devices(whapps_call:account_db(Call), AgentId),
                 wh_json:get_value(<<"_id">>, D) =/= DeviceId
             ]
        of
            [] -> 'ok';
            OldDevices -> couch_mgr:save_docs(whapps_call:account_db(Call), OldDevices)
        end,
    {'ok', _} = couch_mgr:save_doc(whapps_call:account_db(Call), wh_json:set_value(<<"owner_id">>, AgentId, Device)).

-spec send_new_status(ne_binary(), ne_binary(), wh_amqp_worker:publish_fun()) -> 'ok'.
send_new_status(AcctId, AgentId, PubFun) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
                ,{<<"Agent-ID">>, AgentId}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    PubFun(Update).

-spec play_status_prompt(whapps_call:call(), ne_binary()) -> ne_binary().
play_status_prompt(Call, <<"login">>) ->
    whapps_call_command:prompt(<<"agent-logged_in">>, Call);
play_status_prompt(Call, <<"logout">>) ->
    whapps_call_command:prompt(<<"agent-logged_out">>, Call);
play_status_prompt(Call, <<"resume">>) ->
    whapps_call_command:prompt(<<"agent-resume">>, Call).

-spec play_failed_update(whapps_call:call()) -> ne_binary().
play_failed_update(Call) ->
    whapps_call_command:prompt(<<"agent-invalid_choice">>, Call).
