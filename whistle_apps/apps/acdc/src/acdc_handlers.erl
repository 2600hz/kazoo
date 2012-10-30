%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
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
    true = wapi_route:req_v(JObj),
    Call = whapps_call:set_controller_queue(props:get_value(queue, Props)
                                            ,whapps_call:from_route_req(JObj)
                                           ),
    AcctId = whapps_call:account_id(Call),
    Id = whapps_call:request_user(Call),
    maybe_route_respond(JObj, Call, AcctId, Id).

maybe_route_respond(_JObj, _Call, undefined, _Id) -> ok;
maybe_route_respond(ReqJObj, Call, AcctId, Id) ->
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call), Id) of
        {error, _} -> ok;
        {ok, Doc} ->
            maybe_route_respond(ReqJObj, Call, AcctId, Id, wh_json:get_value(<<"pvt_type">>, Doc))
    end.

maybe_route_respond(ReqJObj, Call, AcctId, QueueId, <<"queue">> = T) ->
    send_route_response(ReqJObj, Call, AcctId, QueueId, T);
maybe_route_respond(ReqJObj, Call, AcctId, AgentId, <<"user">> = T) ->
    send_route_response(ReqJObj, Call, AcctId, AgentId, T);
maybe_route_respond(_ReqJObj, _Call, _AcctId, _Id, _) ->
    ok.

send_route_response(ReqJObj, Call, AcctId, Id, Type) ->
    CCVs = [{<<"ACDc-ID">>, Id}
            ,{<<"ACDc-Type">>, Type}
           ],

    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, ReqJObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
            | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
           ],
    wapi_route:publish_resp(wh_json:get_value(<<"Server-ID">>, ReqJObj), Resp),
    _ = whapps_call:cache(Call),
    lager:debug("sent route response to park the call for ~s(~s)", [Id, AcctId]).

handle_route_win(JObj, _Props) ->
    true = wapi_route:win_v(JObj),

    case whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj)) of
        {ok, Call} ->
            Call1 = whapps_call:from_route_win(JObj, Call),
            whapps_call_command:answer(Call1),

            case catch update_acdc_actor(Call1) of
                {'EXIT', _R} -> lager:debug("crash: ~p", [_R]);
                _R -> timer:sleep(5000), lager:debug("update resulted in ~p", [_R])
            end,
            whapps_call_command:hangup(Call1);
        {error, _R} ->
            lager:debug("failed to retrieve cached call: ~p", [_R])
    end.

update_acdc_actor(Call) ->
    update_acdc_actor(Call
                      ,whapps_call:custom_channel_var(<<"ACDc-ID">>, Call)
                      ,whapps_call:custom_channel_var(<<"ACDc-Type">>, Call)
                     ).
update_acdc_actor(_Call, undefined, _) -> ok;
update_acdc_actor(_Call, _, undefined) -> ok;
update_acdc_actor(Call, _QueueId, <<"queue">>) -> whapps_call_command:hangup(Call);
update_acdc_actor(Call, AgentId, <<"user">>) ->
    AcctId = whapps_call:account_id(Call),

    case acdc_util:agent_status(AcctId, AgentId) of
        <<"logout">> -> update_acdc_agent(Call, AcctId, AgentId, <<"login">>, fun wapi_acdc_agent:publish_login/1);
        <<"pause">> -> update_acdc_agent(Call, AcctId, AgentId, <<"resume">>, fun wapi_acdc_agent:publish_resume/1);
        _ -> update_acdc_agent(Call, AcctId, AgentId, <<"logout">>, fun wapi_acdc_agent:publish_logout/1)
    end.

update_acdc_agent(Call, AcctId, AgentId, Status, PubFun) ->
    lager:debug("going to new status ~s", [Status]),

    AcctDb = whapps_call:account_db(Call),
    Doc = wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                             ,{<<"agent_id">>, AgentId}
                             ,{<<"method">>, <<"acdc_blf">>}
                             ,{<<"action">>, Status}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                            ]),
    {ok, _D} = couch_mgr:save_doc(AcctDb, wh_doc:update_pvt_parameters(Doc, AcctDb)),
    lager:debug("saved status doc ~p", [_D]),

    send_new_status(AcctId, AgentId, PubFun),
    play_status_prompt(Call, Status).

-spec send_new_status/3 :: (ne_binary(), ne_binary(), wh_amqp_worker:publish_fun()) -> 'ok'.
send_new_status(AcctId, AgentId, PubFun) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
                ,{<<"Agent-ID">>, AgentId}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    PubFun(Update).

play_status_prompt(Call, <<"login">>) ->
    whapps_call_command:prompt(<<"agent-logged_in">>, Call);
play_status_prompt(Call, <<"logout">>) ->
    whapps_call_command:prompt(<<"agent-logged_out">>, Call);
play_status_prompt(Call, <<"resume">>) ->
    whapps_call_command:prompt(<<"agent-resume">>, Call).
