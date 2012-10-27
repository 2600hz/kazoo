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
            update_acdc_actor(whapps_call:from_route_win(JObj, Call)),
            whapps_call_command:hangup(Call);
        {error, _R} ->
            lager:debug("failed to retrieve cached call: ~p", [_R])
    end.

update_acdc_actor(Call) ->
    update_acdc_actor(Call
                      ,whapps_call:custom_channel_var(<<"ACDc-ID">>, Call)
                      ,whapps_call:custom_channel_var(<<"ACDc-Type">>, Call)
                     ).
update_acdc_actor(Call, undefined, _) -> ok;
update_acdc_actor(Call, _, undefined) -> ok;
update_acdc_actor(Call, QueueId, <<"queue">>) -> whapps_call_command:hangup(Call);
update_acdc_actor(Call, AgentId, <<"user">>) ->
    AcctId = whapps_call:account_id(Call),

    case acdc_util:agent_status(AcctId, AgentId) of
        <<"logout">> ->
            {ok, Doc} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), AgentId),
            lager:debug("recv win to toggle ~s(~s) from logout", [AgentId, AcctId]),
            acdc_agents_sup:new(Doc),
            whapps_call_command:b_prompt(<<"agent-logged_in">>, Call);
        <<"login">> ->
            lager:debug("recv win to toggle ~s(~s) from login", [AgentId, AcctId]),
            Super = acdc_agents_sup:find_agent_supervisor(AcctId, AgentId),
            whapps_call_command:b_prompt(<<"agent-logged_out">>, Call)
    ok.

update_agent_status() ->
    AcctDb = whapps_call:account_db(Call),
    Doc = wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                             ,{<<"agent_id">>, AgentId}
                             ,{<<"method">>, <<"callflow">>}
                             ,{<<"action">>, Status}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                            ]),
    {ok, _D} = couch_mgr:save_doc(AcctDb, wh_doc:update_pvt_parameters(Doc, AcctDb)),
