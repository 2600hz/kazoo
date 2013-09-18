%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%% Handles changing an agent's status
%%%
%%% "data":{
%%%   "action":["login","logout"] // one of these
%%%   ,"id":"queue_id" // which queue to login/logout the caller
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_acdc_queue).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),
    _ = case cf_acdc_agent:find_agent(Call) of
            {'ok', 'undefined'} ->
                lager:info("not an agent calling in"),
                cf_acdc_agent:play_not_an_agent(Call);
            {'ok', AgentId} ->
                Action = wh_json:get_value(<<"action">>, Data),
                QueueId = wh_json:get_value(<<"id">>, Data),
                Status = cf_acdc_agent:find_agent_status(Call, AgentId),

                update_queues(Call, AgentId, QueueId, Action),
                maybe_update_status(Call, AgentId, QueueId, Status, Action);
            {'error', 'multiple_owners'} ->
                lager:info("too many owners of device ~s, not logging in", [whapps_call:authorizing_id(Call)]),
                cf_acdc_agent:play_agent_invalid(Call)
        end,
    cf_exe:continue(Call).

maybe_update_status(Call, AgentId, QueueId, <<"logout">>, <<"login">>) ->
    lager:info("agent ~s is logged out, log in to queue ~s", [AgentId, QueueId]),

    update_status(Call, AgentId, <<"login">>),

    send_agent_message(Call, AgentId, QueueId, fun wapi_acdc_agent:publish_login_queue/1),
    whapps_call_command:b_prompt(<<"agent-logged_in">>, Call);
maybe_update_status(Call, AgentId, QueueId, _Curr, <<"login">>) ->
    send_agent_message(Call, AgentId, QueueId, fun wapi_acdc_agent:publish_login_queue/1),
    whapps_call_command:b_prompt(<<"agent-logged_in">>, Call);
maybe_update_status(Call, AgentId, _QueueId, <<"logout">>, _Action) ->
    lager:debug("agent ~s is logged out completely already", [AgentId]),
    cf_acdc_agent:play_agent_invalid(Call);
maybe_update_status(Call, AgentId, QueueId, _Status, <<"logout">>) ->
    send_agent_message(Call, AgentId, QueueId, fun wapi_acdc_agent:publish_logout_queue/1),
    whapps_call_command:b_prompt(<<"agent-logged_out">>, Call);
maybe_update_status(Call, _AgentId, _QueueId, _Status, _Action) ->
    lager:info("invalid agent action: ~s to ~s", [_Status, _Action]),
    cf_acdc_agent:play_agent_invalid(Call).

update_status(Call, AgentId, Status) ->
    Extra = [{<<"call_id">>, whapps_call:call_id(Call)}
             ,{<<"method">>, <<"callflow">>}
            ],

    {'ok', _D} = acdc_util:update_agent_status(whapps_call:account_id(Call), AgentId, Status, Extra).

send_agent_message(Call, AgentId, QueueId, PubFun) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, whapps_call:account_id(Call)}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Queue-ID">>, QueueId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    PubFun(Prop).

update_queues(Call, AgentId, QueueId, <<"login">>) ->
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call), AgentId) of
        {'error', _} -> 'ok';
        {'ok', AgentJObj} ->
            maybe_add_queue(Call, AgentJObj, QueueId)
    end;
update_queues(Call, AgentId, QueueId, <<"logout">>) ->
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call), AgentId) of
        {'error', _} -> 'ok';
        {'ok', AgentJObj} ->
            maybe_rm_queue(Call, AgentJObj, QueueId)
    end.

maybe_add_queue(Call, AgentJObj, QueueId) ->
    Qs = wh_json:get_value(<<"queues">>, AgentJObj, []),
    case lists:member(QueueId, Qs) of
        'false' ->
            save_agent(Call, wh_json:set_value(<<"queues">>, [QueueId | Qs], AgentJObj));
        'true' -> 'ok'
    end.

maybe_rm_queue(Call, AgentJObj, QueueId) ->
    Qs = wh_json:get_value(<<"queues">>, AgentJObj, []),
    case lists:member(QueueId, Qs) of
        'true' ->
            Qs1 = lists:delete(QueueId, Qs),
            QRemoved = wh_json:set_value(<<"queues">>, Qs1, AgentJObj),
            save_agent(Call, QRemoved);
        'false' -> 'ok'
    end.

save_agent(Call, AgentJObj) ->
    couch_mgr:save_doc(whapps_call:account_db(Call), AgentJObj).
