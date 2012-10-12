%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),
    case cf_acdc_agent:find_agent(Call) of
        {ok, undefined} ->
            lager:debug("not an agent calling in"),
            cf_acdc_agent:play_not_an_agent(Call);
        {ok, AgentId} ->
            Action = wh_json:get_value(<<"action">>, Data),
            QueueId = wh_json:get_value(<<"id">>, Data),
            lager:debug("agent ~s wants to ~s to queue ~s", [AgentId, Action, QueueId]),
            notify_agent(Call, AgentId, QueueId, Action)
    end,
    cf_exe:continue(Call).

notify_agent(Call, AgentId, QueueId, <<"login">>) ->
    send_agent_message(Call, AgentId, QueueId, fun wapi_acdc_agent:publish_login_queue/1);
notify_agent(Call, AgentId, QueueId, <<"logout">>) ->
    send_agent_message(Call, AgentId, QueueId, fun wapi_acdc_agent:publish_logout_queue/1);
notify_agent(Call, _AgentId, _QueueId, _Action) ->
    lager:debug("invalid agent action: ~s", [_Action]),
    cf_acdc_agent:play_agent_invalid(Call).

send_agent_message(Call, AgentId, QueueId, PubFun) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, whapps_call:account_id(Call)}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Queue-ID">>, QueueId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    PubFun(Prop).
