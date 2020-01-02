%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Handles changing an agent's status
%%% "data":{
%%%   "action":["login","logout"] // one of these
%%%   ,"id":"queue_id" // which queue to login/logout the caller
%%% }
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_acdc_queue).

-export([handle/2]).

-include_lib("callflow/src/callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    kapps_call_command:answer(Call),
    _ = case cf_acdc_agent:find_agent(Call) of
            {'ok', 'undefined'} ->
                lager:info("not an agent calling in"),
                cf_acdc_agent:play_not_an_agent(Call);
            {'ok', AgentId} ->
                Action = kz_json:get_ne_binary_value(<<"action">>, Data),
                QueueId = kz_json:get_ne_binary_value(<<"id">>, Data),
                Status = cf_acdc_agent:find_agent_status(Call, AgentId),

                update_queues(Call, AgentId, QueueId, Action),
                maybe_update_status(Call, AgentId, QueueId, Status, Action);
            {'error', 'multiple_owners'} ->
                lager:info("too many owners of device ~s, not logging in", [kapps_call:authorizing_id(Call)]),
                cf_acdc_agent:play_agent_invalid(Call)
        end,
    cf_exe:continue(Call).

maybe_update_status(Call, AgentId, QueueId, <<"logout">>, <<"login">>) ->
    lager:info("agent ~s is logged out, log in to queue ~s", [AgentId, QueueId]),

    update_status(Call, AgentId, <<"login">>),

    send_agent_message(Call, AgentId, QueueId, fun kapi_acdc_agent:publish_login_queue/1),
    kapps_call_command:b_prompt(<<"agent-logged_in">>, Call);
maybe_update_status(Call, AgentId, QueueId, _Curr, <<"login">>) ->
    send_agent_message(Call, AgentId, QueueId, fun kapi_acdc_agent:publish_login_queue/1),
    kapps_call_command:b_prompt(<<"agent-logged_in">>, Call);
maybe_update_status(Call, AgentId, _QueueId, <<"logout">>, _Action) ->
    lager:debug("agent ~s is logged out completely already", [AgentId]),
    cf_acdc_agent:play_agent_invalid(Call);
maybe_update_status(Call, AgentId, QueueId, _Status, <<"logout">>) ->
    send_agent_message(Call, AgentId, QueueId, fun kapi_acdc_agent:publish_logout_queue/1),
    kapps_call_command:b_prompt(<<"agent-logged_out">>, Call);
maybe_update_status(Call, _AgentId, _QueueId, _Status, _Action) ->
    lager:info("invalid agent action: ~s to ~s", [_Status, _Action]),
    cf_acdc_agent:play_agent_invalid(Call).

update_status(Call, AgentId, Status) ->
    Extra = [{<<"call_id">>, kapps_call:call_id(Call)}
            ,{<<"method">>, <<"callflow">>}
            ],

    'ok' = acdc_agent_util:update_status(kapps_call:account_id(Call), AgentId, Status, Extra).

send_agent_message(Call, AgentId, QueueId, PubFun) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, kapps_call:account_id(Call)}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Queue-ID">>, QueueId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    PubFun(Prop).

-spec update_queues(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()}
              | kz_datamgr:data_error().
update_queues(Call, AgentId, QueueId, <<"login">>) ->
    kz_datamgr:update_cache_doc(kapps_call:account_db(Call)
                               ,AgentId
                               ,fun (JObj) -> kzd_agent:maybe_add_queue(JObj, QueueId, 'skip') end
                               );
update_queues(Call, AgentId, QueueId, <<"logout">>) ->
    kz_datamgr:update_cache_doc(kapps_call:account_db(Call)
                               ,AgentId
                               ,fun (JObj) -> kzd_agent:maybe_rm_queue(JObj, QueueId, 'skip') end
                               ).
