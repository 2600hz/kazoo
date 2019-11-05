%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Starts a {@link cf_exe} and processes the "flow" sans DB.
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_route_resume).

-export([handle_req/2]).

-include("callflow.hrl").

-spec handle_req(kapi_callflow:resume(), kz_term:proplist()) -> 'ok'.
handle_req(ResumeJObj, _Props) ->
    'true' = kapi_callflow:resume_v(ResumeJObj),
    kz_log:put_callid(ResumeJObj),
    kz_amqp_worker:worker_pool(callflow_sup:pool_name()),

    try_handle_req(ResumeJObj, kz_amqp_worker:checkout_worker()).

-spec try_handle_req(kapi_callflow:resume(), {'ok', pid()} | {'error', any()}) -> 'ok'.
try_handle_req(JObj, {'error', _E}) ->
    lager:warning("unable to handle call resume, republishing: ~p", [_E]),
    _ = kz_amqp_worker:cast(JObj, fun kapi_callflow:publish_resume/1);
try_handle_req(JObj, {'ok', AMQPWorker}) ->
    lager:info("received call resume, taking control using worker ~p", [AMQPWorker]),
    _ = kz_amqp_channel:consumer_pid(AMQPWorker),

    Flow = kz_json:get_json_value(<<"Flow">>, JObj),
    Call = kapps_call:exec([{fun kapps_call:kvs_store/3, 'consumer_pid', AMQPWorker}
                           ,{fun kapps_call:kvs_store/3, 'cf_flow', Flow}
                           ]
                          ,kapps_call:from_json(kz_json:get_json_value(<<"Call">>, JObj))
                          ),

    handle_if_up(JObj, Call, kapps_call_command:b_channel_status(Call)).

-spec handle_if_up(kapi_callflow:resume(), kapps_call:call(), {'ok', kz_json:object()} | {'error', 'not_found'}) -> 'ok'.
handle_if_up(JObj, Call, {'ok', _ChannelStatus}) ->
    lager:info("channel is still up"),
    cf_util:flush_control_queue(Call),
    _ = cf_route_win:execute_callflow(JObj, Call),
    'ok';
handle_if_up(_JObj, _Call, {'error', 'not_found'}) ->
    lager:info("channel status failed, not resuming call").
