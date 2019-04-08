%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Starts a {@link cf_exe} and processes the "flow" sans DB.
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_route_resume).

-export([handle_req/2]).

-include("callflow.hrl").

-spec handle_req(kapi_callflow:resume(), kz_term:proplist()) -> 'ok'.
handle_req(ResumeJObj, _Props) ->
    'true' = kapi_callflow:resume_v(ResumeJObj),
    kz_util:put_callid(ResumeJObj),

    try_handle_req(ResumeJObj, kz_amqp_worker:checkout_worker(callflow_sup:pool_name())).

-spec try_handle_req(kapi_callflow:resume(), {'ok', pid()} | {'error', any()}) -> 'ok'.
try_handle_req(JObj, {'error', _E}) ->
    lager:warning("unable to handle call resume, republishing: ~p", [_E]),
    _ = kz_amqp_worker:cast(JObj, fun kapi_callflow:publish_resume/1);
try_handle_req(JObj, {'ok', AMQPWorker}) ->
    lager:info("received call resume, taking control using worker ~p", [AMQPWorker]),
    kz_amqp_worker:worker_pool(callflow_sup:pool_name()),

    Flow = kz_json:get_json_value(<<"Flow">>, JObj),
    Call = kapps_call:exec([{fun kapps_call:kvs_store/3, 'consumer_pid', AMQPWorker}
                           ,{fun kapps_call:kvs_store/3, 'cf_flow', Flow}
                           ]
                          ,kapps_call:from_json(kz_json:get_json_value(<<"Call">>, JObj))
                          ),

    cf_util:flush_control_queue(Call),
    _ = cf_route_win:execute_callflow(JObj, Call),
    'ok'.
