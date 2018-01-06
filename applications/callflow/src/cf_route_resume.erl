%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%% Starts a cf_exe and processes the "flow" sans DB
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_route_resume).

-export([handle_req/2]).

-include("callflow.hrl").

-spec handle_req(kz_json:object(), kz_proplist()) -> kapps_call:call().
handle_req(JObj, _Props) ->
    'true' = kapi_callflow:resume_v(JObj),
    Call0 = kapps_call:from_json(kz_json:get_json_value(<<"Call">>, JObj)),
    kapps_call:put_callid(Call0),
    lager:info("received call resume, taking control ~p", [Call0]),
    Flow = kz_json:get_value(<<"Flow">>, JObj),
    Call = kapps_call:kvs_store('cf_flow', Flow, Call0),
    cf_util:flush_control_queue(Call),
    cf_route_win:execute_callflow(JObj, Call).
