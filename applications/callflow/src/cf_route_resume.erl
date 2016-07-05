%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Starts a cf_exe and processes the "flow" sans DB
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_route_resume).

-export([handle_req/2]).

-include("callflow.hrl").

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_callflow:resume_v(JObj),
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    kapps_call:put_callid(Call),
    lager:info("received call resume, taking control"),
    cf_route_win:execute_callflow(
      JObj
				 ,kapps_call:kvs_store('cf_flow', kz_json:get_value(<<"Flow">>, JObj), Call)
     ).
