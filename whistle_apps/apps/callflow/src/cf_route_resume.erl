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

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_callflow:resume_v(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    whapps_call:put_callid(Call),
    lager:info("received call resume, taking control"),
    cf_route_win:maybe_restrict_call(
      whapps_call:kvs_store('cf_flow', wh_json:get_value(<<"Flow">>, JObj), Call)
     ).
