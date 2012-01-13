%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%% Handle customer connect requests, bridge customer to next available
%%% agent
%%% @end
%%% Created : 11 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(dg_customer).

-export([init/0, handle_req/2]).

-include("datinggame.hrl").

init() ->
    ok.

-spec handle_req/2 :: (json_object(), proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    Customer = #dg_customer{
      call_id = wh_json:get_value(<<"Call-ID">>, JObj)
      ,control_queue = wh_json:get_value(<<"Control-Queue">>, JObj)
      ,skills_needed = wh_json:get_value(<<"Skills-Needed">>, JObj, wh_json:new())
      ,record_call = wh_json:is_true(<<"Record-Call">>, JObj, true)
     },
    Srv = props:get_value(server, Props),
    datinggame_listener:connect_agent(Srv, Customer).
