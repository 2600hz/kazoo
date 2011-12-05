%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% 
%%% @end
%%% Created :  1 Dec 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hook_route).

-export([init/0, add_binding/1, handle_req/2]).

-export([win_resp/2]).

-include("webhooks.hrl").

%% called by gen_listener
-spec init/0 :: () -> 'ok'.
init() -> 'ok'.

%% called by webhook_acct
-spec add_binding/1 :: (pid() | atom()) -> 'ok'.
add_binding(Srv) ->
    gen_listener:add_binding(Srv, route, []), % add AMQP bindings
    gen_listener:add_responder(Srv, ?MODULE, [{<<"dialplan">>, <<"route_req">>}
					      ,{<<"dialplan">>, <<"route_win">>}
					     ]). % register callbacks

%% called by gen_listener after calling webhook_acct:handle_event to get Props
-spec handle_req/2 :: (json_object(), proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    wh_util:put_callid(JObj),

    case wapi_route:req_v(JObj) of
	true ->
	    ?LOG("Starting route_req webhook"),
	    process_req(JObj, Props);
	false ->
	    ?LOG("Starting route_win webhook"),
	    process_win(JObj, Props)
    end.

process_req(JObj, Props) ->
    case {props:get_value(realm, Props), wapi_route:get_auth_realm(JObj)} of
	{R, R} -> ?LOG("Realms (~s) match", [R]);
	{R, AR} ->
	    ?LOG("Realm ~s doesn't match requested realm ~s", [R, AR]),
	    exit(mismatched_realm)
    end,

    Hooks = orddict:fetch(<<"route">>, props:get_value(hooks, Props)), %% list of callbacks to call for the request

    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    MyQ = props:get_value(queue, Props),
    Self = self(),

    Reqs = [spawn_monitor(fun() -> gen_hook:call_webhook(Self, Hook, JObj) end) || Hook <- Hooks],
    gen_hook:wait_for_resps(Reqs, RespQ, MyQ, fun wapi_route:publish_resp/2).

process_win(JObj, Props) ->
    true = wapi_route:win_v(JObj),

    Hooks = orddict:fetch(<<"route">>, props:get_value(hooks, Props)), %% list of callbacks to call for the win

    RespQ = wh_json:get_value(<<"Control-Queue">>, JObj),
    MyQ = props:get_value(queue, Props),
    Self = self(),

    Reqs = [spawn_monitor(fun() ->
				  gen_hook:call_webhook(Self, wh_json:set_value(<<"retries">>, 1, Hook), JObj)
			  end)
	    || Hook <- Hooks],
    gen_hook:wait_for_resps(Reqs, RespQ, MyQ, fun ?MODULE:win_resp/2).

%% RespQ is 
-spec win_resp/2 :: (ne_binary(), json_object()) -> 'ok'.
win_resp(RespQ, RespJObj) ->
    true = wapi_webhooks:v(RespJObj),
    wapi_webhooks:publish_action(RespQ, RespJObj).
