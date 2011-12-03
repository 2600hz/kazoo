%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Bind to wapi_authn bindings, exclude ones not for this account, and call the URI
%%% supplied when a valid authn_req is received, hopefully receive a valid
%%% authn_resp, and send it along to Whistle.
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hook_authn).

-behaviour(gen_hook).

%% gen_hook callbacks
-export([init/0, add_binding/1, handle_req/2, add_followup_binding/2]).

-include("webhooks.hrl").

%% called by gen_listener
-spec init/0 :: () -> 'ok'.
init() -> 'ok'.

%% called by webhook_acct
-spec add_binding/1 :: (pid() | atom()) -> 'ok'.
add_binding(Srv) ->
    gen_listener:add_binding(Srv, authn, []), % add AMQP bindings
    gen_listener:add_responder(Srv, ?MODULE, [{<<"directory">>, <<"authn_req">>}]). % register callbacks

%% called by gen_listener after calling webhook_acct:handle_event to get Props
-spec handle_req/2 :: (json_object(), proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    wh_util:put_callid(JObj),

    ?LOG("Starting authn_req webhook"),

    case {props:get_value(realm, Props), wapi_authn:get_auth_realm(JObj)} of
	{R, R} -> ?LOG("Realms (~s) match", [R]);
	{R, AR} ->
	    ?LOG("Realm ~s doesn't match requested realm ~s", [R, AR]),
	    exit(mismatched_realm)
    end,

    Hooks = orddict:fetch(<<"authn">>, props:get_value(hooks, Props)), %% list of callbacks to call for the request

    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    MyQueue = props:get_value(queue, Props),
    Srv = props:get_value(server, Props),

    Self = self(),

    Reqs = [spawn_monitor(fun() -> gen_hook:call_webhook(Self, Hook, JObj) end) || Hook <- Hooks],
    gen_hook:wait_for_resps(Reqs, RespQ, MyQueue, fun wapi_authn:publish_resp/2, Srv).


%% If an authn_resp includes a callback uri, bind to the reg_success event and send it to the callback uri.
