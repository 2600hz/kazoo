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
-export([init/0, add_binding/1, handle_req/2]).

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
    Hooks = props:get_value(hooks, Props), %% list of callbacks to call for the request

    Q = wh_json:get_value(<<"Server-ID">>, JObj),
    Self = self(),
    Reqs = [spawn_monitor(fun() -> call_webhook(Self, Hook, JObj) end) || Hook <- Hooks],
    wait_for_resps(Reqs, Q).

-spec wait_for_resps/2 :: ([{pid(), reference()},...] | [], ne_binary()) -> 'ok'.
wait_for_resps([], _) -> ok;
wait_for_resps(Reqs, Q) ->
    receive
	{ok, Resp} ->
	    ?LOG("Authn resp recv: ~p", [Resp]),
	    wapi_authn:publish_resp(Q, Resp),
	    wait_for_resps(Reqs, Q);
	{'DOWN', Ref, process, Pid, Reason} ->
	    ?LOG("Pid ~p down: ~p", [Pid, Reason]),
	    wait_for_resps(lists:keydelete(Ref, 2, Reqs), Q)
    end.

-spec call_webhook/3 :: (pid(), json_object(), json_object()) -> no_return().
call_webhook(Parent, Hook, JObj) ->
    wh_util:put_callid(JObj),
    Uri = wh_json:get_value(<<"callback_uri">>, Hook),
    Method = get_method_atom(wh_json:get_binary_value(<<"http_method">>, Hook, <<"post">>)),

    ?LOG("Sending json to ~s using method ~s", [Uri, Method]),

    Retries = wh_json:get_integer_value(<<"retries">>, Hook, 3),

    try_send_req(Uri, Method, Parent, JObj, Retries).

-spec try_send_req/5 :: (ne_binary(), 'put' | 'post' | 'get', pid(), json_object(), non_neg_integer()) -> no_return().
try_send_req(_, _, _, _, R) when R =< 0 -> ?LOG("Retries exceeded");
try_send_req(Uri, Method, Parent, JObj, Retries) ->
    case ibrowse:send_req(wh_util:to_list(Uri)
			  ,[{"content-type", "application/json"}
			    ,{"accept", "application/json"}
			   ]
			  ,Method, wh_json:encode(JObj)) of
	{ok, Status, _ResponseHeaders, ResponseBody} ->
	    ?LOG("Recv status ~s: ~s", [Status, ResponseBody]),
	    Parent ! {ok, wh_json:decode(ResponseBody)};
	{error, Reason} ->
	    ?LOG("Request failed: ~p", [Reason]),
	    try_send_req(Uri, Method, Parent, JObj, Retries-1)
    end.

-spec get_method_atom/1 :: (<<_:24,_:_*8>>) -> 'put' | 'post' | 'get'.
get_method_atom(<<"put">>) -> put;
get_method_atom(<<"post">>) -> post;
get_method_atom(<<"get">>) -> get.
