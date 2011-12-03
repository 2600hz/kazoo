%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% The Hook behaviour
%%%
%%% Hook modules provide several functions for working with webhook_acct
%%% instances and sending/receiving JSON.
%%%
%%% add_binding/1 :: (pid()) -> 'ok'
%%%   Takes, as argument, the webhook_acct server's PID
%%%   
%%%
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james2600hz.org>
%%%-------------------------------------------------------------------
-module(gen_hook).

-export([call_webhook/3, wait_for_resps/4, wait_for_resps/5, no_resp/2]).

-export([behaviour_info/1]).

-include("webhooks.hrl").

behaviour_info(callbacks) ->
    [{init, 0} %% gen_listener:add_responder looks for a init/0
     ,{handle_req, 2} %% when receiving an AMQP event, handle_req/2 is called
     ,{add_binding, 1} %% takes the webhook_acct's PID to invoke the add_binding/2
    ];
behaviour_info(_) ->
    undefined.

%% When sending events, route_win, etc, that don't require a response from the webhook
-spec no_resp/2 :: (_, _) -> 'ok'.
no_resp(_, _) ->
    ?LOG("Not publishing the response from webhook").

-spec wait_for_resps/4 :: ([{pid(), reference()},...] | [], ne_binary(), ne_binary(), fun((ne_binary(), json_object()) -> 'ok')) -> 'ok'.
-spec wait_for_resps/5 :: ([{pid(), reference()},...] | [], ne_binary(), ne_binary(), fun((ne_binary(), json_object()) -> 'ok'), pid() | 'undefined') -> 'ok'.
wait_for_resps(Reqs, RespQ, MyQ, PubFun) ->
    wait_for_resps(Reqs, RespQ, MyQ, PubFun, undefined).

wait_for_resps([], _, _, _, _) -> ok;
wait_for_resps(Reqs, RespQ, MyQ, PubFun, AcctSrv) ->
    receive
	{ok, Resp} ->
	    ?LOG("response received"),
	    case catch(PubFun(RespQ, wh_json:set_value(<<"Server-ID">>, MyQ, Resp))) of
		ok ->
		    ?LOG("Response sent successfully"),
		    check_for_callback(Resp, AcctSrv);
		{'EXIT', _Why} ->
		    ?LOG("Failed to send response: ~p", [_Why])
	    end,
	    wait_for_resps(Reqs, RespQ, MyQ, PubFun);
	{'DOWN', Ref, process, Pid, Reason} ->
	    ?LOG("Pid ~p down: ~p", [Pid, Reason]),
	    wait_for_resps(lists:keydelete(Ref, 2, Reqs), RespQ, MyQ, PubFun)
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
try_send_req(Uri, Method, Parent, ReqJObj, Retries) ->
    try
	case ibrowse:send_req(wh_util:to_list(Uri)
			      ,[{"content-type", "application/json"}
				,{"accept", "application/json"}
			       ]
			      ,Method, wh_json:encode(ReqJObj)) of
	    {ok, Status, ResponseHeaders, ResponseBody} ->
		?LOG("Resp status: ~s", [Status]),
		_ = [?LOG("Resp header: ~s: ~s", [K,V]) || {K,V} <- ResponseHeaders],
		?LOG("Resp body: ~s", [ResponseBody]),

		?DEFAULT_CONTENT_TYPE = wh_util:to_binary(props:get_value("Content-Type", ResponseHeaders)),

		Parent ! {ok, wh_json:decode(ResponseBody)};
	    {error, Reason} ->
		?LOG("Request failed: ~p", [Reason]),
		try_send_req(Uri, Method, Parent, ReqJObj, Retries-1)
	end
    catch
	T:R ->
	    ?LOG("Caught ~s:~p", [T, R]),
	    try_send_req(Uri, Method, Parent, ReqJObj, Retries-1)
    end.

-spec get_method_atom/1 :: (<<_:24,_:_*8>>) -> 'put' | 'post' | 'get'.
get_method_atom(<<"put">>) -> put;
get_method_atom(<<"post">>) -> post;
get_method_atom(<<"get">>) -> get.

check_for_callback(Resp, AcctSrv) ->
    ok.
