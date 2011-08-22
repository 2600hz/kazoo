-module(t_evtsub).

-include("crossbar.hrl").

%% Test the full API using ibrowse to make calls to the rest endpoint
-export([start_full_test/0]).

start_full_test() ->
    ibrowse:start(),
    logger:start(),

    UrlBase = "http://localhost:8000/v1/accounts",

    Headers = [{"X-Auth-Token", "evtsub-test"}
	       ,{"Content-Type", "application/json"}
	       ,{"Accept", "application/json"}
	      ],

    EmptyEvtSubResp = [{[<<"data">>, <<"streams">>], []}
		       ,{[<<"data">>, <<"events">>], ?EMPTY_JSON_OBJECT}
		      ],

    MaxEvents = 5,

    PutJSON = get_put_json(<<"directory.authn_req">>, MaxEvents),
    DeleteJSON = get_delete_json(false),

    logger:format_log(info, "GET ~s~n", [UrlBase]),
    {ok, "200", _, JSON} = ibrowse:send_req(UrlBase, Headers, get),
    AcctJObj = mochijson2:decode(JSON),
    AcctId = wh_json:get_value([<<"data">>, 1, <<"id">>], AcctJObj),

    UrlEvtBase = lists:flatten([UrlBase, "/", wh_util:to_list(AcctId), "/evtsub/"]),

    try
	logger:format_log(info, "DELETE ~s ~s~n", [UrlEvtBase, DeleteJSON]),
	true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, delete, DeleteJSON), "200", EmptyEvtSubResp),

	logger:format_log(info, "GET ~s~n", [UrlEvtBase]),
	true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, get), "200", EmptyEvtSubResp),

	logger:format_log(info, "PUT ~s ~s~n", [UrlEvtBase, PutJSON]),
	true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, put, PutJSON), "200", [{[<<"data">>, <<"streams">>], [<<"directory.authn_req">>]}]),

	logger:format_log(info, "DELETE ~s ~s~n", [UrlEvtBase, DeleteJSON]),
	true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, delete, DeleteJSON), "200", EmptyEvtSubResp),

	logger:format_log(info, "PUT ~s ~s~n", [UrlEvtBase, PutJSON]),
	true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, put, PutJSON), "200", [{[<<"data">>, <<"streams">>], [<<"directory.authn_req">>]}]),

	PublishNTimes = 25, % divisible by MaxEvents please
	lists:foreach(fun(_) -> publish_authn_req() end, lists:seq(1, PublishNTimes)),

	CmpFun = fun(V) ->
			 logger:format_log(info, "Len == ~p~n", [length(V)]),
			 length(V) =:= MaxEvents
		 end,
	
	lists:foreach(fun(_) ->
			      logger:format_log(info, "GET ~s~n", [UrlEvtBase]),
			      true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, get), "200", [{[<<"data">>, <<"events">>, <<"directory.authn_req">>]
												      , CmpFun
												     }])
		      end, lists:seq(1, PublishNTimes div MaxEvents)),

	logger:format_log(info, "DELETE ~s ~s~n", [UrlEvtBase, DeleteJSON]),
	true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, delete, DeleteJSON), "200", EmptyEvtSubResp),

	logger:format_log(info, "Testing evtsub successful~n", [])
    catch
	E:R ->
	    logger:format_log(info, "DELETE ~s ~s~n", [UrlEvtBase, DeleteJSON]),
	    true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, delete, DeleteJSON), "200", EmptyEvtSubResp),
	    logger:format_log(error, "Error ~p:~p~n~p~n", [E, R, erlang:get_stacktrace()])
    end.

verify_resp({_,Code,_,JSON}, Code, Rules) ->
    logger:format_log(info, "JSON: ~s~n", [JSON]),
    JObj = mochijson2:decode(JSON),
    lists:all(
      fun({KeyPath, Fun}) when is_function(Fun) ->
	      V = wh_json:get_value(KeyPath, JObj),
	      Fun(V);
	 ({KeyPath, Result}) ->
	      V = wh_json:get_value(KeyPath, JObj),
	      logger:format_log(info, "~p: Is ~p == ~p~n", [KeyPath, V, Result]),
	      V == Result
      end, Rules);
verify_resp(_, _, _) -> false.

get_put_json(Stream, MaxEvts) ->
    mochijson2:encode({struct, [{<<"data">>, {struct, [{<<"stream">>, Stream}
						       ,{<<"max_events">>, MaxEvts}
						      ]
					     }
				}
			       ]
		      }).

get_delete_json(Flush) ->
    mochijson2:encode({struct, [{<<"data">>, {struct, [{<<"flush">>, Flush}]}}]}).

publish_authn_req() ->
    JSON = [123,[34,<<"Auth-Domain">>,34],58,[34,<<"auth_realm">>,34],44,[34,<<"Auth-User">>,34],58,[34,<<"auth_user">>,34],44,[34,<<"Orig-IP">>,34],58
	    ,[34,<<"1.2.3.4">>,34],44,[34,<<"From">>,34],58,[34,<<"from@domain.com">>,34],44,[34,<<"To">>,34],58,[34,<<"to@domain.com">>,34]
	    ,44,[34,<<"Msg-ID">>,34],58,[34,<<"id">>,34],44,[34,<<"App-Version">>,34],58,[34,<<"vsn">>,34],44,[34,<<"App-Name">>,34],58,[34,<<"app">>,34]
	    ,44,[34,<<"Event-Name">>,34],58,[34,<<"authn_req">>,34],44,[34,<<"Event-Category">>,34],58,[34,<<"directory">>,34],44,[34,<<"Server-ID">>,34]
	    ,58,[34,<<"srv">>,34],125],
    amqp_util:callmgr_publish(whapps_controller:get_amqp_host(), JSON, <<"application/json">>, <<"auth.req">>).
