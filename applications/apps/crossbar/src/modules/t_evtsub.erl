-module(t_evtsub).

%% Test the full API using ibrowse to make calls to the rest endpoint
-export([start_full_test/0]).

start_full_test() ->
    ibrowse:start(),
    UrlBase = "http://localhost:8000/v1/accounts",

    Headers = [{"X-Auth-Token", "evtsub-test"}
	       ,{"Content-Type", "application/json"}
	       ,{"Accept", "application/json"}
	      ],

    try
    {ok, "200", _, JSON} = ibrowse:send_req(UrlBase, Headers, get),
    AcctJObj = mochijson2:decode(JSON),
    AcctId = whapps_json:get_value([<<"data">>, 1, <<"id">>], AcctJObj),

    EmptyEvtSubResp = [{[<<"data">>, <<"streams">>], []}
		       ,{[<<"data">>, <<"events">>], {struct, []}}
		      ],

    UrlEvtBase = lists:flatten([UrlBase, "/", whistle_util:to_list(AcctId), "/evtsub/"]),
    true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, get), "200", EmptyEvtSubResp),

    PutJSON = get_put_json(<<"directory.auth_req">>),
    DeleteJSON = get_delete_json(false),

    true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, put, PutJSON), "200", [{[<<"data">>, <<"streams">>], [<<"directory.auth_req">>]}]),
    
    true = verify_resp(ibrowse:send_req(UrlEvtBase, Headers, delete, DeleteJSON), "200", EmptyEvtSubResp)
    catch
	E:R ->
	    logger:format_log(error, "Error ~p:~p~n~p~n", [E, R, erlang:get_stacktrace()])
    end.

    %% do a PUT evtsub/ for directory
    %% do a PUT evtsub/ for dialplan
    %% publish a msg (or 20) to be received
    %% do a GET evtsub/directory and count events
    %% do a GET evtsub/dialplan and count events
    %% do a DELETE evtsub/
    %% do a GET evtsub/ nothing there

verify_resp({_,Code,_,JSON}, Code, Rules) ->
    logger:format_log(info, "JSON: ~s~n", [JSON]),
    JObj = mochijson2:decode(JSON),
    lists:all(fun({KeyPath, Result}) ->
		      V = whapps_json:get_value(KeyPath, JObj),
		      logger:format_log(info, "~p: Is ~p == ~p~n", [KeyPath, V, Result]),
		      V == Result
	      end, Rules);
verify_resp(_, _, _) -> false.

get_put_json(Stream) ->
    mochijson2:encode({struct, [{<<"data">>, {struct, [{<<"stream">>, Stream}
						       ,{<<"max_events">>, <<"2">>}
						      ]
					     }
				}
			       ]
		      }).

get_delete_json(Flush) ->
    mochijson2:encode({struct, [{<<"data">>, {struct, [{<<"flush">>, Flush}]}}]}).
