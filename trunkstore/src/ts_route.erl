%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Respond to Route requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_route).

%% API
-export([start_inbound_handler/4, start_outbound_handler/3]).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

% just want to deal with binary K/V pairs
-type proplist() :: list(tuple(binary(), binary())) | [].

-define(APP_NAME, <<"ts_responder.route">>).
-define(APP_VERSION, <<"0.1">>).

-record(state, {to = <<"">> :: binary()
		,from = <<"">> :: binary()
		,customer_info = [] :: proplist()
		,carrier_info = [] :: proplist()
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Start handler for outbound (Customer to Carrier (could be same server))
%% Recv a Route request from ts_responder for To/From combo
%% @end
%%--------------------------------------------------------------------
-spec(start_outbound_handler/3 :: (To :: binary(), From :: binary(), CustInfo :: proplist()) -> pid()).
start_outbound_handler(To, From, CustInfo) ->
    spawn(fun() -> handle_outbound_route_req(#state{to=To
						    ,from=From
						    ,customer_info=CustInfo
						   })
	  end).

%%--------------------------------------------------------------------
%% @doc Start handler for inbound (Carrier to Customer)
%% Recv a Route request from ts_responder for To/From combo
%% @end
%%--------------------------------------------------------------------
-spec(start_inbound_handler/4 :: (To :: binary(), From :: binary()
				,CustInfo :: proplist(), CarrierInfo :: proplist()) -> pid()).
start_inbound_handler(To, From, CustInfo, CarrierInfo) ->
    spawn(fun() -> handle_inbound_route_req(#state{to=To
						   ,from=From
						   ,customer_info=CustInfo
						   ,carrier_info=CarrierInfo
						  })
	  end).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(handle_outbound_route_req/1 :: (State :: tuple()) -> no_return()).
handle_outbound_route_req(#state{to=To, from=From}=State) ->
    receive
	{route_req, ResponderPid, To, From, RouteProp} ->
	    get_outbound_route(State, RouteProp, ResponderPid);
	_Other ->
	    format_log(info, "TS_ROUTE:O(~p): Recv Other: ~p~n", [_Other]),
	    handle_outbound_route_req(State)
    after 30000 ->
	    format_log(error, "TS_ROUTE:O(~p): Timeout, no route req came~n", [])
    end.

-spec(handle_inbound_route_req/1 :: (State :: tuple()) -> no_return()).
handle_inbound_route_req(#state{to=To, from=From}=State) ->
    receive
	{route_req, ResponderPid, To, From, RouteProp} ->
	    get_inbound_route(State, RouteProp, ResponderPid);
	_Other ->
	    format_log(info, "TS_ROUTE:I(~p): Recv Other: ~p~n", [_Other]),
	    handle_inbound_route_req(State)
    after 30000 ->
	    format_log(error, "TS_ROUTE:I(~p): Timeout, no route req came~n", [])
    end.

get_inbound_route(#state{customer_info=CustInfo}, RouteProp, Pid) ->
    Routes1 = [ get_primary_route(CustInfo, RouteProp) | get_failover_routes(CustInfo, RouteProp)],
    case has_credit(CustInfo, Routes1) of
	false ->
	    send_response(Pid, RouteProp, 503, []);
	true ->
	    track_call(CustInfo, RouteProp, Pid, Routes1)
    end.

get_outbound_route(#state{customer_info=CustInfo}, RouteProp, Pid) ->
    case get_outbound_routes(CustInfo, RouteProp) of
	[] ->
	    send_response(Pid, RouteProp, 404, []);
	Routes0 ->
	    Routes1 = condition_routes(Routes0),
	    track_call(CustInfo, RouteProp, Pid, Routes1)
    end.


-spec(get_primary_route/2 :: (CustInfo :: proplist(), RouteProp :: proplist()) -> proplist()).
get_primary_route(CustInfo, RouteProp) ->
    %% <Route> API
    [].

%% get the failover routes (if specified) for this call. 
-spec(get_failover_routes/2 :: (CustInfo :: proplist(), RouteProp :: proplist()) -> list(proplist())).
get_failover_routes(CustInfo, ReqProp) ->
    %% [<Route>]
    [].

-spec(get_outbound_routes/2 :: (CustInfo :: proplist(), RouteProp :: proplist()) -> list(proplist())).
get_outbound_routes(CustInfo, ReqProp) ->
    %% [<Route>]
    [].

track_call(CustInfo, RouteProp, Pid, Routes) ->
    MyQ = setup_queues(RouteProp),
    send_response(Pid, [{<<"Server-ID">>, MyQ} | RouteProp], 200, Routes),
    wait_for_hangup(CustInfo).

%% create a listener for the Call's Event Queue and a Queue
%% for CallMgr to return the Call's Control Queue
setup_queues(RouteProp) ->
    %% consume call's event messages
    %% create targeted queue for this process
    undefined.

wait_for_hangup(CustInfo) ->
    receive
	_Other ->
	    format_log(info, "TS_ROUTE(~p): Waiting for hangup, got ~p~n", [_Other])
    end.

%% eventually do E911 conditioning, etc...
%% will involve adding channel variables (like caller-id stuff) to routes
-spec(condition_routes/1 :: (Routes :: list(proplist())) -> list(proplist())).
condition_routes(Routes) ->
    Routes.

%% compare credits to Routes and remove Route entries if now enough credits exist
has_credit(CustInfo, Routes) ->
    false.

-spec(send_response/4 :: (Pid :: pid(), Prop :: proplist(), Code :: integer(), Routes :: proplist()) -> no_return()).
send_response(Pid, Prop, Code, Routes) ->
    Data0 = [{<<"App-Name">>, ?APP_NAME}
	     ,{<<"App-Version">>, ?APP_VERSION}
	     | Prop],
    Data1 = lists:umerge(specific_response(Code, Routes), Data0),
    {ok, JSON} = whistle_api:auth_resp(Data1),
    Pid ! {json, JSON}.

-spec(specific_response/2 :: (Code :: integer(), Routes :: proplist()) -> proplist()).
specific_response(200, Routes) ->
    [{<<"Routes">>, {struct, Routes}}
     ,{<<"Method">>, <<"bridge">>}
    ];
specific_response(404, _Routes) ->
    [{<<"Routes">>, []}
     ,{<<"Method">>, <<"error">>}
     ,{<<"Route-Error-Code">>, <<"404">>}
     ,{<<"Route-Error-Message">>, <<"Not Found">>}
    ];
specific_response(503, _Routes) ->
    [{<<"Routes">>, []}
     ,{<<"Method">>, <<"error">>}
     ,{<<"Route-Error-Code">>, <<"503">>}
     ,{<<"Route-Error-Message">>, <<"Insufficient Credit">>}
    ].
