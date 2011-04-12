%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Maintain list of carriers and match flags to carriers, generating routes
%%% @end
%%% Created : 22 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_carrier).

-behaviour(gen_server).

-include("ts.hrl").

%% API
-export([start_link/0, route/1, force_carrier_refresh/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(REFRESH_RATE, 43200000). % 1000ms * 60s * 60m * 12h = Every twelve hours

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

route(Flags) ->
    gen_server:call(?MODULE, {route, Flags}).

force_carrier_refresh() ->
    ?MODULE ! refresh,
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, _} = timer:send_after(1000, ?MODULE, refresh),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Req, _From, []=C) ->
    logger:format_log(error, "TS_CARRIER(~p): No carrier information for Req ~p, refreshing~n", [self(), Req]),
    ?MODULE ! refresh,
    {reply, no_carrier_information, C};
handle_call({route, Flags}, _From, Carriers) ->
    {reply, get_routes(Flags, Carriers), Carriers}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(refresh, OldCarriers) ->
    case get_current_carriers() of
	{ok, Carriers} ->
	    {noreply, Carriers};
	{error, _Err} ->
	    logger:format_log(error, "TS_CARRIER(~p): Error getting carriers: ~p~n", [self(), _Err]),
	    {noreply, OldCarriers}
    end;
handle_info({document_changes, DocID, Changes}, Carriers) ->
    logger:format_log(info, "TS_CARRIER(~p): Changes on ~p. ~p~n", [self(), DocID, Changes]),
    CurrRev =props:get_value(<<"_rev">>, Carriers),
    ChangedCarriers = lists:foldl(fun(ChangeProp, Cs) ->
					  case props:get_value(<<"rev">>, ChangeProp) of
					      undefined -> Cs;
					      CurrRev -> Cs;
					      _ ->
						  {ok, NewCarriers} = get_current_carriers(),
						  NewCarriers
					  end
				  end, Carriers, Changes),
    logger:format_log(info, "TS_CARRIER(~p): Changed carriers from ~p to ~p~n", [self(), CurrRev,props:get_value(<<"_rev">>, ChangedCarriers)]),
    {noreply, ChangedCarriers};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    logger:format_log(error, "TS_CARRIER(~p): Going down: ~p~n", [self(), _Reason]),
    couch_mgr:rm_change_handler(?TS_DB, ?TS_CARRIERS_DOC),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_current_carriers() ->
    case couch_mgr:open_doc(?TS_DB, ?TS_CARRIERS_DOC) of
	{error, not_found} ->
	    logger:format_log(info, "TS_CARRIER(~p): No document(~p) found~n", [self(), ?TS_CARRIERS_DOC]),
	    {error, "No matching carriers"};
	{error, db_not_reachable} ->
	    logger:format_log(info, "TS_CARRIER(~p): No DB(~p) found~n", [self(), ?TS_DB]),
	    {error, "DB not accessible"};
	{ok, []} ->
	    logger:format_log(info, "TS_CARRIER(~p): No Carriers defined~n", [self()]),
	    {error, "No matching carriers"};
	{ok, {struct, Carriers}} when is_list(Carriers) ->
	    logger:format_log(info, "TS_CARRIER(~p): Carriers pulled. Rev: ~p~n", [self(),props:get_value(<<"_rev">>, Carriers)]),
	    couch_mgr:add_change_handler(?TS_DB, ?TS_CARRIERS_DOC),
	    {ok, lists:map(fun process_carriers/1, lists:filter(fun active_carriers/1, Carriers))}
    end.

active_carriers({<<"_id">>, _}) ->
    true;
active_carriers({<<"_rev">>, _}) ->
    true;
active_carriers({_CarrierName, {struct, CarrierOptions}}) ->
   props:get_value(<<"enabled">>, CarrierOptions) =:= <<"1">>.

process_carriers({<<"_id">>, _}=ID) -> ID;
process_carriers({<<"_rev">>, _}=Rev) -> Rev;
process_carriers({CarrierName, {struct, CarrierOptions}}) ->
    RoutesRegexStrs =props:get_value(<<"routes">>, CarrierOptions, []),
    RouteRegexs = lists:map(fun(Str) -> {ok, R} = re:compile(Str), R end, RoutesRegexStrs),
    Gateways =props:get_value(<<"gateways">>, CarrierOptions, []),

    COs1 = proplists:delete(<<"gateways">>, CarrierOptions),
    {CarrierName, [{<<"routes">>, RouteRegexs}
		   ,{<<"options">>, lists:map(fun({struct, Gateway}) -> Gateway end, Gateways)}
		   | proplists:delete(<<"routes">>, COs1)]}.

-spec(get_routes/2 :: (Flags :: #route_flags{}, Carriers :: proplist()) -> {ok, proplist()} | {error, string()}).
get_routes(Flags, Carriers) ->
    User = Flags#route_flags.to_user,
    logger:format_log(info, "TS_CARRIER(~p): Find route to ~p~n", [self(), User]),

    Carriers1 = lists:filter(fun({_CarrierName, CarrierData}) ->
				     lists:any(fun(Regex) ->
						       re:run(User, Regex) =/= nomatch
					       end, props:get_value(<<"routes">>, CarrierData))
			     end, props:delete(<<"_rev">>, props:delete(<<"_id">>, Carriers))),
    case Carriers1 of
	[] ->
	    {error, "No carriers match outbound number"};
	Cs ->
	    create_routes(Flags, lists:sort(fun sort_carriers/2, Cs))
    end.

%% see http://erldocs.com/R14A/stdlib/lists.html#sort/2
%% return true when A should come before B
%% so return true if A's weight is greater than B's weight
-spec(sort_carriers/2 :: (CarrierA :: tuple(), CarrierB :: tuple()) -> boolean()).
sort_carriers({_CarrierAName, CarrierAData}, {_CarrierBName, CarrierBData}) ->
    ts_util:constrain_weight(props:get_value(<<"weight_cost">>, CarrierAData, 0)) >= ts_util:constrain_weight(props:get_value(<<"weight_cost">>, CarrierBData, 0)).

%% transform Carriers proplist() into a list of Routes for the API
-type routes() :: json_objects().
-spec(create_routes/2 :: (Flags :: #route_flags{}, Carriers :: proplist()) -> tuple(ok, routes()) | tuple(error, string())).
create_routes(Flags, Carriers) ->
    CallerID = case Flags#route_flags.caller_id of
		   {} -> [];
		   {Name, Number} -> [{<<"Caller-ID-Name">>, Name} ,{<<"Caller-ID-Number">>, Number}]
	       end,

    ChannelVars = ts_util:get_base_channel_vars(Flags),

    case lists:foldr(fun carrier_to_routes/2, {[], Flags#route_flags.to_user, CallerID, ChannelVars}, Carriers) of
	{[], _, _, _} ->
	    {error, "Failed to find routes for the call"};
	{Routes, _, _, _} ->
	    {ok, Routes}
    end.

-type c2r_acc() :: tuple(routes(), binary(), proplist(), proplist()).
-spec(carrier_to_routes/2 :: (tuple(binary(), proplist()), c2r_acc()) -> c2r_acc()).
carrier_to_routes({_CarrierName, CarrierData}, {Routes, User, CallerID, ChannelVars}) ->
    CallerIDData = case props:get_value(<<"callerid_type">>, CarrierData) of
			undefined -> CallerID;
			Type -> [{<<"Caller-ID-Type">>, Type} | CallerID]
		    end,
    BaseRouteData = [ {<<"Weight-Cost">>,props:get_value(<<"weight_cost">>, CarrierData, <<"1">>)}
		      ,{<<"Weight-Location">>,props:get_value(<<"weight_location">>, CarrierData, <<"1">>)}
		      | CallerIDData
		    ],
    Regexed = lists:foldl(fun(Regex, Acc) ->
				  case re:run(User, Regex, [{capture, [1], binary}]) of
				      {match, [Capture]} -> Capture;
				      _ -> Acc
				  end
			  end, [],props:get_value(<<"routes">>, CarrierData)),
    {GatewayRoutes, _, _, _} = lists:foldl(fun gateway_to_route/2
				     ,{Routes, Regexed, BaseRouteData, ChannelVars}
				     , props:get_value(<<"options">>, CarrierData)),
    {GatewayRoutes, User, CallerID, ChannelVars}.

-type g2r_acc() :: tuple(routes(), binary(), proplist(), proplist()).
-spec(gateway_to_route/2 :: (Gateway :: proplist(), Acc :: g2r_acc()) -> g2r_acc()).
gateway_to_route(Gateway, {CRs, Regexed, BaseRouteData, ChannelVars}=Acc) ->
    case whistle_util:to_binary(props:get_value(<<"enabled">>, Gateway, <<"0">>)) of
	<<"1">> ->
	    Dialstring = list_to_binary([<<"sip:">>
					     ,props:get_value(<<"prefix">>, Gateway)
					 ,Regexed
					 ,props:get_value(<<"suffix">>, Gateway)
					 ,"@"
					 ,props:get_value(<<"server">>, Gateway)
					]),
	    R = [{<<"Route">>, Dialstring}
		 ,{<<"Invite-Format">>, <<"route">>}
		 ,{<<"Media">>, ts_util:get_media_handling(props:get_value(<<"media_handling">>, Gateway))}
		 ,{<<"Auth-User">>,props:get_value(<<"username">>, Gateway)}
		 ,{<<"Auth-Password">>,props:get_value(<<"password">>, Gateway)}
		 ,{<<"Codecs">>,props:get_value(<<"codecs">>, Gateway, [])}
		 ,{<<"Progress-Timeout">>,props:get_value(<<"progress_timeout">>, Gateway, ?DEFAULT_PROGRESS_TIMEOUT)}
		 ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Carrier-Route">>, Dialstring} | ChannelVars]}}
		 | BaseRouteData ],
	    case whistle_api:route_resp_route_v(R) of
		true -> {[{struct, R} | CRs], Regexed, BaseRouteData, ChannelVars};
		false ->
		    logger:format_log(error, "TS_CARRIER.gateway_to_route Error validating route~n~p~n", [R]),
		    Acc
	    end;
	_ -> Acc
    end.
    
