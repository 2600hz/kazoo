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

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

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
    timer:send_interval(?REFRESH_RATE, refresh),
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
    format_log(error, "TS_CARRIER(~p): No carrier information for Req ~p~n", [self(), Req]),
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
	    format_log(error, "TS_CARRIER(~p): Error getting carriers: ~p~n", [self(), _Err]),
	    {noreply, OldCarriers}
    end;
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
    case ts_couch:open_doc(?TS_DB, ?TS_CARRIERS_DOC) of
	not_found ->
	    format_log(info, "TS_CARRIER(~p): No document(~p) found~n", [self(), ?TS_CARRIERS_DOC]),
	    {error, "No matching carriers"};
	{error, unhandled_call} ->
	    format_log(info, "TS_CARRIER(~p): No database host found~n", [self()]),
	    {error, "Unknown Database Host"};
	[] ->
	    format_log(info, "TS_CARRIER(~p): No Carriers defined~n", [self()]),
	    {error, "No matching carriers"};
	Carriers when is_list(Carriers) ->
	    format_log(info, "TS_CARRIER(~p): Carriers pulled. Rev: ~p~n", [self(), get_value(<<"_rev">>, Carriers)]),
	    {ok, lists:map(fun process_carriers/1, Carriers)};
	Other ->
	    format_log(error, "TS_CARRIER(~p): Unexpected error ~p~n", [self(), Other]),
	    {error, "Unexpected error"}
    end.

process_carriers({<<"_id">>, _}=ID) ->
    ID;
process_carriers({<<"_rev">>, _}=Rev) ->
    Rev;
process_carriers({CarrierName, {CarrierOptions}}) ->
    RoutesRegexStrs = get_value(<<"routes">>, CarrierOptions, []),
    RouteRegexs = lists:map(fun(Str) -> {ok, R} = re:compile(Str), R end, RoutesRegexStrs),
    Gateways = get_value(<<"gateways">>, CarrierOptions, []),

    COs1 = proplists:delete(<<"gateways">>, CarrierOptions),
    {CarrierName, [{<<"routes">>, RouteRegexs}
		   ,{<<"options">>, lists:map(fun({Gateway}) -> Gateway end, Gateways)}
		   | proplists:delete(<<"routes">>, COs1)]}.

-spec(get_routes/2 :: (Flags :: tuple(), Carriers :: proplist()) -> {ok, proplist()} | {error, string()}).
get_routes(Flags, Carriers) ->
    User = Flags#route_flags.to_user,
    format_log(info, "TS_CARRIER(~p): Find route to ~p~n", [self(), User]),

    Carriers0 = proplists:delete(<<"_rev">>, proplists:delete(<<"_id">>, Carriers)),
    Carriers1 = lists:filter(fun({_CarrierName, CarrierData}) ->
				     lists:any(fun(Regex) ->
						       re:run(User, Regex) =/= nomatch
					       end, get_value(<<"routes">>, CarrierData))
			     end, Carriers0),
    case Carriers1 of
	[] ->
	    {error, "No carriers match outbound number"};
	Cs ->
	    create_routes(Flags, lists:sort(fun sort_carriers/2, Cs))
    end.

%% sort on weight_cost; return true if weightA >= weightB, else false
-spec(sort_carriers/2 :: (CarrierA :: tuple(), CarrierB :: tuple()) -> boolean()).
sort_carriers({_CarrierAName, CarrierAData}, {_CarrierBName, CarrierBData}) ->
    get_value(<<"weight_cost">>, CarrierAData, 0) >= get_value(<<"weight_cost">>, CarrierBData, 0).
    

%% transform Carriers proplist() into a list of Routes for the API
-spec(create_routes/2 :: (Flags :: tuple(), Carriers :: proplist()) -> {ok, proplist()} | {error, string()}).
create_routes(Flags, Carriers) ->
    CallerID = case Flags#route_flags.callerid of
		   {} -> [];
		   {Name, Number} -> [{<<"Caller-ID-Name">>, Name} ,{<<"Caller-ID-Number">>, Number}]
	       end,
    case lists:foldl(fun carrier_to_routes/2, {[], Flags#route_flags.to_user, CallerID}, Carriers) of
	{[], _, _} ->
	    {error, "Failed to find routes for the call"};
	{Routes, _, _} ->
	    {ok, Routes}
    end.

carrier_to_routes({_CarrierName, CarrierData}, {Routes, User, CallerID}) ->
    CallerIDData = case get_value(<<"callerid_type">>, CarrierData) of
			undefined -> CallerID;
			Type -> [{<<"Caller-ID-Type">>, Type} | CallerID]
		    end,
    BaseRouteData = [ {<<"Weight-Cost">>, get_value(<<"weight_cost">>, CarrierData, <<"0">>)}
		      ,{<<"Weight-Location">>, get_value(<<"weight_location">>, CarrierData, <<"0">>)}
		      | CallerIDData
		    ],
    Regexed = lists:foldl(fun(Regex, Acc) ->
				  case re:run(User, Regex, [{capture, [1], binary}]) of
				      {match, [Capture]} -> Capture;
				      _ -> Acc
				  end
			  end, [], get_value(<<"routes">>, CarrierData)),
    {GatewayRoutes, _, _} = lists:foldl(fun gateway_to_route/2
				     ,{Routes, Regexed, BaseRouteData}
				     ,get_value(<<"options">>, CarrierData)),
    {GatewayRoutes, User, CallerID}.

gateway_to_route(Gateway, {CRs, Regexed, BaseRouteData}=Acc) ->
    case get_value(<<"enabled">>, Gateway, <<"1">>) of
	<<"1">> ->
	    Dialstring = list_to_binary([<<"sip:">>
					 ,get_value(<<"prefix">>, Gateway)
					 ,Regexed
					 ,get_value(<<"suffix">>, Gateway)
					 ,"@"
					 ,get_value(<<"server">>, Gateway)
					]),
	    R = [{<<"Route">>, Dialstring}
		 ,{<<"Media">>, <<"bypass">>}
		 ,{<<"Auth-User">>, get_value(<<"username">>, Gateway)}
		 ,{<<"Auth-Password">>, get_value(<<"password">>, Gateway)}
		 ,{<<"Codecs">>, get_value(<<"codecs">>, Gateway, [])}
		 | BaseRouteData ],
	    case whistle_api:route_resp_route_v(R) of
		true -> {[{struct, R} | CRs], Regexed, BaseRouteData};
		false ->
		    format_log(error, "TS_CARRIER.gateway_to_route Error validating route~n~p~n", [R]),
		    Acc
	    end;
	_ -> Acc
    end.
