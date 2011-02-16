%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Server to maintain a list of resources available and who can serve them;
%%% query for a resource type and get a server to handle the request.
%%% @end
%%% Created : 11 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(resource_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0, set_amqp_host/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("whistle_api.hrl").
-include("whistle_amqp.hrl").
-include("../include/amqp_client/include/amqp_client.hrl").
-include("ecallmgr.hrl").

-define(SERVER, ?MODULE). 

-record(state, {amqp_host = "" :: string()
		,callmgr_q = <<>> :: binary()
	       }).

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

set_amqp_host(Host) ->
    gen_server:cast(?MODULE, {set_amqp_host, Host}).

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
    try
	H = net_adm:localhost(),
	Q = start_amqp(H, "", <<>>),
	{ok, #state{amqp_host=H, callmgr_q=Q}}
    catch
	_:_ -> {ok, #state{}}
    end.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({set_amqp_host, Host}, #state{amqp_host=OldHost, callmgr_q=OldQ}=State) ->
    NewQ = start_amqp(Host, OldHost, OldQ),
    format_log(info, "RSCMGR(~p): Change Amqp from ~p to ~p: ~p~n", [self(), OldHost, Host, NewQ]),
    {noreply, State#state{amqp_host=Host, callmgr_q=NewQ}};
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
handle_info({#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">> }
					   ,payload = Payload}}, #state{amqp_host=AmqpHost}=State) ->
    spawn(fun() -> handle_resource_req(Payload, AmqpHost) end),
    {noreply, State};
handle_info(_Info, State) ->
    format_log(info, "RSCMGR(~p): Unhandled info ~p~n", [self(), _Info]),
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
terminate(_Reason, #state{amqp_host=Host, callmgr_q=Q}) ->
    format_log(info, "RSCMGR(~p): Going down(~p). H: ~p Q: ~p~n", [self(), _Reason, Host, Q]),
    amqp_util:delete_callmgr_queue(Host, Q),
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
-spec(start_amqp/3 :: (Host :: string(), OldHost :: string(), OldQ :: binary()) -> binary()).
start_amqp(Host, "", <<>>) ->
    amqp_util:callmgr_exchange(Host),
    amqp_util:targeted_exchange(Host),
    Q = amqp_util:new_callmgr_queue(Host, <<>>),
    amqp_util:bind_q_to_callmgr(Host, Q, ?KEY_RESOURCE_REQ),
    amqp_util:basic_consume(Host, Q),
    Q;
start_amqp(Host, OldHost, OldQ) ->
    amqp_util:delete_callmgr_queue(OldHost, OldQ),
    amqp_util:channel_close(OldHost),
    start_amqp(Host, "", <<>>).

-spec(handle_resource_req/2 :: (Payload :: binary(), AmqpHost :: string()) -> no_return()).
handle_resource_req(Payload, AmqpHost) ->
    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
    case whistle_api:resource_req_v(Prop) of
	true ->
	    format_log(info, "RSCMGR.h_res_req(~p): Req: ~p~n", [self(), Prop]),
	    Options = get_request_options(Prop),
	    Nodes = get_resources(request_type(Prop), Options),

	    Min = whistle_util:to_integer(get_value(min_channels_requested, Options)),
	    Max = whistle_util:to_integer(get_value(max_channels_requested, Options)),
	    Route = ecallmgr_fs_route:build_route(AmqpHost, [{<<"Realm">>, ?DEFAULT_DOMAIN} | Prop], get_value(<<"Invite-Format">>, Prop)),
	    case start_channels(Nodes, {AmqpHost, Prop}, Route, Min, Max-Min) of
		{error, failed_starting, Failed} ->
		    send_failed_req(Prop, AmqpHost, Failed),
		    fail;
		ok -> ok
	    end;
	false ->
	    format_log(error, "RSCMGR.h_res_req(~p): Failed to validate ~p~n", [self(), Prop])
    end.

-spec(get_resources/2 :: (tuple(binary(), binary(), binary()), Options :: proplist()) -> list(proplist()) | []).
get_resources({<<"originate">>, <<"resource_req">>, <<"audio">>=Type}, Options) ->
    format_log(info, "RSCMGR.get_res(~p): Type ~p Options: ~p~n", [self(), Type, Options]),
    FSAvail = ecallmgr_fs_handler:request_resource(Type, Options), % merge other switch results into this list as well (eventually)
    lists:usort(fun sort_resources/2, FSAvail);
get_resources(_Type, _Options) ->
    format_log(error, "RSCMGR.get_res(~p): Unknown request type ~p~n", [self(), _Type]),
    [].

-spec(request_type/1 :: (Prop :: proplist()) -> tuple(binary(), binary(), binary())).
request_type(Prop) ->
    {get_value(<<"Event-Category">>, Prop), get_value(<<"Event-Name">>, Prop), get_value(<<"Resource-Type">>, Prop)}.

-spec(get_request_options/1 :: (Prop :: proplist()) -> proplist()).
get_request_options(Prop) ->
    Min = whistle_util:to_integer(get_value(<<"Resource-Minimum">>, Prop, 1)),
    [{min_channels_requested, Min}
     ,{max_channels_requested, whistle_util:to_integer(get_value(<<"Resource-Maximum">>, Prop, Min))}
    ].

-spec(start_channels/5 :: (Nodes :: list(), Amqp :: tuple(), Route :: binary() | list(), Min :: integer(), Max :: integer()) -> tuple(error, failed_starting, integer()) | ok).
start_channels(_Ns, _Amqp, _Route, 0, 0) -> ok; %% started all channels requested
start_channels([], _Amqp, _Route, 0, _) -> ok; %% started at least the minimum channels, but ran out of servers with available resources
start_channels([], _Amqp, _Route, M, _) -> {error, failed_starting, M}; %% failed to start the minimum channels before server resources ran out
start_channels([N | Ns]=Nodes, Amqp, Route, 0, Max) -> %% these are bonus channels not required but desired
    case start_channel(N, Route, Amqp) of
	{ok, 0} -> start_channels(Ns, Amqp, Route, 0, Max-1);
	{ok, _} -> start_channels(Nodes, Amqp, Route, 0, Max-1);
	{error, _} -> start_channels(Ns, Amqp, Route, 0, Max)
    end;
start_channels([N | Ns]=Nodes, Amqp, Route, Min, Max) -> %% start the minimum channels
    case start_channel(N, Route, Amqp) of
	{ok, 0} -> start_channels(Ns, Amqp, Route, Min-1, Max);
	{ok, _} -> start_channels(Nodes, Amqp, Route, Min-1, Max);
	{error, _} -> start_channels(Ns, Amqp, Route, Min, Max)
    end.

-spec(start_channel/3 :: (N :: proplist(), Route :: binary() | list(), Amqp :: tuple()) -> tuple(ok, integer()) | tuple(error, timeout | binary())).
start_channel(N, Route, Amqp) ->
    Pid = get_value(node, N),
    case ecallmgr_fs_node:resource_consume(Pid, Route) of
	{resource_consumed, UUID, CtlQ, AvailableChan} ->
	    spawn(fun() -> send_uuid_to_app(Amqp, UUID, CtlQ) end),
	    {ok, AvailableChan};
	{resource_error, E} ->
	    format_log(error, "RSCMGR.st_ch(~p): Error starting channel on ~p: ~p~n", [self(), Pid, E]),
	    spawn(fun() -> send_failed_consume(Route, Amqp, E) end),
	    {error, E}
    end.

-spec(send_uuid_to_app/3 :: (Amqp :: tuple(string(), proplist()), UUID :: binary(), CtlQ :: binary()) -> no_return()).
send_uuid_to_app({Host, Prop}, UUID, CtlQ) ->
    Msg = get_value(<<"Msg-ID">>, Prop),
    AppQ = get_value(<<"Server-ID">>, Prop),
    {ok, Vsn} = application:get_key(ecallmgr, vsn),

    RespProp = [{<<"Msg-ID">>, Msg}
	       ,{<<"Call-ID">>, UUID}
	       ,{<<"Control-Queue">>, CtlQ}
		| whistle_api:default_headers(CtlQ, <<"originate">>, <<"resource_resp">>, <<"resource_mgr">>, whistle_util:to_binary(Vsn))],
    {ok, JSON} = whistle_api:resource_resp(RespProp),
    format_log(info, "RSC_MGR: Sending resp to ~p: ~s~n", [AppQ, JSON]),
    amqp_util:targeted_publish(Host, AppQ, JSON, <<"application/json">>).

-spec(send_failed_req/3 :: (Prop :: proplist(), Host :: string(), Failed :: integer()) -> no_return()).
send_failed_req(Prop, Host, Failed) ->
    Msg = get_value(<<"Msg-ID">>, Prop),
    AppQ = get_value(<<"Server-ID">>, Prop),
    {ok, Vsn} = application:get_key(ecallmgr, vsn),

    RespProp = [{<<"Msg-ID">>, Msg}
		,{<<"Failed-Attempts">>, Failed}
		| whistle_api:default_headers(<<>>, <<"originate">>, <<"resource_error">>, <<"resource_mgr">>, Vsn)],
    {ok, JSON} = whistle_api:resource_error(RespProp),
    format_log(info, "RSC_MGR: Sending err to ~p~n~s~n", [AppQ, JSON]),
    amqp_util:targeted_publish(Host, AppQ, JSON, <<"application/json">>).

-spec(send_failed_consume/3 :: (Route :: binary() | list(), Amqp :: tuple(Host :: string(), Prop :: proplist()), E :: binary()) -> no_return()).
send_failed_consume(Route, {Host, Prop}, E) ->
    Msg = get_value(<<"Msg-ID">>, Prop),
    AppQ = get_value(<<"Server-ID">>, Prop),
    {ok, Vsn} = application:get_key(ecallmgr, vsn),

    RespProp = [{<<"Msg-ID">>, Msg}
		,{<<"Failed-Route">>, Route}
		,{<<"Failure-Message">>, whistle_util:to_binary(E)}
		| whistle_api:default_headers(<<>>, <<"originate">>, <<"originate_error">>, <<"resource_mgr">>, Vsn)],
    {ok, JSON} = whistle_api:resource_error(RespProp),
    format_log(info, "RSC_MGR: Sending err to ~p~n~s~n", [AppQ, JSON]),
    amqp_util:targeted_publish(Host, AppQ, JSON, <<"application/json">>).

%% sort first by percentage utilized (less utilized first), then by bias (larger goes first), then by available channels (more available first) 
%% [
%%   [{node, 1}, {p_u, 80}, {a_c, 3}, ...]
%%  ,[{node, 2}, {p_u, 50}, {a_c, 5}, ...]
%%  ,[{node, 3}, {p_u, 50}, {a_c, 1}, ...]
%%  ,[{node, 4}, {p_u, 10}, {a_c, 10}, ...]
%% ] ==>
%% [ [{node, 4}], [{node, 2}], [{node, 3}], [{node, 1}] ]
%%    least util,  more chan    most chan    most util
-spec(sort_resources/2 :: (PropA :: proplist(), PropB :: proplist()) -> boolean()).
sort_resources(PropA, PropB) ->
    UtilB = get_value(percent_utilized, PropB),
    case get_value(percent_utilized, PropA) of
        UtilB ->                   % same utilization, use node with more available channels
	    BiasB = get_value(bias, PropB, 1),
	    case get_value(bias, PropA, 1) of
		BiasB -> %% same bias, use node with more available channels
		    ACB = get_value(available_channels, PropB),
		    case get_value(available_channels, PropA) of
			C when C >= ACB -> true;
			_ -> false
		    end;
		B when B > BiasB -> true; % A has bigger bias
		_B -> true
	    end;
	A when A > UtilB -> false; % B is less utilized
	_A -> true                 % A is less utilized
    end.
