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
    H = net_adm:localhost(),
    Q = start_amqp(H, "", <<>>),
    {ok, #state{amqp_host=H, callmgr_q=Q}}.

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
    format_log(info, "RSCMGR.h_res_req(~p): Req: ~p~n", [self(), Prop]),
    Options = get_request_options(Prop),
    Nodes = get_resources(request_type(Prop), Options),

    Min = get_value(min_channels_requested, Options),
    Max = get_value(max_channels_requested, Options),
    Route = get_value(<<"Route">>, Prop),
    start_channels(Nodes, {AmqpHost, Prop}, Route, Min, Max-Min).

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
    Min = get_value(<<"Resource-Minimum">>, Prop, 1),
    [{min_channels_requested, Min}
     ,{max_channels_requested, get_value(<<"Resource-Maximum">>, Prop, Min)}
    ].

-spec(start_channels/5 :: (Nodes :: list(), Amqp :: tuple(), Route :: binary() | list(), Min :: integer(), Max :: integer()) -> no_return()).
start_channels(_Ns, _Amqp, _Route, 0, 0) -> ok;
start_channels([], _Amqp, _Route, 0, _) -> ok;
start_channels([], _Amqp, _Route, M, _) -> {error, failed_starting, M};
start_channels([N | Ns]=Nodes, Amqp, Route, 0, Max) ->
    case start_channel(N, Route, Amqp) of
	{ok, 0} -> start_channels(Ns, Amqp, Route, 0, Max-1);
	{ok, _Left} -> start_channels(Nodes, Amqp, Route, 0, Max-1);
	{error, _} -> start_channels(Ns, Amqp, Route, 0, Max)
    end;
start_channels([N | Ns]=Nodes, Amqp, Route, Min, Max) ->
    case start_channel(N, Route, Amqp) of
	{ok, 0} -> start_channels(Ns, Amqp, Route, Min-1, Max);
	{ok, _Left} -> start_channels(Nodes, Amqp, Route, Min-1, Max);
	{error, _} -> start_channels(Ns, Amqp, Route, Min, Max)
    end.

-spec(start_channel/3 :: (N :: proplist(), Route :: binary() | list(), Amqp :: tuple()) -> tuple(ok | error, term())).
start_channel(N, Route, Amqp) ->
    Pid = get_value(node, N),
    Pid ! {resource_consume, self(), Route},
    receive
	{resource_consumed, UUID, AvailableChan} ->
	    send_uuid_to_app(Amqp, UUID),
	    {ok, AvailableChan};
	{resource_error, E} ->
	    format_log(error, "RSCMGR.st_ch(~p): Error starting channel on ~p: ~p~n", [self(), Pid, E]),
	    {error, E}
    after
	10000 -> {error, timeout}
    end.

-spec(send_uuid_to_app/2 :: (tuple(string(), proplist()), binary()) -> no_return()).
send_uuid_to_app({Host, Prop}, UUID) ->
    CtlQ = amqp_util:new_callctl_queue(Host, UUID),
    Msg = get_value(<<"Msg-ID">>, Prop),
    AppQ = get_value(<<"Server-ID">>, Prop),
    {ok, Vsn} = application:get_key(ecallmgr, vsn),

    RespProp = [{<<"Msg-ID">>, Msg}
	       ,{<<"Call-ID">>, UUID}
	       ,{<<"Control-Queue">>, CtlQ}
		| whistle_api:default_headers(CtlQ, <<"originate">>, <<"resource_resp">>, <<"resource_mgr">>, Vsn)],
    {ok, JSON} = whistle_api:resource_resp(RespProp),
    format_log(info, "RSC_MGR: Sending resp to ~p~n~s~n", [AppQ, JSON]),
    amqp_util:targeted_publish(Host, AppQ, JSON, <<"application/json">>).

%% sort first by percentage utilized (less utilized first), then by available channels (more available first)
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
    UtilA = get_value(percent_utilized, PropA),
    UtilB = get_value(percent_utilized, PropB),
    case UtilA of
        UtilB ->                   % same utilization, use node with more available channels
	    get_value(available_channels, PropA) > get_value(available_channels, PropB);
	X when X > UtilB -> false; % B is less utilized
	_X -> true                  % A is less utilized
    end.
