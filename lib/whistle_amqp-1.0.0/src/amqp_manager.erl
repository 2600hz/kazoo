%%%-------------------------------------------------------------------
%%% File    : amqp_manager.erl
%%% Authors  : K Anderson
%%%          : James Aimonetti
%%% Description : The AMQP connection manager.
%%%
%%% Created :  March 24 2010
%%%-------------------------------------------------------------------
-module(amqp_manager).

-behaviour(gen_server).

%% API
-export([set_host/1, get_host/0]).

-export([start_link/0, publish/3, consume/2, misc_req/2, misc_req/3]).

-export([open_channel/1, open_channel/2, is_available/0, is_available/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_AMQP_HOST, net_adm:localhost()).
-define(START_TIMEOUT, 500).
-define(MAX_TIMEOUT, 5000).

-record(state, {
	  host = "" :: string()
	 ,handler_pid = undefined :: undefined | pid()
         ,handler_ref = undefined :: undefined | reference()
         ,conn_params = #'amqp_params'{} :: #'amqp_params'{}
         ,conn_type = direct :: direct | network
         ,timeout = ?START_TIMEOUT :: integer()
       }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_host(H) ->
    gen_server:call(?SERVER, {set_host, H}).

get_host() ->
    gen_server:call(?SERVER, get_host).

publish(Host, BP, AM) ->
    gen_server:call(?SERVER, {publish, Host, BP, AM}).

consume(Host, BC) ->
    gen_server:call(?SERVER, {consume, Host, BC}).

misc_req(Host, Req) ->
    gen_server:call(?SERVER, {misc_req, Host, Req}).

misc_req(Host, Req1, Req2) ->
    gen_server:call(?SERVER, {misc_req, Host, Req1, Req2}).

%% for backwards-compat
open_channel(Host) ->
    gen_server:call(?SERVER, {open_channel, Host}).

open_channel(_, Host) ->
    gen_server:call(?SERVER, {open_channel, Host}).

is_available(Host) ->
    is_available().

is_available() ->
    gen_server:call(?SERVER, is_available).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec(init/1 :: (list()) -> tuple(ok, #state{}, 0)).
init([]) ->
    %% Start a connection to the AMQP broker server
    process_flag(trap_exit, true),
    {ok, #state{host=?DEFAULT_AMQP_HOST}, 0}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%
%%--------------------------------------------------------------------
handle_call({set_host, Host}, _, State) ->
    logger:format_log(info, "AMQP_MGR(~p): Host being changed from ~p to ~p: all channels going down~n", [State#state.host, Host]),
    try
	stop_amqp_host(State),
	case start_amqp_host(Host, State) of
	    {ok, State1} -> {reply, ok, State1};
	    {error, _}=E -> {reply, E, State}
	end
    catch
	_A:_B ->
	    logger:format_log(error, "AMQP_MGR(~p): Failed to change host: ~p ~p~n~p~n", [self(), _A, _B, erlang:get_stacktrace()]),
	    {reply, {error, failed_to_change_host}, State}
    end;

handle_call(_, _, #state{handler_pid = undefined}=State) ->
    {reply, {error, amqp_down}, State};

handle_call(get_host, _, State) ->
    {reply, State#state.host, State};

handle_call(is_available, _, #state{handler_pid=HPid}=State) ->
    {reply, erlang:is_pid(HPid) andalso erlang:is_process_alive(HPid), State};

handle_call({publish, _, BP, AM}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:publish(HPid, From, BP, AM) end),
    logger:format_log(info, "Sent publish to amqp_h(~p)~n", [HPid]),
    {noreply, State};

handle_call({consume, Host, Msg}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:consume(HPid, From, Msg) end),
    {noreply, State};

handle_call({misc_req, Host, Req}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:misc_req(HPid, From, Req) end),
    {noreply, State};

handle_call({misc_req, Host, Req1, Req2}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:misc_req(HPid, From, Req1, Req2) end),
    {noreply, State};

handle_call({open_channel, Host}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:get_misc_channel(HPid, From) end),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Req, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, #state{handler_pid = undefined, timeout=T}=State) when T > ?MAX_TIMEOUT ->
    handle_info(timeout, State#state{timeout=?MAX_TIMEOUT});

handle_info(timeout, #state{host=Host, handler_pid = undefined, timeout=T}=State) ->
    case start_amqp_host(Host, State) of
	{ok, State1} -> {noreply, State1#state{timeout=?START_TIMEOUT}};
	{error, _} -> {noreply, State#state{timeout=T*2}, T}
    end;
    
handle_info({'DOWN', Ref, process, HPid, _Reason}, #state{handler_ref = Ref1}=State) when Ref =:= Ref1 ->
    logger:format_log(error, "AMQP_MGR(~p): amqp_host(~p) went down: ~p~n", [self(), HPid, _Reason]),
    {noreply, #state{host = State#state.host}, 0};

handle_info({nodedown, RabbitNode}, #state{conn_params=#'amqp_params'{node=RabbitNode}}=State) ->
    logger:format_log(error, "AMQP_MGR(~p): AMQP Node ~p is down~n", [self(), RabbitNode]),
    stop_amqp_host(State),
    {noreply, State#state{handler_pid=undefined, handler_ref=undefined}};

handle_info({nodeup, RabbitNode}, #state{host=Host, conn_params=#'amqp_params'{node=RabbitNode}=ConnParams, conn_type=ConnType}=State) ->
    logger:format_log(info, "AMQP_MGR(~p): AMQP Node ~p is up~n", [self(), RabbitNode]),
    case start_amqp_host(Host, State, {ConnType, ConnParams}) of
	{error, E} ->
	    logger:format_log(error, "AMQP_MGR(~p): unable to bring host ~p back up: ~p~n", [self(), Host, E]),
	    {noreply, #state{host=?DEFAULT_AMQP_HOST}};
	{ok, State1} ->
	    {noreply, State1}
    end;

handle_info(_Info, State) ->
    logger:format_log(info, "AMQP_MGR(~p): Unhandled info: ~p~n", [self(), _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec(terminate/2 :: (Reason :: term(), State :: dict()) -> no_return()).
terminate(Reason, _State) ->
    logger:format_log(info, "AMQP_MGR(~p): Going down(~p)~n", [self(), Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec(create_amqp_params/1 :: (Host :: string()) -> tuple(direct | network, #'amqp_params'{})).
create_amqp_params(Host) ->
    create_amqp_params(Host, ?PROTOCOL_PORT).
-spec(create_amqp_params/2 :: (Host :: string(), Port :: integer()) -> tuple()).
create_amqp_params(Host, Port) ->
    Node = list_to_atom([$r,$a,$b,$b,$i,$t,$@ | Host]),
    case net_adm:ping(Node) of
	pong ->
	    %% erlang:monitor_node(Node, true),
	    net_kernel:monitor_nodes(true),
	    {direct, #'amqp_params'{ port = Port, host = Host, node = Node }};
	pang ->
	    {network, #'amqp_params'{ port = Port, host = Host }}
    end.

-spec(get_new_connection/1 :: (tuple(Type :: direct | network, P :: #'amqp_params'{})) -> pid() | tuple(error, econnrefused)).
get_new_connection({Type, #'amqp_params'{}=P}) ->
    case amqp_connection:start(Type, P) of
	{ok, Connection} ->
	    logger:format_log(info, "AMQP_MGR(~p): Conn ~p started.~n", [self(), Connection]),
	    Connection;
	{error, econnrefused}=E ->
	    logger:format_log(error, "AMQP_MGR(~p): Refusing to connect to ~p~n", [self(), P#'amqp_params'.host]),
	    E;
	{error, broker_not_found_on_node}=E ->
	    logger:format_log(error, "AMQP_MGR(~p): Node found, broker not.~n", [self()]),
	    E
    end.

stop_amqp_host(#state{handler_pid=HPid, handler_ref=HRef}) ->
    %% erlang:demonitor(HRef, [flush]),
    net_kernel:monitor_nodes(false),
    amqp_host:stop(HPid).

start_amqp_host("localhost", State) ->
    start_amqp_host(net_adm:localhost(), State);
start_amqp_host(Host, State) ->
    start_amqp_host(Host, State, create_amqp_params(Host)).

start_amqp_host(Host, State, {ConnType, ConnParams} = ConnInfo) ->
    case get_new_connection(ConnInfo) of
	{error, E} ->
	    logger:format_log(error, "AMQP_MGR(~p): unable to set host to ~p: ~p~n", [self(), Host, E]),
	    {error, E};
	Conn ->
	    {ok, HPid} = amqp_host_sup:start_host(Host, Conn),
	    Ref = erlang:monitor(process, HPid),
	    {ok, State#state{handler_pid = HPid, handler_ref = Ref, conn_type = ConnType, conn_params = ConnParams, timeout=?START_TIMEOUT}}
    end.
