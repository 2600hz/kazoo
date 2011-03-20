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
-export([start_link/0, publish/3, consume/2, misc_req/2, misc_req/3]).

-export([open_channel/1, open_channel/2, is_available/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_AMQP_HOST, net_adm:localhost()).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

publish("localhost", BP, AM) ->
    publish(net_adm:localhost(), BP, AM);
publish(Host, BP, AM) ->
    gen_server:call(?SERVER, {publish, Host, BP, AM}).

consume("localhost", BC) ->
    consume(net_adm:localhost(), BC);
consume(Host, BC) ->
    gen_server:call(?SERVER, {consume, Host, BC}).

misc_req("localhost", Req) ->
    misc_req(net_adm:localhost(), Req);
misc_req(Host, Req) ->
    gen_server:call(?SERVER, {misc_req, Host, Req}).

misc_req("localhost", Req1, Req2) ->
    misc_req(net_adm:localhost(), Req1, Req2);
misc_req(Host, Req1, Req2) ->
    gen_server:call(?SERVER, {misc_req, Host, Req1, Req2}).

%% for backwards-compat
open_channel("localhost") ->
    open_channel(net_adm:localhost());
open_channel(Host) ->
    gen_server:call(?SERVER, {open_channel, Host}).

open_channel(_, "localhost") ->
    open_channel(net_adm:localhost());
open_channel(_, Host) ->
    gen_server:call(?SERVER, {open_channel, Host}).

is_available("localhost") ->
    is_available(net_adm:localhost());
is_available(Host) ->
    gen_server:call(?SERVER, {is_available, Host}).

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
-spec(init/1 :: (list()) -> tuple(ok, dict())).
init([]) ->
    %% Start a connection to the AMQP broker server
    process_flag(trap_exit, true),
    {ok, dict:new()}.

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
handle_call({is_available, Host}, _, State) ->
    case find_host(Host, State) of
	{ok, _, State1} ->
	    {reply, true, State1};
	_ ->
	    {reply, false, State}
    end;

handle_call({publish, Host, BP, AM}, From, State) ->
    case find_host(Host, State) of
	{ok, HPid, State1} ->
	    spawn(fun() -> amqp_host:publish(HPid, From, BP, AM) end),
	    logger:format_log(info, "Sent publish to amqp_h(~p)~n", [HPid]),
	    {noreply, State1};
	{error, _}=E ->
	    {reply, E, State}
    end;

handle_call({consume, Host, Msg}, From, State) ->
    case find_host(Host, State) of
	{ok, HPid, State1} ->
	    spawn(fun() -> amqp_host:consume(HPid, From, Msg) end),
	    {noreply, State1};
	{error, _}=E ->
	    {reply, E, State}
    end;

handle_call({misc_req, Host, Req}, From, State) ->
    case find_host(Host, State) of
	{ok, HPid, State1} ->
	    spawn(fun() -> amqp_host:misc_req(HPid, From, Req) end),
	    {noreply, State1};
	{error, _}=E ->
	    {reply, E, State}
    end;

handle_call({misc_req, Host, Req1, Req2}, From, State) ->
    case find_host(Host, State) of
	{ok, HPid, State1} ->
	    spawn(fun() -> amqp_host:misc_req(HPid, From, Req1, Req2) end),
	    {noreply, State1};
	{error, _}=E ->
	    {reply, E, State}
    end;

handle_call({open_channel, Host}, From, State) ->
    case find_host(Host, State) of
	{ok, HPid, State1} ->
	    spawn(fun() -> amqp_host:get_misc_channel(HPid, From) end),
	    {noreply, State1};
	{error, _}=E ->
	    {reply, E, State}
    end.

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
handle_info({'DOWN', Ref, process, HPid, _Reason}, State) ->
    logger:format_log(info, "AMQP_MGR(~p): amqp_host(~p) went down: ~p~n", [self(), HPid, _Reason]),
    {noreply, remove_hpid(HPid, Ref, State)};

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
	    erlang:monitor_node(Node, true),
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
	    E
    end.

-spec(find_host/2 :: (Host :: string(), State :: dict()) -> tuple(ok, pid(), dict()) | tuple(error, econnrefused)).
find_host(Host, State) ->
    case dict:find(Host, State) of
	error ->
	    case get_new_connection(create_amqp_params(Host)) of
		{error, _}=E -> E;
		Conn ->
		    {ok, HPid} = amqp_host_sup:start_host(Host, Conn),
		    Ref = erlang:monitor(process, HPid),
		    {ok, HPid, dict:store(Host, {HPid, Ref}, State)}
	    end;
	{ok, {HPid, _}} ->
	    {ok, HPid, State}
    end.

remove_hpid(HPid, Ref, State) ->
    dict:filter(fun(_, {HPid1, Ref1}) when HPid =:= HPid1 andalso Ref =:= Ref1 -> false;
		   (_, _) -> true end, State).
