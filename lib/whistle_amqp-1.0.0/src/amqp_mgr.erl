%%%-------------------------------------------------------------------
%%% File    : amqp_mgr.erl
%%% Authors  : K Anderson
%%%          : James Aimonetti
%%% Description : The AMQP connection manager.
%%%
%%% Created :  March 24 2010
%%%-------------------------------------------------------------------
-module(amqp_mgr).

-behaviour(gen_server).

%% API
-export([set_host/1, get_host/0]).

-export([start_link/0, publish/2, consume/1, misc_req/1, misc_req/2, register_return_handler/0]).

-export([is_available/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(START_TIMEOUT, 500).
-define(MAX_TIMEOUT, 5000).
-define(STARTUP_FILE, [code:lib_dir(whistle_amqp, priv), "/startup.config"]).

-record(state, {
	  host = "" :: string() | tuple(string(), integer())
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

publish(BP, AM) ->
    gen_server:call(?SERVER, {publish, BP, AM}).

consume(BC) ->
    gen_server:call(?SERVER, {consume, BC}).

misc_req(Req) ->
    gen_server:call(?SERVER, {misc_req, Req}).

misc_req(Req1, Req2) ->
    gen_server:call(?SERVER, {misc_req, Req1, Req2}).

is_available() ->
    gen_server:call(?SERVER, is_available).

register_return_handler() ->
    gen_server:call(?SERVER, {register_return_handler}).

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
-spec init/1 :: ([]) -> {ok, #state{}, 0}.
init([]) ->
    %% Start a connection to the AMQP broker server
    ?LOG_SYS("starting amqp manager server"),
    process_flag(trap_exit, true),
    Init = get_config(),
    {ok, #state{host=props:get_value(default_host, Init, net_adm:localhost())}, 0}.

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
handle_call({set_host, Host}, _, #state{host=OldHost}=State) ->
    ?LOG_SYS("changing amqp host from ~s to ~s, all channels going down", [OldHost, Host]),
    stop_amqp_host(State),
    case start_amqp_host(Host, State) of
	{ok, State1} -> {reply, ok, State1, hibernate};
	{error, _}=E -> {reply, E, State, 0}
    end;

handle_call(is_available, _, #state{handler_pid=HPid}=State) ->
    {reply, erlang:is_pid(HPid) andalso erlang:is_process_alive(HPid), State};

handle_call(_, _, #state{handler_pid = undefined}=State) ->
    {reply, {error, amqp_down}, State, 0};

handle_call(get_host, _, #state{host=Host}=State) ->
    {reply, Host, State};

handle_call({publish, BP, AM}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:publish(HPid, From, BP, AM) end),
    {noreply, State};

handle_call({consume, Msg}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:consume(HPid, From, Msg) end),
    {noreply, State};

handle_call({misc_req, Req}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:misc_req(HPid, From, Req) end),
    {noreply, State};

handle_call({misc_req, Req1, Req2}, From, #state{handler_pid=HPid}=State) ->
    spawn(fun() -> amqp_host:misc_req(HPid, From, Req1, Req2) end),
    {noreply, State};

handle_call({register_return_handler}, From, #state{handler_pid=HPid}=State)->
    spawn(fun() -> amqp_host:register_return_handler(HPid, From) end),
    {noreply, State};

handle_call(_, _, State) ->
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
handle_info(timeout, #state{host={Host,Port}=Settings, handler_pid=undefined}=State) ->
    ?LOG_SYS("attempting to connect to ~s on port ~b", [Host, Port]),
    case start_amqp_host(Settings, State) of
	{ok, State1} ->
            ?LOG_SYS("connected to AMQP host"),
            {noreply, State1, hibernate};
	{error, R} ->
            ?LOG_SYS("attempting to connect again(~w) in ~b ms", [R, ?START_TIMEOUT]),
            {ok, _} = timer:send_after(?START_TIMEOUT, {reconnect, ?START_TIMEOUT}),
            {noreply, State}
    end;

handle_info(timeout, #state{host=Host, handler_pid=undefined}=State) ->
    ?LOG_SYS("attempting to connect to ~s", [Host]),
    case start_amqp_host(Host, State) of
	{ok, State1} ->
            ?LOG_SYS("connected to AMQP host"),
            {noreply, State1, hibernate};
	{error, R} ->
            ?LOG_SYS("attempting to connect again(~w) in ~b ms", [R, ?START_TIMEOUT]),
            {ok, _} = timer:send_after(?START_TIMEOUT, {reconnect, ?START_TIMEOUT}),
            {noreply, State}
    end;

handle_info({reconnect, T}, #state{host={Host,Port}=Settings}=State) ->
    ?LOG_SYS("attempting to reconnect to ~s on port ~b", [Host, Port]),
    case start_amqp_host(Settings, State) of
	{ok, State1} ->
            {noreply, State1, hibernate};
	{error, _} ->
            case T * 2 of
                Timeout when Timeout > ?MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect again in ~b ms", [?MAX_TIMEOUT]),
                    {ok, _} = timer:send_after(?MAX_TIMEOUT, {reconnect, ?MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect again in ~b ms", [Timeout]),
                    {ok, _} = timer:send_after(Timeout, {reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({reconnect, T}, #state{host=Host}=State) ->
    ?LOG_SYS("attempting to reconnect to ~s", [Host]),
    case start_amqp_host(Host, State) of
	{ok, State1} ->
            {noreply, State1, hibernate};
	{error, _} ->
            case T * 2 of
                Timeout when Timeout > ?MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect again in ~b ms", [?MAX_TIMEOUT]),
                    {ok, _} = timer:send_after(?MAX_TIMEOUT, {reconnect, ?MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect again in ~b ms", [Timeout]),
                    {ok, _} = timer:send_after(Timeout, {reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({'DOWN', Ref, process, _, _Reason}, #state{handler_ref=Ref}=State) ->
    ?LOG_SYS("amqp host process went down, ~w", [_Reason]),
    erlang:demonitor(Ref, [flush]),
    {ok, _} = timer:send_after(?START_TIMEOUT, {reconnect, ?START_TIMEOUT}),
    {noreply, State#state{handler_pid=undefined, handler_ref=undefined}, hibernate};

handle_info({nodedown, RabbitNode}, #state{conn_params=#'amqp_params'{node=RabbitNode}}=State) ->
    ?LOG_SYS("received node down notification for amqp"),
    stop_amqp_host(State),
    {noreply, State#state{handler_pid=undefined, handler_ref=undefined}, hibernate};

handle_info({nodeup, RabbitNode}, #state{host=Host, conn_params=#'amqp_params'{node=RabbitNode}=ConnParams, conn_type=ConnType}=State) ->
    ?LOG_SYS("received node up notification for amqp"),
    case start_amqp_host(Host, State, {ConnType, ConnParams}) of
	{error, E} ->
            ?LOG_SYS("unable to bring amqp node back up, ~p", [E]),
	    {noreply, #state{host="localhost"}, hibernate};
	{ok, State1} ->
	    {noreply, State1, hibernate}
    end;

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec(terminate/2 :: (Reason :: term(), State :: #state{}) -> no_return()).
terminate(Reason, #state{host=H}) when is_list(H) ->
    save_config([{default_host, H}]),
    terminate(Reason, ok);
terminate(_Reason, _) ->
    ?LOG_SYS("amqp manager ~p termination", [_Reason]),
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
-spec create_amqp_params/1 :: (Host) -> tuple(direct | network, #'amqp_params'{}) when
      Host :: string().
-spec create_amqp_params/2 :: (Host, Port) -> tuple(direct | network, #'amqp_params'{}) when
      Host :: string(),
      Port :: integer().
create_amqp_params(Host) ->
    create_amqp_params(Host, ?PROTOCOL_PORT).
create_amqp_params(Host, Port) ->
    Node = list_to_atom([$r,$a,$b,$b,$i,$t,$@ | Host]),
    case net_adm:ping(Node) of
	pong ->
	    %% erlang:monitor_node(Node, true),
	    _ = net_kernel:monitor_nodes(true),
	    {direct, #'amqp_params'{ port = Port, host = Host, node = Node }};
	pang ->
	    {network, #'amqp_params'{ port = Port, host = Host }}
    end.

-spec get_new_connection/1 :: (Params) -> pid() | tuple(error, econnrefused) when
      Params :: tuple(Type :: direct | network, P :: #'amqp_params'{}).
get_new_connection({Type, #'amqp_params'{host=_Host}=P}) ->
    case amqp_connection:start(Type, P) of
	{ok, Connection} ->
            ?LOG_SYS("established ~s connection to amqp broker at ~s", [Type, _Host]),
	    Connection;
	{error, econnrefused}=E ->
	    ?LOG_SYS("amqp connection to ~s refused", [_Host]),
	    E;
	{error, broker_not_found_on_node}=E ->
	    ?LOG_SYS("found node ~s but no amqp broker", [_Host]),
	    E
    end.

stop_amqp_host(#state{handler_pid=undefined}) ->
    ok;
stop_amqp_host(#state{handler_pid=HPid, handler_ref=HRef}) ->
    erlang:demonitor(HRef, [flush]),
    _ = net_kernel:monitor_nodes(false),
    ok = amqp_host:stop(HPid).

start_amqp_host("localhost", State) ->
    [_, Host] = string:tokens(wh_util:to_list(node()), "@"),
    ?LOG_SYS("Instead of localhost, use ~s", [Host]),
    start_amqp_host(Host, State);
start_amqp_host({Host,Port}, State) ->
    start_amqp_host(Host, State, create_amqp_params(Host, Port));
start_amqp_host(Host, State) ->
    start_amqp_host(Host, State, create_amqp_params(Host)).

start_amqp_host(Host, State, {ConnType, ConnParams} = ConnInfo) ->
    case get_new_connection(ConnInfo) of
	{error,_}=E ->
	    E;
	Conn ->
	    {ok, HPid} = amqp_host_sup:start_host(Host, Conn),
	    Ref = erlang:monitor(process, HPid),
	    {ok, State#state{host=Host, handler_pid = HPid
			     ,handler_ref = Ref, conn_type = ConnType
			     ,conn_params = ConnParams, timeout=?START_TIMEOUT
			    }}
    end.

-spec get_config/0 :: () -> proplist().
get_config() ->
    case file:consult(?STARTUP_FILE) of
	{ok, Prop} ->
            ?LOG_SYS("loaded amqp manager configuration from ~s", [?STARTUP_FILE]),
            Prop;
	E ->
            ?LOG_SYS("unable to load amqp manager configuration ~p", [E]),
            []
    end.

-spec save_config/1 :: (Prop) -> no_return() when
      Prop :: proplist().
save_config(Prop) ->
    ?LOG_SYS("updating config ~s", [?STARTUP_FILE]),
    file:write_file(?STARTUP_FILE
		    ,lists:foldl(fun(Item, Acc) -> [io_lib:format("~p.~n", [Item]) | Acc] end, "", Prop)
		   ).
