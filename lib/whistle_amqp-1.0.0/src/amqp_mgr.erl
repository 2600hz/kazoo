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
-export([start_conn/1, start_conn/2, get_host/0]).

-export([start_link/0, publish/2, consume/1, misc_req/1, register_return_handler/0]).

-export([is_available/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(START_TIMEOUT, 500).
-define(MAX_TIMEOUT, 2000).
-define(STARTUP_FILE, [code:lib_dir(whistle_amqp, priv), "/startup.config"]).

-record(state, {
          amqp_uri = "" :: string()
         ,use_federation = true :: boolean()
         ,handler_pid = 'undefined' :: 'undefined' | pid()
         ,handler_ref = 'undefined' :: 'undefined' | reference()
         ,conn_params = 'undefined' :: 'undefined' | #'amqp_params_direct'{} | #'amqp_params_network'{}
         ,conn_ref = 'undefined' :: 'undefined' | reference()
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

-spec start_conn/1 :: (nonempty_string() | ne_binary()) -> 'ok'.
-spec start_conn/2 :: (nonempty_string() | ne_binary(), boolean()) -> 'ok'.
start_conn(AmqpUri) ->
    gen_server:cast(?SERVER, {start_conn, wh_util:to_list(AmqpUri)}).
start_conn(AmqpUri, UseFederation) ->
    gen_server:cast(?SERVER, {start_conn, wh_util:to_list(AmqpUri), wh_util:is_true(UseFederation)}).

-spec get_host/0 :: () -> nonempty_string().
get_host() ->
    gen_server:call(?SERVER, get_host).

-spec publish/2 :: (#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
publish(BP, AM) ->
    gen_server:call(?SERVER, {publish, BP, AM}).

-spec consume/1 :: (amqp_host:consume_records()) -> 'ok' |
                                                    {'ok', pid() | ne_binary()} |
                                                    {'error', 'not_consuming' | atom()}.
consume(BC) ->
    gen_server:call(?SERVER, {consume, BC}).

-spec misc_req/1 :: (amqp_host:mic_records()) -> 'ok' | {'error', atom()}.
misc_req(Req) ->
    gen_server:call(?SERVER, {misc_req, Req}).

-spec is_available/0 :: () -> boolean().
is_available() ->
    gen_server:call(?SERVER, is_available).

-spec register_return_handler/0 :: () -> 'ok'.
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
-spec init/1 :: ([]) -> {'ok', #state{}}.
init([]) ->
    Init = get_config(),
    Uri = props:get_value(amqp_uri, Init, ?DEFAULT_AMQP_URI),
    UseFederation = props:get_value(use_federation, Init, true),

    case start_amqp_host(Uri, UseFederation) of
        {ok, State} ->
            ?LOG_SYS("starting amqp manager server"),
            process_flag(trap_exit, true),
            {ok, State#state{amqp_uri=Uri, use_federation=UseFederation}};
        {error, E} -> {stop, E}
    end.

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
handle_call(is_available, _, #state{handler_pid=undefined}=State) ->
    {reply, false, State};
handle_call(is_available, _, #state{handler_pid=HPid}=State) ->
    case erlang:is_pid(HPid) andalso erlang:is_process_alive(HPid) of
        true -> {reply, true, State};
        false ->
            _ = stop_amqp_host(State),
            {reply, false, State#state{handler_pid=undefined, handler_ref=undefined}, 0}
    end;

handle_call(get_host, _, #state{conn_params=ConnP}=State) ->
    {reply, wh_amqp_params:host(ConnP), State};

handle_call(_, _, #state{handler_pid = undefined}=State) ->
    {reply, {error, amqp_down}, State, 0};

handle_call({publish, BP, AM}, From, #state{handler_pid=HPid}=State) ->
    send_req(HPid, From, fun() -> catch amqp_host:publish(HPid, From, BP, AM) end),
    {noreply, State};

handle_call({consume, Msg}, From, #state{handler_pid=HPid}=State) ->
    send_req(HPid, From, fun() -> catch amqp_host:consume(HPid, From, Msg) end),
    {noreply, State};

handle_call({misc_req, Req}, From, #state{handler_pid=HPid}=State) ->
    send_req(HPid, From, fun() -> catch amqp_host:misc_req(HPid, From, Req) end),
    {noreply, State};

handle_call({register_return_handler}, From, #state{handler_pid=HPid}=State)->
    send_req(HPid, From, fun() -> catch amqp_host:register_return_handler(HPid, From) end),
    {noreply, State};

handle_call(_, _, State) ->
    {noreply, State}.

send_req(HPid, From, Fun) ->
    case erlang:is_process_alive(HPid) of
        true -> spawn(Fun);
        false -> gen_server:reply(From, {error, amqp_host_missing})
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({start_conn, ""}, State) ->
    handle_cast({start_conn, ?DEFAULT_AMQP_URI}, State);

handle_cast({start_conn, Uri}, #state{use_federation=UseFederation}) ->
    ?LOG_SYS("Starting connection with uri: ~s: ~s", [Uri, UseFederation]),

    case start_amqp_host(Uri, UseFederation) of
        {ok, State1} -> {noreply, State1#state{amqp_uri=Uri, use_federation=UseFederation}};
        {error, E} -> {stop, E, normal}
    end;
handle_cast({start_conn, Uri, UseFederation}, _State) ->
    ?LOG_SYS("Starting connection with uri: ~s: ~s", [Uri, UseFederation]),

    case start_amqp_host(Uri, UseFederation) of
        {ok, State1} -> {noreply, State1#state{amqp_uri=Uri, use_federation=UseFederation}};
        {error, E} -> {stop, E, normal}
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({reconnect, Timeout}, #state{conn_ref=undefined
                                         ,conn_params=ConnP
                                         ,use_federation=UseFederation
                                        }=State) ->
    case start_amqp_host(ConnP, State, UseFederation) of
        {ok, State1} ->
            ?LOG_SYS("Reconnected AMQP"),
            {noreply, State1};
        {error, _E} ->
            ?LOG_SYS("Failed to reconnect to AMQP(~p), waiting a bit more", [_E]),
            NextTimeout = next_timeout(Timeout),
            _Ref = erlang:send_after(NextTimeout, self(), {reconnect, NextTimeout}),
            {noreply, State}
    end;

handle_info({'DOWN', ConnRef, process, _Pid, {shutdown, {server_initiated_close,Code,_}}}
             ,#state{conn_params=ConnP, conn_ref=ConnRef}=State) ->
    ?LOG_SYS("connection to ~s (process ~p) went down, error ~p", [wh_amqp_params:host(ConnP), _Pid, Code]),
    erlang:demonitor(ConnRef, [flush]),
    _Ref = erlang:send_after(?START_TIMEOUT, self(), {reconnect, ?START_TIMEOUT}),
    _ = stop_amqp_host(State),
    {noreply, State#state{conn_ref=undefined, handler_pid=undefined
                          ,handler_ref=undefined, use_federation = false
                         }, hibernate};

handle_info({'DOWN', ConnRef, process, _Pid, {shutdown, {internal_error, Code,_}}}
            ,#state{conn_params=ConnP, conn_ref=ConnRef}=State) ->
    ?LOG_SYS("connection to ~s (process ~p) went down, error ~p", [wh_amqp_params:host(ConnP), _Pid, Code]),
    erlang:demonitor(ConnRef, [flush]),
    _Ref = erlang:send_after(?START_TIMEOUT, self(), {reconnect, ?START_TIMEOUT}),
    _ = stop_amqp_host(State),
    {noreply, State#state{conn_ref=undefined, handler_pid=undefined
                          ,handler_ref=undefined, use_federation = false
                         }, hibernate};

handle_info({'DOWN', ConnRef, process, _Pid, _Reason}, #state{conn_params=ConnP, conn_ref=ConnRef}=State) ->
    ?LOG_SYS("connection to ~s (process ~p) went down, ~w", [wh_amqp_params:host(ConnP), _Pid, _Reason]),
    erlang:demonitor(ConnRef, [flush]),
    _Ref = erlang:send_after(?START_TIMEOUT, self(), {reconnect, ?START_TIMEOUT}),
    _ = stop_amqp_host(State),
    {noreply, State#state{conn_ref=undefined, handler_pid=undefined, handler_ref=undefined}, hibernate};

handle_info({'DOWN', Ref, process, _, normal}, #state{handler_ref=Ref}=State) ->
    ?LOG_SYS("amqp host proc down normally"),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{handler_ref=undefined, handler_pid=undefined}};

handle_info({'DOWN', Ref, process, _, _Reason}, #state{handler_ref=Ref}=State) ->
    ?LOG_SYS("amqp host process went down, ~w", [_Reason]),
    erlang:demonitor(Ref, [flush]),
    _Ref = erlang:send_after(?START_TIMEOUT, self(), {reconnect, ?START_TIMEOUT}),
    {noreply, State#state{handler_pid=undefined, handler_ref=undefined}, hibernate};

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
-spec terminate/2 :: (term(), #state{}) -> no_return().
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
-spec get_new_connection/1 :: (#'amqp_params_direct'{} | #'amqp_params_network'{}) -> {'ok', pid()} | {'error', 'econnrefused' | 'broker_not_found_on_node'}.
get_new_connection(ConnP) ->
    case amqp_connection:start(ConnP) of
        {ok, Connection}=OK ->
            ?LOG_SYS("established network connection (~p) to amqp broker at ~s", [Connection, wh_amqp_params:host(ConnP)]),
            OK;
        {error, econnrefused}=E ->
            ?LOG_SYS("amqp connection to ~s refused", [wh_amqp_params:host(ConnP)]),
            E;
        {error, broker_not_found_on_node}=E ->
            ?LOG_SYS("found node ~s but no amqp broker", [wh_amqp_params:host(ConnP)]),
            E
    end.

stop_amqp_host(#state{handler_pid=undefined}) ->
    ok;
stop_amqp_host(#state{handler_pid=HPid, handler_ref=HRef}) ->
    erlang:demonitor(HRef, [flush]),
    _ = net_kernel:monitor_nodes(false),
    spawn(fun() -> amqp_host:stop(HPid) end).

-spec start_amqp_host/2 :: (string(), boolean()) -> {'ok', #state{}} | {'error', 'econnrefused'}.
-spec start_amqp_host/3 :: (#'amqp_params_direct'{} | #'amqp_params_network'{}, #state{}, boolean()) ->
                                   {'ok', #state{}} | {'error', 'econnrefused'}.
start_amqp_host(Uri, UseFederation) ->
    case amqp_uri:parse(Uri) of
        {ok, Settings} ->
            start_amqp_host(Settings, #state{}, UseFederation);
        {error, {Info, _}} ->
            {error, Info}
    end.

start_amqp_host(ConnP, State, UseFederation) ->
    case get_new_connection(ConnP) of
        {error,_}=E ->
            E;
        {ok, Conn} ->
            case amqp_host_sup:start_host(wh_amqp_params:host(ConnP), Conn, UseFederation) of
                {ok, HPid} ->
                    HRef = erlang:monitor(process, HPid),
                    ConnRef = erlang:monitor(process, Conn),

                    ?LOG("Started amqp_host ~p", [HPid]),

                    {ok, State#state{handler_pid=HPid, handler_ref=HRef, conn_ref=ConnRef
                                     ,conn_params=ConnP, timeout=?START_TIMEOUT
                                    }};
                E ->
                    ?LOG("Error starting amqp_host ~p", [E]),
                    case UseFederation of
                        true -> start_amqp_host(ConnP, State, false);
                        false -> E
                    end
            end
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

-spec next_timeout/1 :: (pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT=Timeout) -> Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.
