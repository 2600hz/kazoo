%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle a host's connection/channels
%%% @end
%%% @contributions
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_amqp_connection).

-behaviour(gen_server).

-export([start_link/1]).
-export([does_exchange_exist/2]).
-export([exchange_exist/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(START_TIMEOUT, 500).
-define(MAX_TIMEOUT, 5000).

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
start_link(Broker) ->
    Name = wh_amqp_broker:name(Broker),
    gen_server:start_link({local, Name}, ?MODULE, [Broker], []).

does_exchange_exist(#wh_amqp_channel{connection=Srv}
                    ,#'exchange.declare'{exchange=ExchangeName}) ->
    ets:member(?WH_AMQP_ETS, {Srv, ExchangeName}).

exchange_exist(#wh_amqp_channel{connection=Srv}
               ,#'exchange.declare'{exchange=ExchangeName}=Command) ->
    Exchange = #wh_amqp_exchange{id={Srv, ExchangeName}
                                 ,connection=Srv
                                 ,exchange=Command
                                },
    ets:insert(?WH_AMQP_ETS, Exchange).

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
init([Broker]) ->
    _ = process_flag(trap_exit, true),
    self() ! {connect, ?START_TIMEOUT},
    Name = wh_amqp_broker:name(Broker),
    _ = put(callid, wh_amqp_broker:host(Broker)),
    io:format("~p: started new wh_amqp_connection to ~p~n", [self(), wh_amqp_broker:host(Broker)]),
    {ok, #wh_amqp_connection{broker=Broker, name=Name}}.

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
handle_call(teardown_channels, _, State) ->
    io:format("~p: teardown channels~n", [self()]),
    %% TODO find and teardown channels using this connection...
    {reply, ok, State};
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    io:format("NOT IMPLEMENTED(call): ~p~n", [_Msg]),
    {reply, {error, not_implemented}, State}.

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
    io:format("NOT IMPLEMENTED(cast): ~p~n", [_Msg]),
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
handle_info({'DOWN', Ref, process, _Pid, Reason}
            ,#wh_amqp_connection{connection_ref=Ref}=State) ->
    io:format("down message for the connection: ~p", [Reason]),
    lager:info("connection to the AMQP broker died: ~p", [Reason]),
    %%    notify_consumers({amqp_channel_event, Reason}, Name),
    self() ! {connect, ?START_TIMEOUT},
    {noreply, disconnected(State), hibernate};
%%handle_info({#'basic.return'{}, #amqp_msg{}}=ReturnMsg, State) ->
%%    wh_amqp_mgr:notify_return_handlers(ReturnMsg),
%%    {noreply, State};
handle_info({connect, Timeout}, #wh_amqp_connection{broker=Broker, name=Name}=State) ->
    io:format("~p: attempting connection ~p~n", [self(), Name]),
    Params = wh_amqp_broker:params(Broker),
    case amqp_connection:start(Params) of
        {error, auth_failure} ->
            lager:warning("amqp authentication failure with '~s', will retry in ~p", [Name, Timeout]),
            _Ref = erlang:send_after(Timeout, self(), {connect, next_timeout(Timeout)}),
            {noreply, disconnected(State), hibernate};
        {error, _Reason} ->
            lager:warning("failed to connect to '~s' will retry in ~p: ~p", [Name, Timeout, _Reason]),
            _Ref = erlang:send_after(Timeout, self(), {connect, next_timeout(Timeout)}),
            {noreply, disconnected(State), hibernate};
        {ok, Pid} ->
            io:format("~p: connected via ~p~n", [self(), Pid]),
            {noreply, connected(Pid, State), hibernate}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State, hibernate}.

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
-spec terminate/2 :: (term(), #wh_amqp_connection{}) -> 'ok'.
terminate(_Reason, #wh_amqp_connection{name=Name}) ->
    put(callid, ?LOG_SYSTEM_ID),
%%    notify_consumers({amqp_channel_event, terminated}, Name),
    lager:debug("connection to AMQP broker '~s' terminated: ~p", [Name, _Reason]).

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
connected(Pid, State) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    io:format("create connection ref ~p~n", [Ref]),
    S = State#wh_amqp_connection{connection=Pid
                                 ,connection_ref=Ref
                                 ,available=true},
    wh_amqp_connections:update_connection(S),
    S.

disconnected(#wh_amqp_connection{connection_ref=Ref}=State) when is_reference(Ref) ->
    erlang:demonitor(Ref, [flush]),
    io:format("flush connection ref ~p~n", [Ref]),
    disconnected(State#wh_amqp_connection{connection_ref=undefined});
disconnected(State) ->
    S = State#wh_amqp_connection{connection=undefined
                                 ,available=false},
    wh_amqp_connections:update_connection(S),
    S.

-spec next_timeout/1 :: (pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT=Timeout) ->
    Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.

%%-spec notify_consumers/2 :: ({'amqp_channel_event', atom()}, atom()) -> 'ok'.
%%notify_consumers(Msg, Name) ->
%%    ets:foldl(fun(#wh_amqp_channel{consumer = Consumer}, _) when is_pid(Consumer) ->
%%                      Consumer ! Msg, ok;
%%                 (_, _) -> ok
%%              end, ok, Name).
