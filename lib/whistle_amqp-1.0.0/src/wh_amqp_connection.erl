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
-export([exchange_declared/2]).
-export([is_exchange_declared/2]).
-export([get_channel/1]).
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
start_link(#wh_amqp_connection{manager=Name}=Connection) ->
    gen_server:start_link({local, Name}, ?MODULE, [Connection], []).

-spec is_exchange_declared(ne_binary(), #'exchange.declare'{}) -> boolean().
is_exchange_declared(URI, #'exchange.declare'{exchange=Name}) ->
    case wh_amqp_connections:find(URI) of
        #wh_amqp_connection{exchanges=Exchanges} ->
            lists:keysearch(Name, #'exchange.declare'.exchange, Exchanges) =/= false;
        {error, not_found} -> false
    end.

-spec exchange_declared(ne_binary(), #'exchange.declare'{}) -> 'ok'.
exchange_declared(URI, #'exchange.declare'{}=Command) ->
    case wh_amqp_connections:find(URI) of
        #wh_amqp_connection{manager=Srv} ->
            gen_server:cast(Srv, {exchange_declared, Command});
        {error, not_found} -> ok
    end.

-spec get_channel(#wh_amqp_connection{}) -> {'ok', pid()} | {'error', _}.
get_channel(#wh_amqp_connection{manager=Srv}=Connection) ->
    case gen_server:call(Srv, get_channel) of
        {ok, _}=Ok -> Ok;
        {error, no_channels} ->
            lager:debug("no prechannels available", []),
            open_channel(Connection);
        {error, _}=E -> E
    end.

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
init([#wh_amqp_connection{uri=URI}=Connection]) ->
    _ = process_flag(trap_exit, true),
    put(callid, ?LOG_SYSTEM_ID),
    self() ! {connect, ?START_TIMEOUT},
    %% in case we crashed and our sup has restarted us....
    case wh_amqp_connections:find(URI) of
        {error, not_found} -> {ok, Connection};
        {ok, C} -> {ok, C}
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
handle_call(teardown_channels, _, State) ->
    io:format("~p: teardown channels~n", [self()]),
    %% TODO find and teardown channels using this connection...
    {reply, ok, State};
handle_call(get_channel, _, #wh_amqp_connection{available=true, prechannels=[]}=State) ->
    gen_server:cast(self(), start_prechannel),
    {reply, {error, no_channels}, State};
handle_call(get_channel, _
            ,#wh_amqp_connection{available=true, prechannels=[{Ref, Channel}|Channels]}=State) ->
    erlang:demonitor(Ref, [flush]),
    gen_server:cast(self(), start_prechannel),
    {reply, {ok, Channel}, State#wh_amqp_connection{prechannels=Channels}};
handle_call(get_channel, _, State) ->
    {reply, {error, not_connected}, State};
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
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
handle_cast({exchange_declared, #'exchange.declare'{exchange=Name}=Command}
            ,#wh_amqp_connection{uri=URI}=State) ->
    Exchanges = [Command
                 |lists:filter(fun(#'exchange.declare'{exchange=N}) ->
                                       N =/= Name;
                                  (_) -> true
                               end, State#wh_amqp_connection.exchanges)
                ],
    wh_amqp_connections:update_exchanges(URI, Exchanges),
    {noreply, State#wh_amqp_connection{exchanges=Exchanges}};
handle_cast(start_prechannel, #wh_amqp_connection{available=true, prechannels=Channels}=State) ->
    _ = case length(Channels) < 20 of
            true ->
                spawn(fun() -> start_prechannel(State) end);
            false -> ok
        end,
    {noreply, State};
handle_cast({new_prechannel, Pid}, #wh_amqp_connection{available=true, prechannels=Channels}=State) ->
    Ref = erlang:monitor(process, Pid),
    {noreply, State#wh_amqp_connection{prechannels=[{Ref, Pid}|Channels]}};
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
handle_info({'DOWN', Ref, process, _Pid, Reason}
            ,#wh_amqp_connection{connection_ref=Ref}=State) ->
    lager:critical("connection to the AMQP broker died: ~p", [Reason]),
    %%    notify_consumers({amqp_channel_event, Reason}, Name),
    self() ! {connect, ?START_TIMEOUT},
    {noreply, disconnected(State), hibernate};
handle_info({'DOWN', Ref, process, _Pid, _Reason}
            ,#wh_amqp_connection{prechannels=Channels}=State) ->
    erlang:demonitor(Ref, [flush]),
    gen_server:cast(self(), start_prechannel),
    {noreply, State#wh_amqp_connection{prechannels=lists:keydelete(Ref, 1, Channels)}};
%%handle_info({#'basic.return'{}, #amqp_msg{}}=ReturnMsg, State) ->
%%    wh_amqp_mgr:notify_return_handlers(ReturnMsg),
%%    {noreply, State};
handle_info({connect, Timeout}, #wh_amqp_connection{uri=URI, params=Params}=State) ->
    case amqp_connection:start(Params) of
        {error, auth_failure} ->
            lager:warning("amqp authentication failure with '~s', will retry in ~p", [URI, Timeout]),
            _Ref = erlang:send_after(Timeout, self(), {connect, next_timeout(Timeout)}),
            {noreply, disconnected(State), hibernate};
        {error, _Reason} ->
            lager:warning("failed to connect to '~s' will retry in ~p: ~p", [URI, Timeout, _Reason]),
            _Ref = erlang:send_after(Timeout, self(), {connect, next_timeout(Timeout)}),
            {noreply, disconnected(State), hibernate};
        {ok, Pid} ->
            lager:notice("connected successfully to '~s'", [URI]),
            Ref = erlang:monitor(process, Pid),
            S = State#wh_amqp_connection{connection=Pid
                                         ,connection_ref=Ref},
            {noreply, prepare_connection(S), hibernate}
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
terminate(_Reason, #wh_amqp_connection{uri=Name}=State) ->
    lager:debug("connection to amqp broker '~s' terminated: ~p", [Name, _Reason]),
    disconnected(State).

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
-spec prepare_connection(#wh_amqp_connection{}) -> #wh_amqp_connection{}.
prepare_connection(State) ->
    case open_channel(State) of
        {error, _R} ->
            lager:critical("failed to open control channel to new amqp broker: ~p", [_R]),
            disconnected(State);
        {ok, Pid} ->
            rebuild_exchanges(Pid, State)
    end.

-spec rebuild_exchanges(pid(), #wh_amqp_connection{}) -> #wh_amqp_connection{}.
rebuild_exchanges(Pid, #wh_amqp_connection{exchanges=Exchanges}=State) ->
    rebuild_exchanges(Exchanges, Pid, State).

-spec rebuild_exchanges([] | [#'exchange.declare'{},...], pid(), #wh_amqp_connection{}) -> #wh_amqp_connection{}.
rebuild_exchanges([], _, State) ->
    start_prechannels(State);
rebuild_exchanges([#'exchange.declare'{exchange=_Ex, type=_Ty}=Command
                   |Exchanges
                  ], Pid, State) ->
    lager:debug("redeclared ~s exchange ~s via channel ~p", [_Ty, _Ex, Pid]),
    amqp_channel:call(Pid, Command),
    rebuild_exchanges(Exchanges, Pid, State).

-spec start_prechannels(#wh_amqp_connection{}) -> #wh_amqp_connection{}.
start_prechannels(State) ->
    _ = [start_prechannel(State)
         || _ <- lists:seq(1,10)
        ],
    connected(State).

-spec connected(#wh_amqp_connection{}) -> #wh_amqp_connection{}.
connected(State) ->
    wh_amqp_connections:connected(State).

-spec disconnected(#wh_amqp_connection{}) -> #wh_amqp_connection{}.
disconnected(State) ->
    wh_amqp_channels:lost_connection(State),
    S = wh_amqp_connections:disconnected(State),
    close_connection(S).

-spec close_connection(#wh_amqp_connection{}) -> #wh_amqp_connection{}.
close_connection(#wh_amqp_connection{connection_ref=Ref}=State) when is_reference(Ref) ->
    erlang:demonitor(Ref, [flush]),
    close_connection(State#wh_amqp_connection{connection_ref=undefined});
close_connection(#wh_amqp_connection{prechannels=[{Ref, _}|Channels]}=State) ->
    erlang:demonitor(Ref, [flush]),
    close_connection(State#wh_amqp_connection{prechannels=Channels});
close_connection(State) ->
    State.

-spec next_timeout/1 :: (pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT=Timeout) ->
    Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.

-spec start_prechannel(#wh_amqp_connection{}) -> 'ok'.
start_prechannel(#wh_amqp_connection{manager=Srv}=Connection) ->
    case open_channel(Connection) of
        {ok, Pid} ->
            gen_server:cast(Srv, {new_prechannel, Pid});
        {error, _} ->
            ok
    end.

-spec open_channel(#wh_amqp_connection{}) -> {'ok', pid()} | {'error', _}.
open_channel(#wh_amqp_connection{connection=Pid, manager=Srv}) ->
    try amqp_connection:open_channel(Pid) of
        {ok, Channel}=Ok ->
            %% This is not strickly necessary, but since we
            %% loose the entire CONNECTION if a single message
            %% cant be delivered, better safe then sorry...
            amqp_selective_consumer:register_default_consumer(Channel, whereis(Srv)),
            Ok;
        closing ->
            lager:debug("unable to open channel, connection is closing", []),
            {error, closing};
        {error, _R}=E ->
            lager:critical("failed to open AMQP channel: ~p", [_R]),
            E
    catch
        _:{noproc, {gen_server, call, [P|_]}} ->
            %% TODO: send notice to self...
            lager:debug("amqp connection ~p is no longer valid...", [P]),
            {error, not_connected}
    end.
