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
-export([open_channel/0
         ,open_channel/1
        ]).
-export([monitor_channel/2]).
-export([teardown_channels/1]).
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

teardown_channels(Broker) ->
    gen_server:call(wh_amqp_broker:name(Broker), teardown_channels).

-spec open_channel/0 :: () -> #wh_amqp_channel{} | {'error', _}.
open_channel() ->
    open_channel(#wh_amqp_channel{}).

-spec open_channel/1 :: (#wh_amqp_channel{}) -> #wh_amqp_channel{} | {'error', _}.
open_channel(Channel) ->
    case wh_amqp_connections:current() of
        {error, _}=E -> E;
        {ok, Connection} ->
            open_channel(Channel, Connection)
    end.

monitor_channel(Srv, Channel) ->
    gen_server:cast(Srv, {monitor_channel, Channel}).

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
handle_cast({monitor_channel, #wh_amqp_channel{channel=Channel, consumer=Consumer}=C}, State) ->
    wh_amqp_connections:update_channel(C, monitor_updates(Channel, Consumer)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

monitor_updates(Channel, Consumer) when not is_pid(Consumer) ->
    {#wh_amqp_channel.channel_ref, erlang:monitor(process, Channel)};
monitor_updates(Channel, Consumer) when not is_pid(Channel) ->
    {#wh_amqp_channel.consumer_ref, erlang:monitor(process, Consumer)};
monitor_updates(Channel, Consumer) ->
    [{#wh_amqp_channel.channel_ref, erlang:monitor(process, Channel)}
     ,{#wh_amqp_channel.consumer_ref, erlang:monitor(process, Consumer)}
    ].

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
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    io:format("~p: down msg ~p ~p ~p~n", [self(), Ref, _Pid, Reason]),
    erlang:demonitor(Ref, [flush]),
%%    _ = spawn(fun() ->
                      Matches = wh_amqp_connections:find_reference(Ref),
                      handle_down_msg(Matches, Reason, State),
%%              end),
    {noreply, State, hibernate};
%%handle_info({#'basic.return'{}, #amqp_msg{}}=ReturnMsg, State) ->
%%    wh_amqp_mgr:notify_return_handlers(ReturnMsg),
%%    {noreply, State};
handle_info({connect, Timeout}, #wh_amqp_connection{broker=Broker, name=Name}=State) ->
    io:format("~p: attempting connection ~p~n", [self(), Name]),
    disconnected(State),
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
handle_down_msg([], _, _) -> ok;
handle_down_msg([Match|Matches], Reason, State) ->
    _ = handle_down_msg(Match, Reason, State),
    handle_down_msg(Matches, Reason, State);
handle_down_msg({channel, #wh_amqp_channel{started=Started}=Channel}, _
                ,#wh_amqp_connection{available=true}) ->
%%    io:format("~p: down message for a channel while connected~n", [self()]),
    Duration = wh_util:elapsed_s(Started),
    _ = case Duration < 5 of
            false -> ok;
            true ->
                lager:warning("short lived channel (~ps): ~p", [Duration, Channel])
        end,
    close_channel(Channel),
    open_channel(close_channel(Channel));
handle_down_msg({channel, #wh_amqp_channel{}=Channel}, _
                ,#wh_amqp_connection{available=false}) ->
    io:format("~p: down message for a channel while disconnected~n", [self()]),
    close_channel(Channel);
handle_down_msg({consumer, #wh_amqp_channel{started=Started}=Channel}, _, _) ->
    io:format("~p: down message for a consumer~n", [self()]),
    Duration = wh_util:elapsed_s(Started),
    _ = case Duration < 5 of
            false -> ok;
            true ->
                lager:warning("short lived channel (~ps): ~p", [Duration, Channel])
        end,
    remove_channel(Channel);
handle_down_msg({connection, #wh_amqp_connection{}}, _
                ,#wh_amqp_connection{manager=Srv}) ->
    io:format("~p: down message for the connection", [self()]),
    lager:info("connection to the AMQP broker died", []),
    %%    gen_server:cast(wh_amqp_mgr, {broker_unavailable, Name}),
    %%    notify_consumers({amqp_channel_event, Reason}, Name),
    Srv ! {connect, ?START_TIMEOUT}.

close_channel(#wh_amqp_channel{channel_ref=Ref}=Channel) when is_reference(Ref)->
    io:format("~p: flush channel ref~n", [self()]),
    erlang:demonitor(Ref, [flush]),
    Update = [{#wh_amqp_channel.channel_ref, undefined}],
    wh_amqp_connections:update_channel(Channel, Update),
    close_channel(Channel#wh_amqp_channel{channel_ref=undefined});
close_channel(#wh_amqp_channel{channel=Pid}=Channel) when is_pid(Pid) ->
    io:format("~p: close channel ~p~n", [self(), Pid]),
    catch amqp_channel:close(Pid),
    Update = [{#wh_amqp_channel.channel, undefined}],
    wh_amqp_connections:update_channel(Channel, Update),
    close_channel(Channel#wh_amqp_channel{channel=undefined});
close_channel(Channel) ->
    Channel.

remove_channel(Channel) ->
    wh_amqp_connections:remove_channel(close_channel(Channel)).

connected(Pid, State) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    io:format("~p: create connection ref ~p~n", [self(), Ref]),
    S = State#wh_amqp_connection{connection=Pid
                                 ,connection_ref=Ref
                                 ,available=true},
    wh_amqp_connections:update_connection(S),
    S.

disconnected(#wh_amqp_connection{connection_ref=Ref}=State) when is_reference(Ref) ->
    erlang:demonitor(Ref, [flush]),
    io:format("~p: flush connection ref ~p~n", [self(), Ref]),
    disconnected(State#wh_amqp_connection{connection_ref=undefined});
disconnected(State) ->
    S = State#wh_amqp_connection{connection=undefined
                                 ,available=false},
    wh_amqp_connections:update_connection(S),
    S.

-spec open_channel/2 :: (#wh_amqp_connection{}, #wh_amqp_channel{} | pid()) -> #wh_amqp_channel{}.
open_channel(Channel, #wh_amqp_connection{connection=Connection
                                          ,available=true
                                          ,manager=Srv}) ->
    try amqp_connection:open_channel(Connection) of
        {ok, Pid} ->
            %%            amqp_selective_consumer:register_default_consumer(Channel, Srv),
            %% TODO: dont start the monitor here...
            ConnectedChannel = Channel#wh_amqp_channel{channel=Pid},
            wh_amqp_connections:update_channel(ConnectedChannel),
            wh_amqp_connection:monitor_channel(Srv, ConnectedChannel),
            ConnectedChannel;
        closing ->
            io:format("unable to open channel, connection is closing~n", []),
            close_channel(Channel);
        {error, _R} ->
            lager:critical("failed to open AMQP channel: ~p", [_R]),
            close_channel(Channel)
    catch
        _:{noproc, {gen_server, call, [Pid|_]}} ->
            io:format("AMQP connection ~p is no longer valid...~n", [Pid]),
            close_channel(Channel)
    end;
open_channel(Channel, _) ->
    lager:critical("failed to open AMQP channel: no_connection", []),
    close_channel(Channel).

-spec next_timeout/1 :: (pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT=Timeout) ->
    Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.

clear_channels(Name) ->
    _ = [clear_channel(C) || #wh_amqp_channel{}=C <- ets:tab2list(Name)],
    ok.
clear_channel(#wh_amqp_channel{channel=ChPid
                               ,channel_ref=ChRef
                               ,consumer_ref=ConRef
                               ,consumer=Consumer
                              }) ->
    is_reference(ChRef) andalso erlang:demonitor(ChRef, [flush]),
    is_reference(ConRef) andalso erlang:demonitor(ConRef, [flush]),
    erlang:is_process_alive(ChPid) andalso amqp_channel:close(ChPid),
    is_pid(Consumer) andalso (Consumer ! {amqp_channel_event, closing}).

%%-spec notify_consumers/2 :: ({'amqp_channel_event', atom()}, atom()) -> 'ok'.
%%notify_consumers(Msg, Name) ->
%%    ets:foldl(fun(#wh_amqp_channel{consumer = Consumer}, _) when is_pid(Consumer) ->
%%                      Consumer ! Msg, ok;
%%                 (_, _) -> ok
%%              end, ok, Name).

%%-spec try_to_subscribe/3 :: (atom(), pid(), #'basic.consume'{}) -> 'ok' | {'error', term()}.
%%try_to_subscribe(Srv, Channel, BasicConsume) ->
%%    try amqp_channel:call(Channel, BasicConsume) of
%%        #'basic.consume_ok'{consumer_tag=Tag} ->
%%            update_my_tag(Srv, Tag),
%%            ok;
%%        Other -> {error, Other}
%%    catch
%%        _E:R -> {error, R}
%%    end.
