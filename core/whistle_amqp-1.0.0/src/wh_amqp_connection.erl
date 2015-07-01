%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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
-export([get_connection/1]).
-export([new_exchange/2]).
-export([create_prechannel/1]).
-export([disconnect/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(START_TIMEOUT, 200).
-define(MAX_TIMEOUT, ?MILLISECONDS_IN_SECOND).

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
start_link(#wh_amqp_connection{}=Connection) ->
    gen_server:start_link(?MODULE, [Connection], []).

-spec get_connection(pid()) -> wh_amqp_connection().
get_connection(Srv) ->
    gen_server:call(Srv, 'get_connection').

-spec new_exchange(pid(), wh_amqp_exchange()) -> 'ok'.
new_exchange(Srv, Exchange) ->
    gen_server:cast(Srv, {'new_exchange', Exchange}).

-spec create_prechannel(pid()) -> 'ok'.
create_prechannel(Srv) ->
    gen_server:cast(Srv, 'create_prechannel').

-spec disconnect(pid()) -> 'ok'.
disconnect(Srv) ->
    gen_server:cast(Srv, 'disconnect').

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
init([#wh_amqp_connection{}=Connection]) ->
    _ = process_flag('trap_exit', 'true'),
    wh_util:put_callid(?LOG_SYSTEM_ID),
    {'ok', disconnected(Connection#wh_amqp_connection{manager=self()})}.

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
handle_call('get_connection', _, Connection) ->
    {'reply', Connection, Connection};
handle_call('stop', _, Connection) ->
    {'stop', 'normal', 'ok', disconnected(Connection)};
handle_call(_Msg, _From, Connection) ->
    {'reply', {'error', 'not_implemented'}, Connection}.

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
handle_cast('disconnect'
            ,#wh_amqp_connection{available='false'}=Connection) ->
    {'noreply', Connection, 'hibernate'};
handle_cast('disconnect'
            ,#wh_amqp_connection{available='true'}=Connection) ->
    {'noreply', disconnected(Connection), 'hibernate'};
handle_cast('create_control_channel'
            ,#wh_amqp_connection{available='false'}=Connection) ->
    {'noreply', Connection, 'hibernate'};
handle_cast('create_control_channel'
            ,#wh_amqp_connection{available='true'}=Connection) ->
    {'noreply', create_control_channel(Connection), 'hibernate'};
handle_cast('create_prechannel'
            ,#wh_amqp_connection{available='false'}=Connection) ->
    {'noreply', Connection};
handle_cast('create_prechannel'
            ,#wh_amqp_connection{available='true'}=Connection) ->
    _ = wh_util:spawn(fun() -> establish_prechannel(Connection) end),
    {'noreply', Connection, 'hibernate'};
handle_cast({'new_exchange', _}
            ,#wh_amqp_connection{available='false'}=Connection) ->
    {'noreply', Connection, 'hibernate'};
handle_cast({'new_exchange', Exchange}
            ,#wh_amqp_connection{available='true'}=Connection) ->
    {'noreply', declare_exchanges(Connection, [Exchange]), 'hibernate'};
handle_cast(_Msg, Connection) ->
    {'noreply', Connection}.

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
handle_info({'DOWN', _Ref, 'process', _Pid, _Reason}
            ,#wh_amqp_connection{available='false'}=Connection) ->
    {'noreply', Connection, 'hibernate'};
handle_info({'DOWN', Ref, 'process', _Pid, _Reason}
            ,#wh_amqp_connection{available='true'
                                 ,channel_ref=Ref
                                 ,broker=_Broker}=Connection) ->
    lager:warning("command channel to the AMQP broker ~s died: ~p"
                  ,[_Broker, _Reason]),
    {'noreply', create_control_channel(Connection), 'hibernate'};
handle_info({'DOWN', Ref, 'process', _Pid, _Reason}
            ,#wh_amqp_connection{available='true'
                                 ,connection_ref=Ref
                                 ,broker=_Broker}=Connection) ->
    lager:critical("connection to the AMQP broker ~s died: ~p"
                   ,[_Broker, _Reason]),
    {'noreply', disconnected(Connection), 'hibernate'};
handle_info({'connect', Timeout}
            ,#wh_amqp_connection{available='false'}=Connection) ->
    {'noreply', maybe_connect(Connection, Timeout), 'hibernate'};
handle_info({'connect', _}, #wh_amqp_connection{available='true'}=Connection) ->
    {'noreply', Connection, 'hibernate'};
handle_info(_Info, Connection) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', Connection, 'hibernate'}.

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
-spec terminate(term(), wh_amqp_connection()) -> any().
terminate(_Reason, #wh_amqp_connection{broker=_Broker}=Connection) ->
    lager:debug("connection to amqp broker '~s' terminated: ~p"
                ,[_Broker, _Reason]),
    disconnected(Connection).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, Connection, _Extra) ->
    {'ok', Connection}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec connected(wh_amqp_connection()) -> wh_amqp_connection().
connected(#wh_amqp_connection{reconnect_ref=Ref}=Connection)
  when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    connected(Connection#wh_amqp_connection{reconnect_ref='undefined'});
connected(#wh_amqp_connection{channel='undefined'}=Connection) ->
    case create_control_channel(Connection) of
        #wh_amqp_connection{channel=Pid}=Success
          when is_pid(Pid)-> connected(Success);
        #wh_amqp_connection{}=Error -> Error
    end;
connected(#wh_amqp_connection{exchanges_initialized='false'}=Connection) ->
    case declare_exchanges(Connection) of
        #wh_amqp_connection{exchanges_initialized='false'}=Error -> Error;
        #wh_amqp_connection{exchanges_initialized='true'}=Success ->
            connected(Success)
    end;
connected(#wh_amqp_connection{available='false'}=Connection) ->
    _ = wh_amqp_connections:available(self()),
    connected(Connection#wh_amqp_connection{available='true'});
connected(#wh_amqp_connection{prechannels_initialized='false'}=Connection) ->
    case initialize_prechannels(Connection) of
        #wh_amqp_connection{prechannels_initialized='false'}=Error -> Error;
        #wh_amqp_connection{prechannels_initialized='true'}=Success ->
            connected(Success)
    end;
connected(#wh_amqp_connection{broker=_Broker}=Connection) ->
    lager:info("successfully connected to '~s'", [_Broker]),
    Connection.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec disconnected(wh_amqp_connection()) -> wh_amqp_connection().
disconnected(#wh_amqp_connection{manager=Manager}=State) ->
    case Manager =:= self() of
        'true' -> disconnected(State, ?START_TIMEOUT);
        'false' -> disconnect(Manager)
    end.

-spec disconnected(wh_amqp_connection(), ?START_TIMEOUT..?MAX_TIMEOUT) -> wh_amqp_connection().
disconnected(#wh_amqp_connection{available='true'}=Connection, Timeout) ->
    _ = wh_amqp_connections:unavailable(self()),
    disconnected(Connection#wh_amqp_connection{available='false'}, Timeout);
disconnected(#wh_amqp_connection{channel_ref=Ref}=Connection, Timeout)
  when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    disconnected(Connection#wh_amqp_connection{channel_ref='undefined'}, Timeout);
disconnected(#wh_amqp_connection{channel=Pid}=Connection, Timeout)
  when is_pid(Pid) ->
    _ = (catch wh_amqp_channel:close(Pid)),
    disconnected(Connection#wh_amqp_connection{channel='undefined'}, Timeout);
disconnected(#wh_amqp_connection{connection_ref=Ref}=Connection, Timeout)
  when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    disconnected(Connection#wh_amqp_connection{connection_ref='undefined'}, Timeout);
disconnected(#wh_amqp_connection{connection=Pid}=Connection, Timeout)
  when is_pid(Pid) ->
    _ = (catch amqp_connection:close(Pid, 5000)),
    disconnected(Connection#wh_amqp_connection{connection='undefined'}, Timeout);
disconnected(#wh_amqp_connection{prechannels_initialized='true'}=Connection, Timeout) ->
    disconnected(Connection#wh_amqp_connection{prechannels_initialized='false'}, Timeout);
disconnected(#wh_amqp_connection{exchanges_initialized='true'}=Connection, Timeout) ->
    disconnected(Connection#wh_amqp_connection{exchanges_initialized='false'}, Timeout);
disconnected(#wh_amqp_connection{}=Connection, Timeout) ->
    NextTimeout = next_timeout(Timeout),
    Ref = erlang:send_after(Timeout, self(), {'connect', NextTimeout}),
    Connection#wh_amqp_connection{reconnect_ref=Ref}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec next_timeout(pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT=Timeout) ->
    Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_connect(wh_amqp_connection(), ?START_TIMEOUT..?MAX_TIMEOUT) -> wh_amqp_connection().
maybe_connect(#wh_amqp_connection{broker=_Broker
                                  ,available='false'
                                  ,params=Params}=Connection
                  ,Timeout) ->
    case amqp_connection:start(Params) of
        {'error', 'auth_failure'} ->
            lager:warning("amqp authentication failure with '~s', will retry"
                          ,[_Broker]),
            disconnected(Connection, Timeout);
        {'error', _Reason} ->
            lager:warning("failed to connect to '~s' will retry: ~p"
                          ,[_Broker, _Reason]),
            disconnected(Connection, Timeout);
        {'ok', Pid} ->
            Ref = erlang:monitor('process', Pid),
            connected(Connection#wh_amqp_connection{connection=Pid
                                                    ,connection_ref=Ref})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_control_channel(wh_amqp_connection()) -> wh_amqp_connection().
create_control_channel(#wh_amqp_connection{channel_ref=Ref}=Connection)
  when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    create_control_channel(Connection#wh_amqp_connection{channel_ref='undefined'});
create_control_channel(#wh_amqp_connection{channel=Pid}=Connection)
  when is_pid(Pid) ->
    _ = (catch wh_amqp_channel:close(Pid)),
    create_control_channel(Connection#wh_amqp_connection{channel='undefined'});
create_control_channel(#wh_amqp_connection{broker=Broker}=Connection) ->
    case open_channel(Connection) of
        {'error', _R} ->
            lager:critical("unable to establish command channel to ~s, assuming connection is invalid: ~p"
                           ,[Broker, _R]),
            disconnected(Connection);
        {'ok', Pid} ->
            lager:debug("created command channel ~p to ~s", [Pid, Broker]),
            Ref = erlang:monitor('process', Pid),
            Connection#wh_amqp_connection{channel=Pid
                                          ,channel_ref=Ref}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec initialize_prechannels(wh_amqp_connection()) -> wh_amqp_connection().
initialize_prechannels(#wh_amqp_connection{}=Connection) ->
    initialize_prechannels(Connection, 10).

-spec initialize_prechannels(wh_amqp_connection(), non_neg_integer()) -> wh_amqp_connection().
initialize_prechannels(#wh_amqp_connection{}=Connection, 0) ->
    Connection#wh_amqp_connection{prechannels_initialized='true'};
initialize_prechannels(#wh_amqp_connection{}=Connection, Count) ->
    case establish_prechannel(Connection) of
        #wh_amqp_connection{connection=Pid}=Success
            when is_pid(Pid) ->
            initialize_prechannels(Success, Count - 1);
        #wh_amqp_connection{}=Error ->
            Error#wh_amqp_connection{prechannels_initialized='false'}
    end.

-spec establish_prechannel(wh_amqp_connection()) -> wh_amqp_connection().
establish_prechannel(#wh_amqp_connection{broker=Broker
                                         ,manager=Manager
                                        }=Connection) ->
    case open_channel(Connection) of
        {'error', _R} ->
            lager:critical("unable to establish prechannel to ~s, assuming connection is invalid: ~p"
                           ,[Broker, _R]),
            disconnected(Connection);
        {'ok', Pid} ->
            wh_amqp_assignments:add_channel(Broker, Manager, Pid),
            Connection
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec open_channel(wh_amqp_connection()) -> {'ok', pid()} | {'error', _}.
open_channel(#wh_amqp_connection{connection=Pid}) ->
    try amqp_connection:open_channel(Pid) of
        {'ok', Channel}=Ok ->
            %% This is not strickly necessary, but since we
            %% lose the entire CONNECTION if a single message
            %% cant be delivered, better safe then sorry...
            amqp_selective_consumer:register_default_consumer(Channel, self()),
            Ok;
        'closing' ->
            lager:info("unable to open channel, connection is closing", []),
            {'error', 'closing'};
        {'error', _R}=E ->
            lager:critical("failed to open AMQP channel: ~p", [_R]),
            E
    catch
        _:{'noproc', {'gen_server', 'call', [P|_]}} ->
            lager:warning("amqp connection ~p is no longer valid...", [P]),
            {'error', 'not_connected'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges(wh_amqp_connection()) -> wh_amqp_connection().
declare_exchanges(#wh_amqp_connection{tags=Tags}=Connection) ->
    maybe_add_all_exchanges(Connection, lists:member(?AMQP_HIDDEN_TAG, Tags)).

maybe_add_all_exchanges(Connection, 'false') ->
    declare_exchanges(Connection, wh_amqp_history:list_exchanges());
maybe_add_all_exchanges(Connection, 'true') ->
    Connection#wh_amqp_connection{exchanges_initialized='true'}.

-spec declare_exchanges(wh_amqp_connection(), wh_amqp_exchanges()) -> wh_amqp_connection().
declare_exchanges(#wh_amqp_connection{}=Connection, []) ->
    Connection#wh_amqp_connection{exchanges_initialized='true'};
declare_exchanges(#wh_amqp_connection{channel=Channel
                                      ,broker=_Broker}=Connection
                  ,[Exchange|Exchanges])
  when is_pid(Channel) ->
    try amqp_channel:call(Channel, Exchange) of
        #'exchange.declare_ok'{} ->
            lager:debug("declared ~s exchange ~s on ~s via ~p"
                        ,[Exchange#'exchange.declare'.type
                          ,Exchange#'exchange.declare'.exchange
                          ,_Broker
                          ,Channel
                         ]),
            declare_exchanges(Connection, Exchanges);
        _Else ->
            lager:critical("failed to declare ~s exchange ~s on ~s via ~p: ~p"
                           ,[Exchange#'exchange.declare'.type
                             ,Exchange#'exchange.declare'.exchange
                             ,_Broker
                             ,Channel
                             ,_Else
                            ]),
            declare_exchanges(create_control_channel(Connection)
                              ,[Exchange|Exchanges])
    catch
        _E:_R ->
            lager:critical("exception while declaring ~s exchange ~s on ~s via ~p: ~p"
                           ,[Exchange#'exchange.declare'.type
                             ,Exchange#'exchange.declare'.exchange
                             ,_Broker
                             ,Channel
                             ,_R
                            ]),
            declare_exchanges(create_control_channel(Connection)
                              ,[Exchange|Exchanges])
    end;
declare_exchanges(#wh_amqp_connection{}=Connection, _) ->
    disconnected(Connection#wh_amqp_connection{exchanges_initialized='false'}).
