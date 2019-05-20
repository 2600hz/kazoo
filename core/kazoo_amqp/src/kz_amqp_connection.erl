%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle a host's connection/channels.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_connection).
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

-export([broker/1]).

-include("kz_amqp_util.hrl").
-type state() :: kz_amqp_connection().

-define(SERVER, ?MODULE).

-define(START_TIMEOUT, 100).
-define(MAX_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).
-define(MAX_REMOTE_TIMEOUT, ?MILLISECONDS_IN_MINUTE).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_amqp_connection()) -> kz_types:startlink_ret().
start_link(#kz_amqp_connection{}=Connection) ->
    gen_server:start_link(?SERVER, [Connection], []).

-spec get_connection(pid()) -> kz_amqp_connection().
get_connection(Srv) ->
    gen_server:call(Srv, 'get_connection').

-spec new_exchange(pid(), kz_amqp_exchange()) -> 'ok'.
new_exchange(Srv, Exchange) ->
    gen_server:call(Srv, {'new_exchange', Exchange}).

-spec create_prechannel(pid()) -> 'ok'.
create_prechannel(Srv) ->
    gen_server:cast(Srv, 'create_prechannel').

-spec disconnect(pid()) -> 'ok'.
disconnect(Srv) ->
    gen_server:cast(Srv, 'disconnect').

-spec broker(kz_amqp_connections()) -> kz_term:ne_binary().
broker(#kz_amqp_connections{broker=Broker}) ->
    Broker.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', kz_amqp_connection()}.
init([#kz_amqp_connection{}=Connection]) ->
    _ = process_flag('trap_exit', 'true'),
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),

    {'ok', disconnected(Connection#kz_amqp_connection{manager=self()})}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('get_connection', _, Connection) ->
    {'reply', Connection, Connection};
handle_call('stop', _, Connection) ->
    {'stop', 'normal', 'ok', disconnected(Connection)};
handle_call({'new_exchange', _}
           ,_From
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'reply', 'ok', Connection};
handle_call({'new_exchange', Exchange}
           ,_From
           ,#kz_amqp_connection{available='true'}=Connection
           ) ->
    _ = declare_exchanges(Connection, [Exchange]),
    {'reply', 'ok', Connection};
handle_call(_Msg, _From, Connection) ->
    {'reply', {'error', 'not_implemented'}, Connection}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('disconnect'
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'noreply', Connection, 'hibernate'};
handle_cast('disconnect'
           ,#kz_amqp_connection{available='true'}=Connection
           ) ->
    {'noreply', disconnected(Connection), 'hibernate'};
handle_cast('create_control_channel'
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'noreply', Connection, 'hibernate'};
handle_cast('create_control_channel'
           ,#kz_amqp_connection{available='true'}=Connection
           ) ->
    {'noreply', create_control_channel(Connection), 'hibernate'};
handle_cast('create_prechannel'
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'noreply', Connection};
handle_cast('create_prechannel'
           ,#kz_amqp_connection{available='true'}=Connection
           ) ->
    _ = kz_util:spawn(fun establish_prechannel/1, [Connection]),
    {'noreply', Connection, 'hibernate'};
handle_cast({'new_exchange', _}
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'noreply', Connection, 'hibernate'};
handle_cast({'new_exchange', Exchange}
           ,#kz_amqp_connection{available='true'}=Connection
           ) ->
    {'noreply', declare_exchanges(Connection, [Exchange]), 'hibernate'};
handle_cast(_Msg, Connection) ->
    {'noreply', Connection}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', _Ref, 'process', _Pid, _Reason}
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'noreply', Connection, 'hibernate'};
handle_info({'DOWN', Ref, 'process', _Pid, _Reason}
           ,#kz_amqp_connection{available='true'
                               ,channel_ref=Ref
                               ,broker=_Broker
                               }=Connection
           ) ->
    lager:warning("command channel to the AMQP broker ~s died: ~p"
                 ,[_Broker, _Reason]
                 ),
    {'noreply', create_control_channel(Connection), 'hibernate'};
handle_info({'DOWN', Ref, 'process', _Pid, _Reason}
           ,#kz_amqp_connection{available='true'
                               ,connection_ref=Ref
                               ,broker=_Broker
                               }=Connection
           ) ->
    lager:critical("connection to the AMQP broker ~s died: ~p"
                  ,[_Broker, _Reason]
                  ),
    {'noreply', disconnected(Connection), 'hibernate'};
handle_info({'connect', Timeout}
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'noreply', maybe_connect(Connection, Timeout), 'hibernate'};
handle_info({'connect', _}, #kz_amqp_connection{available='true'}=Connection) ->
    {'noreply', Connection, 'hibernate'};
handle_info(#'basic.cancel_ok'{}=_Cancel, Connection) ->
    {'noreply', Connection};
handle_info(#'connection.blocked'{reason=_Reason}, #kz_amqp_connection{broker=_Broker}=Connection) ->
    lager:warning("conneciton ~p is being blocked on the server, check broker health: ~s"
                 ,[_Broker, _Reason]
                 ),
    {'noreply', Connection};
handle_info(#'connection.unblocked'{}, #kz_amqp_connection{broker=_Broker}=Connection) ->
    lager:notice("conneciton ~p is unblocked on the broker", [_Broker]),
    {'noreply', Connection};
handle_info(_Info, Connection) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', Connection, 'hibernate'}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), kz_amqp_connection()) -> any().
terminate('shutdown', #kz_amqp_connection{broker=_Broker}=Connection) ->
    shutdown(Connection),
    lager:debug("connection to amqp broker '~s' shutdown", [_Broker]);
terminate(_Reason, #kz_amqp_connection{broker=_Broker}=Connection) ->
    lager:debug("connection to amqp broker '~s' terminated: ~p"
               ,[_Broker, _Reason]
               ),
    disconnected(Connection).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, Connection, _Extra) ->
    {'ok', Connection}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec connected(kz_amqp_connection()) -> kz_amqp_connection().
connected(#kz_amqp_connection{reconnect_ref=Ref}=Connection)
  when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    connected(Connection#kz_amqp_connection{reconnect_ref='undefined'});
connected(#kz_amqp_connection{channel='undefined'}=Connection) ->
    case create_control_channel(Connection) of
        #kz_amqp_connection{channel=Pid}=Success
          when is_pid(Pid) -> connected(Success);
        #kz_amqp_connection{}=Error -> Error
    end;
connected(#kz_amqp_connection{exchanges_initialized='false'}=Connection) ->
    Success = declare_exchanges(Connection),
    connected(Success);
connected(#kz_amqp_connection{available='false'}=Connection) ->
    _ = kz_amqp_connections:available(self()),
    connected(Connection#kz_amqp_connection{available='true'});
connected(#kz_amqp_connection{prechannels_initialized='false'}=Connection) ->
    case initialize_prechannels(Connection) of
        #kz_amqp_connection{prechannels_initialized='false'}=Error -> Error;
        #kz_amqp_connection{prechannels_initialized='true'}=Success ->
            connected(Success)
    end;
connected(#kz_amqp_connection{broker=_Broker}=Connection) ->
    lager:info("successfully connected to '~s'", [_Broker]),
    Connection.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec disconnected(kz_amqp_connection()) -> kz_amqp_connection().
disconnected(#kz_amqp_connection{manager=Manager}=State) ->
    case Manager =:= self() of
        'true' -> disconnected(State, ?START_TIMEOUT);
        'false' -> disconnect(Manager)
    end.

-spec disconnected(kz_amqp_connection(), ?START_TIMEOUT..?MAX_TIMEOUT) -> kz_amqp_connection().
disconnected(#kz_amqp_connection{available='true'}=Connection, Timeout) ->
    shutdown_available('true'),
    disconnected(Connection#kz_amqp_connection{available='false'}, Timeout);
disconnected(#kz_amqp_connection{channel_ref=Ref}=Connection, Timeout)
  when is_reference(Ref) ->
    demonitor_refs([Ref]),
    disconnected(Connection#kz_amqp_connection{channel_ref='undefined'}, Timeout);
disconnected(#kz_amqp_connection{channel=Pid}=Connection, Timeout)
  when is_pid(Pid) ->
    shutdown_channel(Pid),
    disconnected(Connection#kz_amqp_connection{channel='undefined'}, Timeout);
disconnected(#kz_amqp_connection{connection_ref=Ref}=Connection, Timeout)
  when is_reference(Ref) ->
    demonitor_refs([Ref]),
    disconnected(Connection#kz_amqp_connection{connection_ref='undefined'}, Timeout);
disconnected(#kz_amqp_connection{connection=Pid}=Connection, Timeout)
  when is_pid(Pid) ->
    shutdown_connection(Pid),
    disconnected(Connection#kz_amqp_connection{connection='undefined'}, Timeout);
disconnected(#kz_amqp_connection{prechannels_initialized='true'}=Connection, Timeout) ->
    disconnected(Connection#kz_amqp_connection{prechannels_initialized='false'}, Timeout);
disconnected(#kz_amqp_connection{exchanges_initialized='true'}=Connection, Timeout) ->
    disconnected(Connection#kz_amqp_connection{exchanges_initialized='false'}, Timeout);
disconnected(#kz_amqp_connection{}=Connection, Timeout) ->
    MaxTimeout = zone_timeout(Connection),
    NextTimeout = next_timeout(Timeout, MaxTimeout),

    Ref = erlang:send_after(Timeout, self(), {'connect', NextTimeout}),
    lager:debug("reconnecting after ~p in ~p", [Timeout, Ref]),
    Connection#kz_amqp_connection{reconnect_ref=Ref}.

shutdown(#kz_amqp_connection{available=Available
                            ,channel_ref=ChannelRef
                            ,channel=ChannelPid
                            ,connection_ref=ConnectionRef
                            ,connection=ConnectionPid
                            }
        ) ->
    shutdown_available(Available),
    demonitor_refs([ChannelRef, ConnectionRef]),
    shutdown_channel(ChannelPid),
    shutdown_connection(ConnectionPid).

shutdown_available('true') -> kz_amqp_connections:unavailable(self());
shutdown_available('false') -> 'ok'.

demonitor_refs([]) -> 'ok';
demonitor_refs([Ref|Refs]) when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("unmonitored channel ref ~p", [Ref]),
    demonitor_refs(Refs);
demonitor_refs([_|Refs]) ->
    demonitor_refs(Refs).

shutdown_channel(ChannelPid) when is_pid(ChannelPid) ->
    try kz_amqp_channel:close(ChannelPid) of
        _Closed -> lager:debug("closed channel ~p: ~p", [ChannelPid, _Closed])
    catch _E:_R -> lager:debug("closing channel ~p failed: ~s: ~p", [ChannelPid, _E, _R])
    end;
shutdown_channel(_ChannelPid) -> lager:debug("shutdown channel is not a pid: ~p", [_ChannelPid]).

shutdown_connection(ConnectionPid) when is_pid(ConnectionPid) ->
    lager:debug("shutting down connection PID ~p", [ConnectionPid]),
    try amqp_connection:close(ConnectionPid, 5 * ?MILLISECONDS_IN_SECOND) of
        _Closed -> lager:debug("closed connection ~p: ~p", [ConnectionPid, _Closed])
    catch _E:_R -> lager:debug("closing connection ~p failed: ~s: ~p", [ConnectionPid, _E, _R])
    end;
shutdown_connection(_ConnectionPid) -> lager:debug("shutdown connection is not a pid: ~p", [_ConnectionPid]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec next_timeout(pos_integer(), pos_integer()) -> pos_integer().
next_timeout(MaxTimeout, MaxTimeout) -> MaxTimeout;
next_timeout(Timeout, MaxTimeout) when Timeout * 2 > MaxTimeout ->
    MaxTimeout;
next_timeout(Timeout, _MaxTimeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout, _MaxTimeout) ->
    Timeout * 2.

-spec zone_timeout(kz_amqp_connection()) -> pos_integer().
zone_timeout(#kz_amqp_connection{broker=Broker}) ->
    case kz_amqp_connections:broker_zone(Broker) of
        'local' -> ?MAX_TIMEOUT;
        _Zone ->   ?MAX_REMOTE_TIMEOUT
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_connect(kz_amqp_connection(), ?START_TIMEOUT..?MAX_TIMEOUT) -> kz_amqp_connection().
maybe_connect(#kz_amqp_connection{broker=_Broker
                                 ,available='false'
                                 ,params=Params
                                 }=Connection
             ,Timeout
             ) ->
    try amqp_connection:start(Params) of
        {'error', 'auth_failure'} ->
            lager:warning("amqp authentication failure with '~s', will retry"
                         ,[_Broker]
                         ),
            disconnected(Connection, Timeout);
        {'error', 'econnrefused'} ->
            lager:warning("connection refused to ~s (check that the broker is running)", [_Broker]),
            disconnected(Connection, Timeout);
        {'error', _Reason} ->
            lager:warning("failed to connect to '~s' will retry: ~p"
                         ,[_Broker, _Reason]
                         ),
            disconnected(Connection, Timeout);
        {'ok', Pid} ->
            Ref = erlang:monitor('process', Pid),
            _ = amqp_connection:register_blocked_handler(Pid, self()),
            connected(Connection#kz_amqp_connection{connection=Pid
                                                   ,connection_ref=Ref
                                                   });
        _E ->
            lager:critical("unhandled case on connect to '~s' will retry: ~p"
                          ,[_Broker, _E]
                          ),
            disconnected(Connection, Timeout)
    catch
        _E:_R ->
            lager:warning("exception connecting to '~s' will retry: ~s: ~p"
                         ,[_Broker, _E, _R]
                         ),
            disconnected(Connection, Timeout)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_control_channel(kz_amqp_connection()) -> kz_amqp_connection().
create_control_channel(#kz_amqp_connection{channel_ref=Ref}=Connection)
  when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    create_control_channel(Connection#kz_amqp_connection{channel_ref='undefined'});
create_control_channel(#kz_amqp_connection{channel=ChannelPid}=Connection)
  when is_pid(ChannelPid) ->
    shutdown_channel(ChannelPid),
    create_control_channel(Connection#kz_amqp_connection{channel='undefined'});
create_control_channel(#kz_amqp_connection{broker=Broker}=Connection) ->
    case open_channel(Connection) of
        {'error', _R} ->
            lager:critical("unable to establish command channel to ~s, assuming connection is invalid: ~p"
                          ,[Broker, _R]
                          ),
            disconnected(Connection);
        {'ok', Pid} ->
            lager:debug("created command channel ~p to ~s", [Pid, Broker]),
            Ref = erlang:monitor('process', Pid),
            Connection#kz_amqp_connection{channel=Pid
                                         ,channel_ref=Ref
                                         }
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec initialize_prechannels(kz_amqp_connection()) -> kz_amqp_connection().
initialize_prechannels(#kz_amqp_connection{}=Connection) ->
    initialize_prechannels(Connection, 10).

-spec initialize_prechannels(kz_amqp_connection(), non_neg_integer()) -> kz_amqp_connection().
initialize_prechannels(#kz_amqp_connection{}=Connection, 0) ->
    Connection#kz_amqp_connection{prechannels_initialized='true'};
initialize_prechannels(#kz_amqp_connection{}=Connection, Count) ->
    case establish_prechannel(Connection) of
        #kz_amqp_connection{connection=Pid}=Success
          when is_pid(Pid) ->
            initialize_prechannels(Success, Count - 1);
        #kz_amqp_connection{}=Error ->
            Error#kz_amqp_connection{prechannels_initialized='false'}
    end.

-spec establish_prechannel(kz_amqp_connection()) -> kz_amqp_connection().
establish_prechannel(#kz_amqp_connection{broker=Broker
                                        ,manager=Manager
                                        }=Connection
                    ) ->
    case open_channel(Connection) of
        {'error', _R} ->
            lager:critical("unable to establish prechannel to ~s, assuming connection is invalid: ~p"
                          ,[Broker, _R]
                          ),
            disconnected(Connection);
        {'ok', Pid} ->
            kz_amqp_assignments:add_channel(Broker, Manager, Pid),
            Connection
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec open_channel(kz_amqp_connection()) -> {'ok', pid()} | {'error', any()}.
open_channel(#kz_amqp_connection{connection=Pid}) ->
    try amqp_connection:open_channel(Pid) of
        {'ok', Channel}=Ok ->
            %% This is not strictly necessary, but since we
            %% lose the entire CONNECTION if a single message
            %% can't be delivered, better safe then sorry...
            amqp_selective_consumer:register_default_consumer(Channel, self()),
            Ok;
        'closing' ->
            lager:info("unable to open channel, connection is closing", []),
            {'error', 'closing'};
        {'error', _R}=E ->
            lager:critical("failed to open AMQP channel: ~p", [_R]),
            E;
        E ->
            lager:critical("unhandled failure on open AMQP channel: ~p", [E]),
            {'error', E}
    catch
        _:{'noproc', {'gen_server', 'call', [P|_]}} ->
            lager:warning("amqp connection ~p is no longer valid...", [P]),
            {'error', 'not_connected'};
        _Exc:_Err ->
            lager:warning("amqp exception opening channel : ~p , ~p", [_Exc, _Err]),
            {'error', 'not_connected'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges(kz_amqp_connection()) -> kz_amqp_connection().
declare_exchanges(#kz_amqp_connection{tags=Tags}=Connection) ->
    maybe_add_all_exchanges(Connection, lists:member(?AMQP_HIDDEN_TAG, Tags)).

maybe_add_all_exchanges(Connection, 'false') ->
    declare_default_exchanges(),
    Connection#kz_amqp_connection{exchanges_initialized='true'};
maybe_add_all_exchanges(Connection, 'true') ->
    Connection#kz_amqp_connection{exchanges_initialized='true'}.

-spec declare_default_exchanges() -> 'ok'.
declare_default_exchanges() ->
    _ = [declare_default_exchange(M)
         || {M, _} <- code:all_loaded(),
            re:run(atom_to_list(M), "^kapi_") =/= 'nomatch',
            kz_module:is_exported(M, 'declare_exchanges', 0)
        ],
    'ok'.

declare_default_exchange(M) ->
    spawn(M, 'declare_exchanges', []).

-spec declare_exchanges(kz_amqp_connection(), kz_amqp_exchanges()) -> kz_amqp_connection().
declare_exchanges(#kz_amqp_connection{}=Connection, []) ->
    Connection#kz_amqp_connection{exchanges_initialized='true'};
declare_exchanges(#kz_amqp_connection{channel=Channel
                                     ,broker=_Broker
                                     }=Connection
                 ,[Exchange|Exchanges]
                 )
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
                             ,[Exchange|Exchanges]
                             )
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
                             ,[Exchange|Exchanges]
                             )
    end;
declare_exchanges(#kz_amqp_connection{}=Connection, _) ->
    disconnected(Connection#kz_amqp_connection{exchanges_initialized='false'}).
