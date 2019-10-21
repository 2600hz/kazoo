%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle a host's connection/channels.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_connection).
-behaviour(gen_server).

-export([start_link/1]).
-export([get_connection/1]).
-export([declare_exchange/2, declare_exchange/3]).
-export([exchange_bind/2, exchange_bind/3]).
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

-define(START_TIMEOUT, 1500).
-define(MAX_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).
-define(MAX_REMOTE_TIMEOUT, ?MILLISECONDS_IN_MINUTE).

-define(SERVER_REPLY_NOT_FOUND, {shutdown,{server_initiated_close,404,_}}).

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

-spec declare_exchange(pid(), kz_amqp_exchange()) -> 'ok' | {'error', any()}.
declare_exchange(Srv, Exchange) ->
    gen_server:call(Srv, {'declare_exchange', Exchange}).

-spec declare_exchange(pid(), pid(), kz_amqp_exchange()) -> 'ok' | {'error', any()}.
declare_exchange(Srv, Channel, Exchange) ->
    gen_server:call(Srv, {'declare_exchange', Channel, Exchange}).

-spec exchange_bind(pid(), kz_amqp_exchange_binding()) -> 'ok' | {'error', any()}.
exchange_bind(Srv, Binding) ->
    gen_server:call(Srv, {'exchange_bind', Binding}).

-spec exchange_bind(pid(), pid(), kz_amqp_exchange_binding()) -> 'ok' | {'error', any()}.
exchange_bind(Srv, Channel, Binding) ->
    gen_server:call(Srv, {'exchange_bind', Channel, Binding}).

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
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),

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
handle_call({'declare_exchange', _Exchange}
           ,_From
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'reply', {'error', 'connection_not_available'}, Connection};
handle_call({'declare_exchange', Exchange}
           ,_From
           ,#kz_amqp_connection{available='true'}=Connection
           ) ->
    case amqp_declare_exchange(Connection, Exchange) of
        #kz_amqp_connection{}=C -> {'reply', 'ok', C};
        Else -> {'reply', Else, Connection}
    end;
handle_call({'declare_exchange', Channel, Exchange}
           ,_From
           ,Connection
           ) ->
    case amqp_declare_exchange(Connection, Channel, Exchange) of
        #kz_amqp_connection{}=C -> {'reply', 'ok', C};
        Else -> {'reply', Else, Connection}
    end;
handle_call({'exchange_bind', _Binding}
           ,_From
           ,#kz_amqp_connection{available='false'}=Connection
           ) ->
    {'reply', {'error', 'connection_not_available'}, Connection};
handle_call({'exchange_bind', Binding}
           ,_From
           ,#kz_amqp_connection{available='true'}=Connection
           ) ->
    case amqp_exchange_bind(Connection, Binding) of
        #kz_amqp_connection{}=C -> {'reply', 'ok', C};
        Else -> {'reply', Else, Connection}
    end;
handle_call({'exchange_bind', Channel, Binding}
           ,_From
           ,Connection
           ) ->
    case amqp_exchange_bind(Connection, Channel, Binding) of
        #kz_amqp_connection{}=C -> {'reply', 'ok', C};
        Else -> {'reply', Else, Connection}
    end;
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
    _ = kz_process:spawn(fun establish_prechannel/1, [Connection]),
    {'noreply', Connection, 'hibernate'};
handle_cast(_Msg, Connection) ->
    lager:debug("unhandled cast : ~p : ~p", [_Msg, Connection]),
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
handle_info({'DOWN', Ref, 'process', _Pid, ?SERVER_REPLY_NOT_FOUND}
           ,#kz_amqp_connection{available='true'
                               ,channel_ref=Ref
                               ,broker=_Broker
                               }=Connection
           ) ->
    lager:debug("command channel to ~s died with server not_found",[_Broker]),
    {'noreply', create_control_channel(Connection), 'hibernate'};
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
    lager:warning("connection ~p is being blocked on the server, check broker health: ~s"
                 ,[_Broker, _Reason]
                 ),
    {'noreply', Connection};
handle_info(#'connection.unblocked'{}, #kz_amqp_connection{broker=_Broker}=Connection) ->
    lager:notice("connection ~p is unblocked on the broker", [_Broker]),
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
connected(#kz_amqp_connection{available='false'}=Connection) ->
    case amqp_declare_exchanges(Connection#kz_amqp_connection{available='true'}) of
        {'error', _} -> disconnected(Connection);
        #kz_amqp_connection{available='true'}= C ->
            _ = kz_amqp_connections:available(self()),
            connected(C)
    end;
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
disconnected(#kz_amqp_connection{}=Connection, Timeout) ->
    MaxTimeout = zone_timeout(Connection),
    NextTimeout = next_timeout(Timeout, MaxTimeout),

    Ref = erlang:send_after(Timeout, self(), {'connect', NextTimeout}),
    lager:debug("reconnecting after ~p in ~p", [Timeout, Ref]),
    Connection#kz_amqp_connection{reconnect_ref=Ref}.

shutdown_available('true') -> kz_amqp_connections:unavailable(self()).

demonitor_refs([]) -> 'ok';
demonitor_refs([Ref|Refs]) when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    lager:debug("unmonitored channel ref ~p", [Ref]),
    demonitor_refs(Refs).

shutdown_channel(ChannelPid) when is_pid(ChannelPid) ->
    try kz_amqp_channel:close(ChannelPid) of
        _Closed -> lager:debug("closed channel ~p: ~p", [ChannelPid, _Closed])
    catch _E:_R:_ST -> lager:debug("closing channel ~p failed: ~s: ~p", [ChannelPid, _E, _R])
    end.

shutdown_connection(ConnectionPid) when is_pid(ConnectionPid) ->
    lager:debug("shutting down connection PID ~p", [ConnectionPid]),
    try amqp_connection:close(ConnectionPid, 5 * ?MILLISECONDS_IN_SECOND) of
        _Closed -> lager:debug("closed connection ~p: ~p", [ConnectionPid, _Closed])
    catch _E:_R:_ST -> lager:debug("closing connection ~p failed: ~s: ~p", [ConnectionPid, _E, _R])
    end.

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
        _E:_R:_ST ->
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
        _:{'noproc', {'gen_server', 'call', [P|_]}}:_ST ->
            lager:warning("amqp connection ~p is no longer valid...", [P]),
            {'error', 'not_connected'};
        _Exc:_Err:_ST ->
            lager:warning("amqp exception opening channel : ~p , ~p", [_Exc, _Err]),
            {'error', 'not_connected'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec amqp_declare_exchanges(kz_amqp_connection()) -> kz_amqp_connection() | {'error', any()}.
amqp_declare_exchanges(#kz_amqp_connection{exchanges=Exchanges}=Connection) ->
    Filtered = maps:filter(fun(_K, Ex) -> not Ex#'exchange.declare'.passive end, Exchanges),
    amqp_declare_exchanges_fold(maps:values(Filtered), Connection#kz_amqp_connection{exchanges=#{}}).

-spec amqp_declare_exchanges_fold(kz_amqp_exchanges(), kz_amqp_connection()) -> kz_amqp_connection() | {'error', any()}.
amqp_declare_exchanges_fold([], Connection) -> Connection;
amqp_declare_exchanges_fold([Exchange | Exchanges]
                           ,#kz_amqp_connection{channel=Channel}=Connection
                           ) ->
    case amqp_declare_exchange(Connection, Channel, Exchange) of
        {'error', _} = Error -> Error;
        'ok' -> amqp_declare_exchanges_fold(Exchanges, Connection);
        #kz_amqp_connection{}=C -> amqp_declare_exchanges_fold(Exchanges, C)
    end.

-spec amqp_declare_exchange(kz_amqp_connection(), kz_amqp_exchange()) -> kz_amqp_connection() | 'ok' | {'error', any()}.
amqp_declare_exchange(#kz_amqp_connection{channel=Channel}=Connection
                     ,Exchange
                     ) ->
    amqp_declare_exchange(Connection, Channel, Exchange).

-spec amqp_declare_exchange(kz_amqp_connection(), pid(), kz_amqp_exchange()) -> kz_amqp_connection() | 'ok' | {'error', any()}.
amqp_declare_exchange(#kz_amqp_connection{broker=_Broker
                                         ,exchanges=ExchangeMap
                                         ,bindings=BindingsMap
                                         }=Connection
                     ,Channel
                     ,Exchange
                     )
  when is_pid(Channel) ->
    ExchangeName = Exchange#'exchange.declare'.exchange,
    try not maps:is_key(ExchangeName, ExchangeMap)
             andalso amqp_channel:call(Channel, Exchange)
    of
        'false' -> 'ok';
        #'exchange.declare_ok'{} ->
            lager:debug("declared ~s exchange ~s on ~s via ~p"
                       ,[Exchange#'exchange.declare'.type
                        ,ExchangeName
                        ,_Broker
                        ,Channel
                        ]),
            Bindings = maps:get(ExchangeName, BindingsMap, []),
            C = Connection#kz_amqp_connection{exchanges=ExchangeMap#{ExchangeName => Exchange}
                                             ,bindings=maps:without([ExchangeName], BindingsMap)
                                             },
            amqp_exchange_bindings(C, Bindings);
        _Else when Exchange#'exchange.declare'.passive ->
            lager:debug("failed to declare ~s passive exchange ~s on ~s via ~p: ~p"
                       ,[Exchange#'exchange.declare'.type
                        ,Exchange#'exchange.declare'.exchange
                        ,_Broker
                        ,Channel
                        ,_Else
                        ]),
            {'error', 'not_found'};
        _Else ->
            lager:critical("failed to declare ~s exchange ~s on ~s via ~p: ~p"
                          ,[Exchange#'exchange.declare'.type
                           ,Exchange#'exchange.declare'.exchange
                           ,_Broker
                           ,Channel
                           ,_Else
                           ]),
            {'error', _Else}
    catch
        _E:{?SERVER_REPLY_NOT_FOUND, _}:_ST
          when Exchange#'exchange.declare'.passive ->
            {'error', 'not_found'};
        _E:_R:_ST ->
            lager:critical("exception while declaring ~s exchange ~s on ~s via ~p: ~p"
                          ,[Exchange#'exchange.declare'.type
                           ,Exchange#'exchange.declare'.exchange
                           ,_Broker
                           ,Channel
                           ,_R
                           ]),
            {'error', _R}
    end;
amqp_declare_exchange(#kz_amqp_connection{}, _, _) ->
    {'error', 'no_channel_for_command'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec amqp_exchange_bindings(kz_amqp_connection(), kz_amqp_exchange_bindings()) -> kz_amqp_connection() | {'error', any()}.
amqp_exchange_bindings(Connection, []) -> Connection;
amqp_exchange_bindings(#kz_amqp_connection{channel=Channel}=Connection
                      ,[Binding | Bindings]
                      ) ->
    case amqp_exchange_bind(Connection, Channel, Binding) of
        {'error', _} = Error -> Error;
        'ok' -> amqp_exchange_bindings(Connection, Bindings);
        #kz_amqp_connection{}=C -> amqp_exchange_bindings(C, Bindings)
    end.

-spec amqp_exchange_bind(kz_amqp_connection(), kz_amqp_exchange_binding()) -> kz_amqp_connection() | 'ok' | {'error', any()}.
amqp_exchange_bind(#kz_amqp_connection{channel=Channel}=Connection
                  ,Binding
                  ) ->
    amqp_exchange_bind(Connection, Channel, Binding).

-spec amqp_exchange_bind(kz_amqp_connection(), pid(), kz_amqp_exchange_binding()) -> kz_amqp_connection() | 'ok' | {'error', any()}.
amqp_exchange_bind(#kz_amqp_connection{broker=_Broker
                                      ,bindings=BindingsMap
                                      }=Connection
                  ,Channel
                  ,Binding
                  )
  when is_pid(Channel) ->
    try amqp_channel:call(Channel, Binding) of
        'false' -> 'ok';
        #'exchange.bind_ok'{} ->
            lager:debug("exchange ~s binding from ~s : ~s on ~s via ~p"
                       ,[Binding#'exchange.bind'.destination
                        ,Binding#'exchange.bind'.source
                        ,Binding#'exchange.bind'.routing_key
                        ,_Broker
                        ,Channel
                        ]),
            ExchangeName = Binding#'exchange.bind'.destination,
            Bindings = maps:get(ExchangeName, BindingsMap, []) ++ [Binding],
            Connection#kz_amqp_connection{bindings=BindingsMap#{ExchangeName => Bindings}};
        _Else ->
            lager:critical("failed to bind ~s exchange from ~s : ~s on ~s via ~p: ~p"
                          ,[Binding#'exchange.bind'.destination
                           ,Binding#'exchange.bind'.source
                           ,Binding#'exchange.bind'.routing_key
                           ,_Broker
                           ,Channel
                           ,_Else
                           ]),
            {'error', _Else}
    catch
        _E:_R:_ST ->
            lager:critical("exception while binding ~s exchange from ~s : ~s on ~s via ~p: ~p"
                          ,[Binding#'exchange.bind'.destination
                           ,Binding#'exchange.bind'.source
                           ,Binding#'exchange.bind'.routing_key
                           ,_Broker
                           ,Channel
                           ,_R
                           ]),
            {'error', _R}
    end;
amqp_exchange_bind(#kz_amqp_connection{}, _, _) ->
    {'error', 'no_channel_for_command'}.
