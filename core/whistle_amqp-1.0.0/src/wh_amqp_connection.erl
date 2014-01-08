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
-export([get_connection/1]).
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
-define(MAX_TIMEOUT, 1000).

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

get_connection(Srv) ->
    gen_server:call(Srv, 'get_connection').

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
    put('callid', ?LOG_SYSTEM_ID),
    self() ! {'connect', ?START_TIMEOUT},
    {'ok', Connection}.

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
handle_call('get_connection', _, State) ->
    {'reply', State, State};
handle_call('stop', _, State) ->
    {'stop', 'normal', 'ok', State};
handle_call(_Msg, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast({'wh_amqp_channel', 'channel_assigned'}
            ,#wh_amqp_connection{available='true'}=State) ->
    _ = start_prechannel(State),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

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
handle_info({'DOWN', Ref, 'process', _Pid, _Reason}
            ,#wh_amqp_connection{connection_ref=Ref
                                ,broker=_Broker}=State) ->
    lager:critical("connection to the AMQP broker ~s died: ~p"
                   ,[_Broker, _Reason]),
    self() ! {'connect', ?START_TIMEOUT},
    {'noreply', disconnected(State), 'hibernate'};
handle_info({'connect', Timeout}, #wh_amqp_connection{broker=_Broker
                                                      ,params=Params}=State) ->
    case amqp_connection:start(Params) of
        {'error', 'auth_failure'} ->
            lager:warning("amqp authentication failure with '~s', will retry in ~p"
                          ,[_Broker, Timeout]),
            _Ref = erlang:send_after(Timeout, self(), {'connect', next_timeout(Timeout)}),
            {'noreply', State, 'hibernate'};
        {'error', _Reason} ->
            lager:warning("failed to connect to '~s' will retry in ~p: ~p"
                          ,[_Broker, Timeout, _Reason]),
            _Ref = erlang:send_after(Timeout, self(), {'connect', next_timeout(Timeout)}),
            {'noreply', State, 'hibernate'};
        {'ok', Connection} ->
            lager:notice("connected successfully to '~s'", [_Broker]),
            Ref = erlang:monitor('process', Connection),
            S = State#wh_amqp_connection{connection=Connection
                                         ,connection_ref=Ref
                                         ,available='true'},
            {'noreply', connected(S), 'hibernate'}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, 'hibernate'}.

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
terminate(_Reason, #wh_amqp_connection{broker=_Broker}=State) ->
    lager:debug("connection to amqp broker '~s' terminated: ~p"
                ,[_Broker, _Reason]),
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
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec connected(wh_amqp_connection()) -> wh_amqp_connection().
connected(#wh_amqp_connection{broker=Broker}=State) ->
    wh_amqp_assignments:add_broker(Broker),
    _ = [start_prechannel(State)
         || _ <- lists:seq(1, 10)
        ],
    State.

-spec disconnected(wh_amqp_connection()) -> wh_amqp_connection().
disconnected(#wh_amqp_connection{connection_ref=Ref}=State) when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    disconnected(State#wh_amqp_connection{connection_ref='undefined'});
disconnected(#wh_amqp_connection{broker=Broker}=State) ->
    wh_amqp_assignments:remove_broker(Broker),    
    State#wh_amqp_connection{connection='undefined', available='false'}.

-spec next_timeout(pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT=Timeout) ->
    Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.

-spec start_prechannel(wh_amqp_connection()) -> 'ok'.
start_prechannel(#wh_amqp_connection{broker=Broker}=Connection) ->
    case open_channel(Connection) of
        {'ok', Channel} ->
            wh_amqp_assignments:new_channel(Broker, self(), Channel);
        {'error', _} -> 'ok'
    end.

-spec open_channel(wh_amqp_connection()) -> {'ok', pid()} | {'error', _}.
open_channel(#wh_amqp_connection{connection=Connection}) ->
    try amqp_connection:open_channel(Connection) of
        {'ok', Channel}=Ok ->
            %% This is not strickly necessary, but since we
            %% loose the entire CONNECTION if a single message
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
            %% TODO: send notice to self...
            lager:warning("amqp connection ~p is no longer valid...", [P]),
            {'error', 'not_connected'}
    end.
