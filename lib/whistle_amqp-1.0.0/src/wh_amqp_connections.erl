%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_connections).

-behaviour(gen_server).

-include("amqp_util.hrl").

-export([start_link/0]).
-export([add/1]).
-export([remove/1]).
-export([current/0]).
-export([find/1]).
-export([available/0]).
-export([is_available/0]).
-export([exchanges/0
         ,exchanges/1
        ]).
-export([wait_for_available/0]).
-export([update_exchanges/2]).
-export([connected/1]).
-export([disconnected/1]).
-export([declare_exchange/1]).
-export([redeclare_exchanges/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {weight=0
                ,exchanges=dict:new()
               }).

-define(TAB, ?MODULE).
-define(ENSURE_TIME, 5000).

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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(text()) -> wh_amqp_connection() | {'error', _}.
add(URI) when not is_binary(URI) ->
    add(wh_util:to_binary(URI));
add(URI) ->
    case catch amqp_uri:parse(wh_util:to_list(URI)) of
        {'EXIT', _R} ->
            lager:error("failed to parse amqp URI '~s': ~p", [URI, _R]),
            {error, invalid_uri};
        {error, {Info, _}} ->
            lager:error("failed to parse amqp URI '~s': ~p", [URI, Info]),
            {error, invalid_uri};
        {ok, Params} ->
            new(#wh_amqp_connection{uri=URI
                                    ,manager=wh_util:to_atom(URI, true)
                                    ,params=Params
                                    ,weight=gen_server:call(?MODULE, assign_weight)})
    end.

-spec new(wh_amqp_connection()) -> wh_amqp_connection() | {'error', _}.
new(#wh_amqp_connection{uri=URI}=Connection) ->
    case ets:member(?TAB, URI) of
        true ->
            lager:error("connection to '~s' already exists", [URI]);
        false ->
            case wh_amqp_connection_sup:add(Connection) of
                {ok, _} ->
                    gen_server:call(?MODULE, {new, Connection});
                {error, {already_started, _}} ->
                    gen_server:call(?MODULE, {new, Connection});
                {error, already_present} ->
                    _ = wh_amqp_connection_sup:remove(Connection),
                    new(Connection);
                {error, Reason} ->
                    lager:warning("unable to start amqp connection to '~s': ~p", [URI, Reason]),
                    {error, Reason}
            end
    end.

-spec remove(text()) -> 'ok'.
remove(URI) when not is_binary(URI) ->
    remove(wh_util:to_binary(URI));
remove(URI) ->
    case ets:lookup(?TAB, URI) of
        [] -> ok;
        [#wh_amqp_connection{}=Connection] ->
            _ = wh_amqp_channels:lost_connection(Connection),
            _ = wh_amqp_connection_sup:remove(Connection),
            ok
    end.

-spec current() -> {'ok', wh_amqp_connection()} |
                   {'error', 'no_available_connection'}.
current() ->
    Match = #wh_amqp_connection{available=true, _='_'},
    case ets:match_object(?TAB, Match) of
        [Connection|_] -> {ok, Connection};
        _Else ->
            {error, no_available_connection}
    end.

-spec available() -> wh_amqp_connections().
available() ->
    Match = #wh_amqp_connection{available=true, _='_'},
    ets:match_object(?TAB, Match).

-spec find(ne_binary() | string()) -> wh_amqp_connection() |
                                      {'error', 'not_found'}.
find(URI) ->
    case ets:lookup(?TAB, wh_util:to_binary(URI)) of
        [#wh_amqp_connection{}=Connection|_] ->
            Connection;
        _Else ->
            {error, not_found}
    end.

-spec is_available() -> boolean().
is_available() ->
    case current() of
        {ok, _} -> true;
        {error, _} -> false
    end.

-spec wait_for_available/0 :: () -> 'ok'.
wait_for_available() ->
    case is_available() of
        true -> ok;
        false ->
            timer:sleep(random:uniform(1000) + 100),
            wait_for_available()
    end.

-spec exchanges() -> [#'exchange.declare'{},...] | [].
exchanges() ->
    gen_server:call(?MODULE, exchanges).

-spec exchanges(ne_binary()) -> [#'exchange.declare'{},...] | [].
exchanges(URI) ->
    case find(URI) of
        #wh_amqp_connection{exchanges=Exchanges} ->
            Exchanges;
        {error, not_found} -> []
    end.


-spec update_exchanges(string() | ne_binary(), wh_exchanges()) -> 'ok'.
update_exchanges(URI, Exchanges) ->
    gen_server:cast(?MODULE, {update_exchanges, wh_util:to_binary(URI), Exchanges}).

-spec connected(wh_amqp_connection()) -> wh_amqp_connection().
connected(#wh_amqp_connection{connection=Pid, weight=Weight}=Connection) when is_pid(Pid) ->
    CurrentWeight = current_weight(),
    C = gen_server:call(?MODULE, {connected, Connection}),
    _ = case Weight > CurrentWeight of
            'true' -> gen_server:cast(?MODULE, {force_reconnect, Connection});
            'false' -> 'ok'
        end,
    C.

-spec disconnected(wh_amqp_connection()) -> wh_amqp_connection().
disconnected(#wh_amqp_connection{}=Connection) ->
    _ = wh_amqp_channels:lost_connection(Connection),
    gen_server:call(?MODULE, {disconnected, Connection}).

-spec declare_exchange(#'exchange.declare'{}) -> ok.
declare_exchange(#'exchange.declare'{}=Command) ->
    gen_server:cast(?MODULE, {add_exchange, Command}),
    declare_exchange(available(), Command).

-spec redeclare_exchanges(wh_amqp_connection()) -> ok.
redeclare_exchanges(#wh_amqp_connection{}=Connection) ->
    redeclare_exchanges(exchanges(), Connection).

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
init([]) ->
    put(callid, ?LOG_SYSTEM_ID),
    _ = ets:new(?TAB, [named_table, {keypos, #wh_amqp_connection.uri}]),
    {ok, #state{}}.

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
handle_call(exchanges, _, #state{exchanges=Exchanges}=State) ->
    {reply, [V || {_, V} <- dict:to_list(Exchanges)], State};
handle_call(assign_weight, _, #state{weight=Weight}=State) ->
    {reply, Weight, State#state{weight=Weight+1}};
handle_call({new, #wh_amqp_connection{uri=URI}=Connection}, _, State) ->
    case ets:insert_new(?TAB, Connection) of
        true -> {reply, Connection, State, ?ENSURE_TIME};
        false -> {reply, find(URI), State, ?ENSURE_TIME}
    end;
handle_call({connected, #wh_amqp_connection{uri=URI, connection=Pid
                                            ,connection_ref=Ref
                                            ,control_channel=CtrlPid}=C}
            ,_, State) ->
    Updates = [{#wh_amqp_connection.connection, Pid}
               ,{#wh_amqp_connection.connection_ref, Ref}
               ,{#wh_amqp_connection.available, true}
               ,{#wh_amqp_connection.control_channel, CtrlPid}
              ],
    ets:update_element(?TAB, URI, Updates),
    {reply, C#wh_amqp_connection{available=true}, State, ?ENSURE_TIME};
handle_call({disconnected, #wh_amqp_connection{uri=URI}=C}, _, State) ->
    Updates = [{#wh_amqp_connection.connection, undefined}
               ,{#wh_amqp_connection.connection_ref, undefined}
               ,{#wh_amqp_connection.available, false}
               ,{#wh_amqp_connection.exchanges, []}
               ,{#wh_amqp_connection.control_channel, undefined}
              ],
    ets:update_element(?TAB, URI, Updates),
    _ = wh_amqp_channels:reconnect(),
    {reply, C#wh_amqp_connection{available=false}, State, ?ENSURE_TIME};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State, ?ENSURE_TIME}.

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
handle_cast({force_reconnect, Connection}, State) ->
    wh_amqp_channels:force_reconnect(Connection),
    {noreply, State, ?ENSURE_TIME};
handle_cast({add_exchange, #'exchange.declare'{exchange=Name}=Command}
            ,#state{exchanges=Exchanges}=State) ->
    {noreply, State#state{exchanges=dict:store(Name, Command, Exchanges)}, ?ENSURE_TIME};
handle_cast({add_exchange, URI, #'exchange.declare'{exchange=Name}=Command}, State) ->
    Exchanges = [Command
                 |lists:filter(fun(#'exchange.declare'{exchange=N}) ->
                                       N =/= Name;
                                  (_) -> true
                               end, exchanges(URI))
                ],
    ets:update_element(?TAB, URI, {#wh_amqp_connection.exchanges, Exchanges}),
    {noreply, State, ?ENSURE_TIME};
handle_cast(_Msg, State) ->
    {noreply, State, ?ENSURE_TIME}.

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
handle_info('timeout', State) ->
    _ = case current() of
            {'ok', #wh_amqp_connection{}=Connection} ->
                wh_amqp_channels:force_reconnect(Connection);
            {'error', _} -> 'ok'
        end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State, ?ENSURE_TIME}.

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
terminate(_Reason, _State) ->
    lager:debug("connections terminating: ~p", [_Reason]).

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
-spec current_weight() -> integer().
current_weight() ->
    case current() of
        {ok, #wh_amqp_connection{weight=Weight}} ->
            Weight;
        {'error', _} ->
            -1
    end.

-spec declare_exchange(wh_amqp_connections(), #'exchange.declare'{}) -> ok.
declare_exchange([], _) -> ok;
declare_exchange([#wh_amqp_connection{control_channel=Pid
                                      ,uri=URI}
                  | Connections
                 ], #'exchange.declare'{exchange=Name, type=_Ty}=Command) ->
    _ = case amqp_channel:call(Pid, Command) of
            #'exchange.declare_ok'{} ->
                lager:debug("declared ~s exchange ~s on ~s via ~p", [_Ty, Name, URI, Pid]),
                gen_server:cast(?MODULE, {add_exchange, URI, Command});
            _Else -> ok
        end,
    declare_exchange(Connections, Command).

-spec redeclare_exchanges([#'exchange.declare'{},...] | [], wh_amqp_connection()) -> ok.
redeclare_exchanges([], _) -> ok;
redeclare_exchanges([Command|Commands], Connection)->
    _ = declare_exchange([Connection], Command),
    redeclare_exchanges(Commands, Connection).
