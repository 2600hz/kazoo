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
-export([available/0]).
-export([wait_for_available/0]).
-export([does_key_exist/1]).
-export([register_return_handler/0]).

-export([find_reference/1]).
-export([find_channel/1]).
-export([update_channel/1
         ,update_channel/2
        ]).
-export([remove_channel/1]).
-export([update_connection/1
         ,update_connection/2
        ]).
-export([update_exchange/1]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {}).

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

-spec add/1 :: (text()) -> 'ok' | {'error', _}.
add(URI) ->
    case catch wh_amqp_broker:set_uri(URI, wh_amqp_broker:new()) of
        {'EXIT', _} ->
            lager:error("failed to parse amqp URI '~p', dropping", [URI]),
            {error, invalid_uri};
        Broker ->
            case wh_amqp_connection_sup:add(Broker) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, already_present} ->
                    _ = wh_amqp_connection_sup:remove(Broker),
                    add(URI);
                {error, Reason} ->
                    Name = wh_amqp_broker:name(Broker),
                    lager:warning("unable to start amqp connection to '~s': ~p", [Name, Reason]),
                    {error, Reason}
            end
    end.

remove(URI) ->
    wh_amqp_connection_sup:remove(URI).

current() ->
    case ets:match_object(?MODULE, #wh_amqp_connection{connection = '$1'
                                                       ,available = true
                                                       ,_ = '_'})
    of
        [Connection|_] -> {ok, Connection};
        _Else ->
            lager:critical("no AMQP connection available", []),
            {error, no_available_connection}
    end.

available() ->
    case current() of
        {ok, _} -> true;
        {error, _} -> false
    end.

-spec wait_for_available/0 :: () -> 'ok'.
wait_for_available() ->
    case available() of
        true -> ok;
        false ->
            timer:sleep(random:uniform(1000) + 100),
            wait_for_available()
    end.

does_key_exist(Key) ->
    ets:member(?MODULE, Key).

-spec register_return_handler/0 :: () -> 'ok'.
register_return_handler() ->
    io:format("~p: register return handler~n", [self()]),
    ok.
%%    gen_server:cast(?SERVER, {register_return_handler, self()}).

find_reference(Ref) ->
    MatchSpec = [{#wh_amqp_channel{channel_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{channel, '$_'}}]},
                 {#wh_amqp_channel{consumer_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{consumer, '$_'}}]},
                 {#wh_amqp_connection{connection_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{connection, '$_'}}]}
                ],
    ets:select(?MODULE, MatchSpec).

find_channel(Pid) ->
    MatchSpec = [{#wh_amqp_channel{channel = '$1'
                                   ,consumer = '$2'
                                   ,_ = '_'},
                  [{'=:=', '$2', {const, Pid}}],
                  ['$_']}
                ],
    case ets:select(?MODULE, MatchSpec) of
        [#wh_amqp_channel{}=Channel|_] ->
            Channel;
         _Else -> {error, not_found}
    end.

update_connection(#wh_amqp_connection{}=Connection) ->
    ets:insert(?MODULE, Connection),
    Connection.

update_connection(#wh_amqp_connection{connection=undefined}, _) ->
    false;
update_connection(#wh_amqp_connection{connection=Key}, Updates) ->
    ets:update_element(?MODULE, Key, Updates).

update_channel(#wh_amqp_channel{}=Channel) ->
    ets:insert(?MODULE, Channel),
    Channel.

update_channel(#wh_amqp_channel{id=undefined}, _) ->
    io:format("update channel had no key~n", []),
    false;
update_channel(#wh_amqp_channel{id=Key}, Updates) ->
    ets:update_element(?MODULE, Key, Updates).

remove_channel(#wh_amqp_channel{id=Id}=Channel) ->
    ets:delete(?MODULE, Id),
    Channel.

update_exchange(#wh_amqp_exchange{}=Exchange) ->
    ets:insert(?MODULE, Exchange).

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
    R = ets:new(?MODULE, [named_table, {keypos, 2}, public]),
    io:format("created ets table: ~p~n", [R]),
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
handle_call(_Request, _From, State) ->
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
handle_info(_Info, State) ->
    {noreply, State}.

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
