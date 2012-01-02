%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Simple cache server
%%% @end
%%% Created : 30 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_cache).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, store/2, store/3, peek/1, fetch/1, erase/1, flush/0, fetch_keys/0, filter/1]).
-export([start_local_link/0, store_local/3, store_local/4, peek_local/2, fetch_local/2,  erase_local/2
	 ,flush_local/1, fetch_keys_local/1, filter_local/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

-define(SERVER, ?MODULE).
-define(EXPIRES, 3600). %% an hour

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?MODULE], []).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

start_local_link() ->
    gen_server:start_link(?MODULE, [local], []).

%% T - seconds to store the pair
-spec store/2 :: (term(), term()) -> 'ok'.
-spec store/3 :: (term(), term(), pos_integer() | 'infinity') -> 'ok'.
store(K, V) ->
    store(K, V, ?EXPIRES).
store(K, V, T) ->
    gen_server:cast(?SERVER, {store, K, V, T}).

-spec peek/1 :: (term()) -> {'ok', term()} | {'error', 'not_found'}.
peek(K) ->
    gen_server:call(?SERVER, {peek, K}).

-spec fetch/1 :: (term()) -> {'ok', term()} | {'error', 'not_found'}.
fetch(K) ->
    gen_server:call(?SERVER, {fetch, K}).

-spec erase/1 :: (term()) -> 'ok'.
erase(K) ->
    gen_server:cast(?SERVER, {erase, K}).

-spec flush/0 :: () -> 'ok'.
flush() ->
    gen_server:cast(?SERVER, {flush}).

-spec fetch_keys/0 :: () -> [term(),...] | [].
fetch_keys() ->
    gen_server:call(?SERVER, fetch_keys).

-spec filter/1 :: (fun((term(), term()) -> boolean())) -> proplist().
filter(Pred) when is_function(Pred, 2) ->
    gen_server:call(?SERVER, {filter, Pred}).

%% Local cache API
-spec store_local/3 :: (pid(), term(), term()) -> 'ok'.
-spec store_local/4 :: (pid(), term(), term(), integer()) -> 'ok'.
store_local(Srv, K, V) when is_pid(Srv) ->
    store_local(Srv, K, V, ?EXPIRES).
store_local(Srv, K, V, T) when is_pid(Srv) ->
    gen_server:cast(Srv, {store, K, V, T}).

-spec peek_local/2 :: (pid(), term()) -> {'ok', term()} | {'error', 'not_found'}.
peek_local(Srv, K) when is_pid(Srv) ->
    gen_server:call(Srv, {peek, K}).

-spec fetch_local/2 :: (pid(), term()) -> {'ok', term()} | {'error', 'not_found'}.
fetch_local(Srv, K) when is_pid(Srv) ->
    gen_server:call(Srv, {fetch, K}).

-spec erase_local/2 :: (pid(), term()) -> 'ok'.
erase_local(Srv, K) when is_pid(Srv) ->
    gen_server:cast(Srv, {erase, K}).

-spec flush_local/1 :: (pid()) -> 'ok'.
flush_local(Srv) when is_pid(Srv) ->
    gen_server:cast(Srv, {flush}).

-spec fetch_keys_local/1 :: (pid()) -> [term(),...] | [].
fetch_keys_local(Srv) when is_pid(Srv) ->
    gen_server:call(Srv, fetch_keys).

-spec filter_local/2 :: (pid(), fun((term(), term()) -> boolean())) -> proplist().
filter_local(Srv, Pred)  when is_pid(Srv) andalso is_function(Pred, 2) ->
    gen_server:call(Srv, {filter, Pred}).

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
init([Name]) ->
    {ok, _} = timer:send_interval(1000, flush),
    put(callid, Name),
    ?LOG("Started new cache proc"),
    {ok, dict:new()}.

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
handle_call({peek, K}, _, Dict) ->
    case dict:find(K, Dict) of
	{ok, {_, V, _}} -> {reply, {ok, V}, Dict};
	error -> {reply, {error, not_found}, Dict}
    end;
handle_call({fetch, K}, _, Dict) ->
    case dict:find(K, Dict) of
	{ok, {infinity=T, V, T}} -> {reply, {ok, V}, Dict};
	{ok, {_, V, T}} -> {reply, {ok, V}, dict:update(K, fun(_) -> {wh_util:current_tstamp()+T, V, T} end, Dict), hibernate};
	error -> {reply, {error, not_found}, Dict}
    end;
handle_call(fetch_keys, _, Dict) ->
    {reply, dict:fetch_keys(Dict), Dict};
handle_call({filter, Pred}, _, Dict) ->
    KV = dict:map(fun(_, {_,V,_}) -> V end, Dict),
    {reply, dict:to_list(dict:filter(Pred, KV)), Dict}.

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
handle_cast({store, K, V, infinity=T}, Dict) ->
    {noreply, dict:store(K, {T, V, T}, Dict), hibernate};
handle_cast({store, K, V, T}, Dict) ->
    {noreply, dict:store(K, {wh_util:current_tstamp()+T, V, T}, Dict), hibernate};
handle_cast({erase, K}, Dict) ->
    {noreply, dict:erase(K, Dict), hibernate};
handle_cast({flush}, _) ->
    {noreply, dict:new(), hibernate}.

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
handle_info(flush, Dict) ->
    Now = wh_util:current_tstamp(),
    {noreply, dict:filter(fun(_, {infinity,_,_}) -> true;
			     (_, {T, _, _}) -> Now < T
			  end, Dict), hibernate};
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
    ok.

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
