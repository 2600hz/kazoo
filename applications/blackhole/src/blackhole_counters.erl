%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Roman Galeev
%%%-------------------------------------------------------------------
-module(blackhole_counters).
-behaviour(gen_server).

%% api
-export([start_link/0, stop/0, inc/1, dec/1, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        counters :: #{}
        }).

%% API

-spec inc(binary()) -> integer().
-spec dec(binary()) -> integer().
-spec get(binary()) -> integer().

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, {'stop'}).
inc(Key) -> gen_server:call(?MODULE, {'inc', Key}).
dec(Key) -> gen_server:call(?MODULE, {'dec', Key}).
get(Key) -> gen_server:call(?MODULE, {'get', Key}).

%% Impl
init([]) -> { 'ok', #state{counters = #{}} }.

handle_call({'inc', Key}, _From, S=#state{counters=T}) ->
    V = maps:get(Key, T, 0) + 1,
    {'reply', V, S#state{ counters=T#{ Key => V } }};
handle_call({'dec', Key}, _From, S=#state{counters=T}) ->
    V = maps:get(Key, T, 0) - 1,
    {'reply', V, S#state{ counters=T#{ Key => V } }};
handle_call({'get', Key}, _From, S=#state{counters=T}) ->
    V = maps:get(Key, T, 0),
    {'reply', V, S};
handle_call(_Request, _From, S=#state{}) -> {'reply', 'ok', S}.

handle_cast({'stop'}, S=#state{}) ->
    {'stop', 'normal', S};
handle_cast(_Msg, S=#state{}) -> {'noreply', S}.

handle_info(_Info, S=#state{}) -> {'noreply', S}.
terminate(_Reason, _S) -> 'ok'.
code_change(_OldVsn, S=#state{}, _Extra) -> {'ok', S}.
