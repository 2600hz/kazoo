%% @author Kirill Trofimov <sinnus@gmail.com>
%% @copyright 2012 Kirill Trofimov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(stress_mgr).
-author('Kirill Trofimov <sinnus@gmail.com>').
-behaviour(gen_server).

%% API
-export([start_link/0, inc_fail_count/0, get_stats/0, inc_init_count/0, inc_finish_count/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(client_stat_counts, [named_table, public, {write_concurrency, true}]),
    ets:insert(client_stat_counts, {fail, 0}),
    ets:insert(client_stat_counts, {init, 0}),
    ets:insert(client_stat_counts, {finish, 0}),
    {ok, #state{}}.
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
inc_fail_count() ->
    ets:update_counter(client_stat_counts, fail, 1).

inc_init_count() ->
    ets:update_counter(client_stat_counts, init, 1).

inc_finish_count() ->
    ets:update_counter(client_stat_counts, finish, 1).

get_stats() ->
    [{fail, FailCount}] = ets:lookup(client_stat_counts, fail),
    [{init, InitCount}] = ets:lookup(client_stat_counts, init),
    [{finish, FinishCount}] = ets:lookup(client_stat_counts, finish),
    [
     {fail_count, FailCount},
     {active_count, InitCount - FinishCount},
     {finish_count, FinishCount}
    ].
