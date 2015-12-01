%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%--------------------------------------------------------------------
%% Cleaner module.
%%
%% The cleaner should be started at the init of each test case.
%% Clients are registered to it (using add_client/2) as they are
%% started. In the end of test case stop/1 should be called which
%% stops all the registered clients.
%%--------------------------------------------------------------------
-module(escalus_cleaner).

% Public API
-export([start/1,
         add_client/2,
         clean/1,
         stop/1]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        clients = [] :: [pid()]
}).

%%%===================================================================
%%% Public API
%%%===================================================================

start(Config) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    [{escalus_cleaner, Pid} | Config].

add_client(Config, Client) ->
    gen_server:cast(get_cleaner(Config), {add_client, Client}).

clean(Config) ->
    gen_server:call(get_cleaner(Config), clean).

stop(Config) ->
    gen_server:cast(get_cleaner(Config), stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(clean, _From, #state{clients = Clients} = State) ->
    lists:foreach(fun escalus_client:stop/1, Clients),
    {reply, ok, State#state{clients = []}}.

handle_cast({add_client, Pid}, #state{clients = Clients} = State) ->
    {noreply, State#state{clients = [Pid | Clients]}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{clients = []}) ->
    ok;
terminate(_Reason, #state{clients = Clients}) ->
    error_logger:warning_msg("cleaner finishes dirty: ~p~n", [Clients]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% helpers
%%%===================================================================

get_cleaner(Config) ->
    {escalus_cleaner, Pid} = lists:keyfind(escalus_cleaner, 1, Config),
    Pid.
