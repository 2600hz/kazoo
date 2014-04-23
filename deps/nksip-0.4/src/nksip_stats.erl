%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc NkSIP (very basic yet) stats management.
%% Calculates periodically some statistics about UAS response times.

-module(nksip_stats).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(gen_server).

-export([info/0, get_uas_avg/0]).
-export([uas_response/1]).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, 
         handle_cast/2, handle_info/2]).

-include("nksip.hrl").

-define(PERIOD, 5).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Gets some statistics about current number of calls, dialogs, queues, etc.
-spec info() ->
    nksip_lib:optslist().

info() ->
    [
        {calls, nksip_counters:value(nksip_calls)},
        {dialogs, nksip_counters:value(nksip_dialogs)},
        {routers_queue, nksip_call_router:pending_msgs()},
        {routers_pending, nksip_call_router:pending_work()},
        {tcp_connections, nksip_counters:value(nksip_transport_tcp)},
        {counters_queue, nksip_counters:pending_msgs()},
        {core_queues, nksip_sipapp_srv:pending_msgs()},
        {uas_response, nksip_stats:get_uas_avg()}
    ].


%% @doc Gets the call statistics for the current period.
-spec get_uas_avg() ->
    {Min::integer(), Max::integer(), Avg::integer(), Std::integer()}.

get_uas_avg() ->
    gen_server:call(?MODULE, get_uas_avg).



%% ===================================================================
%% gen_server
%% ===================================================================

-record(state, {
    last_uas :: {Min::integer(), Max::integer(), Avg::integer(), Std::integer()},
    avg_uas_values :: [integer()],
    last_check :: nksip_lib:timestamp()
}).


%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
        

%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([]) ->
    Now = nksip_lib:timestamp(),
    {ok, #state{last_uas={0,0,0,0}, avg_uas_values=[], last_check=Now}}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call(get_uas_avg, _From, #state{last_uas=LastUas}=State) ->
    {reply, LastUas, State, timeout(State)};

handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State, timeout(State)}.


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast({uas_response, Time}, #state{avg_uas_values=Values}=State) ->
    State1 = State#state{avg_uas_values=[Time|Values]},
    {noreply, State1, timeout(State1)};

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State, timeout(State)}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info(timeout, #state{avg_uas_values=Values}=State) ->
    LastUas = calculate(Values),
    Now = nksip_lib:timestamp(),
    State1 = State#state{last_uas=LastUas, avg_uas_values=[], last_check=Now}, 
    {noreply, State1, 1000*?PERIOD};

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected info: ~p", [?MODULE, Info]),
    {noreply, State, timeout(State)}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    gen_server_code_change(#state{}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    gen_server_terminate().

terminate(_Reason, _State) ->  
    ok.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private Informs the module about the last response time
uas_response(Time) when is_number(Time) ->
    gen_server:cast(?MODULE, {uas_response, Time}).


%% @private
timeout(#state{last_check=Last}) ->
    case (Last+?PERIOD) - nksip_lib:timestamp() of
        Time when Time > 0 -> 1000*Time;
        _ -> 0
    end.


%% @private
-spec calculate([integer()]) ->
    {integer(), integer(), integer(), integer()}.

calculate([]) ->
    {0, 0, 0, 0};

calculate(List) ->
    {Min, Max, Avg} = avg(List),
    {Min, Max, Avg, std(List, Avg)}.


%% @private
-spec avg([integer()]) ->
    {integer(), integer(), integer()}.

avg([]) ->
    {0, 0, 0};
avg(List) -> 
    avg(List, min, 0, 0, 0).
avg([], Min, Max, Sum, Total) -> 
    {Min, Max, round(Sum/Total)};
avg([Num|Rest], Min, Max, Sum, Total) -> 
    Min1 = case Num < Min of true -> Num; false -> Min end,
    Max1 = case Num > Max of true -> Num; false -> Max end,
    avg(Rest, Min1, Max1, Sum+Num, Total+1).


%% @private
-spec std([integer()], integer()) ->
    integer().

std(List, Avg) ->
    round(math:sqrt(element(3, avg([(Num-Avg) * (Num-Avg) || Num <- List])))).

