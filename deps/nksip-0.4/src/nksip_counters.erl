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

%% @doc NkSIP Process Counters.
%%
%% This module implements a ETS-based process counters manager.
%% Any process can register and update any number of counters, and they can be shared
%% with other processes. When the process exists, all updates made from that process 
%% are reverted.

-module(nksip_counters).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(gen_server).

-export([incr/1, incr/2, incr/3, value/1, del/1, del/2, async/1]).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, 
         handle_cast/2, handle_info/2]).
-export([pending_msgs/0, stop/0]).

-include("nksip.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Equivalent to `incr(Name, 1, self())'.
-spec incr(term()) -> 
    ok.

incr(Name) -> 
    incr(Name, 1, self()).


%% @doc Equivalent to `incr(Name, Value, self())'.
-spec incr(term(), integer()) -> 
    ok.

incr(Name, Value) when is_integer(Value) -> 
    incr(Name, Value, self()).


%% @doc Creates or increments synchronously a `Name' counter associated with process
%% having `Pid'. `Value' will be the initial value if the counter doesn't exist,
%% or it will be added to the previous value if it exists.
%% `Value' must be a positive or negative integer.
%% When the process having `Pid' exists, all updates sent from it will be reverted.
-spec incr(term(), integer(), pid()) -> 
    ok.

incr(Name, Value, Pid) when is_integer(Value), is_pid(Pid) ->
    gen_server:call(?SERVER, {incr, Name, Value, Pid}).


%% @doc Gets a counter's current value.
-spec value(term()) -> 
    integer().

value(Name) ->
    case lookup({name, Name}) of
        [] -> 0;
        Val -> Val
    end.


%% @doc Equivalent to `del(Name, self())'.
-spec del(term()) -> 
    ok.

del(Name) -> 
    del(Name, self()).


%% @doc Removes all `Name' counter updates sent from `Pid'.
-spec del(term(), pid()) -> 
    ok.

del(Name, Pid) when is_pid(Pid) -> 
    gen_server:call(?SERVER, {del, Name, Pid}).


%% @doc Performs an asynchronous multiple counter update as a batch.
%% Default value for `Value' is `1', and the current process's `pid()' for `Pid'.
-spec async([Op]) -> ok
    when Op ::  Name | {Name, Value} | {Name, Value, Pid},
         Name :: term(), Value :: integer(),  Pid :: pid().

async(Ops) when is_list(Ops) ->
    gen_server:cast(?SERVER, {multi, expand_multi(Ops)}).


%% @private
pending_msgs() ->
    {_, Len} = erlang:process_info(whereis(?SERVER), message_queue_len),
    Len.





%% ===================================================================
%% gen_server
%% ===================================================================

-record(state, {
}).

%% @private
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).
        
%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([]) ->
    ets:new(?TABLE, [protected, named_table]),
    {ok, #state{}}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call({incr, Name, Value, Pid}, _From, State) ->
    register(Name, Value, Pid),
    {reply, ok, State};

handle_call({del, Name, Pid}, _From, State) ->
    unregister(Name, Pid),
    {reply, ok, State};

handle_call(stop, _from, State) ->
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast({multi, List}, State) ->
    lists:foreach(fun({Name, Value, Pid}) -> register(Name, Value, Pid) end, List),
    {noreply, State};

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    case lookup({pid, Pid}) of
        [] -> Values = [];
        {Ref, Values} -> ok
    end,
    lists:foreach(fun({Name, _V}) -> unregister(Name, Pid) end, Values),
    delete({pid, Pid}),         
    {noreply, State};

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected cast ~p", [?MODULE, Info]),
    {noreply, State}.


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
%% Private
%% ===================================================================

%% @private
register(_Name, 0, _Pid) ->
    ok;
register(Name, Value, Pid) ->
    OldTotal = value(Name),
    case OldTotal+Value of
        0 -> delete({name, Name});
        NewTotal -> insert({name, Name}, NewTotal)
    end,
    case lookup({pid, Pid}) of
        [] ->
            insert({pid, Pid}, {erlang:monitor(process, Pid), [{Name, Value}]});
        {Ref, PidValues} ->
            case lists:keytake(Name, 1, PidValues) of
                false -> 
                    insert({pid, Pid}, {Ref, [{Name, Value}|PidValues]});
                {value, {Name, OldPidCount}, PidValues1} -> 
                    case OldPidCount+Value of
                        0 -> 
                            case PidValues1 of
                                [] -> 
                                    erlang:demonitor(Ref),
                                    delete({pid, Pid});
                                _ ->
                                    insert({pid, Pid}, {Ref, PidValues1})
                            end;
                        NewPidCount ->
                            insert({pid, Pid}, {Ref, [{Name, NewPidCount}|PidValues1]})
                    end
            end
    end.


%% @private
unregister(Name, Pid) ->
    case lookup({pid, Pid}) of
        [] -> 
            PidCount = 0;
        {Ref, PidValues} ->
            case lists:keytake(Name, 1, PidValues) of
                false ->
                    PidCount = 0;
                {value, {Name, PidCount}, []} ->
                    erlang:demonitor(Ref),
                    delete({pid, Pid});
                {value, {Name, PidCount}, PidValues1} ->
                    insert({pid, Pid}, {Ref, PidValues1})
            end
    end,
    OldTotal = value(Name),
    case OldTotal - PidCount of
        0 -> delete({name, Name});
        NewTotal -> insert({name, Name}, NewTotal)
    end.


%% @private
-spec lookup(term()) -> 
    [] | term().

lookup(Name) ->
    case ets:lookup(?TABLE, Name) of
        [] -> [];
        [{_, Val}] -> Val
    end.


%% @private
-spec insert(term(), term()) -> 
    true.

insert(Name, Val) -> 
    true = ets:insert(?TABLE, {Name, Val}).


%% @private
-spec delete(term()) -> 
    true.

delete(Name) -> 
    true = ets:delete(?TABLE, Name).


%% @private
expand_multi(Ops) ->
    Self = self(),
    Fun = fun(Op) ->
        case Op of
            {N, V, P} when is_integer(V), is_pid(P) -> {N, V, P};
            {N, V} when is_integer(V) -> {N, V, Self};
            N -> {N, 1, Self}
        end
    end,
    lists:map(Fun, Ops).



%% ===================================================================
%% EUnit tests
%% ===================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {setup, 
        fun() -> 
            ?debugFmt("Starting ~p", [?MODULE]),
            case start_link() of
                {error, {already_started, _}} ->
                    ok;
                {ok, _} ->
                    do_stop
            end
        end,
        fun(Stop) -> 
            case Stop of
                do_stop -> stop();
                ok -> ok
            end
        end,
        [
            fun test_count/0,
            fun test_multi/0
        ]
    }.

test_count() ->
    del(c),
    ?assert(0 == value(c)),
    incr(c, 0),
    incr(c),
    incr(c, 10),
    ?assert(11 == value(c)),
    incr(c, -11),
    ?assert(0 == value(c)),
    incr(c),
    Self = self(),
    Ref = make_ref(),
    spawn(fun() -> incr(c, 2), incr(c, 4), Self ! Ref, timer:sleep(100) end),
    receive Ref -> ok after 5000 -> error(timeout) end,
    ?assert(7 == value(c)),
    timer:sleep(200),
    ?assert(1 == value(c)),
    incr(c, -1),
    ?assert(0 == value(c)),
    ?assert(0 == value(d)),
    Pid1 = spawn(fun() -> incr(d, 10), Self ! Ref, timer:sleep(100) end),
    incr(d, 2),
    incr(d, 3),
    receive Ref -> ok after 5000 -> error(timeout) end,
    ?assert(15 == value(d)),
    incr(d, -5),
    ?assert(10 == value(d)),
    incr(d, -10),
    ?assert(0 == value(d)),
    del(d, Pid1),
    % timer:sleep(150),
    ?assert(-10 == value(d)),
    incr(d, 10),
    ?assert(0 == value(d)),

    % Complete the coverage
    spawn(fun() -> incr(e), Self ! Ref, timer:sleep(100) end),
    receive Ref -> ok after 5000 -> error(timeout) end,
    ?assert(1 == value(e)),
    incr(e),
    incr(e, -1),
    incr(e, -1),
    ?assert(0 == value(e)),
    timer:sleep(150),
    ?assert(-1 == value(e)),
    incr(e),
    ?assert(0 == value(e)),
    spawn(fun() -> incr(f) end),
    timer:sleep(50),
    ?assert(0 == value(f)),
    Pid2 = spawn(fun() -> incr(f), incr(f, -1)  end),
    spawn(fun() -> incr(g) end),
    timer:sleep(50),
    ?assert(0 == value(f)),
    ?assert(0 == value(g)),
    del(f, Pid2),
    del(f),
    ok.

test_multi() ->
    nksip_counters:del(ma),
    ok = async([ma, {ma, 3}]),
    timer:sleep(500),
    ?assert(4 == value(ma)),
    ok.

-endif.









