% -------------------------------------------------------------------
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

%% @doc NkSIP Process Registry.
%%
%% This module implements an ETS-based process registry. 
%% It allows a process to register any `term()' as a process identification, 
%% and store any metadata with it. When the process exists, these terms are deleted. 
%%
%% It also allows to start `gen_server' or `gen_fsm' processes using any `term()' as 
%% registered name.

-module(nksip_proc).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(gen_server).

-export([put/1, put/2, put/3, del/1, del/2, del_all/0, del_all/1]).
-export([reg/1, reg/2, reg/3, reg/4, values/1]).
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).
-export([wait_put/1, wait_put/2, wait_del/1, wait_del/2]).
-export([fold_names/2, fold_pids/2, size/0, pending_msgs/0]).
-export([start/4, start_link/4]).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, 
            handle_cast/2, handle_info/2]).
-export([dump/0]).

-include("nksip.hrl").


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Equivalent to `put(Name, undefined, self())'.
-spec put(term()) -> ok.
put(Name) -> put(Name, undefined, self()).


%% @doc Equivalent to `put(Name, Value, self())'.
-spec put(term(), term()) -> ok.
put(Name, Value) -> put(Name, Value, self()).


%% @doc Registers `Name' (and `Value') as associated with this process.
%% Several processes can register the same `Name' with the same of different values.
%% When the process ends, its registrations are deleted.
-spec put(term(), term(), pid()) -> ok.
put(Name, Value, Pid) when is_pid(Pid) -> 
    gen_server:cast(?MODULE, {put, Name, Value, Pid}).


%% @doc Equivalent to `del(Name, self())'.
-spec del(term()) -> ok.
del(Name) -> del(Name, self()).


%% @doc Unregisters a previously registered `Name', associated to this process.
-spec del(term(), pid()) -> ok.
del(Name, Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {del, Name, Pid}).


%% @doc Equivalent to `del_all(self())'.
-spec del_all() -> ok.
del_all() -> del_all(self()).


%% @doc Unregisters all registrations from this process.
-spec del_all(pid()) -> ok.
del_all(Pid) ->
    gen_server:cast(?MODULE, {del_all, Pid}).


%% @doc Gets all registered Values for `Name', along with registering pids.
-spec values(term()) -> [{Value::term(), Pid::pid()}].
values(Name) ->
    case lookup_store(Name) of
        [] -> [];
        Items -> [{Value, Pid} || {val, Value, Pid} <- Items]
    end.


%% @doc Equivalent to `reg(Name, undefined, self())'.
-spec reg(term()) -> true | {false, pid()}.
reg(Name) -> reg(Name, undefined, self()).


%% @doc Equivalent to `reg(Name, Value, self())'.
-spec reg(term(), term()) -> true | {false, pid()}.
reg(Name, Value) -> reg(Name, Value, self()).


%% @doc Similar to `put(Name, Value, Pid)' but only allows for one single registration.
%% Returns `true' if this registration has succeded, or `false' if another process
%% has already registered this name.
-spec reg(term(), term(), pid()) -> true | {false, pid()}.
reg(Name, Value, Pid) -> 
    case ets:insert_new(nksip_proc_store,  {Name, [{val, Value, Pid}]}) of
        true -> 
            put(Name, Value, Pid),
            true;
        false ->
            gen_server:call(?MODULE, {reg, Name, Value, Pid})
    end.


%% @doc Similar to `reg(Name, Value, Pid)', but, in case other process has already
%% registered this name, waits for it to be unregistered and then we are automatically
%% registered. 
-spec reg(term(), term(), pid(), integer()|infinity) -> true | timeout.
reg(Name, Value, Pid, Timeout) ->
    case catch gen_server:call(?MODULE, {wait_reg, Name, Value, Pid}, Timeout) of
        ok -> true;
        _ -> timeout
    end.

    
%% @doc Equivalent to `wait_put(Name, 5000)'.
-spec wait_put(term()) -> {ok, pid()} | timeout.
wait_put(Name) -> wait_put(Name, 5000).

%% @doc Waits for `Name' to be registered from any process before `Timeout'.
-spec wait_put(term(), integer()|infinity) -> {ok, {Value::term(), pid()}} | timeout.
wait_put(Name, Timeout) ->
    case values(Name) of
        [] ->
            case catch gen_server:call(?MODULE, {wait_put, Name}, Timeout) of
                {ok, {Value, Pid}} -> {ok, {Value, Pid}};
                _ -> timeout
            end;
        [{Value, Pid}] ->
            {ok, {Value, Pid}}
    end.


%% @doc Equivalent to `wait_del(Name, 5000))'.
-spec wait_del(term()) -> ok | timeout.
wait_del(Name) -> wait_del(Name, 5000).

%% @doc Waits for `Name' to be unregistered from all pids before `Timeout'.
-spec wait_del(term(),  integer()|infinity) -> ok | timeout.
wait_del(Name, Timeout) ->
    case catch gen_server:call(?MODULE, {wait_del, Name}, Timeout) of
        ok -> ok;
        _ -> timeout
    end.


%% @doc Process registration compatible with `global:register_name/2'.
-spec register_name(term(), pid()) -> yes | no.
register_name(Name, Pid) ->
    case reg(Name, undefined, Pid) of
        true -> yes;
        {false, _} -> no
    end.


%% @doc Unregistration compatible with `global:unregister_name/1'.
-spec unregister_name(term()) -> pid() | undefined.
unregister_name(Name) ->
    case whereis_name(Name) of
        undefined ->
            undefined;
        Pid ->
            del(Name, Pid),
            Pid
    end.


%% @doc Finds a registered `Name' compatible with `global:whereis_name/1'.
-spec whereis_name(term()) -> pid() | undefined.
whereis_name(Name) ->
    case values(Name) of
        [{_, Pid}] -> Pid;
        _ -> undefined
    end.


%% @doc Sends a message to a registered process compatible with `global:send/2'.
-spec send(term(), term()) -> pid().
send(Name, Msg) ->
    case whereis(Name) of
        undefined -> error({badarg, {Name, Msg}});
        Pid -> Pid ! Msg
    end.


%% @doc Folds over all registered names in the same way as `lists:foldl/3'.
%% `UserFun' will be called for each record as `UserFun(Name, Values, Acc)' when
%% `Values :: [{Value::term(), pid()}]'.
-spec fold_names(function(), term()) -> term().
fold_names(UserFun, Acc0) when is_function(UserFun, 3) ->
    Fun = fun({Name, Values}, Acc) -> UserFun(Name, Values, Acc) end,
    ets:foldl(Fun, Acc0, nksip_proc_store).


%% @doc Folds over all registered pids in the same way as `lists:foldl/3'.
%% `UserFun' will be called for each record as `UserFun(Pid, Items, Acc)' when
%% `Names :: [{name|put|del|reg, term()}]'.
fold_pids(UserFun, Acc0) when is_function(UserFun, 3) ->
    Fun = fun({Pid, _Mon, Names}, Acc) -> UserFun(Pid, Names, Acc) end,
    ets:foldl(Fun, Acc0, nksip_proc_pids).


%% @doc Starts a new gen_server or gen_fsm, allowing registration under any term.
-spec start(server|fsm, term(), atom(), [term()]) ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start(Type, Name, Module, Args) ->
    start(Type, false, Name, Module, Args).


%% @doc Starts a new `gen_server' or `gen_fsm', allowing registration under any term,
%% and linking the new process.
-spec start_link(server|fsm, term(), atom(), [term()]) ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start_link(Type, Name, Module, Args) ->
    start(Type, true, Name, Module, Args).


%

%% ===================================================================
%% gen_server
%% ===================================================================

-record(state, {
}).

%% @private
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
        

%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([]) ->
    process_flag(priority, high),
    ets:new(nksip_proc_store, [public, named_table]),
    ets:new(nksip_proc_pids, [public, named_table]),
    {ok, #state{}}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call({set_monitor, Pid}, _From, State) ->
    {reply, monitor(process, Pid), State};

handle_call({reg, Name, Value, Pid}, _From, State) ->
    case values(Name) of
        [] ->
            register(Name, val, Value, Pid),
            {reply, true, State};
        [{_, Pid}|_] -> 
            {reply, {false, Pid}, State}
    end;

handle_call({wait_reg, Name, Value, Pid}, {FromPid, FromRef}, State) ->
    case values(Name) of
        [] -> 
            register(Name, val, Value, Pid),
            {reply, ok, State};
        [{Value, Pid}] ->
            {reply, ok, State};
        _ ->
            register(Name, reg, {FromRef, Value, Pid}, FromPid),
            {noreply, State}
    end;

handle_call({wait_put, Name}, {FromPid, FromRef}, State) ->
    case values(Name) of
        [{Value, Pid}|_] -> 
            {reply, {ok, {Value, Pid}}, State};
        [] ->
            register(Name, put, FromRef, FromPid),
            {noreply, State}
    end;

handle_call({wait_del, Name}, {FromPid, FromRef}, State) ->
    case values(Name) of
        [] ->
            {reply, ok, State};
        _ ->
            register(Name, del, FromRef, FromPid),
            {noreply, State}
    end;
                
handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast({put, Name, Value, Pid}, State) ->
    register(Name, val, Value, Pid),
    {noreply, State};

handle_cast({del, Name, Pid}, State) ->
    unregister(Name, val, Pid),
    {noreply, State};

handle_cast({del_all, Pid}, State) ->
    unregister_all(Pid),
    {noreply, State};

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    unregister_all(Pid),
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
-spec register(term(), val|put|del|reg, term(), pid()) -> true.
register(Name, Type, Value, Pid) ->
    insert_pid(Pid, Type, Name),
    NameItems = case lookup_store(Name) of
        [] -> 
            [{Type, Value, Pid}];
        OldNameItems -> 
            NameItemsR = [{T, V, P} || {T, V, P } <- OldNameItems,
                                       T/=Type orelse P/=Pid],
            case Type of
                reg -> NameItemsR ++ [{Type, Value, Pid}];
                _ -> [{Type, Value, Pid}|NameItemsR]
            end
    end,
    case Type of
        val ->
            Fun = fun
                ({put, PutRef, PutPid}) -> 
                    gen_server:reply({PutPid, PutRef}, {ok, {Value, Pid}}), 
                    remove_pid(PutPid, put, Name),
                    false;
                (_) -> 
                    true
            end,
            insert_store(Name, lists:filter(Fun, NameItems));
        _ ->
            insert_store(Name, NameItems)
    end.


%% @private
-spec unregister(term(), val|put|del|reg, pid()) -> true.
unregister(Name, Type, Pid) ->
    remove_pid(Pid, Type, Name),
    NameItems = [{T, Val, P} || {T, Val, P} <- lookup_store(Name), 
                                T/=Type orelse P/=Pid],
    case Type==val andalso not lists:keymember(val, 1, NameItems) of
        true ->
            DelFun = fun
                ({del, PutRef, PutPid}) -> 
                    gen_server:reply({PutPid, PutRef}, ok), 
                    remove_pid(PutPid, del, Name),
                    false;
                (_) -> 
                    true
            end,
            NameItemsD = lists:filter(DelFun, NameItems),
            case lists:keytake(reg, 1, NameItemsD) of
                false ->
                    insert_store(Name, NameItemsD);
                {value, {reg, {FromRef, RegValue, RegPid}, FromPid}, NameItemsR} ->
                    remove_pid(FromPid, reg, Name),
                    insert_pid(RegPid, val, Name),
                    insert_store(Name, [{val, RegValue, RegPid}|NameItemsR]),
                    gen_server:reply({FromPid, FromRef}, ok)
            end;
        false ->
            insert_store(Name, NameItems)
    end.


%% @private
-spec unregister_all(pid()) -> ok.
unregister_all(Pid) ->
    case ets:lookup(nksip_proc_pids, Pid) of
        [] -> 
            ok;
        [{_, _Mon, Items}] -> 
            lists:foreach(fun({Type, Name}) -> unregister(Name, Type, Pid) end, Items)
    end.
    

%% @private
-spec insert_pid(pid(), val|put|del|reg, term()) -> ok.
insert_pid(Pid, Type, Name) ->
    case ets:lookup(nksip_proc_pids, Pid) of
        [] -> 
            Mon = monitor(process, Pid),
            ets:insert(nksip_proc_pids, {Pid, Mon, [{Type, Name}]});
        [{Pid, Mon, PidItems}] ->
            case lists:member({Type, Name}, PidItems) of
                true -> ok;
                false -> ets:insert(nksip_proc_pids, {Pid, Mon, [{Type, Name}|PidItems]})
            end
    end.


%% @private
-spec remove_pid(pid(), val|put|del|reg, term()) -> ok.
remove_pid(Pid, Type, Name) ->
    case ets:lookup(nksip_proc_pids, Pid) of
        [] -> 
            ok;
        [{Pid, Mon, PidItems}] ->
            case lists:member({Type, Name}, PidItems) of
                false -> 
                    ok;
                true -> 
                    case PidItems -- [{Type, Name}] of
                        [] -> 
                            demonitor(Mon),
                            ets:delete(nksip_proc_pids, Pid);
                        Items1 -> 
                            ets:insert(nksip_proc_pids, {Pid, Mon, Items1})
                    end
            end
    end.

%% @private
-spec lookup_store(term()) -> [{val|put|del|reg, term(), pid()}].
lookup_store(Name) ->
    case ets:lookup(nksip_proc_store, Name) of
        [] -> [];
        [{_, Items}] -> Items
    end.

%% @private
-spec insert_store(term(), [{val|put|del|reg, term(), pid()}]) -> true.
insert_store(Name, []) -> ets:delete(nksip_proc_store, Name);
insert_store(Name, Items) -> ets:insert(nksip_proc_store, {Name, Items}).

%% @doc Gets the number of registered processes
-spec size() -> integer().
size() -> ets:info(nksip_proc_pids, size).
 

%% @private
-spec pending_msgs() -> integer().
pending_msgs() ->
    {_, Len} = erlang:process_info(whereis(?MODULE), message_queue_len),
    Len.

%% @private
dump() ->
    {
        ets:tab2list(nksip_proc_store),
        ets:tab2list(nksip_proc_pids)
    }.


%% @private
-spec start(server|fsm, boolean(), term(), atom(), [term()]) ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start(Type, Link, Name, Module, Args) ->
    case whereis_name(Name) of
        undefined ->
            Ref = make_ref(),
            Self = self(),
            Fun = fun() -> do_start(Type, Name, Module, Args, Self, Ref) end,
            Pid = case Link of
                true -> proc_lib:spawn_link(Fun);
                false -> proc_lib:spawn(Fun)
            end,
            receive 
                {Ref, Result} -> Result
            after 60000 -> 
                lager:error("Timeout when creating process ~p", [Name]),
                exit(Pid, kill),
                {error, timeout}
            end;
        Pid ->
            {error, {already_started, Pid}}
    end.


%% @private
-spec do_start(server|fsm, term(), atom(), [term()], pid(), reference()) ->
    ok.

do_start(Type, Name, Module, Args, Pid, Ref) ->
    case reg(Name) of
        true ->
            case catch Module:init(Args) of
                {ok, StateData} when Type==server ->
                    Pid ! {Ref, {ok, self()}},
                    gen_server:enter_loop(Module, [], StateData);
                {ok, StateData, Timeout} when Type==server ->
                    Pid ! {Ref, {ok, self()}},
                    gen_server:enter_loop(Module, [], StateData, Timeout);
                {ok, StateName, StateData} when Type==fsm->
                    Pid ! {Ref, {ok, self()}},
                    gen_fsm:enter_loop(Module, [], StateName, StateData);
                {ok, StateName, StateData, Timeout} when Type==fsm->
                    Pid ! {Ref, {ok, self()}},
                    gen_fsm:enter_loop(Module, [], StateName, StateData, Timeout);
                {stop, Reason} ->
                    Pid ! {Ref, {error, Reason}};
                ignore ->
                    Pid ! {Ref, ignore};
                {'EXIT', Error} ->
                    Pid ! {Ref, {error, Error}};
                Error ->
                    Pid ! {Ref, {error, Error}}
            end;
        {false, CurrentPid} ->
            Pid ! {Ref, {error, {already_started, CurrentPid}}}
    end,
    ok.


%% ===================================================================
%% EUnit tests
%% ===================================================================


-ifdef(TEST).
-compile({no_auto_import, [put/2]}).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {setup, 
        fun() -> 
            ?debugFmt("Starting ~p", [?MODULE]),
            case whereis(?MODULE) of
                undefined -> 
                    {ok, _} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
                    do_stop;
                _ -> 
                    ok
            end
        end,
        fun(Stop) -> 
            case Stop of 
                do_stop -> exit(whereis(?MODULE), kill);
                ok -> ok
            end
        end,
        [
            fun test_put/0,
            fun test_reg/0
        ]
    }.


test_put() ->
    Self = self(),
    del_all(),
    timer:sleep(100),
    [] = values(name),
    ok = wait_del(name),
    spawn(fun() -> {ok, {v1, Self}} = wait_put(name), Self ! ok1 end),
    spawn(fun() -> {ok, {v1, Self}} = wait_put(name), Self ! ok1 end),
    timer:sleep(100),   % Force to enter the wait_put before
    spawn(fun() -> {ok, {v1, Self}} = wait_put(name), Self ! ok1 end),
    put(name, v1),
    put(name2),
    receive ok1 -> ok after 1000 -> error(put_test) end,
    receive ok1 -> ok after 1000 -> error(put_test) end,
    receive ok1 -> ok after 1000 -> error(put_test) end,
    [{v1, Self}] = values(name),
    put(name, v2),
    put(name, v3),
    timer:sleep(100),
    [{v3, Self}] = values(name),
    spawn(fun() -> ok = wait_del(name), Self ! ok2 end),
    spawn(fun() -> ok = wait_del(name), Self ! ok2 end),
    timer:sleep(100),
    spawn(fun() -> ok = wait_del(name), Self ! ok2 end),
    del(name),
    receive ok2 -> ok after 1000 -> error(put_test) end,
    receive ok2 -> ok after 1000 -> error(put_test) end,
    receive ok2 -> ok after 1000 -> error(put_test) end,
    [] = values(name),
    put(name, v2),
    Pid1 = spawn(fun() -> put(name, v3), timer:sleep(10000) end),
    timer:sleep(100),
    [{v2, Self}, {v3, Pid1}] = lists:sort(values(name)),
    del(name),
    timer:sleep(100),
    [{v3, Pid1}] = values(name),
    exit(Pid1, kill),
    timer:sleep(100),
    [] = values(name),
    put(name, v4),
    Pid2 = spawn(fun() -> put(name, v5), timer:sleep(10000) end),
    timer:sleep(100),
    [{v4, Self}, {v5, Pid2}] = lists:sort(values(name)),
    del(name, Pid2),
    timer:sleep(100),
    [{v4, Self}] = values(name),
    del(name),
    del(name2),
    ok = wait_del(name),
    ok = wait_del(name2),
    [] = ets:lookup(nksip_proc_store, name),
    [] = ets:lookup(nksip_proc_store, name2),
    [] = ets:lookup(nksip_proc_pids, name),
    [] = ets:lookup(nksip_proc_pids, name2),
    ok.

test_reg() ->
    Self = self(),
    del_all(),
    timer:sleep(100),

    true = reg(name, v1),
    [{v1, Self}] = values(name),
    {false, Self} = reg(name, v2),
    [{v1, Self}] = values(name),
    del(name),
    ok = wait_del(name),
    true = reg(name),
    [{undefined, Self}] = values(name),
    {ok, {undefined, Self}} = wait_put(name),
    del(name),
    ok = wait_del(name),
    Pid1 = spawn(fun() -> true = reg(name) end),
    {ok, {undefined, Pid1}} = wait_put(name),
    ok = wait_del(name),

    spawn(fun() -> true = reg(name), timer:sleep(100) end),
    timer:sleep(50),
    true = reg(name, v3, Self, 5000),
    [{v3, Self}] = values(name),
    SF = fun(Val) -> 
        true = reg(name, Val, self(), 5000), 
        Self ! Val, 
        timer:sleep(100)
    end,
    spawn(fun() -> SF(f1) end),
    timer:sleep(50),
    spawn(fun() -> SF(f2) end),
    timer:sleep(50),
    spawn(fun() -> SF(f3) end),
    timer:sleep(50),
    del(name),

    receive f1 -> ok after 1000 -> error(put_test) end,
    [{f1, _}] = values(name),
    receive f2 -> ok after 1000 -> error(put_test) end,
    [{f2, _}] = values(name),
    receive f3 -> ok after 1000 -> error(put_test) end,
    [{f3, _}] = values(name),
    ok = wait_del(name),
    [] = ets:lookup(nksip_proc_store, name),
    [] = ets:lookup(nksip_proc_pids, name),
    ok.

-endif.


