%%% Copyright 2011 Eirini Arvaniti (eirinibob@gmail.com)
%%%            and Kostis Sagonas (kostis@cs.ntua.gr)
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @author Eirini Arvaniti <eirinibob@gmail.com>
%%% @copyright 2011 Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @doc This module contains functions for testing stateful systems

-module(proper_statem).
-export([commands/1, commands/2, parallel_commands/1, parallel_commands/2,
	 more_commands/2]).
-export([run_commands/2, run_commands/3, run_parallel_commands/2,
	 run_parallel_commands/3]).
-export([state_after/2, command_names/1, zip/2]).

-include("proper_internal.hrl").

-define(WORKERS, 2).
-define(LIMIT, 12).


%% -----------------------------------------------------------------------------
%% Exported only for testing purposes
%% -----------------------------------------------------------------------------

-export([index/2, all_insertions/3, insert_all/2]).
-export([is_valid/4]).
-export([get_next/6, mk_first_comb/3, fix_gen/8, mk_dict/2]).
-export([is_parallel/4, execute/4, check/7, run_sequential/5,
	 get_initial_state/1, safe_eval_init/2]).


%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type symbolic_state() :: term().
-type dynamic_state() :: term().

-type symb_var() :: {'var',proper_symb:var_id()}.
-type symb_call() :: {'call',mod_name(),fun_name(),[term()]}.
-type command() :: {'init',symbolic_state()}
		   | {'set',symb_var(),symb_call()}.

-type command_list() :: [command()].
-type parallel_test_case() :: {command_list(),[command_list()]}.
-type command_history() :: [{command(),term()}].
-type history() :: [{dynamic_state(),term()}].

%% TODO: import these from proper.erl
-type exc_kind() :: 'throw' | 'error' | 'exit'.
-type exc_reason() :: term().
-type stacktrace() :: [{atom(),atom(),arity() | [term()]}].
-type exception() ::  {'exception',exc_kind(),exc_reason(),stacktrace()}.
-type statem_result() :: 'ok'
			 | 'initialization_error'
			 | {'precondition', boolean() | exception()}
			 | {'postcondition', boolean() | exception()}
			 | exception()
			 | 'no_possible_interleaving'.

-type combination() :: [{pos_integer(),[pos_integer()]}].
-type lookup() :: orddict:orddict().


%% -----------------------------------------------------------------------------
%% Sequential command generation
%% -----------------------------------------------------------------------------

-define(COMMANDS(PropList), proper_types:new_type(PropList, commands)).

-spec commands(mod_name()) -> proper_types:type().
commands(Module) ->
    ?COMMANDS(
       [{generator,
	 fun(Size) -> gen_commands(Size, Module, Module:initial_state(), false) end},
	{is_instance, fun commands_test/1},
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, [fun(Cmds, T, S) -> split_shrinker(Module, Cmds, T, S) end,
		     fun(Cmds, T, S) -> remove_shrinker(Module, Cmds, T, S) end]}]).

-spec commands(mod_name(), symbolic_state()) -> proper_types:type().
commands(Module, InitialState) ->
    ?COMMANDS(
       [{generator, fun(Size) -> gen_commands(Size, Module, InitialState, true) end},
	{is_instance, fun commands_test/1},
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, [fun(Cmds, T, S) -> split_shrinker(Module, Cmds, T, S) end,
		     fun(Cmds, T, S) -> remove_shrinker(Module, Cmds, T, S) end]}]).

-spec more_commands(pos_integer(), proper_types:type()) -> proper_types:type().
more_commands(N, Type) ->
    ?SIZED(Size, proper_types:resize(Size * N, Type)).

-spec gen_commands(size(), mod_name(), symbolic_state(), boolean()) ->
			  command_list().
gen_commands(Size, Mod, InitialState, InitFlag) ->
    Len = proper_arith:rand_int(0, Size),
    erlang:put('$initial_state', InitialState),
    case gen_commands(Mod, InitialState, [], Len, Len) of
	CmdList when is_list(CmdList) ->
	    case InitFlag of
		true -> [{init,InitialState}|CmdList];
		false -> CmdList
	    end;
	{error,cant_generate} -> throw('$cant_generate')
    end.
  
-spec gen_commands(mod_name(), symbolic_state(), command_list(), size(),
		   non_neg_integer()) -> command_list() | {'error','cant_generate'}.
gen_commands(_, _, Commands, _, 0) ->
    lists:reverse(Commands);
gen_commands(Module, State, Commands, Len, Count) ->
    Call = ?SUCHTHAT(X, Module:command(State), Module:precondition(State, X)),
    case proper_gen:clean_instance(proper_gen:safe_generate(Call)) of
	{ok,Instance} ->
	    Var = {var, Len-Count+1},
	    Command = {set, Var, Instance},
	    NextState = Module:next_state(State, Var, Instance),
	    gen_commands(Module, NextState, [Command|Commands], Len, Count-1);
	{error,cant_generate} = Error -> Error
    end.


%% -----------------------------------------------------------------------------
%% Parallel command generation
%% -----------------------------------------------------------------------------

-spec parallel_commands(mod_name()) -> proper_types:type().		       
parallel_commands(Mod) ->
    ?COMMANDS(
       [{generator, 
	 fun(Size) ->
		 gen_parallel_commands(Size + ?WORKERS, Mod,
				       Mod:initial_state(), false)
	 end},
	{is_instance, fun parallel_commands_test/1},
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, 
	 lists:map(fun(I) ->
			   fun(Parallel_Cmds, T, S) ->
				   split_parallel_shrinker(I, Mod, Parallel_Cmds, T, S)
			   end
		   end, lists:seq(1, ?WORKERS)) ++
	 lists:map(fun(I) ->
	 		   fun(Parallel_Cmds, T, S) ->
	 			   remove_parallel_shrinker(I, Mod, Parallel_Cmds, T, S)
	 		   end
	 	   end, lists:seq(1, ?WORKERS)) ++
	 [fun(Parallel_Cmds, T, S) ->
	 	  split_seq_shrinker(Mod, Parallel_Cmds, T, S) end,
	  fun(Parallel_Cmds, T, S) ->
	  	  remove_seq_shrinker(Mod, Parallel_Cmds, T, S) end,
	  fun move_shrinker/3
	 ]}
       ]).

-spec parallel_commands(mod_name(), symbolic_state()) -> proper_types:type(). 
parallel_commands(Mod, InitialState) ->
    ?COMMANDS(
       [{generator, 
	 fun(Size) ->
		 gen_parallel_commands(Size + ?WORKERS, Mod, InitialState, true)
	 end},
	{is_instance, fun parallel_commands_test/1},
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, 
	 lists:map(fun(I) ->
			   fun(Parallel_Cmds, T, S) ->
				   split_parallel_shrinker(I, Mod, Parallel_Cmds, T, S)
			   end
		   end, lists:seq(1, ?WORKERS)) ++
	 lists:map(fun(I) ->
	 		   fun(Parallel_Cmds, T, S) ->
	 			   remove_parallel_shrinker(I, Mod, Parallel_Cmds, T, S)
	 		   end
	 	   end, lists:seq(1, ?WORKERS)) ++
	 [fun(Parallel_Cmds, T, S) ->
	 	  split_seq_shrinker(Mod, Parallel_Cmds, T, S) end,
	  fun(Parallel_Cmds, T, S) ->
	  	  remove_seq_shrinker(Mod, Parallel_Cmds, T, S) end,
	  fun move_shrinker/3
	 ]}
       ]).

-spec gen_parallel_commands(size(), mod_name(), symbolic_state(), boolean()) ->
				   parallel_test_case().
gen_parallel_commands(Size, Mod, InitialState, InitFlag) ->
    erlang:put('$initial_state', InitialState),
    {Sequential,Parallel} = gen_parallel(Mod, InitialState, Size),
    case InitFlag of
	true -> {[{init, InitialState}|Sequential], Parallel};
	false -> {Sequential,Parallel}
    end.

-spec gen_parallel(mod_name(), symbolic_state(), size()) -> parallel_test_case().
gen_parallel(Mod, InitialState, Size) ->
    Len = proper_arith:rand_int(?WORKERS, Size),
    CmdList = gen_commands(Mod, InitialState, [], Len, Len),
    {LenPar, {Seq, P}} =
	if Len =< ?LIMIT -> 
		RandLen = proper_arith:rand_int(?WORKERS, Len),
		{RandLen, lists:split(Len - RandLen, CmdList)};
	   Len > ?LIMIT ->
		{?LIMIT, lists:split(Len - ?LIMIT, CmdList)}
	end,
    State = state_after(Mod, Seq),
    Env = mk_env(Seq, 1),
    Len2 = LenPar div ?WORKERS,
    Comb = mk_first_comb(LenPar, Len2, ?WORKERS),
    LookUp = orddict:from_list(mk_dict(P,1)),
    {Seq, fix_gen(LenPar, Len2, Comb, LookUp, Mod, State, Env, ?WORKERS)}.

-spec fix_gen(pos_integer(), non_neg_integer(), combination() | 'done', lookup(),
	      mod_name(), symbolic_state(), [symb_var()], pos_integer()) ->
		     [command_list()].
fix_gen(_, 0, done, _, _, _, _, _) -> exit(error);   %% not supposed to reach here
fix_gen(MaxIndex, Len, done, LookUp, Mod, State, Env, W) ->
    Comb = mk_first_comb(MaxIndex, Len-1, W),
    case Len of
	1 -> io:format("f");
	_ -> ok
    end,
    fix_gen(MaxIndex, Len-1, Comb , LookUp, Mod, State, Env, W);	     
fix_gen(MaxIndex, Len, Comb, LookUp, Mod, State, Env, W) ->
    Cs = get_commands(Comb, LookUp),
    case is_parallel(Cs, Mod, State, Env) of
	true ->
	    Cs;
	false ->
	    C1 = proplists:get_value(1, Comb),
	    C2 = proplists:get_value(2, Comb),
	    Next = get_next(Comb, Len, MaxIndex, lists:sort(C1 ++ C2), W, 2),
	    fix_gen(MaxIndex, Len, Next, LookUp, Mod, State, Env, W)
    end.

-spec is_parallel([command_list()], mod_name(), symbolic_state(), [symb_var()]) -> 
			 boolean().
is_parallel(Cs, Mod, State, Env) ->
    case lists:all(fun(C) -> is_valid(Mod, State, C, Env) end, Cs) of
	true -> 
	    can_parallelize(Cs, Mod, State, Env);	
	false -> 
	    false
    end.	   

%%TODO: produce possible interleavings in a lazy way
-spec can_parallelize([command_list()], mod_name(), symbolic_state(),
		      [symb_var()]) -> boolean().
can_parallelize(Cs, Mod, S, Env) ->
    Val = fun (C) -> is_valid(Mod, S, C, Env) end,
    lists:all(Val, possible_interleavings(Cs)).


%% -----------------------------------------------------------------------------
%% Sequential command execution
%% -----------------------------------------------------------------------------

-spec run_commands(mod_name(), command_list()) ->
			  {history(),dynamic_state(),statem_result()}.
run_commands(Module, Cmds) ->
    run_commands(Module, Cmds, []).

-spec run_commands(mod_name(), command_list(), proper_symb:var_values()) ->
			  {history(),dynamic_state(),statem_result()}.
run_commands(Module, Commands, Env) ->
    InitialState = get_initial_state(Commands),
    case safe_eval_init(Env, InitialState) of
	{ok,DynState} ->
	    do_run_command(Commands, Env, Module, [], DynState);
	{error,Reason} ->
	    {[], [], Reason}
    end.

-spec safe_eval_init(proper_symb:var_values(), symbolic_state()) ->
			    {'ok',dynamic_state()} | {'error',statem_result()}.
safe_eval_init(Env, SymbState) ->
    try proper_symb:eval(Env, SymbState) of
	DynState ->
	    {ok,DynState}
    catch
	_Exception:_Reason ->
	    {error,initialization_error}
    end.

-spec do_run_command(command_list(), proper_symb:var_values(), mod_name(),
		     history(), dynamic_state()) ->
			    {history(),dynamic_state(),statem_result()}.
do_run_command(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {lists:reverse(History), State, ok};
	[{init,_S}|Rest] ->
	    do_run_command(Rest, Env, Module, History, State);
	[{set, {var,V}, {call,M,F,A}}|Rest] ->
	    M2 = proper_symb:eval(Env, M), 
	    F2 = proper_symb:eval(Env, F), 
	    A2 = proper_symb:eval(Env, A),
	    Call = {call,M2,F2,A2},
	    case check_precondition(Module, State, Call) of
		true ->
		    case safe_apply(M2, F2, A2) of
			{ok,Res} ->
			    State2 = Module:next_state(State, Res, Call),
			    History2 = [{State,Res}|History],
			    case check_postcondition(Module, State, Call, Res) of
				true ->
				    Env2 = [{V,Res}|Env],
				    do_run_command(Rest, Env2, Module,
						   History2, State2);
				false ->
				    {lists:reverse(History2), State2,
				     {postcondition,false}};
				{exception,_,_,_} = Exception ->
				    {lists:reverse(History2), State2,
				     {postcondition,Exception}}
			    end;
			{error,Exception} ->
			    {lists:reverse(History),State,Exception}
		    end;
		false ->
		    {lists:reverse(History),State,{precondition,false}};
		{exception,_,_,_} = Exception ->
		    {lists:reverse(History),State,{precondition,Exception}}
	    end
    end.

-spec check_precondition(mod_name(), dynamic_state(), symb_call()) ->
				boolean() | exception().
check_precondition(Module, State, Call) ->
    try Module:precondition(State, Call) of
	Result -> Result
    catch
	Kind:Reason ->
	    {exception,Kind,Reason,erlang:get_stacktrace()}
    end.

-spec check_postcondition(mod_name(), dynamic_state(), symb_call(), term()) ->
				 boolean() | exception().
check_postcondition(Module, State, Call, Res) ->
    try Module:postcondition(State, Call, Res) of
	Result -> Result
    catch
	Kind:Reason ->
	    {exception,Kind,Reason,erlang:get_stacktrace()}
    end.

-spec safe_apply(mod_name(), fun_name(), [term()]) ->
			{ok,term()} | {error,exception()}.
safe_apply(M, F, A) ->    
    try apply(M, F, A) of
	Result -> {ok,Result}
    catch
	Kind:Reason ->
	    {error,{exception,Kind,Reason,erlang:get_stacktrace()}}
    end.


%% -----------------------------------------------------------------------------
%% Parallel command execution
%% -----------------------------------------------------------------------------


-spec run_parallel_commands(mod_name(), parallel_test_case()) ->
				   {command_history(),[command_history()],statem_result()}.
run_parallel_commands(Module, {_Sequential, _Parallel} = Cmds) ->
    run_parallel_commands(Module, Cmds, []).

-spec run_parallel_commands(mod_name(), parallel_test_case(), proper_symb:var_values()) ->
				   {command_history(),[command_history()],statem_result()}.
run_parallel_commands(Module, {Sequential, Parallel}, Env) ->
    InitialState = get_initial_state(Sequential),
    case safe_eval_init(Env, InitialState) of
	{ok, DynState} ->
	    {{Seq_history, State, ok}, Env1} =
		run_sequential(Sequential, Env, Module, [], DynState), 
	    F = fun(T) -> execute(T, Env1, Module, []) end,
	    Parallel_history = pmap(F, Parallel),
	    case check(Module, State, Env1, Env1, [],
		       Parallel_history, []) of
		true ->  
		    {Seq_history, Parallel_history, ok};
		false ->
		    {Seq_history, Parallel_history,
		     no_possible_interleaving}
	    end;
	{error, Reason} ->
	    {[], [], Reason}
    end.

-spec pmap(fun((command_list()) -> command_history()), [command_list()]) ->
       [command_history()].
pmap(F, L) ->
    await(lists:reverse(spawn_jobs(F,L))).

-spec spawn_jobs(fun((command_list()) -> command_history()), [command_list()]) -> [pid()].
spawn_jobs(F, L) ->
    Parent = self(),
    [proper:spawn_link_migrate(fun() -> Parent ! {self(),catch {ok,F(X)}} end) || X <- L].

-spec await([pid()]) -> [command_history()].
await(Pids) ->
    await_tr(Pids, []).

-spec await_tr([pid()], [command_history()]) -> [command_history()].
await_tr([], Accum) -> Accum;
await_tr([H|T], Accum) ->
    receive
	{H, {ok, Res}} -> await_tr(T, [Res|Accum]);
	{H, {'EXIT',_} = Err} ->
	    _ = [exit(Pid,kill) || Pid <- T],
	    _ = [receive {P,_} -> d_ after 0 -> i_ end || P <- T],
	    %%io:format("Error ~w during parallel execution~n", [Err]),
	    erlang:error(Err)
    end.

-spec check(mod_name(), dynamic_state(), proper_symb:var_values(), 
	    proper_symb:var_values(), [command_history()], [command_history()],
	    command_history()) -> boolean().
check(_Mod, _State, _OldEnv, _Env, [], [], _Accum) ->
    true;
check(_Mod, _State, Env, Env, _Tried, [], _Accum) ->
    false;
check(Mod, State, _OldEnv, Env, Tried, [], Accum) ->
    check(Mod, State, Env, Env, [], Tried, Accum);
check(Mod, State, OldEnv, Env, Tried, [P|Rest], Accum) ->
    case P of 
	[] ->
	    check(Mod, State, OldEnv, Env, Tried, Rest, Accum);
	[H|Tail] ->
	    {{set, {var, N1}, {call, M1, F1, A1}}, Res1} = H,
	    M1_ = proper_symb:eval(Env, M1), 
	    F1_ = proper_symb:eval(Env, F1), 
	    A1_ = proper_symb:eval(Env, A1),
	    Call1 = {call, M1_, F1_, A1_},
	    case Mod:postcondition(State, Call1, Res1) of
		true -> 
		    Env2 = [{N1, Res1}|Env],
		    NextState = Mod:next_state(State, Res1, Call1),
		    V = check(Mod, NextState, OldEnv, Env2, [Tail|Tried], Rest, [H|Accum]),
		    case V of
			true ->
			    true;
			false ->
			    check(Mod, State, OldEnv, Env, [P|Tried], Rest, Accum)
		    end;
		false ->
		    check(Mod, State, OldEnv, Env, [P|Tried], Rest, Accum)
	    end
    end.

-spec run_sequential(command_list(), proper_symb:var_values(), mod_name(),
		     history(), dynamic_state()) ->
       {{command_history(), dynamic_state(), statem_result()}, proper_symb:var_values()}.
run_sequential(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {{lists:reverse(History), State, ok}, Env};
	[{init, _S}|Rest] ->
	    run_sequential(Rest, Env, Module, History, State);
   	[{set, {var,V}, {call,M,F,A}} = Cmd|Rest] ->
	    M2 = proper_symb:eval(Env, M), 
	    F2 = proper_symb:eval(Env, F),
	    A2 = proper_symb:eval(Env, A),
	    Call = {call, M2, F2, A2},
	    true = Module:precondition(State, Call),
	    Res = apply(M2, F2, A2),
	    true = Module:postcondition(State, Call, Res),
	    Env2 = [{V,Res}|Env],
	    State2 = Module:next_state(State, Res, Call),
	    History2 = [{Cmd,Res}|History],
	    run_sequential(Rest, Env2, Module, History2, State2)
    end.

-spec execute(command_list(), proper_symb:var_values(), mod_name(), command_history()) -> 
		     command_history(). 
execute(Commands, Env, Module, History) ->
    case Commands of
	[] -> 
	    lists:reverse(History);
	[{set, {var,V}, {call,M,F,A}} = Cmd|Rest] ->
	    M2 = proper_symb:eval(Env, M), 
	    F2 = proper_symb:eval(Env, F), 
	    A2 = proper_symb:eval(Env, A),
	    Res = apply(M2, F2, A2),
	    Env2 = [{V,Res}|Env],
	    History2 = [{Cmd,Res}|History],
	    execute(Rest, Env2, Module, History2)
    end.


%% -----------------------------------------------------------------------------
%% Command shrinkers
%% -----------------------------------------------------------------------------

-spec split_shrinker(mod_name(), command_list(), proper_types:type(),
		     proper_shrink:state()) ->
			    {[command_list()],proper_shrink:state()}.
split_shrinker(Module, [{init,InitialState}|Commands], Type, State) ->
    {Slices,NewState} = proper_shrink:split_shrinker(Commands, Type, State),
    {[[{init, InitialState}|X] || X <- Slices,
				  is_valid(Module, InitialState, X, [])],
     NewState};

split_shrinker(Module, Commands, Type, State) ->
    {Slices,NewState} =  proper_shrink:split_shrinker(Commands, Type, State),
    InitialState = get_initial_state(Commands),
    {[X || X <- Slices, is_valid(Module, InitialState, X, [])],
     NewState}.

-spec remove_shrinker(mod_name(), command_list(), proper_types:type(),
		      proper_shrink:state()) ->
			     {[command_list()],proper_shrink:state()}.
remove_shrinker(Module, [{init,InitialState}|Commands] = Cmds, Type, State) ->
    {Slices,NewState} = proper_shrink:remove_shrinker(Commands, Type, State),
    case Slices of
	[] -> 
	    {[],NewState};
	_ ->
	    L = [[{init,InitialState}|S] || S <- Slices, 
					    is_valid(Module, InitialState, S, [])],
	    case L of
		[] -> remove_shrinker(Module, Cmds, Type, NewState);
		_ -> {L, NewState}
	    end
    end;

remove_shrinker(Module, Commands, Type, State) ->
    {Slices,NewState} = proper_shrink:remove_shrinker(Commands,Type,State),
    InitialState = get_initial_state(Commands),
    case Slices of
	[] -> 
	    {[],NewState};
	_ ->
	    L = [S || S <- Slices, is_valid(Module, InitialState, S, [])],
	    case L of
		[] -> remove_shrinker(Module, Commands, Type, NewState);
		_ -> {L, NewState}
	    end
    end.

-spec split_parallel_shrinker(pos_integer(), mod_name(), parallel_test_case(), 
			      proper_types:type(), proper_shrink:state()) ->
				     {[parallel_test_case()],proper_shrink:state()}. 
split_parallel_shrinker(I, Module, {Sequential,Parallel}, Type, State) ->
    SeqEnv = mk_env(Sequential, 1),
    SymbState = state_after(Module, Sequential),
    {Slices, NewState} =
	proper_shrink:split_shrinker(lists:nth(I, Parallel), Type, State), 
    {[{Sequential, update_list(I, S, Parallel)}
      || S <- Slices, is_valid(Module, SymbState, S, SeqEnv)],
     NewState}.

-spec remove_parallel_shrinker(pos_integer(), mod_name(), parallel_test_case(),
			       proper_types:type(), proper_shrink:state()) ->
				      {[parallel_test_case()],proper_shrink:state()}.
remove_parallel_shrinker(I, Module, {Sequential,Parallel} = SP, Type, State) ->
    P = lists:nth(I, Parallel),
    SeqEnv = mk_env(Sequential, 1),
    SymbState = state_after(Module, Sequential),
    {Slices,NewState} = proper_shrink:remove_shrinker(P, Type, State),
    case Slices of
	[] -> 
	    {[], done};
	_ ->
	    L = [{Sequential, update_list(I, S, Parallel)}
		 || S <- Slices, is_valid(Module, SymbState, S, SeqEnv)],
	    case L of
		[] -> remove_parallel_shrinker(I, Module, SP, Type, NewState);
		_ -> {L,NewState}
	    end
    end.

-spec split_seq_shrinker(mod_name(), parallel_test_case(), proper_types:type(),
			 proper_shrink:state()) ->
				{[parallel_test_case()],proper_shrink:state()}.
split_seq_shrinker(Module, {Sequential,Parallel}, Type, State) ->
    {Slices,NewState} = split_shrinker(Module, Sequential, Type, State),
    SymbState = get_initial_state(Sequential),
    {[{S, Parallel} || S <- Slices,
		       lists:all(fun(P) -> is_valid(Module, SymbState, S ++ P, []) end,
				 Parallel)],
     NewState}.

-spec remove_seq_shrinker(mod_name(), parallel_test_case(), proper_types:type(),
			  proper_shrink:state()) ->
				 {[parallel_test_case()],proper_shrink:state()}.
remove_seq_shrinker(Module, {Sequential,Parallel} = SP, Type, State) ->
    {Slices,NewState} = remove_shrinker(Module, Sequential, Type, State),
    SymbState = get_initial_state(Sequential),
    case Slices of
	[] ->
	    {[],NewState};
	_ ->
	    L = [{S, Parallel}
		 || S <- Slices,
		    lists:all(fun(P) -> is_valid(Module, SymbState, S ++ P, []) end,
			      Parallel)],
	    case L of
		[] -> remove_seq_shrinker(Module, SP, Type, NewState);
		_ -> {L, NewState}
	    end
    end. 

-spec move_shrinker(parallel_test_case(), proper_types:type(), proper_shrink:state()) ->
			   {[parallel_test_case()],proper_shrink:state()}.
move_shrinker(Instance, Type, init) ->
    move_shrinker(Instance, Type, {move,1});
move_shrinker({Sequential, Parallel}, _Type, {move,1}) ->
     case get_first_commands(Parallel) of
	[] -> 
	    {[], done};
	List ->
	     {[{Sequential ++ [H], remove_first_command(Index, Parallel)}
		    || {H, Index} <- List], {move,2}}
     end;
move_shrinker(_, _, {move,2}) ->
    {[], done};
move_shrinker(Instance, Type, {shrunk,_Pos,{move,2}}) ->
    move_shrinker(Instance, Type, {move,1}).


%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

-spec command_names(command_list()) -> [mfa()].
command_names(Cmds) ->
    [{M, F, length(Args)} || {set, _Var, {call,M,F,Args}} <- Cmds].

-spec state_after(mod_name(), command_list()) -> symbolic_state().
state_after(Module, Commands) ->
    NextState = fun(S, V, C) -> Module:next_state(S, V, C) end,
    lists:foldl(fun({init,S}, _) -> S;
		   ({set,Var,Call}, S) -> NextState(S, Var, Call) end,
		get_initial_state(Commands),
		Commands).

-spec zip([A], [B]) -> [{A,B}].
zip(X, Y) ->
    zip(X, Y, []).

-spec zip([A], [B], [{A,B}]) -> [{A,B}].
zip([], _, Accum) -> lists:reverse(Accum);
zip(_, [], Accum) -> lists:reverse(Accum);
zip([X|Tail1], [Y|Tail2], Accum) ->
    zip(Tail1, Tail2, [{X,Y}|Accum]).

-spec is_valid(mod_name(), symbolic_state(), command_list(), [symb_var()]) ->
		      boolean().
is_valid(_Mod, _State, [], _Env) -> true;
is_valid(Module, _State, [{init,S}|Commands], _Env) ->
    is_valid(Module, S, Commands, _Env);
is_valid(Module, State, [{set,Var,{call,_M,_F,A}=Call}|Commands], Env) ->
    case Module:precondition(State, Call) of
	true ->
	    case args_defined(A, Env) of
		true ->
		    NextState = Module:next_state(State, Var, Call),
		    is_valid(Module, NextState, Commands, [Var|Env]);
		false -> false
	    end;
	false -> false
    end.

-spec args_defined([term()], [symb_var()]) -> boolean().
args_defined(A, Env) ->
    lists:all(fun ({var,I} = V) when is_integer(I) -> lists:member(V, Env);
		  (_V) -> true 
	      end, A).      

-spec get_initial_state(command_list()) -> symbolic_state().
get_initial_state(Cmds) ->
    case Cmds of
	[{init,S}|_] -> S;
	_ -> erlang:get('$initial_state')
    end.

-spec commands_test(proper_gen:imm_instance()) -> boolean().
commands_test(X) when is_list(X) ->
    lists:all(fun is_command/1, X);
commands_test(_X) -> false.

-spec parallel_commands_test(proper_gen:imm_instance()) -> boolean().
parallel_commands_test({S,P}) ->
    commands_test(S) andalso
	lists:foldl(fun(X, Accum) -> commands_test(X) andalso Accum end, true, P);
parallel_commands_test(_) -> false.

-spec is_command(proper_gen:imm_instance()) -> boolean().
is_command({set,{var,V},{call,M,F,A}})
  when is_integer(V), is_atom(M), is_atom(F), is_list(A) -> true;
is_command({init,_S}) -> true;
is_command(_Other) -> false.

-spec possible_interleavings([command_list()]) -> [command_list()].
possible_interleavings([P1,P2]) ->
    insert_all(P1, P2);
possible_interleavings([P1|Rest]) ->
    [I || L <- possible_interleavings(Rest),
	  I <- insert_all(P1, L)].

%% Returns all possible insertions of the elements of the first list,
%% preserving their order, inside the second list, i.e. all possible
%% command interleavings between two parallel processes

-spec insert_all([term()], [term()]) -> [[term()]].
insert_all([], List) ->
    [List];
insert_all([X], List) ->
    all_insertions(X, length(List) + 1, List);

insert_all([X|[Y|Rest]], List) ->
    [L2 || L1 <- insert_all([Y|Rest], List), 
	   L2 <- all_insertions(X,index(Y,L1),L1)].

-spec all_insertions(term(), pos_integer(), [term()]) -> [[term()]].
all_insertions(X, Limit, List) ->
    all_insertions_tr(X, Limit, 0, [], List, []).

-spec all_insertions_tr(term(), pos_integer(), non_neg_integer(),
			[term()], [term()], [[term()]]) -> [[term()]].
all_insertions_tr(X, Limit, LengthFront, Front, [], Acc) ->
    case LengthFront < Limit of
	true ->
	    [Front ++ [X] | Acc];
	false ->
	    Acc
    end;
all_insertions_tr(X, Limit, LengthFront, Front, Back = [BackHead|BackTail], Acc) ->
    case LengthFront < Limit of
	true ->
	    all_insertions_tr(X, Limit, LengthFront+1, Front ++ [BackHead], BackTail,
			      [Front ++ [X] ++ Back | Acc]);
	false -> Acc     
    end.

-spec index(term(), [term(),...]) -> pos_integer().
index(X, List) ->
    index(X, List, 1).

-spec index(term(), [term(),...], pos_integer()) -> pos_integer().
index(X, [X|_], N) -> N;
index(X, [_|Rest], N) -> index(X, Rest, N+1).

-spec mk_env(command_list(), pos_integer()) -> [{'var', pos_integer()}].
mk_env([], _) -> [];
mk_env([_|T], N) -> [{var,N}|mk_env(T, N+1)].

-spec mk_dict(command_list(), pos_integer()) -> [{pos_integer(), command()}].
mk_dict([], _) -> [];
mk_dict([H|T], N) -> [{N,H}|mk_dict(T, N+1)].

-spec mk_first_comb(pos_integer(), non_neg_integer(), pos_integer()) -> 
			   combination().
mk_first_comb(N, Len, W) ->
    mk_first_comb_tr(1, N, Len, [], W).

-spec mk_first_comb_tr(pos_integer(), pos_integer(), non_neg_integer(),
		       combination(), pos_integer()) -> 
			      combination().
mk_first_comb_tr(Start, N, _Len, Accum, 1) ->
    [{1,lists:seq(Start, N)}|Accum];
mk_first_comb_tr(Start, N, Len, Accum, W) ->
    K = Start + Len,
    mk_first_comb_tr(K, N, Len, [{W,lists:seq(Start, K-1)}|Accum], W-1).

-spec get_commands_inner([pos_integer()], lookup()) -> command_list().
get_commands_inner(Indexes, LookUp) ->
    lists:map(fun(Index) -> orddict:fetch(Index, LookUp) end, Indexes).

-spec get_commands(combination(), lookup()) -> [command_list()].
get_commands(PropList, LookUp) ->
    lists:map(fun({_,W}) -> get_commands_inner(W, LookUp) end, PropList).

-spec get_next(combination(), non_neg_integer(), pos_integer(),
	       [pos_integer()], pos_integer(), pos_integer()) ->
		      combination() | 'done'.
get_next(L, _Len, _MaxIndex, Available, _Workers, 1) ->
    [{1,Available}|proplists:delete(1, L)];
get_next(L, Len, MaxIndex, Available, Workers, N) ->
    C = case proplists:is_defined(N, L) of
	    true ->
		next_comb(MaxIndex, proplists:get_value(N, L), Available);
	    false ->
		lists:sublist(Available, Len)
	end,
    case C of
	done -> 
	    if N =:= Workers -> 
		    done;
	       N =/= Workers -> 
		    C2 = proplists:get_value(N+1, L), 
		    NewList = [E || {M,_}=E <- L, M > N],
		    get_next(NewList, Len, MaxIndex, 
			     lists:sort(C2 ++ Available), Workers, N+1)
	    end;
	_ ->
	    get_next([{N,C}|proplists:delete(N, L)], 
		     Len, MaxIndex, Available -- C, Workers, N-1)
    end.

-spec next_comb(pos_integer(), [pos_integer()], [pos_integer()]) ->
			  [pos_integer()] | 'done'.
next_comb(MaxIndex, Comb, Available) ->
    Res = next_comb_tr(MaxIndex, lists:reverse(Comb), []),
    case is_well_defined(Res, Available) of
	true -> Res;
	false -> next_comb(MaxIndex, Res, Available)
    end.

-spec is_well_defined([pos_integer()] | 'done', [pos_integer()]) -> boolean().
is_well_defined(done, _) -> true;
is_well_defined(Comb, Available) ->	    
    lists:usort(Comb) =:= Comb andalso
	lists:all(fun(X) -> lists:member(X, Available) end, Comb).

-spec next_comb_tr(pos_integer(), [pos_integer()], [pos_integer()]) ->
			  [pos_integer()] | 'done'.
next_comb_tr(_MaxIndex, [], _Acc) ->
    done;
next_comb_tr(MaxIndex, [MaxIndex | Rest], Acc) ->
    next_comb_tr(MaxIndex, Rest, [1 | Acc]);
next_comb_tr(_MaxIndex, [X | Rest], Acc) ->
    lists:reverse(Rest) ++ [X+1] ++ Acc.			 

-spec update_list(pos_integer(), term(), [term(),...]) -> [term(),...].
update_list(I, X, List) ->
    update_list(I, X, List, [], 1).

-spec update_list(pos_integer(), term(), [term(),...], [term()], pos_integer()) -> 
			 [term(),...].
update_list(Index, X, [_H|T], Accum, Index) ->
    lists:reverse(Accum) ++ [X] ++ T;
update_list(Index, X, [H|T], Accum, N) ->
    update_list(Index, X, T, [H|Accum], N+1).

-spec get_first_commands([command_list()]) -> [{command(),pos_integer()}].
get_first_commands(List) ->
    get_first_commands(List, [], 1).

-spec get_first_commands([command_list()], [{command(),pos_integer()}], pos_integer()) ->
				[{command(),pos_integer()}].
get_first_commands([], Accum, _N) -> Accum;
get_first_commands([H|T], Accum, N) -> 
    case get_first_command(H) of
	none ->
	    get_first_commands(T, Accum, N+1);
	Cmd ->
	    get_first_commands(T, [{Cmd,N}|Accum], N+1)
    end.

-spec get_first_command(command_list()) -> command() | 'none'.
get_first_command([]) -> none;
get_first_command([H|_]) -> H.
    
-spec remove_first_command(pos_integer(), [command_list()]) -> [command_list()].
remove_first_command(I, List) ->
    remove_first_command(I, List, [], 1).

-spec remove_first_command(pos_integer(), [command_list(),...], [command_list()],
			   pos_integer()) -> [command_list(),...].
remove_first_command(Index, [H|T], Accum, Index) ->
    lists:reverse(Accum) ++ [tl(H)] ++ T;
remove_first_command(Index, [H|T], Accum, N) -> 
    remove_first_command(Index, T, [H|Accum], N+1).
