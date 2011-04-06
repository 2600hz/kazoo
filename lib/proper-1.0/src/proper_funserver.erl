%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
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

%%% @author Manolis Papadakis <manopapad@gmail.com>
%%% @copyright 2010 Manolis Papadakis and Kostis Sagonas
%%% @version {@version}
%%% @doc This module contains the server responsible for tge generation of
%%%	 random functions.
%%% @private

-module(proper_funserver).
-behaviour(gen_server).

-export([start/0, stop/0, reset/0, create_fun/3]).
-export([get_ret_type/1, function_body/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type fun_seed() :: {non_neg_integer(),non_neg_integer()}.
-type imm_ret_type() :: {'type',proper_types:type()} | {'bin',binary()}.
-record(state, {temp_mod     = new_temp_mod_name() :: mod_name(),
		forms        = []                  :: [abs_form()],
		next_fun_num = 1                   :: pos_integer()}).
-opaque state() :: #state{}.

-type server_call() :: 'reset'
		     | {'create_fun',arity(),proper_types:type(),fun_seed()}.
-type server_response() :: 'ok' | function().


%%------------------------------------------------------------------------------
%% Server interface functions
%%------------------------------------------------------------------------------

-spec start() -> 'ok'.
start() ->
    {ok,FunserverPid} = gen_server:start_link(?MODULE, dummy, []),
    put('$funserver_pid', FunserverPid),
    %% TODO: To make PropEr multi-threaded, this should be copied to each
    %%       spawned worker process.
    ok.

-spec stop() -> 'ok'.
stop() ->
    FunserverPid = get('$funserver_pid'),
    erase('$funserver_pid'),
    gen_server:cast(FunserverPid, stop).

-spec reset() -> 'ok'.
reset() ->
    FunserverPid = get('$funserver_pid'),
    gen_server:call(FunserverPid, reset).

-spec create_fun(arity(), proper_types:type(), fun_seed()) -> function().
create_fun(Arity, RetType, FunSeed) ->
    FunserverPid = get('$funserver_pid'),
    gen_server:call(FunserverPid, {create_fun,Arity,RetType,FunSeed}).


%%------------------------------------------------------------------------------
%% Implementation of gen_server interface
%%------------------------------------------------------------------------------

-spec init(_) -> {'ok',state()}.
init(_) ->
    {ok, #state{}}.

-spec handle_call(server_call(), _, state()) ->
	  {'reply',server_response(),state()}.
handle_call(reset, _From, State) ->
    %% TODO: To make PropEr multi-threaded, this should erase only those
    %%       functions that have been created for the calling process, then
    %%       reload temp_mod with all the remaining functions.
    remove_temp_mod(State),
    {reply, ok, #state{}};
handle_call({create_fun,Arity,RetType,FunSeed}, _From, State) ->
    {Fun,NewState} = create_fun(Arity, RetType, FunSeed, State),
    {reply, Fun, NewState}.

-spec handle_cast('stop', state()) -> {'stop','normal',state()}.
handle_cast(stop, State) ->
    {stop, normal, State}.

-spec handle_info(term(), state()) -> {'stop',{'received_info',term()},state()}.
handle_info(Info, State) ->
    {stop, {received_info,Info}, State}.

-spec terminate(term(), state()) -> 'ok'.
terminate(_Reason, State) ->
    %% TODO: Remove temp_mod here or in handle_cast(stop)?
    %% TODO: This may take a while to complete - is that OK?
    remove_temp_mod(State),
    ok.

-spec code_change(term(), state(), _) -> {'ok',state()}.
code_change(_OldVsn, State, _) ->
    {ok, State}.


%%------------------------------------------------------------------------------
%% Function-generation functions
%%------------------------------------------------------------------------------

-spec new_temp_mod_name() -> mod_name().
new_temp_mod_name() ->
    TempModStr = lists:flatten(io_lib:format("$temp_mod$~w$~w",[self(),now()])),
    list_to_atom([case C of $. -> $$; _ -> C end || C <- TempModStr]).

-spec create_fun(arity(), proper_types:type(), fun_seed(), state()) ->
	  {function(),state()}.
create_fun(0, RetType, FunSeed, State) ->
    {fun() -> ?MODULE:function_body([],{type,RetType},FunSeed) end, State};
create_fun(1, RetType, FunSeed, State) ->
    {fun(A) -> ?MODULE:function_body([A],{type,RetType},FunSeed) end, State};
create_fun(2, RetType, FunSeed, State) ->
    {fun(A,B) -> ?MODULE:function_body([A,B],{type,RetType},FunSeed) end,
     State};
create_fun(3, RetType, FunSeed, State) ->
    {fun(A,B,C) -> ?MODULE:function_body([A,B,C],{type,RetType},FunSeed) end,
     State};
create_fun(4, RetType, FunSeed, State) ->
    {fun(A,B,C,D) ->
	 ?MODULE:function_body([A,B,C,D],{type,RetType},FunSeed)
     end,
     State};
create_fun(Arity,RetType,FunSeed,#state{temp_mod = TempMod, forms = Forms,
					next_fun_num = NextFunNum} = State) ->
    FunName = list_to_atom("f" ++ integer_to_list(NextFunNum)),
    Fun = erlang:make_fun(TempMod, FunName, Arity),
    Args = [{var,0,list_to_atom("X" ++ integer_to_list(N))}
	    || N <- lists:seq(1,Arity)],
    ArgsList = lists:foldr(fun(X,Acc) -> {cons,0,X,Acc} end, {nil,0}, Args),
    Body = [{call, 0, {remote,0,{atom,0,?MODULE},{atom,0,function_body}},
	     [ArgsList,
	      erl_parse:abstract({bin,proper_types:to_binary(RetType)}),
	      erl_parse:abstract(FunSeed)]}],
    Form = {function,0,FunName,Arity,[{clause,0,Args,[],Body}]},
    NewState = State#state{forms = [Form|Forms], next_fun_num = NextFunNum + 1},
    reload_temp_mod(NewState),
    {Fun, NewState}.

-spec remove_temp_mod(state()) -> 'ok'.
remove_temp_mod(#state{temp_mod = TempMod}) ->
    case code:is_loaded(TempMod) of
	%% For this to work we must purge the old code everytime we load a new
	%% version of temp_mod.
	false ->
	    ok;
	{file,_Loaded} ->
	    true = code:delete(TempMod),
	    %% TODO: Do a hard purge instead?
	    purge_old_code(TempMod),
	    ok
    end.

-spec reload_temp_mod(state()) -> 'ok'.
reload_temp_mod(#state{forms = []}) ->
    %% TODO: To make PropEr multi-threaded, the reloading will need to occur
    %%       even if no more functions are present.
    ok;
reload_temp_mod(#state{temp_mod = TempMod, forms = RawForms}) ->
    Forms = [{attribute,0,module,TempMod} | RawForms],
    %% TODO: verbose and report options?
    {ok,TempMod,Code} = compile:forms(Forms, [export_all]),
    {module,TempMod} = code:load_binary(TempMod, "no_file", Code),
    purge_old_code(TempMod),
    ok.

-spec purge_old_code(mod_name()) -> 'ok'.
purge_old_code(Mod) ->
    case code:soft_purge(Mod) of
	true  -> ok;
	false -> purge_old_code(Mod)
    end.

-spec get_ret_type(function()) -> proper_types:type().
get_ret_type(Fun) ->
    {arity,Arity} = erlang:fun_info(Fun, arity),
    put('$get_ret_type', true),
    RetType = apply(Fun, lists:duplicate(Arity,dummy)),
    erase('$get_ret_type'),
    RetType.

-spec function_body([term()], imm_ret_type(), fun_seed()) ->
	  proper_types:type() | proper_gen:instance().
function_body(Args, ImmRetType, {Seed1,Seed2}) ->
    RetType = case ImmRetType of
		  {type,T} -> T;
		  {bin,B}  -> proper_types:from_binary(B)
	      end,
    case get('$get_ret_type') of
	true ->
	    RetType;
	_ ->
	    SavedSeed = get(random_seed),
	    put(random_seed, {Seed1,Seed2,erlang:phash2(Args,?SEED_RANGE)}),
	    Ret = proper_gen:clean_instance(proper_gen:generate(RetType)),
	    put(random_seed, SavedSeed),
	    proper_symb:internal_eval(Ret)
    end.
