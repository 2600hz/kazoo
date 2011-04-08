%%% Copyright 2010-2011 Manolis Papadakis (manopapad@gmail.com)
%%%                 and Kostis Sagonas (kostis@cs.ntua.gr)
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
%%% @doc This module contains functions used when symbolically generating
%%%	 datatypes.

-module(proper_symb).
-export([eval/1, eval/2, defined/1, well_defined/1, pretty_print/1,
	 pretty_print/2]).
-export([internal_eval/1, internal_well_defined/1]).

-export_type([var_id/0, var_values/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

%% -type symb_call()  :: {'call' | '$call',mod_name(),fun_name(),[symb_term()]}.
%% TODO: only atoms are allowed as variable identifiers?

-type var_id() :: integer() | atom().
-type var_values() :: [{var_id(),term()}].
-type symb_term() :: term().
-type handled_term() :: term().
-type caller() :: 'user' | 'system'.
-type call_handler() :: fun((mod_name(),fun_name(),[handled_term()]) ->
				handled_term()).
-type term_handler() :: fun((term()) -> handled_term()).
-type handle_info() :: {caller(),call_handler(),term_handler()}.


%%------------------------------------------------------------------------------
%% Evaluation functions
%%------------------------------------------------------------------------------

-spec eval(symb_term()) -> term().
eval(SymbTerm) ->
    eval([], SymbTerm).

-spec eval(var_values(), symb_term()) -> term().
eval(VarValues, SymbTerm) ->
    eval(VarValues, SymbTerm, user).

-spec eval(var_values(), symb_term(), caller()) -> term().
eval(VarValues, SymbTerm, Caller) ->
    HandleInfo = {Caller, fun erlang:apply/3, fun(X) -> X end},
    symb_walk(VarValues, SymbTerm, HandleInfo).

%% @private
-spec internal_eval(symb_term()) -> term().
internal_eval(SymbTerm) ->
    eval([], SymbTerm, system).

-spec defined(symb_term()) -> boolean().
defined(SymbTerm) ->
    defined(SymbTerm, user).

-spec defined(symb_term(), caller()) -> boolean().
defined(SymbTerm, Caller) ->
    try eval([], SymbTerm, Caller) of
	_Term -> true
    catch
	_Exception:_Reason -> false
    end.

-spec well_defined(proper_types:raw_type()) -> proper_types:type().
well_defined(SymbType) ->
    well_defined(SymbType, user).

-spec well_defined(proper_types:raw_type(), caller()) -> proper_types:type().
well_defined(SymbType, Caller) ->
    ?SUCHTHAT(X, SymbType, defined(X,Caller)).

%% @private
-spec internal_well_defined(proper_types:type()) -> proper_types:type().
internal_well_defined(SymbType) ->
    well_defined(SymbType, system).


%%------------------------------------------------------------------------------
%% Pretty-printing functions
%%------------------------------------------------------------------------------

-spec pretty_print(symb_term()) -> string().
pretty_print(SymbTerm) ->
    pretty_print([], SymbTerm).

-spec pretty_print(var_values(), symb_term()) -> string().
pretty_print(VarValues, SymbTerm) ->
    HandleInfo = {user, fun parse_fun/3, fun parse_term/1},
    ExprTree = symb_walk(VarValues, SymbTerm, HandleInfo),
    lists:flatten(erl_pp:expr(ExprTree)).

-spec parse_fun(mod_name(), fun_name(), [abs_expr()]) -> abs_expr().
parse_fun(Module, Function, ArgTreeList) ->
    {call,0,{remote,0,{atom,0,Module},{atom,0,Function}},ArgTreeList}.

-spec parse_term(term()) -> abs_expr().
parse_term(TreeList) when is_list(TreeList) ->
    {RestOfList, Acc0} =
	case proper_arith:cut_improper_tail(TreeList) of
	    {_ProperHead,_ImproperTail} = X -> X;
	    ProperList                      -> {ProperList,{nil,0}}
	end,
    lists:foldr(fun(X,Acc) -> {cons,0,X,Acc} end, Acc0, RestOfList);
parse_term(TreeTuple) when is_tuple(TreeTuple) ->
    {tuple,0,tuple_to_list(TreeTuple)};
parse_term(Term) ->
    %% TODO: pid, port, reference, function value?
    erl_parse:abstract(Term).


%%------------------------------------------------------------------------------
%% Generic symbolic handler function
%%------------------------------------------------------------------------------

-spec symb_walk(var_values(), symb_term(), handle_info()) -> handled_term().
symb_walk(VarValues, {call,Mod,Fun,Args},
	  {user,_HandleCall,_HandleTerm} = HandleInfo) ->
    symb_walk_call(VarValues, Mod, Fun, Args, HandleInfo);
symb_walk(VarValues, {'$call',Mod,Fun,Args}, HandleInfo) ->
    symb_walk_call(VarValues, Mod, Fun, Args, HandleInfo);
symb_walk(VarValues, {var,VarId},
	  {user,_HandleCall,HandleTerm} = HandleInfo) ->
    SymbWalk = fun(X) -> symb_walk(VarValues, X, HandleInfo) end,
    case lists:keyfind(VarId, 1, VarValues) of
	{VarId,VarValue} ->
	    %% TODO: this allows symbolic calls and vars inside var values,
	    %%       which may result in an infinite loop, as in:
	    %%       [{a,{call,m,f,[{var,a}]}}], {var,a}
	    SymbWalk(VarValue);
	false ->
	    HandleTerm({HandleTerm(var),SymbWalk(VarId)})
    end;
symb_walk(VarValues, SymbTerm, HandleInfo) ->
    symb_walk_gen(VarValues, SymbTerm, HandleInfo).

-spec symb_walk_call(var_values(), mod_name(), fun_name(), [symb_term()],
		     handle_info()) -> handled_term().
symb_walk_call(VarValues, Mod, Fun, Args,
	       {_Caller,HandleCall,_HandleTerm} = HandleInfo) ->
    SymbWalk = fun(X) -> symb_walk(VarValues, X, HandleInfo) end,
    HandledArgs = [SymbWalk(A) || A <- Args],
    HandleCall(Mod, Fun, HandledArgs).

-spec symb_walk_gen(var_values(), symb_term(), handle_info()) -> handled_term().
symb_walk_gen(VarValues, SymbTerm,
	      {_Caller,_HandleCall,HandleTerm} = HandleInfo) ->
    SymbWalk = fun(X) -> symb_walk(VarValues, X, HandleInfo) end,
    Term =
	if
	    is_list(SymbTerm)  -> proper_arith:safe_map(SymbWalk, SymbTerm);
	    is_tuple(SymbTerm) -> proper_arith:tuple_map(SymbWalk, SymbTerm);
	    true               -> SymbTerm
	end,
    HandleTerm(Term).
