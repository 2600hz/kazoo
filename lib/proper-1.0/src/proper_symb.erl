%%% Copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
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

%%% @copyright 2010-2011 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis

%%% @doc Symbolic datatypes handling functions.
%%%
%%% == Symbolic datatypes ==
%%% When writing properties that involve abstract data types, such as dicts or
%%% sets, it is usually best to avoid dealing with the ADTs' internal
%%% representation directly. Working, instead, with a symbolic representation of
%%% the ADT's construction process (series of API calls) has several benefits:
%%% <ul>
%%% <li>Failing testcases are easier to read and understand. Compare:
%%%   ``` {call,sets,from_list,[[1,2,3]]} '''
%%%   with:
%%%   ``` {set,3,16,16,8,80,48,
%%%            {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
%%%            {{[],[3],[],[],[],[],[2],[],[],[],[],[1],[],[],[],[]}}} '''</li>
%%% <li>Failing testcases are easier to shrink.</li>
%%% <li>It is especially useful when testing the datatype itself: Certain
%%%   implementation errors may depend on some particular selection and
%%%   ordering of API calls, thus it is important to cover the entire ADT
%%%   construction API.</li>
%%% </ul>
%%%
%%% PropEr supports the symbolic representation of datatypes, using the
%%% following syntax:
%%% <dl>
%%% <dt>`{call,Module,Function,Arguments}'</dt>
%%% <dd>This represents a call to the API function `Module:Function' with
%%%   arguments `Arguments'. Each of the arguments may be a symbolic call itself
%%%   or contain other symbolic calls in lists or tuples of arbitrary
%%%   depth.</dd>
%%% <dt id="$call">``{'$call',Module,Function,Arguments}''</dt>
%%% <dd>Identical to the above, but gets evaluated automatically before being
%%%   applied to a property.</dd>
%%% <dt id="var">`{var,'{@type var_id()}`}'</dt>
%%% <dd>This contruct serves as a placeholder for values that are not known at
%%%   type construction time. It will be replaced by the actual value of the
%%%   variable during evaluation.</dd>
%%% </dl>
%%%
%%% When including the PropEr header file, all
%%% <a href="#index">API functions</a> of this module are automatically
%%% imported, unless `PROPER_NO_IMPORTS' is defined.
%%%
%%% == Auto-ADT ==
%%% To simplify the symbolic testing of ADTs, PropEr comes with the Auto-ADT
%%% subsystem: An opaque native type, if exported from its module, is assumed
%%% to be an abstract data type, causing PropEr to ignore its internal
%%% representation and instead construct symbolic instances of the type. The
%%% API functions used in these symbolic instances are extracted from the ADT's
%%% defining module, which is expected to contain one or more `-spec'ed and
%%% exported functions that can be used to construct instances of the ADT.
%%% Specifically, PropEr will use all functions that return at least one
%%% instance of the ADT. As with recursive native types, the base case is
%%% automatically detected (in the case of ADTs, calls to functions like
%%% `new/0' and `from_list/1' would be considered the base case). The produced
%%% symbolic calls will be <a href="#$call">`$call' tuples</a>, which are
%%% automatically evaluated, thus no call to {@link eval/1} is required inside
%%% the property. Produced instances are guaranteed to evaluate successfully.
%%% Parametric ADTs are supported, so long as they appear fully instantiated
%%% inside `?FORALL's.
%%%
%%% ADTs hard-coded in the Erlang type system (`array', `dict', `digraph',
%%% `gb_set', `gb_tree', `queue', and `set') are automatically detected and
%%% handled as such. PropEr also accepts parametric versions of the above ADTs
%%% in `?FORALL's (`array/1', `dict/2', `gb_set/1', `gb_tree/2', `queue/1',
%%% `set/1', also `orddict/2' and `ordset/1'). If you would like to use these
%%% parametric versions in `-type' and `-spec' declarations as well, to better
%%% document your code and facilitate spec testing, you can include the
%%% complementary header file `proper/include/proper_param_adts.hrl', which
%%% provides the corresponding `-type' definitions. Please note that Dialyzer
%%% currenty treats these the same way as their non-parametric counterparts.
%%%
%%% The use of Auto-ADT is currently subject to the following limitations:
%%% <ul>
%%% <li>In the ADT's `-opaque' declaration, as in all types' declarations,
%%%   only type variables should be used as parameters in the LHS. None of
%%%   these variables can be the special `_' variable and no variable should
%%%   appear more than once in the parameters.</li>
%%% <li>ADTs inside specs can only have simple variables as parameters. These
%%%   variables cannot be bound by any is_subtype constraint. Also, the special
%%%   `_' variable is not allowed in ADT parameters. If this would result in
%%%   singleton variables, as in the specs of functions like `new/0', use
%%%   variable names that begin with an underscore.</li>
%%% <li>Specs that introduce an implicit binding among the parameters of an
%%%   ADT are rejected, e.g.:
%%%   ``` -spec foo(mydict(T,S),mydict(S,T)) -> mydict(T,S). '''
%%%   This includes using the same type variable twice in the parameters of
%%%   an ADT.</li>
%%% <li>While parsing the return type of specs in search of ADT references,
%%%   PropEr only recurses into tuples, unions and lists; all other constructs
%%%   are ignored. This prohibits, among others, indirect references to the ADT
%%%   through other custom types and records.</li>
%%% <li>When encountering a union in the return type, PropEr will pick the
%%%   first choice that can return an ADT. This choice must be distinguishable
%%%   from the others either by having a unique term structure or by having a
%%%   unique tag (if it's a tagged tuple).</li>
%%% <li>When parsing multi-clause specs, only the first clause is considered.
%%%   </li>
%%% <li>The only spec constraints we accept are `is_subtype' constraints whose
%%%   first argument is a simple, non-`_' variable. It is not checked whether or
%%%   not these variables actually appear in the spec. The second argument of an
%%%   `is_subtype' constraint cannot contain any non-`_' variables. Multiple
%%%   constraints for the same variable are not supported.</li>
%%% <li> Unexported opaques and opaques with no suitable specs to serve as API
%%%   calls are silently discarded. Those will be treated like ordinary types.
%%%   </li>
%%% <li>Unexported or unspecced functions are silently rejected.</li>
%%% <li>Functions with unsuitable return values are silently rejected.</li>
%%% <li>Specs that make bad use of variables are silently rejected.</li>
%%% </ul>
%%%
%%% For an example on how to write Auto-ADT-compatible parametric specs, see
%%% the `examples/stack' module, which contains a simple implementation of a
%%% stack, or the `proper/proper_dict module', which wraps the `STDLIB' `dict'
%%% ADT.


-module(proper_symb).
-export([eval/1, eval/2, defined/1, well_defined/1, pretty_print/1,
	 pretty_print/2]).
-export([internal_eval/1, internal_well_defined/1]).

-export_type([var_values/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

%% -type symb_call()  :: {'call' | '$call',mod_name(),fun_name(),[symb_term()]}.
%% TODO: only atoms are allowed as variable identifiers?

-type var_id() :: atom() | pos_integer().
-type var_values() :: [{var_id(),term()}].
%% @type symb_term()
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

%% @equiv eval([], SymbTerm)
-spec eval(symb_term()) -> term().
eval(SymbTerm) ->
    eval([], SymbTerm).

%% @doc Intended for use inside the property-testing code, this function
%% evaluates a symbolic instance `SymbTerm'. It also accepts a proplist
%% `VarValues' that maps variable names to values, which is used to replace any
%% <a href="#var">var tuples</a> inside `SymbTerm' before proceeding with its
%% evaluation.
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

%% @doc Returns true if the `SymbTerm' symbolic instance can be successfully
%% evaluated (its evaluation doesn't raise an error or exception).
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

%% @doc An attribute which can be applied to any symbolic generator `SymbType'
%% that may produce invalid sequences of operations when called. The resulting
%% generator is guaranteed to only produce well-defined symbolic instances.
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

%% @equiv pretty_print([], SymbTerm)
-spec pretty_print(symb_term()) -> string().
pretty_print(SymbTerm) ->
    pretty_print([], SymbTerm).

%% @doc Similar in calling convention to {@link eval/2}, but returns a string
%% representation of the call sequence `SymbTerm' instead of evaluating it.
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
