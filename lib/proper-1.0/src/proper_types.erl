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
%%% @doc Type manipulation functions and predefined types are contained in this
%%%	 module.

-module(proper_types).
-export([is_inst/2, is_inst/3]).

-export([integer/2, float/2, atom/0, binary/0, binary/1, bitstring/0,
	 bitstring/1, list/1, vector/2, union/1, weighted_union/1,tuple/1,
	 loose_tuple/1, exactly/1, fixed_list/1, function/2, any/0]).
-export([integer/0, non_neg_integer/0, pos_integer/0, neg_integer/0, range/2,
	 float/0, non_neg_float/0, number/0, boolean/0, byte/0, char/0,
	 list/0, tuple/0, string/0, wunion/1, term/0, timeout/0, arity/0]).
-export([int/0, nat/0, largeint/0, real/0, bool/0, choose/2, elements/1,
	 oneof/1, frequency/1, return/1, default/2, orderedlist/1, function0/1,
	 function1/1, function2/1, function3/1, function4/1,
	 weighted_default/2]).
-export([resize/2, non_empty/1, noshrink/1]).

-export([cook_outer/1, is_type/1, equal_types/2, is_raw_type/1, to_binary/1,
	 from_binary/1, get_prop/2, find_prop/2, safe_is_instance/2,
	 is_instance/2, unwrap/1, weakly/1, strongly/1, satisfies_all/2,
	 new_type/2, list_get_indices/1]).
-export([lazy/1, sized/1, bind/3, shrinkwith/2, add_constraint/3,
	 native_type/2, distlist/3, with_parameter/3, with_parameters/2,
	 parameter/1, parameter/2]).

-export_type([type/0, raw_type/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Comparison with erl_types
%%------------------------------------------------------------------------------

%% Missing types
%% -------------------
%% will do:
%%	records, maybe_improper_list(T,S), improper_list(T,S)?
%%	maybe_improper_list(), maybe_improper_list(T), iolist, iodata
%% don't need:
%%	nonempty_{list,string,improper_list,maybe_improper_list}
%% won't do:
%%	pid, port, ref, identifier, none, no_return, module, mfa, node
%%	array, dict, digraph, set, gb_tree, gb_set, queue, tid

%% Missing type information
%% ------------------------
%% bin types:
%%	other unit sizes? what about size info?
%% functions:
%%	generally some fun, unspecified number of arguments but specified
%%	return type
%% any:
%%	doesn't cover functions and improper lists


%%------------------------------------------------------------------------------
%% Type declaration macros
%%------------------------------------------------------------------------------

-define(BASIC(PropList), new_type(PropList,basic)).
-define(WRAPPER(PropList), new_type(PropList,wrapper)).
-define(CONSTRUCTED(PropList), new_type(PropList,constructed)).
-define(CONTAINER(PropList), new_type(PropList,container)).
-define(SUBTYPE(Type,PropList), subtype(PropList,Type)).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type type_kind() :: 'basic' | 'wrapper' | 'constructed' | 'container' | atom().
-type instance_test() :: fun((proper_gen:imm_instance()) -> boolean()).
-type index() :: pos_integer().
-type value() :: term().
-type constraint_fun() :: fun((proper_gen:instance()) -> boolean()).

-opaque type() :: {'$type', [type_prop()]}.
-type raw_type() :: type() | [raw_type()] | loose_tuple(raw_type()) | term().
-type type_prop_name() :: 'kind' | 'generator' | 'straight_gen' | 'reverse_gen'
			| 'parts_type' | 'combine' | 'alt_gens'
			| 'shrink_to_parts' | 'size_transform' | 'is_instance'
			| 'shrinkers' | 'noshrink' | 'internal_type'
			| 'internal_types' | 'get_length' | 'split' | 'join'
			| 'get_indices' | 'remove' | 'retrieve' | 'update'
			| 'constraints' | 'content' | 'default'.

-type type_prop_value() :: term().
-type type_prop() ::
      {'kind', type_kind()}
    | {'generator', proper_gen:generator()}
    | {'straight_gen', proper_gen:straight_gen()}
    | {'reverse_gen', proper_gen:reverse_gen()}
    | {'parts_type', type()}
    | {'combine', proper_gen:combine_fun()}
    | {'alt_gens', proper_gen:alt_gens()}
    | {'shrink_to_parts', boolean()}
    | {'size_transform', fun((size()) -> size())}
    | {'is_instance', instance_test()}
    | {'shrinkers', [proper_shrink:shrinker()]}
    | {'noshrink', boolean()}
    | {'internal_type', raw_type()}
    | {'internal_types', tuple() | maybe_improper_list(type(),type() | [])}
      %% The items returned by 'remove' must be of this type.
    | {'get_length', fun((proper_gen:imm_instance()) -> length())}
      %% If this is a container type, this should return the number of elements
      %% it contains.
    | {'split', fun((proper_gen:imm_instance()) -> [proper_gen:imm_instance()])
	      | fun((length(),proper_gen:imm_instance()) ->
		    {proper_gen:imm_instance(),proper_gen:imm_instance()})}
      %% If present, the appropriate form depends on whether get_length is
      %% defined: if get_length is undefined, this must be in the one-argument
      %% form (e.g. a tree should be split into its subtrees), else it must be
      %% in the two-argument form (e.g. a list should be split in two at the
      %% index provided).
    | {'join', fun((proper_gen:imm_instance(),proper_gen:imm_instance()) ->
		   proper_gen:imm_instance())}
    | {'get_indices', fun((proper_gen:imm_instance()) -> [index()])}
      %% If this is a container type, this should return a list of indices we
      %% can use to remove or insert elements from the given instance.
    | {'remove', fun((index(),proper_gen:imm_instance()) ->
		     proper_gen:imm_instance())}
    | {'retrieve', fun((index(), proper_gen:imm_instance() | tuple()
			       | maybe_improper_list(type(),type() | [])) ->
		       value() | type())}
    | {'update', fun((index(),value(),proper_gen:imm_instance()) ->
		     proper_gen:imm_instance())}
    | {'constraints', [{constraint_fun(), boolean()}]}
      %% A list of constraints on instances of this type: each constraint is a
      %% tuple of a fun that must return 'true' for each valid instance and a
      %% boolean field that specifies whether the condition is strict.
    |{'parameters',[{atom(),value()}]}.


%%------------------------------------------------------------------------------
%% Type manipulation functions
%%------------------------------------------------------------------------------

%% @private
%% TODO: just cook/1 ?
-spec cook_outer(raw_type()) -> proper_types:type().
cook_outer(Type = {'$type',_Props}) ->
    Type;
cook_outer(RawType) ->
    if
	is_tuple(RawType) -> tuple(tuple_to_list(RawType));
	%% CAUTION: this must handle improper lists
	is_list(RawType)  -> fixed_list(RawType);
	%% default case (covers integers, floats, atoms, binaries, ...):
	true              -> exactly(RawType)
    end.

%% @private
-spec is_type(term()) -> boolean().
is_type({'$type',_Props}) ->
    true;
is_type(_) ->
    false.

%% @private
-spec equal_types(proper_types:type(), proper_types:type()) -> boolean().
equal_types(SameType, SameType) ->
    true;
equal_types(_, _) ->
    false.

%% @private
-spec is_raw_type(term()) -> boolean().
is_raw_type({'$type',_TypeProps}) ->
    true;
is_raw_type(X) ->
    if
	is_tuple(X) -> is_raw_type_list(tuple_to_list(X));
	is_list(X)  -> is_raw_type_list(X);
	true        -> false
    end.

-spec is_raw_type_list(maybe_improper_list()) -> boolean().
%% CAUTION: this must handle improper lists
is_raw_type_list(List) ->
    proper_arith:safe_any(fun is_raw_type/1, List).

%% @private
-spec to_binary(proper_types:type()) -> binary().
to_binary(Type) ->
    term_to_binary(Type).

%% @private
%% TODO: restore: -spec from_binary(binary()) -> proper_types:type().
from_binary(Binary) ->
    binary_to_term(Binary).

-spec type_from_list([type_prop()]) -> proper_types:type().
type_from_list(KeyValueList) ->
    {'$type',orddict:from_list(KeyValueList)}.

-spec add_prop(type_prop_name(), type_prop_value(), proper_types:type()) ->
	  proper_types:type().
add_prop(PropName, Value, {'$type',Props}) ->
    {'$type',orddict:store(PropName, Value, Props)}.

-spec add_props([type_prop()], proper_types:type()) -> proper_types:type().
add_props(PropList, {'$type',OldProps}) ->
    {'$type', lists:foldl(fun({N,V},Acc) -> orddict:store(N, V, Acc) end,
			  OldProps, PropList)}.

-spec append_to_prop(type_prop_name(), type_prop_value(),
		     proper_types:type()) -> proper_types:type().
append_to_prop(PropName, Value, {'$type',Props}) ->
    {'$type',orddict:append(PropName, Value, Props)}.

-spec append_list_to_prop(type_prop_name(), [type_prop_value()],
			  proper_types:type()) -> proper_types:type().
append_list_to_prop(PropName, List, {'$type',Props}) ->
    {'$type',orddict:append_list(PropName, List, Props)}.

%% @private
-spec get_prop(type_prop_name(), proper_types:type()) -> type_prop_value().
get_prop(PropName, {'$type',Props}) ->
    orddict:fetch(PropName, Props).

%% @private
-spec find_prop(type_prop_name(), proper_types:type()) ->
	  {'ok',type_prop_value()} | 'error'.
find_prop(PropName, {'$type',Props}) ->
    orddict:find(PropName, Props).

-spec new_type([type_prop()], type_kind()) -> proper_types:type().
new_type(PropList, Kind) ->
    Type = type_from_list(PropList),
    add_prop(kind, Kind, Type).

-spec subtype([type_prop()], proper_types:type()) -> proper_types:type().
%% TODO: should the 'is_instance' function etc. be reset for subtypes?
subtype(PropList, Type) ->
    add_props(PropList, Type).

-spec is_inst(proper_gen:instance(), raw_type()) ->
	  boolean() | {'error',{'typeserver',term()}}.
is_inst(Instance, RawType) ->
    is_inst(Instance, RawType, 10).

-spec is_inst(proper_gen:instance(), raw_type(), size()) ->
	  boolean() | {'error',{'typeserver',term()}}.
is_inst(Instance, RawType, Size) ->
    proper:global_state_init_size(Size),
    Result = safe_is_instance(Instance, RawType),
    proper:global_state_erase(),
    Result.

%% @private
-spec safe_is_instance(proper_gen:imm_instance(), raw_type()) ->
	  boolean() | {'error',{'typeserver',term()}}.
safe_is_instance(ImmInstance, RawType) ->
    try is_instance(ImmInstance, RawType) catch
	throw:{'$typeserver',SubReason} -> {error, {typeserver,SubReason}}
    end.

%% @private
-spec is_instance(proper_gen:imm_instance(), raw_type()) -> boolean().
%% TODO: If the second argument is not a type, let it pass (don't even check for
%%	 term equality?) - if it's a raw type, don't cook it, instead recurse
%%	 into it.
is_instance(ImmInstance, RawType) ->
    CleanInstance = proper_gen:clean_instance(ImmInstance),
    Type = cook_outer(RawType),
    (case get_prop(kind, Type) of
	 wrapper     -> wrapper_test(ImmInstance, Type);
	 constructed -> constructed_test(ImmInstance, Type);
	 _           -> false
     end
     orelse
     case find_prop(is_instance, Type) of
	 {ok,IsInstance} -> IsInstance(ImmInstance);
	 error           -> false
     end)
    andalso weakly(satisfies_all(CleanInstance, Type)).

-spec wrapper_test(proper_gen:imm_instance(), proper_types:type()) -> boolean().
wrapper_test(ImmInstance, Type) ->
    %% TODO: check if it's actually a raw type that's returned?
    lists:any(fun(T) -> is_instance(ImmInstance, T) end, unwrap(Type)).

%% @private
%% TODO: restore:-spec unwrap(proper_types:type()) -> [proper_types:type(),...].
%% TODO: check if it's actually a raw type that's returned?
unwrap(Type) ->
    RawInnerTypes = proper_gen:alt_gens(Type) ++ [proper_gen:normal_gen(Type)],
    [cook_outer(T) || T <- RawInnerTypes].

-spec constructed_test(proper_gen:imm_instance(), proper_types:type()) ->
	  boolean().
constructed_test({'$used',ImmParts,ImmInstance}, Type) ->
    PartsType = get_prop(parts_type, Type),
    Combine = get_prop(combine, Type),
    is_instance(ImmParts, PartsType) andalso
    begin
	%% TODO: check if it's actually a raw type that's returned?
	%% TODO: move construction code to proper_gen
	%% TODO: non-type => should we check for strict term equality?
	RawInnerType = Combine(proper_gen:clean_instance(ImmParts)),
	is_instance(ImmInstance, RawInnerType)
    end;
constructed_test({'$to_part',ImmInstance}, Type) ->
    PartsType = get_prop(parts_type, Type),
    get_prop(shrink_to_parts, Type) =:= true andalso
    %% TODO: we reject non-container types
    get_prop(kind, PartsType) =:= container andalso
    case {find_prop(internal_type,PartsType),
	  find_prop(internal_types,PartsType)} of
	{{ok,EachPartType},error} ->
	    %% The parts are in a list or a vector.
	    is_instance(ImmInstance, EachPartType);
	{error,{ok,PartTypesList}} ->
	    %% The parts are in a fixed list.
	    %% TODO: It should always be a proper list.
	    lists:any(fun(T) -> is_instance(ImmInstance,T) end, PartTypesList)
    end;
constructed_test(_CleanInstance, _Type) ->
    %% TODO: can we do anything better?
    false.

%% @private
-spec weakly({boolean(),boolean()}) -> boolean().
weakly({B1,_B2}) -> B1.

%% @private
-spec strongly({boolean(),boolean()}) -> boolean().
strongly({_B1,B2}) -> B2.

-spec satisfies(proper_gen:instance(), {constraint_fun(),boolean()})
	  -> {boolean(),boolean()}.
satisfies(Instance, {Test,false}) ->
    {true,Test(Instance)};
satisfies(Instance, {Test,true}) ->
    Result = Test(Instance),
    {Result,Result}.

%% @private
-spec satisfies_all(proper_gen:instance(), proper_types:type()) ->
	  {boolean(),boolean()}.
satisfies_all(Instance, Type) ->
    case find_prop(constraints, Type) of
	{ok, Constraints} ->
	    L = [satisfies(Instance, C) || C <- Constraints],
	    {L1,L2} = lists:unzip(L),
	    {lists:all(fun(B) -> B end, L1), lists:all(fun(B) -> B end, L2)};
	error ->
	    {true,true}
    end.


%%------------------------------------------------------------------------------
%% Type definition functions
%%------------------------------------------------------------------------------

%% @private
-spec lazy(proper_gen:nosize_generator()) -> proper_types:type().
lazy(Gen) ->
    ?WRAPPER([
	{generator, Gen}
    ]).

%% @private
-spec sized(proper_gen:sized_generator()) -> proper_types:type().
sized(Gen) ->
    ?WRAPPER([
        {generator, Gen}
    ]).

%% @private
-spec bind(raw_type(), proper_gen:combine_fun(), boolean()) ->
	  proper_types:type().
bind(RawPartsType, Combine, ShrinkToParts) ->
    PartsType = cook_outer(RawPartsType),
    ?CONSTRUCTED([
	{parts_type, PartsType},
	{combine, Combine},
	{shrink_to_parts, ShrinkToParts}
    ]).

%% @private
-spec shrinkwith(proper_gen:nosize_generator(), proper_gen:alt_gens()) ->
	  proper_types:type().
shrinkwith(Gen, DelaydAltGens) ->
    ?WRAPPER([
	{generator, Gen},
	{alt_gens, DelaydAltGens}
    ]).

%% @private
-spec add_constraint(raw_type(), constraint_fun(), boolean()) ->
	  proper_types:type().
add_constraint(RawType, Condition, IsStrict) ->
    Type = cook_outer(RawType),
    append_to_prop(constraints, {Condition,IsStrict}, Type).

%% @private
-spec native_type(mod_name(), string()) -> proper_types:type().
native_type(Mod, TypeStr) ->
    ?WRAPPER([
	{generator, fun() ->  proper_gen:native_type_gen(Mod,TypeStr) end}
    ]).


%%------------------------------------------------------------------------------
%% Basic types
%%------------------------------------------------------------------------------

-spec integer(proper_arith:extint(), proper_arith:extint()) ->
	  proper_types:type().
integer(Low, High) ->
    ?BASIC([
	{generator, fun(Size) -> proper_gen:integer_gen(Size, Low, High) end},
	{is_instance, fun(X) -> integer_test(X, Low, High) end},
	{shrinkers,
	 [fun(X,_T,S) -> proper_shrink:number_shrinker(X, Low, High, S) end]}
    ]).

-spec integer_test(proper_gen:imm_instance(), proper_arith:extint(),
		   proper_arith:extint()) -> boolean().
integer_test(X, Low, High) ->
    is_integer(X)
    andalso proper_arith:le(Low, X)
    andalso proper_arith:le(X, High).

-spec float(proper_arith:extnum(), proper_arith:extnum()) ->
	  proper_types:type().
float(Low, High) ->
    ?BASIC([
	{generator, fun(Size) -> proper_gen:float_gen(Size, Low, High) end},
	{is_instance, fun(X) -> float_test(X, Low, High) end},
	{shrinkers,
	 [fun(X,_T,S) -> proper_shrink:number_shrinker(X, Low, High, S) end]}
    ]).

-spec float_test(proper_gen:imm_instance(), proper_arith:extnum(),
		 proper_arith:extnum()) -> boolean().
float_test(X, Low, High) ->
    is_float(X)
    andalso proper_arith:le(Low, X)
    andalso proper_arith:le(X, High).

-spec atom() -> proper_types:type().
atom() ->
    ?WRAPPER([
	{generator, fun proper_gen:atom_gen/1},
	{reverse_gen, fun proper_gen:atom_rev/1},
	{size_transform, fun(Size) -> erlang:min(Size,255) end},
	{is_instance, fun atom_test/1}
    ]).

-spec atom_test(proper_gen:imm_instance()) -> boolean().
atom_test(X) ->
    is_atom(X)
    %% We return false for atoms starting with '$', since these are
    %% atoms used internally and never produced by the atom generator.
    andalso (X =:= '' orelse hd(atom_to_list(X)) =/= $$).

-spec binary() -> proper_types:type().
binary() ->
    ?WRAPPER([
	{generator, fun proper_gen:binary_gen/1},
	{straight_gen, fun proper_gen:binary_str_gen/1},
	{reverse_gen, fun proper_gen:binary_rev/1},
	{is_instance, fun erlang:is_binary/1}
    ]).

-spec binary(length()) -> proper_types:type().
binary(Len) ->
    ?WRAPPER([
	{generator, fun() -> proper_gen:binary_len_gen(Len) end},
	{straight_gen, fun() -> proper_gen:binary_len_str_gen(Len) end},
	{reverse_gen, fun proper_gen:binary_rev/1},
	{is_instance, fun(X) -> binary_len_test(X, Len) end}
    ]).

-spec binary_len_test(proper_gen:imm_instance(), length()) -> boolean().
binary_len_test(X, Len) ->
    is_binary(X) andalso byte_size(X) =:= Len.

-spec bitstring() -> proper_types:type().
bitstring() ->
    ?WRAPPER([
	{generator, fun proper_gen:bitstring_gen/1},
	{reverse_gen, fun proper_gen:bitstring_rev/1},
	{is_instance, fun erlang:is_bitstring/1}
    ]).

-spec bitstring(length()) -> proper_types:type().
bitstring(Len) ->
    ?WRAPPER([
	{generator, fun() -> proper_gen:bitstring_len_gen(Len) end},
	{reverse_gen, fun proper_gen:bitstring_rev/1},
	{is_instance, fun(X) -> bitstring_len_test(X, Len) end}
    ]).

-spec bitstring_len_test(proper_gen:imm_instance(), length()) -> boolean().
bitstring_len_test(X, Len) ->
    is_bitstring(X) andalso bit_size(X) =:= Len.


-spec list(raw_type()) -> proper_types:type().
% TODO: subtyping would be useful here (list, vector, fixed_list)
list(RawElemType) ->
    ElemType = cook_outer(RawElemType),
    ?CONTAINER([
	{generator, fun(Size) -> proper_gen:list_gen(Size, ElemType) end},
	{is_instance, fun(X) -> list_test(X, ElemType) end},
	{internal_type, ElemType},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{get_indices, fun list_get_indices/1},
	{remove, fun proper_arith:list_remove/2},
	{retrieve, fun lists:nth/2},
	{update, fun proper_arith:list_update/3}
    ]).

-spec list_test(proper_gen:imm_instance(), proper_types:type()) -> boolean().
list_test(X, ElemType) ->
    is_list(X)
    andalso lists:all(fun(E) -> is_instance(E, ElemType) end, X).

-spec list_get_indices(list()) -> [position()].
list_get_indices(List) ->
    lists:seq(1, length(List)).

%% @private
%% This assumes that:
%% - instances of size S are always valid instances of size >S
%% - any recursive calls inside Gen are lazy
-spec distlist(size(), proper_gen:sized_generator(), boolean()) ->
	  proper_types:type().
distlist(Size, Gen, NonEmpty) ->
    ParentType = case NonEmpty of
		     true  -> non_empty(list(Gen(Size)));
		     false -> list(Gen(Size))
		 end,
    ?SUBTYPE(ParentType, [
	{generator, fun() -> proper_gen:distlist_gen(Size, Gen, NonEmpty) end}
    ]).

-spec vector(length(), raw_type()) -> proper_types:type().
vector(Len, RawElemType) ->
    ElemType = cook_outer(RawElemType),
    Indices = lists:seq(1, Len),
    ?CONTAINER([
	{generator, fun() -> proper_gen:vector_gen(Len, ElemType) end},
	{is_instance, fun(X) -> vector_test(X, Len, ElemType) end},
	{internal_type, ElemType},
	{get_indices, fun(_X) -> Indices end},
	{retrieve, fun lists:nth/2},
	{update, fun proper_arith:list_update/3}
    ]).

-spec vector_test(proper_gen:imm_instance(), length(), proper_types:type()) ->
	  boolean().
vector_test(X, Len, ElemType) ->
    is_list(X)
    andalso length(X) =:= Len
    andalso lists:all(fun(E) -> is_instance(E, ElemType) end, X).

-spec union([raw_type(),...]) -> proper_types:type().
union(RawChoices) ->
    Choices = [cook_outer(C) || C <- RawChoices],
    ?BASIC([
	{generator, fun() -> proper_gen:union_gen(Choices) end},
	{is_instance, fun(X) -> union_test(X, Choices) end},
	{shrinkers,
	 [fun(X,_T,S) ->
	      proper_shrink:union_first_choice_shrinker(X, Choices, S)
	  end,
	  fun(X,_T,S) ->
	      proper_shrink:union_recursive_shrinker(X, Choices, S)
	  end]}
    ]).

-spec union_test(proper_gen:imm_instance(), [proper_types:type()]) -> boolean().
union_test(X, Choices) ->
    lists:any(fun(C) -> is_instance(X, C) end, Choices).

-spec weighted_union([{frequency(),raw_type()},...]) -> proper_types:type().
weighted_union(RawFreqChoices) ->
    CookFreqType = fun({Freq,RawType}) -> {Freq,cook_outer(RawType)} end,
    FreqChoices = lists:map(CookFreqType, RawFreqChoices),
    Choices = [T || {_F,T} <- FreqChoices],
    ?SUBTYPE(union(Choices), [
	{generator, fun() -> proper_gen:weighted_union_gen(FreqChoices) end}
    ]).

-spec tuple([raw_type()]) -> proper_types:type().
tuple(RawFields) ->
    Fields = [cook_outer(F) || F <- RawFields],
    Indices = lists:seq(1, length(Fields)),
    ?CONTAINER([
	{generator, fun() -> proper_gen:tuple_gen(Fields) end},
	{is_instance, fun(X) -> tuple_test(X, Fields) end},
	{internal_types, list_to_tuple(Fields)},
	{get_indices, fun(_X) -> Indices end},
	{retrieve, fun erlang:element/2},
	{update, fun tuple_update/3}
    ]).

-spec tuple_test(proper_gen:imm_instance(), [proper_types:type()]) -> boolean().
tuple_test(X, Fields) ->
    is_tuple(X) andalso fixed_list_test(tuple_to_list(X), Fields).

-spec tuple_update(index(), value(), tuple()) -> tuple().
tuple_update(Index, NewElem, Tuple) ->
    setelement(Index, Tuple, NewElem).

-spec loose_tuple(raw_type()) -> proper_types:type().
loose_tuple(RawElemType) ->
    ElemType = cook_outer(RawElemType),
    ?WRAPPER([
	{generator, fun(Size) -> proper_gen:loose_tuple_gen(Size,ElemType) end},
	{reverse_gen, fun(X) -> proper_gen:loose_tuple_rev(X, ElemType) end},
	{is_instance, fun(X) -> loose_tuple_test(X, ElemType) end}
    ]).

-spec loose_tuple_test(proper_gen:imm_instance(), proper_types:type()) ->
	  boolean().
loose_tuple_test(X, ElemType) ->
    is_tuple(X) andalso list_test(tuple_to_list(X), ElemType).

-spec exactly(term()) -> proper_types:type().
exactly(E) ->
    ?BASIC([
	{generator, fun() -> proper_gen:exactly_gen(E) end},
	{is_instance, fun(X) -> X =:= E end}
    ]).

-spec fixed_list(maybe_improper_list(raw_type(),raw_type() | [])) ->
	  proper_types:type().
fixed_list(MaybeImproperRawFields) ->
    %% CAUTION: must handle improper lists
    {Fields, Internal, Indices, Retrieve, Update} =
	case proper_arith:cut_improper_tail(MaybeImproperRawFields) of
	    % TODO: have cut_improper_tail return the length and use it in test?
	    {ProperRawHead, ImproperRawTail} ->
		HeadLen = length(ProperRawHead),
		CookedHead = [cook_outer(F) || F <- ProperRawHead],
		CookedTail = cook_outer(ImproperRawTail),
		{{CookedHead,CookedTail},
		 CookedHead ++ CookedTail,
		 lists:seq(1, HeadLen + 1),
		 fun(I,L) -> improper_list_retrieve(I, L, HeadLen) end,
		 fun(I,V,L) -> improper_list_update(I, V, L, HeadLen) end};
	    ProperRawFields ->
		LocalFields = [cook_outer(F) || F <- ProperRawFields],
		{LocalFields,
		 LocalFields,
		 lists:seq(1, length(ProperRawFields)),
		 fun lists:nth/2,
		 fun proper_arith:list_update/3}
	end,
    ?CONTAINER([
	{generator, fun() -> proper_gen:fixed_list_gen(Fields) end},
	{is_instance, fun(X) -> fixed_list_test(X, Fields) end},
	{internal_types, Internal},
	{get_indices, fun(_X) -> Indices end},
	{retrieve, Retrieve},
	{update, Update}
    ]).

-spec fixed_list_test(proper_gen:imm_instance(),
		      [proper_types:type()] | {[proper_types:type()],
					       proper_types:type()}) ->
	  boolean().
fixed_list_test(X, {ProperHead,ImproperTail}) ->
    is_list(X) andalso
    begin
	ProperHeadLen = length(ProperHead),
	proper_arith:head_length(X) >= ProperHeadLen andalso
	begin
	    {XHead,XTail} = lists:split(ProperHeadLen, X),
	    fixed_list_test(XHead, ProperHead)
	    andalso is_instance(XTail, ImproperTail)
	end
    end;
fixed_list_test(X, ProperFields) ->
    is_list(X)
    andalso length(X) =:= length(ProperFields)
    andalso lists:all(fun({E,T}) -> is_instance(E, T) end,
		      lists:zip(X, ProperFields)).

%% TODO: Move these 2 functions to proper_arith?
-spec improper_list_retrieve(index(), nonempty_improper_list(value(),value()),
			     pos_integer()) -> value().
improper_list_retrieve(Index, List, HeadLen) ->
    case Index =< HeadLen of
	true  -> lists:nth(Index, List);
	false -> lists:nthtail(HeadLen, List)
    end.

-spec improper_list_update(index(), value(),
			   nonempty_improper_list(value(),value()),
			   pos_integer()) ->
	  nonempty_improper_list(value(),value()).
improper_list_update(Index, Value, List, HeadLen) ->
    case Index =< HeadLen of
	%% TODO: This happens to work, but is not implied by list_update's spec.
	true  -> proper_arith:list_update(Index, Value, List);
	false -> lists:sublist(List, HeadLen) ++ Value
    end.

-spec function([raw_type()] | arity(), raw_type()) -> proper_types:type().
function(Arity, RawRetType) when is_integer(Arity), Arity >= 0, Arity =< 255 ->
    RetType = cook_outer(RawRetType),
    ?BASIC([
	{generator, fun() -> proper_gen:function_gen(Arity, RetType) end},
	{is_instance, fun(X) -> function_test(X, Arity, RetType) end}
    ]);
function(RawArgTypes, RawRetType) ->
    function(length(RawArgTypes), RawRetType).

-spec function_test(proper_gen:imm_instance(), arity(), proper_types:type()) ->
	  boolean().
function_test(X, Arity, RetType) ->
    is_function(X, Arity)
    %% TODO: what if it's not a function we produced?
    andalso equal_types(RetType, proper_gen:get_ret_type(X)).

-spec any() -> proper_types:type().
any() ->
    AllTypes = [integer(),float(),atom(),bitstring(),?LAZY(loose_tuple(any())),
		?LAZY(list(any()))],
    ?SUBTYPE(union(AllTypes), [
	{generator, fun proper_gen:any_gen/1}
    ]).


%%------------------------------------------------------------------------------
%% Type aliases
%%------------------------------------------------------------------------------

-spec integer() -> proper_types:type().
integer() -> integer(inf, inf).

-spec non_neg_integer() -> proper_types:type().
non_neg_integer() -> integer(0, inf).

-spec pos_integer() -> proper_types:type().
pos_integer() -> integer(1, inf).

-spec neg_integer() -> proper_types:type().
neg_integer() -> integer(inf, -1).

-spec range(proper_arith:extint(),proper_arith:extint()) -> proper_types:type().
range(Low, High) -> integer(Low, High).

-spec float() -> proper_types:type().
float() -> float(inf, inf).

-spec non_neg_float() -> proper_types:type().
non_neg_float() -> float(0.0, inf).

-spec number() -> proper_types:type().
number() -> union([integer(), float()]).

-spec boolean() -> proper_types:type().
boolean() -> union(['false', 'true']).

-spec byte() -> proper_types:type().
byte() -> integer(0, 255).

-spec char() -> proper_types:type().
char() -> integer(0, 16#10ffff).

-spec list() -> proper_types:type().
list() -> list(any()).

-spec tuple() -> proper_types:type().
tuple() -> loose_tuple(any()).

-spec string() -> proper_types:type().
string() -> list(char()).

-spec wunion([{frequency(),raw_type()},...]) -> proper_types:type().
wunion(FreqChoices) -> weighted_union(FreqChoices).

-spec term() -> proper_types:type().
term() -> any().

-spec timeout() -> proper_types:type().
timeout() -> union([non_neg_integer(), 'infinity']).

-spec arity() -> proper_types:type().
arity() -> integer(0, 255).


%%------------------------------------------------------------------------------
%% QuickCheck compatibility types
%%------------------------------------------------------------------------------

-spec int() -> proper_types:type().
int() -> ?SIZED(Size, integer(-Size,Size)).

-spec nat() -> proper_types:type().
nat() -> ?SIZED(Size, integer(0,Size)).

-spec largeint() -> proper_types:type().
largeint() -> integer().

-spec real() -> proper_types:type().
real() -> float().

-spec bool() -> proper_types:type().
bool() -> boolean().

-spec choose(proper_arith:extint(), proper_arith:extint()) ->
	  proper_types:type().
choose(Low, High) -> integer(Low, High).

-spec elements([raw_type(),...]) -> proper_types:type().
elements(Choices) -> union(Choices).

-spec oneof([raw_type(),...]) -> proper_types:type().
oneof(Choices) -> union(Choices).

-spec frequency([{frequency(),raw_type()},...]) -> proper_types:type().
frequency(FreqChoices) -> weighted_union(FreqChoices).

-spec return(term()) -> proper_types:type().
return(X) -> exactly(X).

-spec default(raw_type(), raw_type()) -> proper_types:type().
default(Default, OtherType) ->
    union([Default, OtherType]).

-spec orderedlist(raw_type()) -> proper_types:type().
orderedlist(RawElemType) ->
    ?LET(L, list(RawElemType), lists:sort(L)).

-spec function0(raw_type()) -> proper_types:type().
function0(RawRetType) ->
    function(0, RawRetType).

-spec function1(raw_type()) -> proper_types:type().
function1(RawRetType) ->
    function(1, RawRetType).

-spec function2(raw_type()) -> proper_types:type().
function2(RawRetType) ->
    function(2, RawRetType).

-spec function3(raw_type()) -> proper_types:type().
function3(RawRetType) ->
    function(3, RawRetType).

-spec function4(raw_type()) -> proper_types:type().
function4(RawRetType) ->
    function(4, RawRetType).

-spec weighted_default({frequency(),raw_type()}, {frequency(),raw_type()}) ->
	  proper_types:type().
weighted_default(Default, OtherType) ->
    weighted_union([Default, OtherType]).

%%------------------------------------------------------------------------------
%% Additional type specification functions
%%------------------------------------------------------------------------------

-spec resize(size(), raw_type()) -> proper_types:type().
resize(Size, RawType) ->
    Type = cook_outer(RawType),
    case find_prop(size_transform, Type) of
	{ok,Transform} ->
	    add_prop(size_transform, fun(_S) -> Transform(Size) end, Type);
	error ->
	    add_prop(size_transform, fun(_S) -> Size end, Type)
    end.

-spec non_empty(raw_type()) -> proper_types:type().
non_empty(RawListType) ->
    ?SUCHTHAT(L, RawListType, L =/= [] andalso L =/= <<>>).

-spec noshrink(raw_type()) -> proper_types:type().
noshrink(RawType) ->
    add_prop(noshrink, true, cook_outer(RawType)).

-spec with_parameter(atom(), value(), raw_type()) -> proper_types:type().
with_parameter(Param, Value, RawType) ->
    with_parameters([{Param,Value}], RawType).

-spec with_parameters([{atom(),value()}], raw_type()) -> proper_types:type().
with_parameters(PVlist, RawType) ->
    Type = cook_outer(RawType),
    case find_prop(parameters, Type) of
	{ok,Params} when is_list(Params) ->
	    append_list_to_prop(parameters, PVlist, Type);
	error ->
	    add_prop(parameters, PVlist, Type)
    end.

-spec parameter(atom()) -> value().
parameter(Param) ->
    parameter(Param, undefined).

-spec parameter(atom(), value()) -> value().
parameter(Param, Default) ->
    Parameters =
	case erlang:get('$parameters') of
	    undefined -> [];
	    List -> List
	end,
    proplists:get_value(Param, Parameters, Default).
