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
%%% @doc The shrinking subsystem and all predefined shrinkers are contained in
%%%	 this module.
%%% @private

-module(proper_shrink).

-export([shrink/3]).
-export([number_shrinker/4, union_first_choice_shrinker/3,
	 union_recursive_shrinker/3]).
-export([split_shrinker/3, remove_shrinker/3]).

-export_type([state/0, shrinker/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type state() :: 'init' | 'done' | {'shrunk',position(),state()} | term().
-type shrinker() :: fun((proper_gen:imm_instance(), proper_types:type(),
			 state()) -> {[proper_gen:imm_instance()],state()}).


%%------------------------------------------------------------------------------
%% Main shrinking functions
%%------------------------------------------------------------------------------

-spec shrink(proper_gen:imm_instance(), proper_types:type(), state()) ->
	  {[proper_gen:imm_instance()],state()}.
%% We reject all shrunk instances that don't satisfy all constraints. A full
%% is_instance check is not necessary if we assume that generators and shrinkers
%% always return valid instances of the base type.
shrink(ImmInstance, Type, init) ->
    Shrinkers = get_shrinkers(Type),
    shrink(ImmInstance, Type, {shrinker,Shrinkers,dummy,init});
shrink(_ImmInstance, _Type, {shrinker,[],_Lookup,init}) ->
    {[], done};
shrink(ImmInstance, Type, {shrinker,[_Shrinker | Rest],_Lookup,done}) ->
    shrink(ImmInstance, Type, {shrinker,Rest,dummy,init});
shrink(ImmInstance, Type, {shrinker,Shrinkers,_Lookup,State}) ->
    [Shrinker | _Rest] = Shrinkers,
    {DirtyImmInstances,NewState} = Shrinker(ImmInstance, Type, State),
    SatisfiesAll =
	fun(I) ->
	    Instance = proper_gen:clean_instance(I),
	    proper_types:weakly(proper_types:satisfies_all(Instance, Type))
	end,
    {NewImmInstances,NewLookup} =
	proper_arith:filter(SatisfiesAll, DirtyImmInstances),
    {NewImmInstances, {shrinker,Shrinkers,NewLookup,NewState}};
shrink(ImmInstance, Type, {shrunk,N,{shrinker,Shrinkers,Lookup,State}}) ->
    ActualN = lists:nth(N, Lookup),
    shrink(ImmInstance, Type,
	   {shrinker,Shrinkers,dummy,{shrunk,ActualN,State}}).

-spec get_shrinkers(proper_types:type()) -> [shrinker()].
get_shrinkers(Type) ->
    case proper_types:find_prop(noshrink, Type) of
	{ok, true} ->
	    [];
	_ ->
	    CustomShrinkers =
		case proper_types:find_prop(shrinkers, Type) of
		    {ok, Shrinkers} -> Shrinkers;
		    error           -> []
		end,
	    StandardShrinkers =
		case proper_types:get_prop(kind, Type) of
		    basic ->
			[];
		    wrapper ->
			[fun alternate_shrinker/3, fun unwrap_shrinker/3];
		    constructed ->
			case proper_types:get_prop(shrink_to_parts, Type) of
			    true ->
				[fun to_part_shrinker/3, fun parts_shrinker/3,
				 fun in_shrinker/3];
			    false ->
				[fun parts_shrinker/3, fun in_shrinker/3]
			end;
		    container ->
			[fun split_shrinker/3, fun remove_shrinker/3,
			 fun elements_shrinker/3];	    
		    _Other ->
			[]
		end,
	    CustomShrinkers ++ StandardShrinkers
    end.


%%------------------------------------------------------------------------------
%% Wrapper type shrinkers
%%------------------------------------------------------------------------------

%% Since shrinking only happens for generated values, any native types have
%% already been produced by the typeserver, thus we are sure we won't get a
%% typeserver exception.
-spec alternate_shrinker(proper_gen:imm_instance(), proper_types:type(),
			 state()) -> {[proper_gen:imm_instance()],state()}.
%% we stop at the smaller alternative shrinker
%% TODO: 'is_raw_type' check?
alternate_shrinker(Instance, Type, init) ->
    Choices = proper_types:unwrap(Type),
    union_first_choice_shrinker(Instance, Choices, init);
alternate_shrinker(_Instance, _Type, _State) ->
    {[], done}.

-spec unwrap_shrinker(proper_gen:imm_instance(), proper_types:type(),
		      state()) -> {[proper_gen:imm_instance()],state()}.
unwrap_shrinker(Instance, Type, init) ->
    Choices = proper_types:unwrap(Type),
    union_recursive_shrinker(Instance, Choices, init);
unwrap_shrinker(Instance, _Type, State) ->
    union_recursive_shrinker(Instance, [], State).


%%------------------------------------------------------------------------------
%% Constructed type shrinkers
%%------------------------------------------------------------------------------

-spec to_part_shrinker(proper_gen:imm_instance(), proper_types:type(),
		       state()) -> {[proper_gen:imm_instance()],state()}.
to_part_shrinker({'$used',ImmParts,_ImmInstance}, _Type, init) ->
    {[{'$to_part',P} || P <- ImmParts], done};
to_part_shrinker(_Instance, _Type, _State) ->
    {[], done}.

-spec parts_shrinker(proper_gen:imm_instance(), proper_types:type(), state()) ->
	  {[proper_gen:imm_instance()],state()}.
%% TODO: move some of the generation code in the proper_gen module
parts_shrinker(Instance = {'$used',_ImmParts,_ImmInstance}, Type, init) ->
    PartsType = proper_types:get_prop(parts_type, Type),
    parts_shrinker(Instance, Type, {parts,PartsType,dummy,init});
parts_shrinker(_CleanInstance, _Type, init) ->
    {[], done};
parts_shrinker(_Instance, _Type, {parts,_PartsType,_Lookup,done}) ->
    {[], done};
parts_shrinker({'$used',ImmParts,ImmInstance}, Type,
	       {parts,PartsType,_Lookup,PartsState}) ->
    {NewImmParts,NewPartsState} = shrink(ImmParts, PartsType, PartsState),
    Combine = proper_types:get_prop(combine, Type),
    DirtyInstances = [try_combine(P, ImmInstance, Combine) || P <- NewImmParts],
    NotError = fun({ok,_}) -> true; (error) -> false end,
    {NewOKInstances,NewLookup} = proper_arith:filter(NotError, DirtyInstances),
    NewInstances = [X || {ok,X} <- NewOKInstances],
    {NewInstances, {parts,PartsType,NewLookup,NewPartsState}};
parts_shrinker(Instance, Type,
	       {shrunk,N,{parts,PartsType,Lookup,PartsState}}) ->
    ActualN = lists:nth(N, Lookup),
    parts_shrinker(Instance, Type,
		   {parts,PartsType,dummy,{shrunk,ActualN,PartsState}}).

-spec try_combine(proper_gen:imm_instance(), proper_gen:imm_instance(),
		  proper_gen:combine_fun()) ->
	  {'ok',proper_gen:imm_instance()} | 'error'.
try_combine(ImmParts, OldImmInstance, Combine) ->
    Parts = proper_gen:clean_instance(ImmParts),
    ImmInstance = Combine(Parts),
    case proper_types:is_raw_type(ImmInstance) of
	true ->
	    InnerType = proper_types:cook_outer(ImmInstance),
	    %% TODO: special case if the immediately internal is a LET?
	    %% TODO: more specialized is_instance check here?
	    %% This should never throw an exception, provided the instance
	    %% has already been instance-checked.
	    case proper_types:is_instance(OldImmInstance, InnerType) of
		true ->
		    {ok,{'$used',ImmParts,OldImmInstance}};
		false ->
		    %% TODO: return more than one? then we must flatten
		    case proper_gen:safe_generate(InnerType) of
			{ok,NewImmInstance} ->
			    {ok,{'$used',ImmParts,NewImmInstance}};
			{error,_Reason} ->
			    error
		    end
	    end;
	false ->
	    {ok,{'$used',ImmParts,ImmInstance}}
    end.

-spec in_shrinker(proper_gen:imm_instance(), proper_types:type(), state()) ->
	  {[proper_gen:imm_instance()],state()}.
in_shrinker(Instance = {'$used',ImmParts,_ImmInstance}, Type, init) ->
    Combine = proper_types:get_prop(combine, Type),
    Parts = proper_gen:clean_instance(ImmParts),
    ImmInstance = Combine(Parts),
    %% TODO: more specialized raw_type check here?
    case proper_types:is_raw_type(ImmInstance) of
	true ->
	    InnerType = proper_types:cook_outer(ImmInstance),
	    in_shrinker(Instance, Type, {inner,InnerType,init});
	false ->
	    {[], done}
    end;
in_shrinker(Instance = {'$to_part',ImmInstance}, Type, init) ->
    %% TODO: move this to proper_types
    PartsType = proper_types:get_prop(parts_type, Type),
    case {proper_types:find_prop(internal_type,PartsType),
	  proper_types:find_prop(internal_types,PartsType)} of
	{{ok,EachPartType},error} ->
	    in_shrinker(Instance, Type, {part_rec,EachPartType,init});
	{error,{ok,PartTypesList}} ->
	    IsInst = fun(T) -> proper_types:is_instance(ImmInstance,T) end,
	    {_Pos,PartType} = proper_arith:find_first(IsInst, PartTypesList),
	    in_shrinker(Instance, Type, {part_rec,PartType,init})
    end;
in_shrinker(_CleanInstance, _Type, init) ->
    {[], done};
in_shrinker(_Instance, _Type, {_Decl,_RecType,done}) ->
    {[], done};
in_shrinker({'$used',ImmParts,ImmInstance}, _Type,
	    {inner,InnerType,InnerState}) ->
    {NewImmInstances,NewInnerState} =
	shrink(ImmInstance, InnerType, InnerState),
    NewInstances = [{'$used',ImmParts,I} || I <- NewImmInstances],
    {NewInstances, {inner,InnerType,NewInnerState}};
in_shrinker({'$to_part',ImmInstance}, _Type, {part_rec,PartType,PartState}) ->
    {NewImmInstances,NewPartState} = shrink(ImmInstance, PartType, PartState),
    NewInstances = [{'$to_part',I} || I <- NewImmInstances],
    {NewInstances, {part_rec,PartType,NewPartState}};
in_shrinker(Instance, Type, {shrunk,N,{Decl,RecType,InnerState}}) ->
    in_shrinker(Instance, Type, {Decl,RecType,{shrunk,N,InnerState}}).


%%------------------------------------------------------------------------------
%% Container type shrinkers
%%------------------------------------------------------------------------------

-spec split_shrinker(proper_gen:imm_instance(), proper_types:type(), state()) ->
	  {[proper_gen:imm_instance()],state()}.
split_shrinker(Instance, Type, init) ->
    case {proper_types:find_prop(split, Type),
	  proper_types:find_prop(get_length, Type),
	  proper_types:find_prop(join, Type)} of
	{error, _, _} ->
	    {[], done};
	{{ok,_Split}, error, _} ->
	    split_shrinker(Instance, Type, no_pos);
	{{ok,_Split}, {ok,GetLength}, {ok,_Join}} ->
	    split_shrinker(Instance, Type, {slices,2,GetLength(Instance)})
    end;
split_shrinker(Instance, Type, no_pos) ->
    Split = proper_types:get_prop(split, Type),
    {Split(Instance), done};
split_shrinker(Instance, Type, {shrunk,done}) ->
    split_shrinker(Instance, Type, no_pos);
%% implementation of the ddmin algorithm, but stopping before the granularity
%% reaches 1, since we run a 'remove' shrinker after this
%% TODO: on success, start over with the whole testcase or keep removing slices?
split_shrinker(Instance, Type, {slices,N,Len}) ->
    case Len < 2 * N of
	true ->
	    {[], done};
	false ->
	    {SmallSlices,BigSlices} = slice(Instance, Type, N, Len),
	    {SmallSlices ++ BigSlices, {slices,2*N,Len}}
    end;
split_shrinker(Instance, Type, {shrunk,Pos,{slices,DoubleN,_Len}}) ->
    N = DoubleN div 2,
    GetLength = proper_types:get_prop(get_length, Type),
    case Pos =< N of
	true ->
	    split_shrinker(Instance, Type, {slices,2,GetLength(Instance)});
	false ->
	    split_shrinker(Instance, Type, {slices,N-1,GetLength(Instance)})
    end.

-spec slice(proper_gen:imm_instance(), proper_types:type(), pos_integer(),
	    length()) ->
	  {[proper_gen:imm_instance()],[proper_gen:imm_instance()]}.
slice(Instance, Type, Slices, Len) ->
    BigSlices = Len rem Slices,
    SmallSlices = Slices - BigSlices,
    SmallSliceLen = Len div Slices,
    BigSliceLen = SmallSliceLen + 1,
    BigSliceTotal = BigSlices * BigSliceLen,
    WhereToSlice =
	[{1 + X * BigSliceLen, BigSliceLen}
	 || X <- lists:seq(0, BigSlices - 1)] ++
	[{BigSliceTotal + 1 + X * SmallSliceLen, SmallSliceLen}
	 || X <- lists:seq(0, SmallSlices - 1)],
    lists:unzip([take_slice(Instance, Type, From, SliceLen)
		 || {From,SliceLen} <- WhereToSlice]).

-spec take_slice(proper_gen:imm_instance(), proper_types:type(), pos_integer(),
		 length()) ->
	  {proper_gen:imm_instance(),proper_gen:imm_instance()}.
take_slice(Instance, Type, From, SliceLen) ->
    Split = proper_types:get_prop(split, Type),
    Join = proper_types:get_prop(join, Type),
    {Front,ImmBack} = Split(From - 1, Instance),
    {Slice,Back} = Split(SliceLen, ImmBack),
    {Slice, Join(Front, Back)}.

-spec remove_shrinker(proper_gen:imm_instance(), proper_types:type(),
		      state()) -> {[proper_gen:imm_instance()], state()}.
%% TODO: try removing more than one elemnent: 2,4,... or 2,3,... - when to stop?
remove_shrinker(Instance, Type, init) ->
    case {proper_types:find_prop(get_indices, Type),
	  proper_types:find_prop(remove, Type)} of
	{{ok,_GetIndices}, {ok,_Remove}} ->
	    remove_shrinker(Instance, Type,
			    {shrunk,1,{indices,ordsets:from_list([]),dummy}});
	_ ->
	    {[], done}
    end;
remove_shrinker(_Instance, _Type, {indices,_Checked,[]}) ->
    {[], done};
remove_shrinker(Instance, Type, {indices,Checked,[Index | Rest]}) ->
    Remove = proper_types:get_prop(remove, Type),
    {[Remove(Index, Instance)],
     {indices,ordsets:add_element(Index, Checked),Rest}};
remove_shrinker(Instance, Type, {shrunk,1,{indices,Checked,_ToCheck}}) ->
    %% TODO: normally, indices wouldn't be expected to change for the remaining
    %%       elements, but this happens for lists, so we'll just avoid
    %%       re-checking any indices we have already tried (even though these
    %%       might correspond to new elements now - at least they don't in the
    %%       case of lists)
    %% TODO: ordsets are used to ensure efficiency, but the ordsets module
    %%       compares elements with == instead of =:=, that could cause us to
    %%       miss some elements in some cases
    GetIndices = proper_types:get_prop(get_indices, Type),
    Indices = ordsets:from_list(GetIndices(Instance)),
    NewToCheck = ordsets:subtract(Indices, Checked),
    remove_shrinker(Instance, Type, {indices,Checked,NewToCheck}).

-spec elements_shrinker(proper_gen:imm_instance(), proper_types:type(),
			state()) -> {[proper_gen:imm_instance()],state()}.
%% TODO: is it safe to assume that all functions and the indices will not change
%%	 after any update?
%% TODO: shrink many elements concurrently?
elements_shrinker(Instance, Type, init) ->
    case {proper_types:find_prop(get_indices, Type),
	  proper_types:find_prop(retrieve, Type),
	  proper_types:find_prop(update, Type)} of
	{{ok,GetIndices}, {ok,Retrieve}, {ok,_Update}} ->
	    GetElemType =
		case proper_types:find_prop(internal_type, Type) of
		    {ok,RawInnerType} ->
			InnerType = proper_types:cook_outer(RawInnerType),
			fun(_I) -> InnerType end;
		    error ->
			InnerTypes =
			    proper_types:get_prop(internal_types, Type),
			fun(I) -> Retrieve(I, InnerTypes) end
		end,
	    Indices = GetIndices(Instance),
	    elements_shrinker(Instance, Type, {inner,Indices,GetElemType,init});
	_ ->
	    {[], done}
    end;
elements_shrinker(_Instance, _Type,
		  {inner,[],_GetElemType,init}) ->
    {[], done};
elements_shrinker(Instance, Type,
		  {inner,[_Index | Rest],GetElemType,done}) ->
    elements_shrinker(Instance, Type, {inner,Rest,GetElemType,init});
elements_shrinker(Instance, Type,
		  {inner,Indices = [Index | _Rest],GetElemType,InnerState}) ->
    Retrieve = proper_types:get_prop(retrieve, Type),
    Update = proper_types:get_prop(update, Type),
    ImmElem = Retrieve(Index, Instance),
    InnerType = GetElemType(Index),
    {NewImmElems,NewInnerState} = shrink(ImmElem, InnerType, InnerState),
    NewInstances = [Update(Index, E, Instance) || E <- NewImmElems],
    {NewInstances, {inner,Indices,GetElemType,NewInnerState}};
elements_shrinker(Instance, Type,
		  {shrunk,N,{inner,Indices,GetElemType,InnerState}}) ->
    elements_shrinker(Instance, Type,
		      {inner,Indices,GetElemType,{shrunk,N,InnerState}}).


%%------------------------------------------------------------------------------
%% Custom shrinkers
%%------------------------------------------------------------------------------

-spec number_shrinker(number(), proper_arith:extnum(), proper_arith:extnum(),
		      state()) -> {[number()],state()}.
number_shrinker(X, Low, High, init) ->
    {Target,Inc,OverLimit} = find_target(X, Low, High),
    case X =:= Target of
	true  -> {[], done};
	false -> {[Target], {inc,Target,Inc,OverLimit}}
    end;
number_shrinker(_X, _Low, _High, {inc,Last,Inc,OverLimit}) ->
    NewLast = Inc(Last),
    case OverLimit(NewLast) of
	true  -> {[], done};
	false -> {[NewLast], {inc,NewLast,Inc,OverLimit}}
    end;
number_shrinker(_X, _Low, _High, {shrunk,_Pos,_State}) ->
    {[], done}.

-spec find_target(number(), number(), number()) ->
	  {number(),fun((number()) -> number()),fun((number()) -> boolean())}.
find_target(X, Low, High) ->
    case {proper_arith:le(Low,0), proper_arith:le(0,High)} of
	{false, _} ->
	    Limit = find_limit(X, Low, High, High),
	    {Low, fun(Y) -> Y + 1 end, fun(Y) -> Y > Limit end};
	{true,false} ->
	    Limit = find_limit(X, Low, High, Low),
	    {High, fun(Y) -> Y - 1 end, fun(Y) -> Y < Limit end};
	{true,true} ->
	    Sign = sign(X),
	    OverLimit =
		case X >= 0 of
		    true ->
			Limit = find_limit(X, Low, High, High),
			fun(Y) -> Y > Limit end;
		    false ->
			Limit = find_limit(X, Low, High, Low),
			fun(Y) -> Y < Limit end
		end,
	    {zero(X), fun(Y) -> Y + Sign end, OverLimit}
    end.

-spec find_limit(number(), number(), number(), number()) -> number().
find_limit(X, Low, High, FallBack) ->
    case proper_arith:le(Low, X) andalso proper_arith:le(X, High) of
	true  -> X;
	false -> FallBack
    end.

-spec sign(number()) -> number().
sign(X) ->
    if
	X > 0 -> 1;
	X < 0 -> -1;
	true  -> zero(X)
    end.

-spec zero(number()) -> number().
zero(X) when is_integer(X) -> 0;
zero(X) when is_float(X)   -> 0.0.

-spec union_first_choice_shrinker(proper_gen:imm_instance(),
				  [proper_types:type()], state()) ->
	  {[proper_gen:imm_instance()],state()}.
%% TODO: do this incrementally?
union_first_choice_shrinker(Instance, Choices, init) ->
    case first_plausible_choice(Instance, Choices) of
	none ->
	    {[],done};
	{N,_Type} ->
	    PriorChoices = lists:sublist(Choices, N - 1),
	    DirtyInstances = [proper_gen:safe_generate(T) || T <- PriorChoices],
	    NewInstances = [X || {ok,X} <- DirtyInstances],
	    {NewInstances,done}
    end;
union_first_choice_shrinker(_Instance, _Choices, {shrunk,_Pos,done}) ->
    {[], done}.

-spec union_recursive_shrinker(proper_gen:imm_instance(), [proper_types:type()],
			       state()) ->
	  {[proper_gen:imm_instance()],state()}.
union_recursive_shrinker(Instance, Choices, init) ->
    case first_plausible_choice(Instance, Choices) of
	none ->
	    {[],done};
	{N,Type} ->
	    union_recursive_shrinker(Instance, Choices, {inner,N,Type,init})
    end;
union_recursive_shrinker(_Instance, _Choices, {inner,_N,_Type,done}) ->
    {[],done};
union_recursive_shrinker(Instance, _Choices, {inner,N,Type,InnerState}) ->
    {NewInstances,NewInnerState} = shrink(Instance, Type, InnerState),
    {NewInstances, {inner,N,Type,NewInnerState}};
union_recursive_shrinker(Instance, Choices,
			 {shrunk,Pos,{inner,N,Type,InnerState}}) ->
    union_recursive_shrinker(Instance, Choices,
			     {inner,N,Type,{shrunk,Pos,InnerState}}).

-spec first_plausible_choice(proper_gen:imm_instance(),
			     [proper_types:type()]) ->
	  {position(),proper_types:type()} | 'none'.
first_plausible_choice(Instance, Choices) ->
    %% This should never throw an exception, provided the instance has already
    %% been instance-checked.
    IsInstance = fun(Type) -> proper_types:is_instance(Instance, Type) end,
    proper_arith:find_first(IsInstance, Choices).
