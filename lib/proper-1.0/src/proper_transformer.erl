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

%%% @doc PropEr's main parse transform. It is automatically applied to modules
%%% when including the main PropEr header, unless `PROPER_NO_TRANS' is defined.
%%% Applying this transform has the following effects:
%%% <ul>
%%% <li>All 0-arity functions whose name begins with `prop_' are automatically
%%%   exported.</li>
%%% <li>Type declarations in `?FORALL's that correspond to native types are
%%%   properly substituted (with some limitations, see the
%%%   {@link proper_typeserver} module for details).</li>
%%% </ul>

-module(proper_transformer).
-export([parse_transform/2]).

-export_type([]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-record(mod_info, {name                    :: mod_name(),
		   export_all = false      :: boolean(),
		   funs       = sets:new() :: proper_typeserver:mod_exp_funs(),
		   imports    = sets:new() :: proper_typeserver:mod_exp_funs(),
		   no_autos   = sets:new() :: proper_typeserver:mod_exp_funs(),
		   exp_types  = sets:new() :: proper_typeserver:mod_exp_types(),
		   exp_funs   = sets:new() :: proper_typeserver:mod_exp_funs(),
		   helper_pid              :: pid()}).
-type mod_info() :: #mod_info{}.
-type exp_dict() :: dict().
%% dict(mod_name(),'no_data' | {'data',proper_typeserver:mod_exp_types(),
%%                                     proper_typeserver:mod_exp_funs()})


%%------------------------------------------------------------------------------
%% Top-level functions
%%------------------------------------------------------------------------------

%% @private
-spec parse_transform([abs_form()], [compile:option()]) -> [abs_form()].
parse_transform(Forms, Options) ->
    RawModInfo = collect_info(Forms, Options),
    #mod_info{name = ModName, export_all = ExportAll, funs = AllFuns,
	      exp_types = ExpTypes, exp_funs = RawExpFuns} = RawModInfo,
    {ExpFuns,PropsToExport} =
	case ExportAll of
	    true ->
		{AllFuns, []};
	    false ->
		AllProps = sets:filter(fun is_prop/1, AllFuns),
		{RawExpFuns, sets:to_list(sets:subtract(AllProps,RawExpFuns))}
	end,
    HelperPid = helper_start(ModName, ExpTypes, ExpFuns),
    ModInfo = RawModInfo#mod_info{helper_pid = HelperPid},
    NewForms = [rewrite_form(F,ModInfo) || F <- Forms],
    helper_stop(HelperPid),
    add_exports(NewForms, PropsToExport).

-spec collect_info([abs_form()], [compile:option()]) -> mod_info().
collect_info(Forms, Options) ->
    StartModInfo = add_options(#mod_info{}, Options),
    lists:foldl(fun add_info/2, StartModInfo, Forms).

-spec add_options(mod_info(), compile:option() | [compile:option()]) ->
	  mod_info().
add_options(ModInfo, []) ->
    ModInfo;
add_options(ModInfo, [export_all | Rest]) ->
    add_options(ModInfo#mod_info{export_all = true}, Rest);
add_options(#mod_info{no_autos = NoAutos} = ModInfo,
	    [{no_auto_import,FunsList} | Rest]) ->
    NewNoAutos = sets:union(sets:from_list(FunsList), NoAutos),
    add_options(ModInfo#mod_info{no_autos = NewNoAutos}, Rest);
add_options(ModInfo, [_OtherOption | Rest]) ->
    add_options(ModInfo, Rest);
add_options(ModInfo, SingleOption) ->
    add_options(ModInfo, [SingleOption]).

-spec add_info(abs_form(), mod_info()) -> mod_info().
add_info({attribute,_Line,module,ModName}, ModInfo) ->
    ModInfo#mod_info{name = ModName};
add_info({function,_Line,Name,Arity,_Clauses},
	 #mod_info{funs = Funs} = ModInfo) ->
    NewFuns = sets:add_element({Name,Arity}, Funs),
    ModInfo#mod_info{funs = NewFuns};
add_info({attribute,_Line,import,{_FromMod,MoreImports}},
	 #mod_info{imports = Imports} = ModInfo) ->
    NewImports = sets:union(sets:from_list(MoreImports), Imports),
    ModInfo#mod_info{imports = NewImports};
add_info({attribute,_Line,export_type,MoreExpTypes},
	 #mod_info{exp_types = ExpTypes} = ModInfo) ->
    NewExpTypes = sets:union(sets:from_list(MoreExpTypes), ExpTypes),
    ModInfo#mod_info{exp_types = NewExpTypes};
add_info({attribute,_Line,export,MoreExpFuns},
	 #mod_info{exp_funs = ExpFuns} = ModInfo) ->
    NewExpFuns = sets:union(sets:from_list(MoreExpFuns), ExpFuns),
    ModInfo#mod_info{exp_funs = NewExpFuns};
add_info({attribute,_Line,compile,Options}, ModInfo) ->
    add_options(ModInfo, Options);
add_info(_Form, ModInfo) ->
    ModInfo.

-spec is_prop({fun_name(),arity()}) -> boolean().
is_prop({Name,0}) ->
    lists:prefix(?PROPERTY_PREFIX, atom_to_list(Name));
is_prop(_) ->
    false.

-spec add_exports([abs_form()], [{fun_name(),arity()}]) -> [abs_form()].
add_exports(Forms, ToExport) ->
    add_exports_tr(Forms, [], ToExport).

-spec add_exports_tr([abs_form()], [abs_form()], [{fun_name(),arity()}]) ->
	  [abs_form()].
add_exports_tr([], Acc, _ToExport) ->
    lists:reverse(Acc);
add_exports_tr([{attribute,_,module,_} = ModAttr | Rest], Acc, ToExport) ->
    ExpAttr = {attribute,0,export,ToExport},
    lists:reverse(Acc) ++ [ModAttr, ExpAttr | Rest];
add_exports_tr([Form | Rest], Acc, ToExport) ->
    add_exports_tr(Rest, [Form | Acc], ToExport).


%%------------------------------------------------------------------------------
%% Helper server interface
%%------------------------------------------------------------------------------

-spec helper_start(mod_name(), proper_typeserver:mod_exp_types(),
		   proper_typeserver:mod_exp_funs()) -> pid().
helper_start(Mod, ModExpTypes, ModExpFuns) ->
    spawn(fun() -> helper_init(Mod,ModExpTypes,ModExpFuns) end).

-spec helper_stop(pid()) -> 'ok'.
helper_stop(HelperPid) ->
    HelperPid ! stop,
    ok.

-spec is_exported_type(mod_name(), atom(), arity(), pid()) -> boolean().
is_exported_type(Mod, Call, Arity, HelperPid) ->
    HelperPid ! {is_exported_type,self(),Mod,Call,Arity},
    receive
	Answer -> Answer
    end.

-spec helper_init(mod_name(), proper_typeserver:mod_exp_types(),
		  proper_typeserver:mod_exp_funs()) -> 'ok'.
helper_init(Mod, ModExpTypes, ModExpFuns) ->
    ExpDict = dict:from_list([{Mod,{data,ModExpTypes,ModExpFuns}}]),
    helper_loop(ExpDict).

-spec helper_loop(exp_dict()) -> 'ok'.
helper_loop(ExpDict) ->
    receive
	stop ->
	    ok;
	{is_exported_type,From,Mod,Call,Arity} ->
	    NewExpDict = add_module(Mod, ExpDict),
	    Answer = case dict:fetch(Mod, NewExpDict) of
			 {data,ModExpTypes,ModExpFuns} ->
			     CallRef = {Call,Arity},
			     not sets:is_element(CallRef, ModExpFuns)
			     andalso sets:is_element(CallRef, ModExpTypes);
			 nodata ->
			     false
		     end,
	    From ! Answer,
	    helper_loop(NewExpDict)
    end.

-spec add_module(mod_name(), exp_dict()) -> exp_dict().
add_module(Mod, ExpDict) ->
    case dict:is_key(Mod, ExpDict) of
	true ->
	    ExpDict;
	false ->
	    case proper_typeserver:get_exp_info(Mod) of
		{ok,ModExpTypes,ModExpFuns} ->
		    dict:store(Mod, {data,ModExpTypes,ModExpFuns}, ExpDict);
		{error,_Reason} ->
		    dict:store(Mod, nodata, ExpDict)
	    end
    end.


%%------------------------------------------------------------------------------
%% ?FORALL detection functions
%%------------------------------------------------------------------------------

-spec rewrite_form(abs_form(), mod_info()) -> abs_form().
rewrite_form({attribute,Line,record,{RecName,FieldInits}}, ModInfo) ->
    NewFieldInits = [rewrite_field_init(F,ModInfo) || F <- FieldInits],
    {attribute,Line,record,{RecName,NewFieldInits}};
rewrite_form({function,Line,Name,Arity,Clauses}, ModInfo) ->
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {function,Line,Name,Arity,NewClauses};
rewrite_form(Form, _ModInfo) ->
    Form.

-spec rewrite_field_init(abs_rec_field(), mod_info()) -> abs_rec_field().
rewrite_field_init({record_field,_Line,_FieldName} = FieldInit, _ModInfo) ->
    FieldInit;
rewrite_field_init({record_field,Line,FieldName,InitExpr}, ModInfo) ->
    {record_field,Line,FieldName,rewrite_expr(InitExpr,ModInfo)}.

-spec rewrite_clause(abs_clause(), mod_info()) -> abs_clause().
rewrite_clause({clause,Line,PatSeq,Guards,Body}, ModInfo) ->
    NewPatSeq = [rewrite_expr(P,ModInfo) || P <- PatSeq],
    NewBody = [rewrite_expr(E,ModInfo) || E <- Body],
    {clause,Line,NewPatSeq,Guards,NewBody}.

%% This also covers some other constructs that don't clash with expressions:
%% binary element specifications, list and binary comprehension generators and
%% filters, remote function references. It also covers patterns.
-spec rewrite_expr(abs_expr(), mod_info()) -> abs_expr().
rewrite_expr({match,Line,Pattern,Expr}, ModInfo) ->
    NewPattern = rewrite_expr(Pattern, ModInfo),
    NewExpr = rewrite_expr(Expr, ModInfo),
    {match,Line,NewPattern,NewExpr};
rewrite_expr({tuple,Line,FieldExprs}, ModInfo) ->
    NewFieldExprs = [rewrite_expr(F,ModInfo) || F <- FieldExprs],
    {tuple,Line,NewFieldExprs};
rewrite_expr({cons,Line,HeadExpr,TailExpr}, ModInfo) ->
    NewHeadExpr = rewrite_expr(HeadExpr, ModInfo),
    NewTailExpr = rewrite_expr(TailExpr, ModInfo),
    {cons,Line,NewHeadExpr,NewTailExpr};
rewrite_expr({bin,Line,BinElems}, ModInfo) ->
    NewBinElems = [rewrite_expr(B,ModInfo) || B <- BinElems],
    {bin,Line,NewBinElems};
rewrite_expr({bin_element,Line,ValueExpr,Size,TSL}, ModInfo) ->
    {bin_element,Line,rewrite_expr(ValueExpr,ModInfo),Size,TSL};
rewrite_expr({op,Line,Op,LeftExpr,RightExpr}, ModInfo) ->
    NewLeftExpr = rewrite_expr(LeftExpr, ModInfo),
    NewRightExpr = rewrite_expr(RightExpr, ModInfo),
    {op,Line,Op,NewLeftExpr,NewRightExpr};
rewrite_expr({op,Line,Op,Expr}, ModInfo) ->
    {op,Line,Op,rewrite_expr(Expr,ModInfo)};
rewrite_expr({record,Line,RecName,FieldInits}, ModInfo) ->
    NewFieldInits = [rewrite_field_init(F,ModInfo) || F <- FieldInits],
    {record,Line,RecName,NewFieldInits};
rewrite_expr({record,Line,RecExpr,RecName,FieldInits}, ModInfo) ->
    NewRecExpr = rewrite_expr(RecExpr, ModInfo),
    NewFieldInits = [rewrite_field_init(F,ModInfo) || F <- FieldInits],
    {record,Line,NewRecExpr,RecName,NewFieldInits};
rewrite_expr({record_field,Line,RecExpr,RecName,FieldName}, ModInfo) ->
    {record_field,Line,rewrite_expr(RecExpr,ModInfo),RecName,FieldName};
rewrite_expr({'catch',Line,Expr}, ModInfo) ->
    {'catch',Line,rewrite_expr(Expr,ModInfo)};
rewrite_expr({call,Line,
	      {remote,_,{atom,_,proper},{atom,_,forall}} = FunRef,
	      [RawType,Prop]}, ModInfo) ->
    NewRawType = rewrite_type(RawType, ModInfo),
    NewProp = rewrite_expr(Prop, ModInfo),
    {call,Line,FunRef,[NewRawType,NewProp]};
rewrite_expr({call,Line,FunRef,Args}, ModInfo) ->
    NewFunRef = rewrite_expr(FunRef, ModInfo),
    NewArgs = [rewrite_expr(A,ModInfo) || A <- Args],
    {call,Line,NewFunRef,NewArgs};
rewrite_expr({remote,Line,ModExpr,FunExpr}, ModInfo) ->
    NewModExpr = rewrite_expr(ModExpr, ModInfo),
    NewFunExpr = rewrite_expr(FunExpr, ModInfo),
    {remote,Line,NewModExpr,NewFunExpr};
rewrite_expr({lc,Line,Expr,GensAndFilters}, ModInfo) ->
    NewExpr = rewrite_expr(Expr, ModInfo),
    NewGensAndFilters = [rewrite_expr(W,ModInfo) || W <- GensAndFilters],
    {lc,Line,NewExpr,NewGensAndFilters};
rewrite_expr({bc,Line,Expr,GensAndFilters}, ModInfo) ->
    NewExpr = rewrite_expr(Expr, ModInfo),
    NewGensAndFilters = [rewrite_expr(W,ModInfo) || W <- GensAndFilters],
    {bc,Line,NewExpr,NewGensAndFilters};
rewrite_expr({generate,Line,Pattern,Expr}, ModInfo) ->
    NewPattern = rewrite_expr(Pattern, ModInfo),
    NewExpr = rewrite_expr(Expr, ModInfo),
    {generate,Line,NewPattern,NewExpr};
rewrite_expr({b_generate,Line,Pattern,Expr}, ModInfo) ->
    NewPattern = rewrite_expr(Pattern, ModInfo),
    NewExpr = rewrite_expr(Expr, ModInfo),
    {b_generate,Line,NewPattern,NewExpr};
rewrite_expr({block,Line,Body}, ModInfo) ->
    NewBody = [rewrite_expr(E,ModInfo) || E <- Body],
    {block,Line,NewBody};
rewrite_expr({'if',Line,Clauses}, ModInfo) ->
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {'if',Line,NewClauses};
rewrite_expr({'case',Line,Expr,Clauses}, ModInfo) ->
    NewExpr = rewrite_expr(Expr, ModInfo),
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {'case',Line,NewExpr,NewClauses};
rewrite_expr({'try',Line,Body1,Clauses1,Clauses2,Body2}, ModInfo) ->
    NewBody1 = [rewrite_expr(E,ModInfo) || E <- Body1],
    NewClauses1 = [rewrite_clause(C,ModInfo) || C <- Clauses1],
    NewClauses2 = [rewrite_clause(C,ModInfo) || C <- Clauses2],
    NewBody2 = [rewrite_expr(E,ModInfo) || E <- Body2],
    {'try',Line,NewBody1,NewClauses1,NewClauses2,NewBody2};
rewrite_expr({'receive',Line,Clauses}, ModInfo) ->
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {'receive',Line,NewClauses};
rewrite_expr({'receive',Line,Clauses,AfterExpr,AfterBody}, ModInfo) ->
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    NewAfterExpr = rewrite_expr(AfterExpr, ModInfo),
    NewAfterBody = [rewrite_expr(E,ModInfo) || E <- AfterBody],
    {'receive',Line,NewClauses,NewAfterExpr,NewAfterBody};
rewrite_expr({'fun',Line,{clauses,Clauses}}, ModInfo) ->
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {'fun',Line,{clauses,NewClauses}};
rewrite_expr({'query',Line,ListCompr}, ModInfo) ->
    {'query',Line,rewrite_expr(ListCompr,ModInfo)};
rewrite_expr({record_field,Line,Expr,FieldName}, ModInfo) ->
    {record_field,Line,rewrite_expr(Expr,ModInfo),FieldName};
rewrite_expr(Expr, _ModInfo) ->
    Expr.


%%------------------------------------------------------------------------------
%% Type rewriting functions
%%------------------------------------------------------------------------------

-spec rewrite_type(abs_expr(), mod_info()) -> abs_expr().
rewrite_type({tuple,Line,FieldExprs}, ModInfo) ->
    NewFieldExprs = [rewrite_type(F,ModInfo) || F <- FieldExprs],
    {tuple,Line,NewFieldExprs};
rewrite_type({cons,Line,HeadExpr,TailExpr}, ModInfo) ->
    NewHeadExpr = rewrite_type(HeadExpr, ModInfo),
    NewTailExpr = rewrite_type(TailExpr, ModInfo),
    {cons,Line,NewHeadExpr,NewTailExpr};
rewrite_type({op,Line,'++',LeftExpr,RightExpr}, ModInfo) ->
    NewLeftExpr = rewrite_type(LeftExpr, ModInfo),
    NewRightExpr = rewrite_type(RightExpr, ModInfo),
    {op,Line,'++',NewLeftExpr,NewRightExpr};
rewrite_type({call,Line,{remote,_,{atom,_,Mod},{atom,_,Call}} = FunRef,
	      Args} = Expr,
	     #mod_info{name = ModName, helper_pid = HelperPid} = ModInfo) ->
    case is_exported_type(Mod, Call, length(Args), HelperPid) of
	true ->
	    native_type_call(ModName, Expr);
	false ->
	    NewArgs = [rewrite_type(A,ModInfo) || A <- Args],
	    {call,Line,FunRef,NewArgs}
    end;
rewrite_type({call,Line,{atom,_,Fun} = FunRef,Args} = Expr,
	     #mod_info{name = ModName, funs = Funs, imports = Imports,
		       no_autos = NoAutos} = ModInfo) ->
    Arity = length(Args),
    CallRef = {Fun,Arity},
    case sets:is_element(CallRef,Funs) orelse sets:is_element(CallRef,Imports)
	 orelse erl_internal:bif(Fun,Arity)
		andalso not sets:is_element(CallRef,NoAutos) of
	true ->
	    NewArgs = [rewrite_type(A,ModInfo) || A <- Args],
	    {call,Line,FunRef,NewArgs};
	false ->
	    native_type_call(ModName, Expr)
    end;
rewrite_type(Expr, _ModInfo) ->
    Expr.

-spec native_type_call(mod_name(), abs_expr()) -> abs_expr().
native_type_call(ModName, Expr) ->
    AbsModName = {atom,0,ModName},
    AbsTypeStr = {string,0,lists:flatten(erl_pp:expr(Expr))},
    FunRef = {remote,0,{atom,0,proper_types},{atom,0,native_type}},
    {call,0,FunRef,[AbsModName,AbsTypeStr]}.
