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
%%% @doc This module contains PropEr's helper parse transformer. It is
%%%	 automatically applied to modules when compiled internally by the
%%%	 typeserver. It essentially removes all functions that contain ?FORALLs,
%%%	 to counter an obscure bug.
%%% @private

-module(proper_prop_remover).
-export([parse_transform/2]).

-export_type([]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Top-level functions
%%------------------------------------------------------------------------------

-spec parse_transform([abs_form()], [compile:option()]) -> [abs_form()].
parse_transform(Forms, _Options) ->
    [Form || Form <- Forms, safe_form(Form)].

-spec safe_form(abs_form()) -> boolean().
safe_form({function,_Line,_Name,_Arity,Clauses}) ->
    lists:all(fun safe_clause/1, Clauses);
safe_form(_Form) ->
    true.

-spec safe_clause(abs_clause()) -> boolean().
safe_clause({clause,_Line,PatSeq,_Guards,Body}) ->
    lists:all(fun safe_expr/1, PatSeq)
    andalso lists:all(fun safe_expr/1, Body).

%% This also covers some other constructs that don't clash with expressions:
%% binary element specifications, list and binary comprehension generators and
%% filters, remote function references. It also covers patterns.
-spec safe_expr(abs_expr()) -> boolean().
safe_expr({match,_Line,Pattern,Expr}) ->
    safe_expr(Pattern) andalso safe_expr(Expr);
safe_expr({tuple,_Line,FieldExprs}) ->
    lists:all(fun safe_expr/1, FieldExprs);
safe_expr({cons,_Line,HeadExpr,TailExpr}) ->
    safe_expr(HeadExpr) andalso safe_expr(TailExpr);
safe_expr({bin,_Line,BinElems}) ->
    lists:all(fun safe_expr/1, BinElems);
safe_expr({bin_element,_Line,ValueExpr,_Size,_TSL}) ->
    safe_expr(ValueExpr);
safe_expr({op,_Line,_Op,LeftExpr,RightExpr}) ->
    safe_expr(LeftExpr) andalso safe_expr(RightExpr);
safe_expr({op,_Line,_Op,Expr}) ->
    safe_expr(Expr);
safe_expr({record,_Line,_RecName,FieldInits}) ->
    lists:all(fun safe_field_init/1, FieldInits);
safe_expr({record,_Line,RecExpr,_RecName,FieldInits}) ->
    safe_expr(RecExpr) andalso lists:all(fun safe_field_init/1, FieldInits);
safe_expr({record_field,_Line,RecExpr,_RecName,_FieldName}) ->
    safe_expr(RecExpr);
safe_expr({'catch',_Line,Expr}) ->
    safe_expr(Expr);
safe_expr({call,_Line,FunRef,Args}) ->
    safe_expr(FunRef) andalso lists:all(fun safe_expr/1, Args);
safe_expr({remote,_Line,{atom,_,proper},{atom,_,forall}}) ->
    false;
safe_expr({remote,_Line,ModExpr,FunExpr}) ->
    safe_expr(ModExpr) andalso safe_expr(FunExpr);
safe_expr({lc,_Line,Expr,GensAndFilters}) ->
    safe_expr(Expr) andalso lists:all(fun safe_expr/1, GensAndFilters);
safe_expr({bc,_Line,Expr,GensAndFilters}) ->
    safe_expr(Expr) andalso lists:all(fun safe_expr/1, GensAndFilters);
safe_expr({generate,_Line,Pattern,Expr}) ->
    safe_expr(Pattern) andalso safe_expr(Expr);
safe_expr({b_generate,_Line,Pattern,Expr}) ->
    safe_expr(Pattern) andalso safe_expr(Expr);
safe_expr({block,_Line,Body}) ->
    lists:all(fun safe_expr/1, Body);
safe_expr({'if',_Line,Clauses}) ->
    lists:all(fun safe_clause/1, Clauses);
safe_expr({'case',_Line,Expr,Clauses}) ->
    safe_expr(Expr) andalso lists:all(fun safe_clause/1, Clauses);
safe_expr({'try',_Line,Body1,Clauses1,Clauses2,Body2}) ->
    lists:all(fun safe_expr/1, Body1)
    andalso lists:all(fun safe_clause/1, Clauses1)
    andalso lists:all(fun safe_clause/1, Clauses2)
    andalso lists:all(fun safe_expr/1, Body2);
safe_expr({'receive',_Line,Clauses}) ->
    lists:all(fun safe_clause/1, Clauses);
safe_expr({'receive',_Line,Clauses,AfterExpr,AfterBody}) ->
    lists:all(fun safe_clause/1, Clauses)
    andalso safe_expr(AfterExpr)
    andalso lists:all(fun safe_expr/1, AfterBody);
safe_expr({'fun',_Line,{clauses,Clauses}}) ->
    lists:all(fun safe_clause/1, Clauses);
safe_expr({'query',_Line,ListCompr}) ->
    safe_expr(ListCompr);
safe_expr({record_field,_Line,Expr,_FieldName}) ->
    safe_expr(Expr);
safe_expr(_Expr) ->
    true.

-spec safe_field_init(abs_rec_field()) -> boolean().
safe_field_init({record_field,_Line,_FieldName}) ->
    true;
safe_field_init({record_field,_Line,_FieldName,InitExpr}) ->
    safe_expr(InitExpr).
