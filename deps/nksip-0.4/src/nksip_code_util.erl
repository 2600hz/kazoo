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

%% @doc NkSIP Erlang code parser and hot loader utilities

-module(nksip_code_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([expression/1, getter/2, fun_expr/4, call_expr/4, callback_expr/3]).
-export([case_expr/5, compile/2]).
-export([get_funs/1]).


%% ===================================================================
%% Private
%% ===================================================================


%% @doc Parses an erlang expression intro a syntaxTree()
%% i.e. expres
-spec expression(string()) ->
    {ok, erl_syntax:syntaxTree()} | error.

expression(Expr) ->
    case erl_scan:string(Expr) of
        {ok, Tokens, _} ->
            case erl_parse:parse_form(Tokens) of
                {ok, Form} -> {ok, Form};
                _ -> error
            end;
        _ ->
            error
    end.


%% @doc Generates a getter function (fun() -> Value.)
-spec getter(atom(), term()) ->
    erl_syntax:syntaxTree().

getter(Fun, Value) ->
    erl_syntax:function(
       erl_syntax:atom(Fun),
       [erl_syntax:clause([], none, [erl_syntax:abstract(Value)])]).


%% @doc Generates a function expression (fun(A1,B1,..) -> Value)
%% Vers represents the suffix to use in the variable names
-spec fun_expr(atom(), pos_integer(), pos_integer(), term()) ->
    erl_syntax:syntaxTree().

fun_expr(Fun, Arity, Vers, Value) ->
    erl_syntax:function(
        erl_syntax:atom(Fun),
        [erl_syntax:clause(var_list(Arity, Vers), none, Value)]).


%% @doc Generates a call expression (mod:fun(A1,B1,..))
%% Vers represents the suffix to use in the variable names.
-spec call_expr(atom(), atom(), pos_integer(), pos_integer()) ->
    erl_syntax:syntaxTree().

call_expr(Mod, Fun, Arity, Vers) ->
    erl_syntax:application(
        erl_syntax:atom(Mod),
        erl_syntax:atom(Fun),
        var_list(Arity, Vers)).


%% @doc Generates a call expression (fun(A0,B0...) -> mod:fun(A0,B0,..))
-spec callback_expr(atom(), atom(), pos_integer()) ->
    erl_syntax:syntaxTree().

callback_expr(Mod, Fun, Arity) ->
    fun_expr(Fun, Arity, 0, [call_expr(Mod, Fun, Arity, 0)]).


%% @doc Generates a case expression
%% case mod:fun(A2,B2...) of
%%     continue -> [A1,B1..] = [A2,B2..], (NextCode);
%%     {continue, (NextCode); 
%%     Other -> Other
%% end
%% Vers represents the suffix to use in the variable names.
-spec case_expr(atom(), atom(), pos_integer(), pos_integer(), 
               [erl_syntax:syntaxTree()]) ->
    erl_syntax:syntaxTree().

case_expr(Mod, Fun, Arity, Vers, NextCode) ->
    erl_syntax:case_expr(
        call_expr(Mod, Fun, Arity, Vers),
        [
            erl_syntax:clause(
                [erl_syntax:atom(continue)],
                none,
                case Arity of
                    0 ->
                        NextCode;
                    _ ->
                        [
                            erl_syntax:match_expr(
                                erl_syntax:list(var_list(Arity, Vers-1)),
                                erl_syntax:list(var_list(Arity, Vers)))
                        | NextCode]
                end),
            erl_syntax:clause(
                [erl_syntax:tuple([
                    erl_syntax:atom(continue), 
                    erl_syntax:list(var_list(Arity, Vers-1))])],
                none,
                NextCode),
            erl_syntax:clause(
                [erl_syntax:variable('Other')],
                none,
                [erl_syntax:variable('Other')])
        ]).


%% @doc Compiles a syntaxTree into a module
-spec compile(atom(), [erl_syntax:syntaxTree()]) ->
    ok | {error, term()}.

compile(Mod, Tree) ->
    Tree1 = [
        erl_syntax:attribute(
            erl_syntax:atom(module),
            [erl_syntax:atom(Mod)]),
        erl_syntax:attribute(
            erl_syntax:atom(compile),
            [erl_syntax:list([erl_syntax:atom(export_all)])])
        | Tree
    ],

    % io:format("\nGenerated ~p:\n\n", [Mod]),
    % [io:format("~s\n\n", [erl_prettypr:format(S)]) || S<-Tree],
   
    Forms1 = [erl_syntax:revert(X) || X <- Tree1],
    Options = [report_errors, report_warnings, return_errors],
    case compile:forms(Forms1, Options) of
        {ok, Mod, Bin} ->
            code:purge(Mod),
            File = atom_to_list(Mod)++".erl",
            case code:load_binary(Mod, File, Bin) of
                {module, Mod} -> ok;
                Error -> {error, Error}
            end;
        Error ->
            {error, Error}
    end.


%% @doc Gets the list of exported functions of a module
-spec get_funs(atom()) ->
    [{atom(), pos_integer()}] | error.

get_funs(Mod) ->
    case catch Mod:module_info() of
        List when is_list(List) ->
            lists:foldl(
                fun({Fun, Arity}, Acc) ->
                    case Fun of
                        module_info -> Acc;
                        behaviour_info -> Acc;
                        _ -> [{Fun, Arity}|Acc]
                    end
                end,
                [],
                nksip_lib:get_value(exports, List));
        _ ->
            error
    end.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private Generates a var list (A1,B1..)
var_list(Arity, Vers) ->
    VersS = nksip_lib:to_list(Vers),
    [erl_syntax:variable([V|VersS]) || V <- lists:seq(65, 64+Arity)].




