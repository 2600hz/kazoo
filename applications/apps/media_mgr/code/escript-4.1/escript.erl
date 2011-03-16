%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 2002, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(escript).

-export([start/1, interpret/1]).

-import(lists, [foldl/3, map/2, member/2, reverse/1]).

start([_|X0]=_L) ->
    %% io:format("Escript L=~p~n",[_L]),
    X = map(fun(I) ->
		    [_EscapeChar | Arg] = atom_to_list(I),
		    Arg
	    end, X0),
    %% io:format("Escript X=~p~n",[X]),
    [File|Args] = X,
    %% io:format("File=~p~n",[File]),
    {Nerrs, Parse, Mode} = parse_file(File),
    if
	Nerrs > 0 ->
	    io:format("Script terminated~n"),
	    erlang:halt();
	true ->
	    case Mode of
		interpret ->
		    interpret(File, Parse, Args);
		compile ->
		    compile(Parse, Args)
	    end
    end.

interpret([AFile]) ->
    File = atom_to_list(AFile),
    {Nerrs, Parse, Mode} = parse_include_file(File, 0, [], interpret),
    Args = [],
    if
	Nerrs > 0 ->
	    io:format("Script terminated~n"),
	    erlang:halt();
	true ->
	    case Mode of
		interpret ->
		    interpret(File, Parse, Args);
		compile ->
		    compile(Parse, Args)
	    end
    end.

interpret(File, Parse, Args) ->
    Dict  = parse_to_dict(Parse),
    ArgsA = erl_parse:abstract(Args, 999),
    Call = {call,999,{atom,999,main},[ArgsA]},
    erl_eval:expr(Call,
		  erl_eval:new_bindings(),
		  {value, fun(I, J) ->
				  code_handler(I, J, Dict, File)
			  end}),
    erlang:halt().


compile(Parse, Args) ->
    Mod = mk_mod(),
    case compile:forms([Mod|Parse]) of
	{ok, Module, BeamCode} -> 
	    erlang:load_module(Module, BeamCode),
	    apply(Module, main, [Args]),
	    erlang:halt();
	_Other ->
	    io:format("Compiler errors~n"),
	    erlang:halt()
    end.


parse_to_dict(L) -> parse_to_dict(L, dict:new()).

parse_to_dict([{function,_,Name,Arity,Clauses}|T], Dict0) ->
    Dict = dict:store({local, Name,Arity}, Clauses, Dict0),
    parse_to_dict(T, Dict);
parse_to_dict([{attribute,_,import,{Mod,Funcs}}|T], Dict0) ->
    Dict = foldl(fun(I, D) ->
			 dict:store({remote,I}, Mod, D)
		 end, Dict0, Funcs),
    parse_to_dict(T, Dict);
parse_to_dict([_|T], Dict) ->
    parse_to_dict(T, Dict);
parse_to_dict([], Dict) ->
    Dict.

%% make a temporary module name

mk_mod() ->
    {I,J,K} = erlang:now(),
    Mod = list_to_atom("tmp" ++ integer_to_list(I) ++ integer_to_list(J) ++
		       integer_to_list(K)),
    {attribute,999,module, Mod}.

parse_file(File) ->
    {Nerrs, L, Mode} = parse_file(File, 0, [], interpret),
    {Nerrs, reverse(L), Mode}.

parse_file(File, Nerrs, L, Mode) ->
    {ok, P} = file:open(File, read),
    %% This is to skip the first line in the script
    io:get_line(P, ''),
    Ret = parse_loop(P, File, io:parse_erl_form(P, '', 2), Nerrs, L, Mode),
    file:close(P),
    Ret.

parse_include_file(File, Nerrs, L, Mode) ->
    {ok, P} = file:open(File, read),
    Ret = parse_loop(P, File, io:parse_erl_form(P, '', 1), Nerrs, L, Mode),
    file:close(P),
    Ret.

parse_loop(_, _, {eof,_}, Nerrs, L, Mode) ->
    {Nerrs, L, Mode};
parse_loop(P, File, {ok, Form, Ln}, Nerrs, L0, Mode) ->
    case Form of
	{attribute,_,mode,compile} ->
	    parse_loop(P,File,io:parse_erl_form(P,'',Ln),Nerrs,L0,compile);
	{attribute,_,include,Include} ->
	    {Nerrs1, L1, Mode1} = parse_include_file(Include, Nerrs, L0, Mode),
	    parse_loop(P,File,io:parse_erl_form(P,'',Ln),Nerrs1,L1,Mode1);
	Form ->
	    parse_loop(P,File,io:parse_erl_form(P,'',Ln),Nerrs,[Form|L0],Mode)
    end;
parse_loop(P, File, {error,{Ln,Mod,Args}, Ln1}, Nerrs, L, Mode) ->
    io:format("Error in File:~s Line:~w ~s~n",
	      [File, Ln, Mod:format_error(Args)]),
    parse_loop(P, File, io:parse_erl_form(P, '', Ln1), Nerrs+1, L, Mode);
parse_loop(_, _, X, Nerrs, L, Mode) ->
    io:format("Unexpected form:~p~n",[X]),
    {Nerrs+1, L, Mode}.
    
code_handler(local, [file], _, File) ->
    File;
code_handler(Name, Args, Dict, File) ->
    %% io:format("code handler=~p~n",[{Name, Args}]),
    Arity = length(Args),
    case dict:find({local,Name,Arity}, Dict) of
	{ok, Cs} ->
	    LF = {value, fun(I, J) ->
				 code_handler(I, J, Dict, File)
			 end},
	    case erl_eval:match_clause(Cs, Args,erl_eval:new_bindings(),LF) of
		{Body, Bs} ->
		    {value, Val, _Bs1} = erl_eval:exprs(Body, Bs, LF),
		    Val;
		nomatch ->
		    io:format("escript: Fatal error"),
		    erlang:halt(-1)
	    end;
	error ->
	    case dict:find({remote,{Name,Arity}}, Dict) of
		{ok, Mod} ->
		    %% io:format("Calling:~p~n",[{Mod,Name,Args}]),
		    apply(Mod, Name, Args);
		error ->
		    io:format("Script does not export ~w/~w~n",
			      [Name,Arity]),
		    erlang:halt()
	    end
    end.


	


    



