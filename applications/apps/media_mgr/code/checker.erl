%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(checker).

-compile(export_all).
-import(lists, [all/2, foreach/2, member/2]).

%% checker:start()
%%    checks consistency of names

start() ->
    F = lib_find:files(".", "*.erl", true),
    foreach(fun(I) -> check(I) end, F).

test() ->
    check("./checker.erl").

check(File) ->
    io:format("Check:~p~n",[File]),
    case epp:parse_file(File, ".", []) of
	{ok, Forms} ->
	    chk(File, Forms);
	_ ->
	    true
    end.

chk(File, F) ->
    try do(F)
    catch
	_:W ->
	    io:format("File:~p ~p~n",[File,W])
    end.

do({function,_,Name,Arity,Clauses}) ->
    %% io:format("checfun ~p~n",[Name]),
    isFuname(Name),
    do(Clauses);
do({call,_,{remote,_,A,B},Args}) ->
    is_valid_function_name(A),
    is_valid_function_name(B),
    do(Args);
do({call,_,F,Args}) ->
    is_valid_function_name(F),
    do(Args); 
do({var,_,V}) ->
    is_valid_variable_name(V);
do(T) when tuple(T) ->
    foreach(fun do/1, tuple_to_list(T));
do(T) when list(T) ->
    foreach(fun do/1, T);
do(_) ->
    true.

is_valid_function_name({atom,_,N}) ->
    isFuname(N);
is_valid_function_name(X) ->
    do(X).

isFuname(N) ->
    %% io:format("Checking:~p~n",[N]),
    case is_valid_funname(atom_to_list(N)) of
	false -> io:format("** invalid function name:~p~n",[N]);
	_ -> void
    end.

-include("stdmacros.hrl").

is_valid_funname([H1,H2|T]) when ?IN(H1,$a,$z),
				 ?IN(H2,$A,$Z) ->
    false;
is_valid_funname([_|T]) ->
    is_valid_funname(T);
is_valid_funname([]) ->
    true.

is_valid_variable_name(V_n) ->
    L =  atom_to_list(V_n),
    case L of
	"_" ++ T ->
	    true;
	_ ->
	    case (not member($_,L)) of
		false -> io:format("** invalid variable name:~p~n",[L]);
		_ -> void
	    end
    end.

is_fun_name_char($_) -> true;
is_fun_name_char(X) when $a =< X, X =< $z -> true;
is_fun_name_char(_) -> false.

doIt(A) ->
    a.

