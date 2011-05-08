%% THIS IS A PRE-RELEASE OF LEEX - RELEASED ONLY BECAUSE MANY PEOPLE
%% WANTED IT - THE OFFICIAL RELEASE WILL PROVIDE A DIFFERENT INCOMPATIBLE
%% AND BETTER INTERFACE - BE WARNED
%% PLEASE REPORT ALL BUGS TO THE AUTHOR.

-module(imap_scan).

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% User code. This is placed here to allow extra attributes.

-export([imap_string/1]).

%%% Check to see of this is a BODY[]<> command, use unquote and requote to hide then replace DQUOTE to formulat string properly
%%% Regular Expression could be better.
imap_string(String) ->
	RegExp = "(([bB][oO][dD][yY])(\.[pP][eE][eE][kK])?)(\\\[(.)*\\\])(\\\<[0-9]+\\\>)?",
	case regexp:match(String,RegExp) of
		{match,_,_} ->
			UString = unquote(String),
			Prep = fetch_prep(UString),
			{ok,Tokens,Lines} = string(Prep),
			{ok,requote(lists:flatten(Tokens),[]),Lines};
		nomatch -> 
			Prep = fetch_prep(String),
			string(Prep)
	end.


string_type(TokenChars,TokenLen) ->
	S = lists:sublist(TokenChars, 2, TokenLen - 2),
	if
		length(S) < 100 ->
			case list_to_atom(S) of
				'quoted-printable' -> {encoding,S};
				'base64'           -> {encoding,S};
				'7bit'             -> {encoding,S};
				'8bit'             -> {encoding,S};
				'application'      -> {media_type_str,S};
				'image'            -> {media_type_str,S};
				'text'             -> {media_type_str,S};
				'plain'            -> {media_subtype_str,S};
				'html'             -> {media_subtype_str,S};
				'jpeg'             -> {media_subtype_str,S};
				'vnd.ms-excel'     -> {media_subtype_str,S};
				'alternative'      -> {media_subtype_str,S};
				'mixed'            -> {media_subtype_str,S};
				_ -> {string,S}
			end;
		true -> {string,S}
	end.


type(Token) ->
	case string:to_integer(Token) of
		{error,_Reason} -> Value = list_to_atom(http_util:to_lower(Token));
		{Value,_} -> ok
	end,
	case Value of
		'nil'        -> {nil,Token};
		ok           -> {response_code,Token};
		no           -> {response_code,Token};
		bad          -> {response_code,Token};
		bye          -> {response_code,Token};
		append       -> {command,Token};
		authenticate -> {command,Token};
		capability   -> {command,Token};
		check        -> {command,Token};
		close        -> {command,Token};
		copy         -> {command,Token};
		delete       -> {command,Token};
		examine      -> {command,Token};
		expunge      -> {command,Token};
		fetch        -> {command,Token};
		list         -> {command,Token};
		login        -> {command,Token};
		logout       -> {command,Token};
		lsub         -> {command,Token};
		noop         -> {command,Token};
		rename       -> {command,Token};
		search       -> {command,Token};
		select       -> {command,Token};
		sort         -> {command,Token};
		status       -> {command,Token};
		store        -> {command,Token};
		subscribe    -> {command,Token};
		unsubscribe  -> {command,Token};
		uid          -> {command,Token};
		Number when is_integer(Number) -> {integer,Number};
		_  -> {string,Token}
	end.

fetch_prep(String) ->
	RegExp = "\{[0-9]+\}",
	case regexp:matches(String,RegExp) of
		nomatch -> String;
		{match,[]} -> String;
		{match,Matches} -> 
			QList = fetch_matches(String,Matches),
			fetch_quote(String,QList)
	end.

fetch_quote(String,[H|T]) -> fetch_quote(String,[H|T],0).
fetch_quote(String,[H|T],C) ->
	{Begin,End} = lists:split(H+C,String),
	fetch_quote(Begin ++ [34] ++ End,T,C+1);
fetch_quote(String,[],_C) -> String.



fetch_matches(String,Matches) ->
	L = lists:foldl(fun({Start,Length},Acc) -> 
		First = Start+Length+1,
		Count = list_to_integer(string:substr(String,Start+1,Length-2)),
		[First,First+Count|Acc]
		 end,[],Matches),
	lists:sort(L).

unquote(String) -> 
	{ok,S,_} = regexp:gsub(String,[34],[0]),
	S.
requote(String) -> 
	{ok,S,_} = regexp:gsub(String,[0],[34]),
	S.

requote([H|T],Acc) ->
	case H of
		{string,String} -> 
%			io:format("Found String: ~p~n", [length(String)]),
			requote(T,[{string,requote(String)}|Acc]);
		_ -> requote(T,[H|Acc])
	end;
requote([],Acc) -> lists:reverse(Acc).
format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%%    {ok,Tokens,Line} | {error,ErrorInfo,Line}.

string([], L, [], Ts) ->			%No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
	{A,Alen,Ics1,L1} ->			%Accepting end state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{A,Alen,Ics1,L1,_S1} ->		%After an accepting state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{reject,_Alen,Tlen,_Ics1,L1,_S1} ->
	    {error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
	{A,Alen,_Tlen,_Ics1,L1,_S1} ->
	    string_cont(yysuf(Tcs, Alen), L1, yyaction(A, Alen, Tcs, L1), Ts)
    end.

%% string_cont(RestChars, Line, Token, Tokens)
%%  Test for and remove the end token wrapper.

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(_Rest, Line, {error,S}, _Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(Chars, Line, yystate(), Chars, 0, reject, 0);
token({Line,State,Tcs,Tlen,Action,Alen}, Chars, _) ->
    token(Chars, Line, State, Tcs ++ Chars, Tlen, Action, Alen).

%% token(InChars, Line, State, TokenChars, TokenLen, AcceptAction, AcceptLen) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

token(Ics0, L0, S0, Tcs, Tlen0, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{L1,S1,Tcs,Alen1,A1,Alen1}};
	{A1,Alen1,Ics1,L1,_S1} ->
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{L1,S1,Tcs,Tlen1,A1,Alen1}};
	{reject,_Alen1,_Tlen1,eof,L1,_S1} ->
	    {done,{eof,L1},[]};
	{reject,_Alen1,Tlen1,Ics1,L1,_S1} ->
	    {done,{error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},L1},Ics1};
	{A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
	    token_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1))
    end.

%% tokens_cont(RestChars, Line, Token)
%%  If we have a token or error then return done, else if we have a
%%  skip_token then continue.

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, skip_token) ->
    token(Rest, Line, yystate(), Rest, 0, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(Chars, Line, yystate(), Chars, 0, [], reject, 0);
tokens({tokens,Line,State,Tcs,Tlen,Ts,Action,Alen}, Chars, _) ->
    tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Ts, Action, Alen);
tokens({skip_tokens,Line,State,Tcs,Tlen,Error,Action,Alen}, Chars, _) ->
    skip_tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Error, Action, Alen).

%% tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, AcceptAction, AcceptLen) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(Ics0, L0, S0, Tcs, Tlen0, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{tokens,L1,S1,Tcs,Alen1,Ts,A1,Alen1}};
	{A1,Alen1,Ics1,L1,_S1} ->
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{tokens,L1,S1,Tcs,Tlen1,Ts,A1,Alen1}};
	{reject,_Alen1,_Tlen1,eof,L1,_S1} ->
	    {done,if Ts == [] -> {eof,L1};
		     true -> {ok,yyrev(Ts),L1} end,[]};
	{reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1+1), L1,
			{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}});
	{A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
	    tokens_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Ts)
    end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%%  If we have a end_token or error then return done, else if we have
%%  a token then save it and continue, else if we have a skip_token
%%  just continue.

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, _Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%%skip_tokens(InChars, Line, Error) -> {done,{error,Error,Line},Ics}.
%%  Skip tokens until an end token, junk everything and return the error.

skip_tokens(Ics, Line, Error) ->
    skip_tokens(Ics, Line, yystate(), Ics, 0, Error, reject, 0).

%% skip_tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, AcceptAction, AcceptLen) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(Ics0, L0, S0, Tcs, Tlen0, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Alen1,Error,A1,Alen1}};
	{A1,Alen1,Ics1,L1,_S1} ->
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Tlen1,Error,A1,Alen1}};
	{reject,_Alen1,_Tlen1,eof,L1,_S1} ->
	    {done,{error,Error,L1},[]};
	{reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1), L1, Error);
	{A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
	    skip_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Error)
    end.

%% skip_cont(RestChars, Line, Token, Error)
%%  Skip tokens until we have an end_token or error then reutrn done
%%  with the original rror.

skip_cont(Rest, Line, {token,_T}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, {end_token,_T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {error,_S}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0).

yyrev(List) -> lists:reverse(List).
yyrev(List, Tail) -> lists:reverse(List, Tail).
yypre(List, N) -> lists:sublist(List, N).
yysuf(List, N) -> lists:nthtail(N, List).

%% yystate() -> InitialState.
%% yystate(State, InChars, Line, TokenLen, AcceptAction, AcceptLen) ->
%%      {Action, AcceptLength, RestChars, Line} |         Accepting end state
%%      {Action, AcceptLength, RestChars, Line, State} |  Accepting state
%%      {Action, AcceptLength, TokLength, RestChars, Line, State} |
%%      {reject, AcceptLength, TokLength, RestChars, Line, State}.
%% Generated state transition functions. The non-accepting end state
%% return signal either an unrecognised character or end of current
%% input.

yystate() -> 28.

yystate(31, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, [$F|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(29, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $E ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $G, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(31, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,31};
yystate(30, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(30, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(30, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(30, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(30, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(30, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(30, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(30, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,30};
yystate(29, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [$A|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [$B|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [$C|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(22, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $D, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(29, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,29};
yystate(28, [$\000|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(24, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(20, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(28, [$\r|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(20, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$\s|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(16, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$(|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$)|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$:|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$;|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$<|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(19, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$R|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(31, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$[|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(5, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$r|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(14, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [${|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$}|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(26, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $/ ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(7, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Q ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $S, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $q ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $s, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, _Action, _Alen);
yystate(28, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,28};
yystate(27, Ics, Line, Tlen, _Action, _Alen) ->
    {1,Tlen,Ics,Line};
yystate(26, Ics, Line, Tlen, _Action, _Alen) ->
    {5,Tlen,Ics,Line};
yystate(25, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, [$2|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(21, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $1 ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $3, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(25, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,25};
yystate(24, [$\s|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(24, Ics, Line, Tlen+1, 9, Tlen);
yystate(24, Ics, Line, Tlen, _Action, _Alen) ->
    {9,Tlen,Ics,Line,24};
yystate(23, [$>|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(27, Ics, Line, Tlen+1, _Action, _Alen);
yystate(23, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(23, Ics, Line, Tlen+1, _Action, _Alen);
yystate(23, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,23};
yystate(22, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, [$8|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(25, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $7 ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $9, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(22, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,22};
yystate(21, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, [$2|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(17, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $1 ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $3, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(21, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,21};
yystate(20, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(20, Ics, Line+1, Tlen+1, 7, Tlen);
yystate(20, [$\r|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(20, Ics, Line, Tlen+1, 7, Tlen);
yystate(20, Ics, Line, Tlen, _Action, _Alen) ->
    {7,Tlen,Ics,Line,20};
yystate(19, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(23, Ics, Line, Tlen+1, _Action, _Alen);
yystate(19, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,19};
yystate(18, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [$a|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [$b|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [$c|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(22, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $d, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(18, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,18};
yystate(17, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, [$.|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(13, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $- ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $/, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(17, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,17};
yystate(16, [$\s|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(16, Ics, Line, Tlen+1, 8, Tlen);
yystate(16, Ics, Line, Tlen, _Action, _Alen) ->
    {8,Tlen,Ics,Line,16};
yystate(15, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [$:|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [$;|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $/ ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(15, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 3, Tlen);
yystate(15, Ics, Line, Tlen, _Action, _Alen) ->
    {3,Tlen,Ics,Line,15};
yystate(14, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [$f|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(18, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $; ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $e ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $g, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(14, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,14};
yystate(13, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [$:|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [$;|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $/ ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(9, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(9, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(9, Ics, Line, Tlen+1, 4, Tlen);
yystate(13, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,13};
yystate(12, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(12, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(12, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(8, Ics, Line, Tlen+1, _Action, _Alen);
yystate(12, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(4, Ics, Line, Tlen+1, _Action, _Alen);
yystate(12, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(12, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(12, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(12, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(12, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,12};
yystate(11, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [$:|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [$;|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $/ ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(15, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(11, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,11};
yystate(10, Ics, Line, Tlen, _Action, _Alen) ->
    {0,Tlen,Ics,Line};
yystate(9, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [$:|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [$;|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $/ ->
    yystate(30, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(9, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(9, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(9, Ics, Line, Tlen+1, 2, Tlen);
yystate(9, Ics, Line, Tlen, _Action, _Alen) ->
    {2,Tlen,Ics,Line,9};
yystate(8, Ics, Line, Tlen, _Action, _Alen) ->
    {6,Tlen,Ics,Line};
yystate(7, [$$|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [$-|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(11, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [$.|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [$/|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [$:|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [$;|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [$=|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [$_|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [$~|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $*, C =< $, ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(7, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $A, C =< $Z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $a, C =< $z ->
    yystate(30, Ics, Line, Tlen+1, 4, Tlen);
yystate(7, Ics, Line, Tlen, _Action, _Alen) ->
    {4,Tlen,Ics,Line,7};
yystate(6, [$.|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(2, Ics, Line, Tlen+1, _Action, _Alen);
yystate(6, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(10, Ics, Line, Tlen+1, _Action, _Alen);
yystate(6, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,6};
yystate(5, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(1, Ics, Line, Tlen+1, 5, Tlen);
yystate(5, Ics, Line, Tlen, _Action, _Alen) ->
    {5,Tlen,Ics,Line,5};
yystate(4, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(12, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(4, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(0, Ics, Line, Tlen+1, _Action, _Alen);
yystate(4, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(4, Ics, Line, Tlen+1, _Action, _Alen);
yystate(4, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(4, [$^|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(3, Ics, Line, Tlen+1, _Action, _Alen);
yystate(4, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(4, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(4, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(4, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $_, C =< $ÿ ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(4, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,4};
yystate(3, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(12, Ics, Line+1, Tlen+1, _Action, _Alen);
yystate(3, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(0, Ics, Line, Tlen+1, _Action, _Alen);
yystate(3, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(4, Ics, Line, Tlen+1, _Action, _Alen);
yystate(3, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(3, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(3, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(3, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(12, Ics, Line, Tlen+1, _Action, _Alen);
yystate(3, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,3};
yystate(2, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(6, Ics, Line, Tlen+1, _Action, _Alen);
yystate(2, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,2};
yystate(1, [$.|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(2, Ics, Line, Tlen+1, _Action, _Alen);
yystate(1, [$]|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(10, Ics, Line, Tlen+1, _Action, _Alen);
yystate(1, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $0, C =< $9 ->
    yystate(1, Ics, Line, Tlen+1, _Action, _Alen);
yystate(1, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,1};
yystate(0, [$\n|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(12, Ics, Line+1, Tlen+1, 6, Tlen);
yystate(0, [$"|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(8, Ics, Line, Tlen+1, 6, Tlen);
yystate(0, [$\\|Ics], Line, Tlen, _Action, _Alen) ->
    yystate(4, Ics, Line, Tlen+1, 6, Tlen);
yystate(0, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\000, C =< $\t ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(0, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $\v, C =< $! ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(0, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $#, C =< $[ ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(0, [C|Ics], Line, Tlen, _Action, _Alen) when C >= $], C =< $ÿ ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(0, Ics, Line, Tlen, _Action, _Alen) ->
    {6,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.


%% yyaction(Action, TokenLength, TokenChars, TokenLine) ->
%%        {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, TokenLen, YYtcs, _) ->
    TokenChars = yypre(YYtcs, TokenLen),
    S = lists:sublist(TokenChars, 2, TokenLen - 2),{token,[{'['},{string,S},{']'}]};
yyaction(1, TokenLen, YYtcs, _) ->
    TokenChars = yypre(YYtcs, TokenLen),
    S = lists:sublist(TokenChars, 2, TokenLen - 2),{token,[{'<'},{integer,list_to_integer(S)},{'>'}]};
yyaction(2, TokenLen, YYtcs, _) ->
    TokenChars = yypre(YYtcs, TokenLen),
    {token,{string,TokenChars}};
yyaction(3, TokenLen, YYtcs, _) ->
    TokenChars = yypre(YYtcs, TokenLen),
    {token,{string,TokenChars}};
yyaction(4, TokenLen, YYtcs, _) ->
    TokenChars = yypre(YYtcs, TokenLen),
    {token,type(TokenChars)};
yyaction(5, TokenLen, YYtcs, _) ->
    TokenChars = yypre(YYtcs, TokenLen),
    if
        TokenChars == "\\" ->
            skip_token;
        true ->
            {token,{list_to_atom(TokenChars)}}
    end;
yyaction(6, TokenLen, YYtcs, _) ->
    TokenChars = yypre(YYtcs, TokenLen),
    {token,string_type(TokenChars, TokenLen)};
yyaction(7, _, _, _) ->
    skip_token;
yyaction(8, _, _, _) ->
    skip_token;
yyaction(9, _, _, _) ->
    {end_token,{'$end'}};
yyaction(_, _, _, _) -> error.
