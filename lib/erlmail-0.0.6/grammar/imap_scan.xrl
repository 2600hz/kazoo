%%%----------------------------------------------------------------------
%%% File    : imap_scan.xrl
%%% Author  : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose : Token definitions for ErlMail IMAP Scanner.
%%% Created : 2006-12-08
%%% Updated : 2006-12-09
%%%----------------------------------------------------------------------

Definitions.
O    = [0-7]
D	 = [0-9]
U	 = [A-Z]
L    = [a-z]
H	 = [0-9a-fA-F]
CRLF = [\r\n]
WS   = ([\s])
END  = [\000]
A    = ({U}|{L}|{D})
P    = ({A}|{WS})
N    = {D}+
SPEC   = [\*\=\$\_\-\+\~\/\,\.\:\;]
TOKEN  = ({A}|{SPEC})+
RFC822 = (RFC|rfc)822
TRUNC  = (\<{N}\>)
PARTNUM = \[[0-9]+(\.[0-9])*\]

Rules.

{PARTNUM} : 
	S = lists:sublist(TokenChars, 2, TokenLen - 2),	
	{token,[{'['},{string,S},{']'}]}.

{TRUNC} : 
	S = lists:sublist(TokenChars, 2, TokenLen - 2),	
	{token,[{'<'},{integer,list_to_integer(S)},{'>'}]}.

{RFC822}\.{A}+ : {token,{string,TokenChars}}.
{N}\-{N} : {token,{string,TokenChars}}.
{TOKEN} : {token,type(TokenChars)}.


[]()[}{\\\.] : 
	if
		TokenChars == "\\" -> skip_token;
		true -> {token,{list_to_atom(TokenChars)}}
	end.

"(\\\^.|\\.|[^"])*" : {token,string_type(TokenChars,TokenLen)}.

{CRLF}+ : skip_token.
{WS}+   : skip_token.

{END}{WS}* : {end_token,{'$end'}}.


Erlang code.

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