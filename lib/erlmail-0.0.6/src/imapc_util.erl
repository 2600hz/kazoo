%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP client utility funcations
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.1
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(imapc_util).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").

-export([write/2,response/1,response/2]).
-export([fetch_response/2,fetch_split/1]).
-export([mailbox/1,mailbox/2,status/2,expunge_response/2]).
-export([tag/0,to_seq/1,to_low_atom/1,reduce/1]).
-export([build_flags/1,build_search/1,build_order/1,build_fetch/1,build_statuscodes/1,build_store/1]).



write(Socket,Msg) -> 
	Last = string:right(Msg,2),
	case Last of
		?CRLF -> gen_tcp:send(Socket,Msg);
		_     -> gen_tcp:send(Socket,Msg ++ ?CRLF)
	end.

response(Socket) -> response(Socket,[],[]).
response(Socket,Tag) -> response(Socket,Tag,[]).
response(Socket,Tag,Response) ->
	receive
		{tcp, Socket, Bin}   -> 
			NewResponse = process_line(Bin,Tag),
			NextResponse = lists:append([Response,NewResponse]),
			imapc_fsm:set_socket_opts(Socket),
			case find_tag(NextResponse,to_low_atom(Tag)) of
				true -> NextResponse;
				false -> response(Socket,Tag,NextResponse)
			end;
			
		{tcp_closed, Socket} -> {error,socket_closed}
	end.

fetch_response(Socket,Tag) -> fetch_response(Socket,Tag,<<>>).
fetch_response(Socket,Tag,Response) -> 
	receive
		{tcp, Socket, Bin}   -> 
			NextResponse = <<Response/binary,Bin/binary>>,
			LowTag = to_low_atom(Tag),
			Tokens = string:tokens(binary_to_list(NextResponse),[13,10]),
			Last = lists:last(Tokens),
			imapc_fsm:set_socket_opts(Socket),
			case catch process_line([Last],Tag) of
				[#imap_resp{tag = LowTag}] -> 
					FetchSplit = fetch_split(NextResponse),
					process_line(FetchSplit,Tag,[]);
				_ -> fetch_response(Socket,Tag,NextResponse)
			end;
		{tcp_closed, Socket} -> error
	end.

fetch_split(Binary) when is_binary(Binary) -> fetch_split(binary_to_list(Binary));
fetch_split(Fetch) ->
	NewFetch = case string:right(Fetch,2) of
		?CRLF -> 
			string:left(Fetch,length(Fetch) - 2);
		_ -> Fetch
	end,
	Pos = string:rstr(NewFetch,?CRLF),
	{NextFetch,Completed} = lists:split(Pos+1,Fetch),
	FetchSplit = fetch_split(NextFetch,[]),
	lists:append(FetchSplit,[Completed]).


fetch_split(Fetch,Acc) -> 
	case string:str(Fetch,[13,10,42]) of
		0 -> lists:reverse([Fetch|Acc]);
		Pos -> 
			{NewFetch,NextFetch} = lists:split(Pos + 1,Fetch),
			fetch_split(NextFetch,[NewFetch|Acc])
			
	end.



find_tag(_,[]) -> true;
find_tag(_,'') -> true;
find_tag([],_Tag) -> false;
find_tag([#imap_resp{tag = Tag}|_T],Tag) -> true;
find_tag([#imap_resp{}|T],Tag) -> find_tag(T,Tag).


to_low_atom(String) ->  list_to_atom(http_util:to_lower(String)).




process_line(Bin,Tag) when is_binary(Bin) -> process_line(string:tokens(binary_to_list(Bin),"\r\n"),Tag,[]);
process_line(List,Tag)                    -> process_line(List,Tag,[]).
process_line([H|T],Tag,Acc) ->
	case parse(H) of
		Resp when is_record(Resp,imap_resp) ->
			process_line(T,Tag,[Resp|Acc]);
		_ -> {error,bad_respsonse}
	end;
process_line([],_Tag,Acc) -> lists:reverse(Acc).

parse(Line) -> 
	case imap_scan:imap_string(Line ++ [0]) of
		{ok,Tokens,_Lines} -> 
			imap_parse(Tokens);
		_ -> {error,token_error}
	end.

imap_parse(Tokens) ->
	case catch imap_parser:parse(Tokens) of
		{error,Reason} -> {error,Reason};
		{'EXIT',_} ->
			case clean_tokens(Tokens) of
				{error,Reason} -> {error,Reason};
				NewTokens -> imap_parse(NewTokens)
			end;
		{ok,Results} -> Results
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Specific patterns that are not RFC3501 compliant or cannot fit into the ABNF pattern - THESE SHOULD BE RARE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_tokens([{string,Tag},{response_code,ResponseCode},{String,RespText}|_Rest]) -> [{string,Tag},{response_code,ResponseCode},{String,RespText},{'$end'}];
clean_tokens(Tokens) -> {error,{cannot_clean,Tokens}}.






reduce(List) -> lists:map(fun(R) -> R#imap_resp.data end,List).



mailbox(List) -> mailbox(List,#mailbox{}).
mailbox(List,MailBox) when is_list(MailBox) -> mailbox(List,#mailbox{name=MailBox});
mailbox([H|T],MailBox) when is_record(MailBox,mailbox) ->
	NewMailBox = case {H#imap_resp.cmd,H#imap_resp.code} of
		{exists,_}                       -> MailBox#mailbox{exists=H#imap_resp.data};
		{flags,_}                        -> MailBox#mailbox{flags=H#imap_resp.data};
		{recent,_}                       -> MailBox#mailbox{recent=H#imap_resp.data};
		{_,[{permanentflags,PermFlags}]} -> MailBox#mailbox{permflags=PermFlags};
		{_,[{myrights,MyRights}]}        -> MailBox#mailbox{myrights=MyRights};
		{_,[{unseen,UnSeen}]}            -> MailBox#mailbox{unseen=list_to_integer(UnSeen)};
		{_,[{uidvalidity,UIDValidity}]}  -> MailBox#mailbox{uidvalidity=list_to_integer(UIDValidity)};
		{_,[{uidnext,UIDNext}]}          -> MailBox#mailbox{uidnext=list_to_integer(UIDNext)};
		{_,['read-write']}               -> MailBox#mailbox{readwrite=true};
		_ -> MailBox
	end,
	mailbox(T,NewMailBox);
mailbox([],MailBox) -> MailBox.



status(List,MailBox) when is_list(MailBox) -> status(List,#mailbox{name=MailBox});
status([],MailBox) -> MailBox;
status([H|T],MailBox) when is_record(MailBox,mailbox) ->
	NewMailBox = case H of
		{messages,Messages}       -> MailBox#mailbox{messages=Messages};
		{recent,Recent}           -> MailBox#mailbox{recent=Recent};
		{uidnext,UIDNext}         -> MailBox#mailbox{uidnext=UIDNext};
		{uidvalidity,UIDValidity} -> MailBox#mailbox{uidvalidity=UIDValidity};
		{unseen,Unseen}           -> MailBox#mailbox{unseen=Unseen};
		_ -> MailBox
	end,
	status(T,NewMailBox).



expunge_response(List,State) -> expunge_response(List,State,[]).

expunge_response([],_State,Acc) -> lists:reverse(Acc);
expunge_response([#imap_resp{status = ok} = OK],State,Acc) -> {OK,State,lists:reverse(Acc)};
expunge_response([#imap_resp{status = no} = NO|_T],State,_Acc) -> {NO,State};
expunge_response([#imap_resp{status = bad} = BAD|_T],State,_Acc) -> {BAD,State};
expunge_response([#imap_resp{cmd= expunge,data = Number}|T],State,Acc) ->
	expunge_response(T,State,[Number|Acc]);
expunge_response([#imap_resp{} = Resp|T],State,Acc) -> 
	MB = State#imapc_fsm.mailbox,
	NewMB = case Resp of
		#imap_resp{cmd = exists, data = Data} -> MB#mailbox{exists = Data};
		#imap_resp{cmd = recent, data = Data} -> MB#mailbox{recent = Data};
		Other ->
			?D(Other), 
			MB
	end,
	expunge_response(T,State#imapc_fsm{mailbox = NewMB},Acc).







%%%%----------------------------------------------------------------
%%%%-- KEEP THESE FUNCATIONS --
%%%%----------------------------------------------------------------
tag() -> 
	Alpha = random:uniform(26) + 64,
	Value = random:uniform(899) + 100,
	lists:flatten([Alpha,integer_to_list(Value)]).

to_seq(Number) when is_integer(Number) -> integer_to_list(Number);
to_seq(List) -> 
	String = lists:flatten(lists:map(fun(N) -> [integer_to_list(N),[44]] end,List)),
	string:left(String,length(String)-1).


build_flags(List) -> build_flags(List,[]).
build_flags([H|T],Acc) ->
	case H of
		answered -> build_flags(T,["\\Answered",32|Acc]);
		seen -> build_flags(T,["\\Seen",32|Acc]);
		flagged -> build_flags(T,["\\Flagged",32|Acc]);
		draft -> build_flags(T,["\\Draft",32|Acc]);
		recent -> build_flags(T,["\\Recent",32|Acc]);
		deleted -> build_flags(T,["\\Deleted",32|Acc])
	end;
build_flags([],Acc) -> lists:flatten([40] ++ string:strip(Acc,both) ++ [41]).


build_fetch(List) -> build_fetch(List,[]).

build_fetch([H|T],Acc) ->
	case H of
		all             -> build_fetch(T,[[32],"ALL"|Acc]);
		body            -> build_fetch(T,[[32],"BODY"|Acc]);
		bodystructure   -> build_fetch(T,[[32],"BODYSTRUCTURE"|Acc]);
		envelope        -> build_fetch(T,[[32],"ENVELOPE"|Acc]);
		fast            -> build_fetch(T,[[32],"FAST"|Acc]);
		flags           -> build_fetch(T,[[32],"FLAGS"|Acc]);
		full            -> build_fetch(T,[[32],"FULL"|Acc]);
		internaldate    -> build_fetch(T,[[32],"INTERNALDATE"|Acc]);
		rfc822          -> build_fetch(T,[[32],"RFC822"|Acc]);
		'rfc822.header' -> build_fetch(T,[[32],"RFC822.HEADER"|Acc]);
		'rfc822.size'   -> build_fetch(T,[[32],"RFC822.SIZE"|Acc]);
		'rfc822.text'   -> build_fetch(T,[[32],"RFC822.TEXT"|Acc]);
		uid             -> build_fetch(T,[[32],"UID"|Acc]);
		_Other          -> 
			S = atom_to_list(H),
			case string:str(S,"body") of
				N when N > 0 -> build_fetch(T,[[32],http_util:to_upper(S)|Acc]);
				_ -> build_fetch(T,Acc)
			end
			
	end;
build_fetch([],Acc) -> 
	Query = "(" ++ string:strip(lists:flatten(lists:reverse(Acc)),both) ++ ")",
	Query.


build_order(List) -> build_order(List,[]).

build_order([H|T],Acc) ->
	case H of
		arrival -> build_order(T,[[32],"ARRIVAL"|Acc]);
		cc      -> build_order(T,[[32],"CC"|Acc]);
		date    -> build_order(T,[[32],"DATE"|Acc]);
		from    -> build_order(T,[[32],"FROM"|Acc]);
		reverse -> build_order(T,[[32],"REVERSE"|Acc]);
		size    -> build_order(T,[[32],"SIZE"|Acc]);
		subject -> build_order(T,[[32],"SUBJECT"|Acc]);
		to      -> build_order(T,[[32],"TO"|Acc]);
		_Other -> build_order(T,Acc)
	end;
build_order([],Acc) ->
	Order = string:strip(lists:flatten(lists:reverse(Acc)),both),
%	io:format("~p~n",[Query]),
	Order.



build_store(ItemName) ->
	case ItemName of
		replace -> "FLAGS";
		add -> "+FLAGS";
		remove -> "-FLAGS";
		delete -> "-FLAGS";
		replace_silent -> "FLAGS.silent";
		add_silent -> "+FLAGS.silent";
		remove_silent -> "-FLAGS.silent";
		delete_silent -> "-FLAGS.silent"
	end.

build_search(List) -> build_search(List,[]).

build_search([H|T],Acc) ->
	case H of
		all -> build_search(T,[[32],"ALL"|Acc]);
		answered -> build_search(T,[[32],"ANSWERED"|Acc]);
		deleted -> build_search(T,[[32],"DELETED"|Acc]);
		draft -> build_search(T,[[32],"DRAFT"|Acc]);
		flagged -> build_search(T,[[32],"FLAGGED"|Acc]);
		new -> build_search(T,[[32],"NEW"|Acc]);
		old -> build_search(T,[[32],"OLD"|Acc]);
		recent -> build_search(T,[[32],"RECENT"|Acc]);
		seen -> build_search(T,[[32],"SEEN"|Acc]);
		unanswered -> build_search(T,[[32],"UNANSWERED"|Acc]);
		undeleted -> build_search(T,[[32],"UNDELETED"|Acc]);
		undraft -> build_search(T,[[32],"UNDRAFT"|Acc]);
		unflagged -> build_search(T,[[32],"UNFLAGGED"|Acc]);
		unseen -> build_search(T,[[32],"UNSEEN"|Acc]);
		{bcc,String} -> build_search(T,[[32],"BCC " ++ String|Acc]);
		{before,Date} -> build_search(T,[[32],"BEFORE " ++ Date|Acc]);
		{body,String} -> build_search(T,[[32],"BODY " ++ String|Acc]);
		{cc,String} -> build_search(T,[[32],"CC " ++ String|Acc]);
		{from,String} -> build_search(T,[[32],"FROM " ++ String|Acc]);
		{keyword,Flag} -> build_search(T,[[32],"KEYWORD " ++ Flag|Acc]);
		{unkeyword,Flag} -> build_search(T,[[32],"UNKEYWORD " ++ Flag|Acc]);
		{larger,Size} when is_integer(Size) -> build_search(T,[[32],"LARGER " ++ integer_to_list(Size)|Acc]);
		{larger,Size} -> build_search(T,[[32],"LARGER " ++ Size|Acc]);
		{smaller,Size} when is_integer(Size) -> build_search(T,[[32],"SMALLER " ++ integer_to_list(Size)|Acc]);
		{smaller,Size} -> build_search(T,[[32],"SMALLER " ++ Size|Acc]);
		{on,Date} -> build_search(T,[[32],"ON " ++ Date|Acc]);
		{sentbefore,Date} -> build_search(T,[[32],"SENTBEFORE " ++ Date|Acc]);
		{senton,Date} -> build_search(T,[[32],"SENTON " ++ Date|Acc]);
		{sentsince,Date} -> build_search(T,[[32],"SENTSINCE " ++ Date|Acc]);
		{since,Date} -> build_search(T,[[32],"SINCE " ++ Date|Acc]);
		{subject,String} -> build_search(T,[[32],"SUBJECT " ++ String|Acc]);
		{text,String} -> build_search(T,[[32],"TEXT " ++ String|Acc]);
		{to,String} -> build_search(T,[[32],"TO " ++ String|Acc]);
		{uid,Set} when is_integer(Set) -> build_search(T,[[32],"UID " ++ integer_to_list(Set)|Acc]);
		{uid,Set} -> build_search(T,[[32],"UID " ++ Set|Acc]);
		_Other -> build_search(T,Acc)
	end;
build_search([],Acc) ->
	Query = string:strip(lists:flatten(lists:reverse(Acc)),both),
	Query.


build_statuscodes(List) -> build_statuscodes(List,[]).
build_statuscodes([H|T],Acc) ->	build_statuscodes(T,[32,http_util:to_upper(atom_to_list(H))|Acc]);
build_statuscodes([],Acc) -> [40] ++ string:strip(lists:flatten(lists:reverse(Acc)),both) ++ [41].


