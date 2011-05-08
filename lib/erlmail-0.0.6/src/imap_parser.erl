-module(imap_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 289).

-include("../include/imap.hrl").


value_of(Token) -> element(2,Token).

v({_Type,Value}) -> v(Value);
v(Token) ->
	case string:to_integer(Token) of
		{error,not_a_list} -> Value = Token;
		{error,_Reason} -> Value = list_to_atom(http_util:to_lower(Token));
		{Value,_} -> ok
	end,
	case Value of
		Value when is_atom(Value) -> Value;
		Value when is_list(Value) -> list_to_atom(http_util:to_lower(Value));
		Value when is_integer(Value) -> Value
	end.
	
clean_parts(List) -> clean_parts(List,[],[],[]).
clean_parts([H|T],Type,Ext,Parts) ->
	case H of
		Part when is_record(Part,part) -> clean_parts(T,Type,Ext,[Part|Parts]);
		Body when is_record(Body,body) -> clean_parts(T,Type,Ext,[Body|Parts]);
		List when is_list(List) ->  clean_parts(lists:append([List,T]),Type,Ext,Parts);			
		{Part,NewType}  when is_record(Part,part) -> clean_parts(T,NewType,Ext,[Part|Parts]);
		{Part,NewType,NewExt}  when is_record(Part,part) -> clean_parts(T,NewType,NewExt,[Part|Parts])
	end;	
clean_parts([],Type,Ext,Parts) ->
	{lists:reverse(Parts),Type,Ext}.


clean_param(List) -> clean_param(List,[]).

clean_param([H,J|T],Acc) ->
	PV = {list_to_atom(H),J},
	clean_param(T,[PV|Acc]);
clean_param([],Acc) -> lists:reverse(Acc).

clean_resp_code(Atom) when is_atom(Atom) -> [Atom];
clean_resp_code(List) -> clean_resp_code(List,[]).
clean_resp_code([H,J|T],Acc) ->
	{Code,Tail} = clean_resp_code_case(H,J,T),
	clean_resp_code(Tail,[Code|Acc]);
clean_resp_code([],Acc) -> lists:reverse(Acc).

clean_resp_code_case(Code,Next,Tail) ->
	case Code of
		unseen -> {{unseen,Next},Tail};
		uidnext -> {{uidnext,Next},Tail};
		uidvalidity -> {{uidvalidity,Next},Tail};
		permanentflags -> {{permanentflags,Next},Tail};
		myrights -> {{myrights,Next},Tail};
		_Other -> {Code,[Next|Tail]}
	end.

build_fetch(Tuple) when is_tuple(Tuple) -> build_fetch([Tuple],#fetch{});
build_fetch(List) -> build_fetch(lists:flatten(List),#fetch{}).

build_fetch([H|T],Fetch) ->
%	io:format("~p~n",[H]),
	case H of
		{flags,Flags} -> build_fetch(T,Fetch#fetch{flags=Flags});
		{internaldate,InternalDate}  -> build_fetch(T,Fetch#fetch{internaldate=InternalDate});
		{'rfc822.size',Size}  -> build_fetch(T,Fetch#fetch{size=Size});
		{body,Body}  -> build_fetch(T,Fetch#fetch{body=Body});
		{'body.peek',Body}  -> build_fetch(T,Fetch#fetch{body=Body});
		{bodystructure,Body}  -> build_fetch(T,Fetch#fetch{body_structure=Body});
		{uid,UID}  -> build_fetch(T,Fetch#fetch{uid=UID});
		{'rfc822',String}  -> build_fetch(T,Fetch#fetch{rfc822=String});
		{'rfc822.header',String}  -> build_fetch(T,Fetch#fetch{rfc822_header=String});
		{'rfc822.text',String}  -> build_fetch(T,Fetch#fetch{rfc822_text=String});
		{envelope,Env}  -> 
			NewFetch = Fetch#fetch{
				date=Env#envelope.date,
				subject=Env#envelope.subject,
				from=Env#envelope.from,
				sender=Env#envelope.sender,
				reply_to=Env#envelope.reply_to,
				to=Env#envelope.to,
				cc=Env#envelope.cc,
				bcc=Env#envelope.bcc,
				in_reply_to=Env#envelope.in_reply_to,
				message_id=Env#envelope.message_id
				},
			build_fetch(T,NewFetch);
		_ -> Fetch
	end;
build_fetch([],Fetch) -> Fetch.




-file("/usr/lib64/erlang/lib/parsetools-1.4/include/yeccpre.hrl", 0).
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
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
                   Error % probably from return_error/1
    end.

% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.erl", 190).

yeccpars2(0, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(response, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(response, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(form, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(response, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(5, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(response, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(response, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(8, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, response_code, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(continue_req, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(response_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(response_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 217, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 218, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 219, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 yeccpars2(yeccgoto(mailbox_data, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, response_code, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_14_(__Stack),
 yeccpars2(yeccgoto(resp_text, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(15, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, response_code, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 yeccpars2(yeccgoto(resp_text, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(resp_text, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(mailbox_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 yeccpars2(yeccgoto(resp_text, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, response_code, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_20_(__Stack),
 yeccpars2(yeccgoto(resp_text, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(21, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_22_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(flag_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(23, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_23_(__Stack),
 yeccpars2(yeccgoto(flag, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_24_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(flag, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_25_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(flag_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(26, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, response_code, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_26_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(response_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(response_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, response_code, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(29, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, response_code, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(response_done, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(32, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(resp_text_code, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_34_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(resp_text_code_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(35, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_36_(__Stack),
 yeccpars2(yeccgoto(resp_text_code, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_(__Stack),
 yeccpars2(yeccgoto(resp_text_code, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(resp_text_code, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(39, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_40_(__Stack),
 yeccpars2(yeccgoto(capability, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(cap_string, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(cap_string, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_43_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(capability, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_44_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(capability_data_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_45_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(resp_text_code_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_46_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(greeting, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(response_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, response_code, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(response_done, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(50, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_50_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(message_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_51_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(mailbox_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_52_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(message_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(53, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_54_(__Stack),
 yeccpars2(yeccgoto(msg_att_list, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(msg_att, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(msg_att, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(57, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 211, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(58, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(59, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(60, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_60_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(msg_att_dynamic, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_61_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_62_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(63, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, media_type_str, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(64, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_65_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(68, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(72, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(78, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_78_(__Stack),
 __Nss = lists:nthtail(7, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(79, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(82, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(83, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_84_(__Stack),
 __Nss = lists:nthtail(10, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(85, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(86, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(87, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(88, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_90_(__Stack),
 __Nss = lists:nthtail(7, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(91, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(92, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(93, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(94, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(95, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_96_(__Stack),
 __Nss = lists:nthtail(10, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(97, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 169, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 170, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(98, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(body_type_1part, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(99, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 194, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(100, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(body_type_1part, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(101, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(102, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, media_subtype_str, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 163, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(103, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, media_type_str, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(104, media_subtype_str, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 163, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(105, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(106, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_106_(__Stack),
 yeccpars2(yeccgoto(flag, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(107, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(108, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_108_(__Stack),
 yeccpars2(yeccgoto(flag, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(109, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(110, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(env_from, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(111, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [111 | __Ss], [__T | __Stack]);
yeccpars2(111, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(112, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_112_(__Stack),
 yeccpars2(yeccgoto(address_list, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(113, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(114, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(addr_name, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(116, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(117, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_117_(__Stack),
 yeccpars2(yeccgoto(nstring, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(118, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_118_(__Stack),
 yeccpars2(yeccgoto(nstring, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(119, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(addr_adl, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(120, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(121, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(addr_mailbox, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(122, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(123, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(addr_host, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(124, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(125, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_125_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(address, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(126, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_126_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(address, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(127, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_127_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(address_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(128, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(129, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(env_sender, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(130, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(131, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(env_reply_to, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(132, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(133, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(env_to, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(134, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(135, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(env_cc, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(136, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(137, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(env_bcc, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(138, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(env_in_reply_to, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(139, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(140, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(env_message_id, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(141, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(142, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_142_(__Stack),
 __Nss = lists:nthtail(11, __Ss),
 yeccpars2(yeccgoto(envelope, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(143, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(144, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(145, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(146, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(147, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(148, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(149, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(150, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(151, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_151_(__Stack),
 __Nss = lists:nthtail(11, __Ss),
 yeccpars2(yeccgoto(envelope, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(152, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(153, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(154, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(155, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(156, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(157, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [157 | __Ss], [__T | __Stack]);
yeccpars2(157, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [157 | __Ss], [__T | __Stack]);
yeccpars2(157, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(158, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(159, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(160, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 161, [160 | __Ss], [__T | __Stack]);
yeccpars2(160, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(161, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_161_(__Stack),
 __Nss = lists:nthtail(11, __Ss),
 yeccpars2(yeccgoto(envelope, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(162, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_162_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(media_text, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(163, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_163_(__Stack),
 yeccpars2(yeccgoto(media_subtype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(164, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_164_(__Stack),
 yeccpars2(yeccgoto(media_subtype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(165, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 169, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 170, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_165_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(body_type_mpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(166, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_166_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(body_type_mpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(167, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 179, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_167_(__Stack),
 yeccpars2(yeccgoto(body_ext_mpart, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(168, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_168_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(body_type_mpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(169, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 172, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 173, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(170, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_170_(__Stack),
 yeccpars2(yeccgoto(body_fld_param_list, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(171, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 176, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(172, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 175, [172 | __Ss], [__T | __Stack]);
yeccpars2(172, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(173, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 173, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_173_(__Stack),
 yeccpars2(yeccgoto(body_fld_param, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(174, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_174_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(body_fld_param, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(175, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_175_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(body_fld_param_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(176, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_176_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(body_fld_param_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(177, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_177_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(body_ext_mpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(178, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 180, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(179, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_179_(__Stack),
 yeccpars2(yeccgoto(body_fld_dsp, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(180, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 169, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 170, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(181, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 182, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(182, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_182_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(body_fld_dsp, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(183, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(body_fld_lang, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(184, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [184 | __Ss], [__T | __Stack]);
yeccpars2(184, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [184 | __Ss], [__T | __Stack]);
yeccpars2(184, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_184_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(body_ext_mpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(185, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(body_fld_loc, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(186, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_186_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(body_ext_mpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(187, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_187_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(body, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(188, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(body_fld_md5, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(189, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [189 | __Ss], [__T | __Stack]);
yeccpars2(189, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 179, [189 | __Ss], [__T | __Stack]);
yeccpars2(189, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_189_(__Stack),
 yeccpars2(yeccgoto(body_ext_1part, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(190, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_190_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(body_type_1part, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(191, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_191_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(body_ext_1part, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(192, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [192 | __Ss], [__T | __Stack]);
yeccpars2(192, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [192 | __Ss], [__T | __Stack]);
yeccpars2(192, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_192_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(body_ext_1part, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(193, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_193_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(body_ext_1part, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(194, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_194_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(body, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(195, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_195_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(body_type_1part, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(196, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [196 | __Ss], [__T | __Stack]);
yeccpars2(196, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [196 | __Ss], [__T | __Stack]);
yeccpars2(196, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(197, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 199, [197 | __Ss], [__T | __Stack]);
yeccpars2(197, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_197_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(body_type_basic, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(198, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_198_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(body_type_text, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(199, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_199_(__Stack),
 yeccpars2(yeccgoto(body_fld_lines, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(200, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(body_fld_id, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(201, nil, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [201 | __Ss], [__T | __Stack]);
yeccpars2(201, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [201 | __Ss], [__T | __Stack]);
yeccpars2(201, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(202, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(body_fld_desc, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(203, encoding, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 205, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(204, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 207, [204 | __Ss], [__T | __Stack]);
yeccpars2(204, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(205, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_205_(__Stack),
 yeccpars2(yeccgoto(body_fld_enc, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(body_fields, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(207, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_207_(__Stack),
 yeccpars2(yeccgoto(body_fld_octets, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(208, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_208_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(msg_att_static, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(209, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_209_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(msg_att, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(210, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_210_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(msg_att, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(211, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_211_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(msg_att_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(212, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_212_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(mailbox_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(213, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 234, [213 | __Ss], [__T | __Stack]);
yeccpars2(213, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(214, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_214_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(mailbox_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(215, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 227, [215 | __Ss], [__T | __Stack]);
yeccpars2(215, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(216, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_216_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(capability_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(217, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 222, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 223, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(218, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 218, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_218_(__Stack),
 yeccpars2(yeccgoto(numbers, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(219, '$end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(cap_string, hd(__Ss)), '$end', __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(219, command, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(cap_string, hd(__Ss)), command, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(219, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(cap_string, hd(__Ss)), string, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(219, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_219_(__Stack),
 yeccpars2(yeccgoto(mailbox, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(220, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_220_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(numbers, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(221, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 225, [221 | __Ss], [__T | __Stack]);
yeccpars2(221, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(222, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_222_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(mbx_list_flags, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(223, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 223, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_223_(__Stack),
 yeccpars2(yeccgoto(mbx_list_flag, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(224, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_224_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(mbx_list_flag, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(225, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_225_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(mbx_list_flags, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(226, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_226_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(mailbox_data, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(227, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 229, [227 | __Ss], [__T | __Stack]);
yeccpars2(227, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 230, [227 | __Ss], [__T | __Stack]);
yeccpars2(227, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(228, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 233, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(229, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_229_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(status_att_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(230, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 231, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(231, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 230, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_231_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(status_att, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(232, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_232_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(status_att, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(233, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_233_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(status_att_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(234, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 236, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(235, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_235_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(mailbox_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(236, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_236_(__Stack),
 yeccpars2(yeccgoto(mailbox, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(addr_adl, 116) ->
 120;
yeccgoto(addr_host, 122) ->
 124;
yeccgoto(addr_mailbox, 120) ->
 122;
yeccgoto(addr_name, 114) ->
 116;
yeccgoto(address, 111) ->
 113;
yeccgoto(address, 125) ->
 126;
yeccgoto(address_list, 107) ->
 110;
yeccgoto(address_list, 108) ->
 110;
yeccgoto(address_list, 109) ->
 129;
yeccgoto(address_list, 128) ->
 131;
yeccgoto(address_list, 130) ->
 133;
yeccgoto(address_list, 132) ->
 135;
yeccgoto(address_list, 134) ->
 137;
yeccgoto(address_list, 143) ->
 129;
yeccgoto(address_list, 144) ->
 131;
yeccgoto(address_list, 145) ->
 133;
yeccgoto(address_list, 146) ->
 135;
yeccgoto(address_list, 147) ->
 137;
yeccgoto(address_list, 152) ->
 110;
yeccgoto(address_list, 153) ->
 129;
yeccgoto(address_list, 154) ->
 131;
yeccgoto(address_list, 155) ->
 133;
yeccgoto(address_list, 156) ->
 135;
yeccgoto(address_list, 157) ->
 137;
yeccgoto(body, 59) ->
 62;
yeccgoto(body, 63) ->
 102;
yeccgoto(body, 102) ->
 102;
yeccgoto(body, 103) ->
 102;
yeccgoto(body_ext_1part, 98) ->
 195;
yeccgoto(body_ext_1part, 100) ->
 190;
yeccgoto(body_ext_mpart, 165) ->
 168;
yeccgoto(body_fields, 97) ->
 197;
yeccgoto(body_fld_desc, 201) ->
 203;
yeccgoto(body_fld_dsp, 167) ->
 177;
yeccgoto(body_fld_dsp, 189) ->
 191;
yeccgoto(body_fld_enc, 203) ->
 204;
yeccgoto(body_fld_id, 196) ->
 201;
yeccgoto(body_fld_lang, 177) ->
 184;
yeccgoto(body_fld_lang, 191) ->
 192;
yeccgoto(body_fld_lines, 197) ->
 198;
yeccgoto(body_fld_loc, 184) ->
 186;
yeccgoto(body_fld_loc, 192) ->
 193;
yeccgoto(body_fld_md5, 98) ->
 189;
yeccgoto(body_fld_md5, 100) ->
 189;
yeccgoto(body_fld_octets, 204) ->
 206;
yeccgoto(body_fld_param, 169) ->
 171;
yeccgoto(body_fld_param, 173) ->
 174;
yeccgoto(body_fld_param_list, 97) ->
 196;
yeccgoto(body_fld_param_list, 165) ->
 167;
yeccgoto(body_fld_param_list, 180) ->
 181;
yeccgoto(body_type_1part, 63) ->
 101;
yeccgoto(body_type_1part, 103) ->
 101;
yeccgoto(body_type_basic, 63) ->
 100;
yeccgoto(body_type_basic, 103) ->
 100;
yeccgoto(body_type_mpart, 63) ->
 99;
yeccgoto(body_type_mpart, 102) ->
 166;
yeccgoto(body_type_mpart, 103) ->
 99;
yeccgoto(body_type_text, 63) ->
 98;
yeccgoto(body_type_text, 103) ->
 98;
yeccgoto(cap_string, 12) ->
 40;
yeccgoto(cap_string, 35) ->
 40;
yeccgoto(cap_string, 40) ->
 40;
yeccgoto(capability, 12) ->
 216;
yeccgoto(capability, 35) ->
 39;
yeccgoto(capability, 40) ->
 43;
yeccgoto(capability_data, 0) ->
 7;
yeccgoto(capability_data_list, 14) ->
 28;
yeccgoto(continue_req, 0) ->
 6;
yeccgoto(env_bcc, 134) ->
 136;
yeccgoto(env_bcc, 147) ->
 148;
yeccgoto(env_bcc, 157) ->
 158;
yeccgoto(env_cc, 132) ->
 134;
yeccgoto(env_cc, 146) ->
 147;
yeccgoto(env_cc, 156) ->
 157;
yeccgoto(env_from, 107) ->
 143;
yeccgoto(env_from, 108) ->
 109;
yeccgoto(env_from, 152) ->
 153;
yeccgoto(env_in_reply_to, 136) ->
 139;
yeccgoto(env_in_reply_to, 148) ->
 149;
yeccgoto(env_in_reply_to, 158) ->
 159;
yeccgoto(env_message_id, 139) ->
 141;
yeccgoto(env_message_id, 149) ->
 150;
yeccgoto(env_message_id, 159) ->
 160;
yeccgoto(env_reply_to, 128) ->
 130;
yeccgoto(env_reply_to, 144) ->
 145;
yeccgoto(env_reply_to, 154) ->
 155;
yeccgoto(env_sender, 109) ->
 128;
yeccgoto(env_sender, 143) ->
 144;
yeccgoto(env_sender, 153) ->
 154;
yeccgoto(env_to, 130) ->
 132;
yeccgoto(env_to, 145) ->
 146;
yeccgoto(env_to, 155) ->
 156;
yeccgoto(envelope, 59) ->
 61;
yeccgoto(flag, 18) ->
 21;
yeccgoto(flag, 23) ->
 24;
yeccgoto(flag, 63) ->
 21;
yeccgoto(flag, 106) ->
 24;
yeccgoto(flag, 108) ->
 24;
yeccgoto(flag_list, 15) ->
 17;
yeccgoto(flag_list, 29) ->
 33;
yeccgoto(flag_list, 37) ->
 33;
yeccgoto(flag_list, 59) ->
 60;
yeccgoto(form, 0) ->
 5;
yeccgoto(greeting, 0) ->
 4;
yeccgoto(mailbox, 12) ->
 215;
yeccgoto(mailbox, 234) ->
 235;
yeccgoto(mailbox_data, 8) ->
 11;
yeccgoto(mailbox_list, 12) ->
 214;
yeccgoto(mbx_list_flag, 217) ->
 221;
yeccgoto(mbx_list_flag, 223) ->
 224;
yeccgoto(mbx_list_flags, 12) ->
 213;
yeccgoto(media_subtype, 102) ->
 165;
yeccgoto(media_subtype, 104) ->
 162;
yeccgoto(media_text, 63) ->
 97;
yeccgoto(media_text, 103) ->
 97;
yeccgoto(message_data, 8) ->
 10;
yeccgoto(msg_att, 53) ->
 57;
yeccgoto(msg_att_dynamic, 53) ->
 56;
yeccgoto(msg_att_dynamic, 57) ->
 210;
yeccgoto(msg_att_list, 50) ->
 52;
yeccgoto(msg_att_static, 53) ->
 55;
yeccgoto(msg_att_static, 57) ->
 209;
yeccgoto(nstring, 98) ->
 188;
yeccgoto(nstring, 100) ->
 188;
yeccgoto(nstring, 114) ->
 115;
yeccgoto(nstring, 116) ->
 119;
yeccgoto(nstring, 120) ->
 121;
yeccgoto(nstring, 122) ->
 123;
yeccgoto(nstring, 136) ->
 138;
yeccgoto(nstring, 139) ->
 140;
yeccgoto(nstring, 148) ->
 138;
yeccgoto(nstring, 149) ->
 140;
yeccgoto(nstring, 158) ->
 138;
yeccgoto(nstring, 159) ->
 140;
yeccgoto(nstring, 177) ->
 183;
yeccgoto(nstring, 184) ->
 185;
yeccgoto(nstring, 191) ->
 183;
yeccgoto(nstring, 192) ->
 185;
yeccgoto(nstring, 196) ->
 200;
yeccgoto(nstring, 201) ->
 202;
yeccgoto(numbers, 12) ->
 212;
yeccgoto(numbers, 218) ->
 220;
yeccgoto(resp_text, 8) ->
 9;
yeccgoto(resp_text, 14) ->
 27;
yeccgoto(resp_text, 15) ->
 16;
yeccgoto(resp_text, 20) ->
 16;
yeccgoto(resp_text, 26) ->
 47;
yeccgoto(resp_text, 28) ->
 46;
yeccgoto(resp_text, 30) ->
 31;
yeccgoto(resp_text, 48) ->
 49;
yeccgoto(resp_text_code, 29) ->
 32;
yeccgoto(resp_text_code, 37) ->
 38;
yeccgoto(resp_text_code_list, 14) ->
 26;
yeccgoto(response, 0) ->
 3;
yeccgoto(response_data, 0) ->
 2;
yeccgoto(response_done, 0) ->
 1;
yeccgoto(status_att, 227) ->
 228;
yeccgoto(status_att, 231) ->
 232;
yeccgoto(status_att_list, 215) ->
 226;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_9_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 128).
yeccpars2_9_([__2,__1 | __Stack]) ->
 [begin
   # imap_resp { tag = v ( __1 ) , status = v ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 250).
yeccpars2_10_([__2,__1 | __Stack]) ->
 [begin
   case __2 of
    Fetch when is_record ( Fetch , fetch ) -> # imap_resp { tag = v ( __1 ) , data = __2 } ;
    { Data , Cmd } -> # imap_resp { tag = v ( __1 ) , data = Data , cmd = Cmd } ;
    _ -> __2
    end
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 248).
yeccpars2_11_([__2,__1 | __Stack]) ->
 [begin
   __2 # imap_resp { tag = v ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 174).
yeccpars2_12_([__1 | __Stack]) ->
 [begin
   # imap_resp { cmd = v ( __1 ) , data = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 276).
yeccpars2_14_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 275).
yeccpars2_15_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 277).
yeccpars2_16_([__2,__1 | __Stack]) ->
 [begin
   [ value_of ( __1 ) , [ 32 ] , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 180).
yeccpars2_17_([__2,__1 | __Stack]) ->
 [begin
   # imap_resp { data = __2 , cmd = v ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 276).
yeccpars2_19_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 275).
yeccpars2_20_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 150).
yeccpars2_22_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 157).
yeccpars2_23_([__1 | __Stack]) ->
 [begin
   v ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 158).
yeccpars2_24_([__2,__1 | __Stack]) ->
 [begin
   [ v ( __1 ) , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 152).
yeccpars2_25_([__3,__2,__1 | __Stack]) ->
 [begin
   if
    is_list ( __2 ) -> lists : flatten ( __2 ) ;
    true -> [ __2 ]
    end
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 245).
yeccpars2_26_([__3,__2,__1 | __Stack]) ->
 [begin
   # imap_resp { tag = v ( __1 ) , status = v ( __2 ) , code = clean_resp_code ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 243).
yeccpars2_27_([__3,__2,__1 | __Stack]) ->
 [begin
   # imap_resp { tag = v ( __1 ) , info = lists : flatten ( __3 ) , status = v ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 258).
yeccpars2_31_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # imap_resp { tag = v ( __1 ) , status = v ( __2 ) , info = __4 , cmd = v ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 267).
yeccpars2_34_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 270).
yeccpars2_36_([__1 | __Stack]) ->
 [begin
   integer_to_list ( v ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 269).
yeccpars2_37_([__1 | __Stack]) ->
 [begin
   v ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 272).
yeccpars2_38_([__2,__1 | __Stack]) ->
 [begin
   [ v ( __1 ) , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 118).
yeccpars2_40_([__1 | __Stack]) ->
 [begin
   [ value_of ( __1 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 119).
yeccpars2_43_([__2,__1 | __Stack]) ->
 [begin
   [ value_of ( __1 ) | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 124).
yeccpars2_44_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { v ( __2 ) , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 268).
yeccpars2_45_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 161).
yeccpars2_46_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { Cmd , Data } = __3 ,
    # imap_resp { tag = v ( __1 ) , status = v ( __2 ) , cmd = Cmd , data = Data , info = lists : flatten ( __4 ) }
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 247).
yeccpars2_47_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # imap_resp { tag = v ( __1 ) , status = v ( __2 ) , code = clean_resp_code ( __3 ) , info = lists : flatten ( __4 ) }
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 260).
yeccpars2_49_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   Cmd = v ( __4 ) ,
    if
    Cmd == append -> # imap_resp { tag = v ( __1 ) , status = v ( __2 ) , info = __5 , cmd = Cmd } ;
    Cmd == copy -> # imap_resp { tag = v ( __1 ) , status = v ( __2 ) , info = __5 , cmd = Cmd } ;
    true -> # imap_resp { tag = v ( __1 ) , status = v ( __2 ) , code = clean_resp_code ( __3 ) , info = __5 , cmd = Cmd }
    end
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 191).
yeccpars2_50_([__2,__1 | __Stack]) ->
 [begin
   { value_of ( __1 ) , v ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 178).
yeccpars2_51_([__2,__1 | __Stack]) ->
 [begin
   # imap_resp { cmd = v ( __2 ) , data = value_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 193).
yeccpars2_52_([__3,__2,__1 | __Stack]) ->
 [begin
   case __3 of
    { _ , Flags } -> # fetch { flags = Flags , seqnum = value_of ( __1 ) } ;
    _ -> __3 # fetch { seqnum = value_of ( __1 ) }
    end
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 200).
yeccpars2_54_([__1 | __Stack]) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 209).
yeccpars2_60_([__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 214).
yeccpars2_61_([__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 215).
yeccpars2_62_([__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 212).
yeccpars2_65_([__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 211).
yeccpars2_66_([__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 216).
yeccpars2_70_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __5 ) }
  end | __Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 217).
yeccpars2_78_([__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __8 ) }
  end | __Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 218).
yeccpars2_84_([__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __11 ) }
  end | __Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 219).
yeccpars2_90_([__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __8 ) }
  end | __Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 220).
yeccpars2_96_([__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __11 ) }
  end | __Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 157).
yeccpars2_106_([__1 | __Stack]) ->
 [begin
   v ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 157).
yeccpars2_108_([__1 | __Stack]) ->
 [begin
   v ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_112_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 26).
yeccpars2_112_([__1 | __Stack]) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_117_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 223).
yeccpars2_117_([__1 | __Stack]) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_118_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 222).
yeccpars2_118_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_125_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 34).
yeccpars2_125_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # address { addr_name = __2 , addr_adl = __3 , addr_mailbox = __4 , addr_host = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_126_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 36).
yeccpars2_126_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ # address { addr_name = __2 , addr_adl = __3 , addr_mailbox = __4 , addr_host = __5 } , __7 ]
  end | __Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 28).
yeccpars2_127_([__3,__2,__1 | __Stack]) ->
 [begin
   case __2 of
    A when is_record ( A , address ) -> [ A ] ;
    L when is_list ( L ) -> L
    end
  end | __Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 132).
yeccpars2_142_([__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # envelope { date = value_of ( __2 ) , subject = value_of ( __3 ) , from = __4 , sender = __5 , reply_to = __6 , to = __7 , cc = __8 , bcc = __9 , in_reply_to = __10 , message_id = __11 }
  end | __Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 134).
yeccpars2_151_([__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # envelope { date = value_of ( __2 ) , subject = nil , from = __4 , sender = __5 , reply_to = __6 , to = __7 , cc = __8 , bcc = __9 , in_reply_to = __10 , message_id = __11 }
  end | __Stack].

-compile({inline,{yeccpars2_161_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 136).
yeccpars2_161_([__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # envelope { date = nil , subject = value_of ( __3 ) , from = __4 , sender = __5 , reply_to = __6 , to = __7 , cc = __8 , bcc = __9 , in_reply_to = __10 , message_id = __11 }
  end | __Stack].

-compile({inline,{yeccpars2_162_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 187).
yeccpars2_162_([__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_163_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 185).
yeccpars2_163_([__1 | __Stack]) ->
 [begin
   v ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 186).
yeccpars2_164_([__1 | __Stack]) ->
 [begin
   v ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_165_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 100).
yeccpars2_165_([__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_166_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 103).
yeccpars2_166_([__2,__1 | __Stack]) ->
 [begin
   [ __1 , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_167_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 60).
yeccpars2_167_([__1 | __Stack]) ->
 [begin
   { __1 , [ ] , [ ] , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 101).
yeccpars2_168_([__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 79).
yeccpars2_170_([__1 | __Stack]) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 83).
yeccpars2_173_([__1 | __Stack]) ->
 [begin
   [ value_of ( __1 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_174_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 84).
yeccpars2_174_([__2,__1 | __Stack]) ->
 [begin
   [ value_of ( __1 ) | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 80).
yeccpars2_175_([__3,__2,__1 | __Stack]) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_176_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 81).
yeccpars2_176_([__3,__2,__1 | __Stack]) ->
 [begin
   clean_param ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_177_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 61).
yeccpars2_177_([__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 , [ ] , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_179_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 70).
yeccpars2_179_([__1 | __Stack]) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_182_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 69).
yeccpars2_182_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { v ( __2 ) , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_184_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 62).
yeccpars2_184_([__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 , __3 , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_186_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 63).
yeccpars2_186_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 , __3 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_187_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 43).
yeccpars2_187_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_189_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 55).
yeccpars2_189_([__1 | __Stack]) ->
 [begin
   { __1 , [ ] , [ ] , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_190_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 92).
yeccpars2_190_([__2,__1 | __Stack]) ->
 [begin
   { MD5 , Dsp , Lang , Loc } = __2 ,
    __1 # part { md5 = MD5 , dsp = Dsp , lang = Lang , loc = Loc }
  end | __Stack].

-compile({inline,{yeccpars2_191_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 56).
yeccpars2_191_([__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 , [ ] , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 57).
yeccpars2_192_([__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 , __3 , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_193_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 58).
yeccpars2_193_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 , __3 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 45).
yeccpars2_194_([__3,__2,__1 | __Stack]) ->
 [begin
   case __2 of
    Part when is_record ( Part , part ) -> # body { type = basic , parts = Part } ;
    List when is_list ( List ) ->
    case clean_parts ( List ) of
    { Parts , Type , [ ] } -> # body { type = Type , parts = Parts } ;
    { Parts , Type , { MD5 , Dsp , Lang , Loc } } -> # body { type = Type , parts = Parts , md5 = MD5 , dsp = Dsp , lang = Lang , loc = Loc }
    end ;
    Other -> Other
    end
  end | __Stack].

-compile({inline,{yeccpars2_195_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 89).
yeccpars2_195_([__2,__1 | __Stack]) ->
 [begin
   { MD5 , Dsp , Lang , Loc } = __2 ,
    __1 # part { md5 = MD5 , dsp = Dsp , lang = Lang , loc = Loc }
  end | __Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 96).
yeccpars2_197_([__2,__1 | __Stack]) ->
 [begin
   { Type , SubType } = __1 ,
    { Params , Id , Desc , Enc , Octets } = __2 ,
    # part { type = Type , subtype = SubType , params = Params , id = Id , desc = Desc , encoding = Enc , octets = Octets }
  end | __Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 107).
yeccpars2_198_([__3,__2,__1 | __Stack]) ->
 [begin
   { Type , SubType } = __1 ,
    { Params , Id , Desc , Enc , Octets } = __2 ,
    # part { type = Type , subtype = SubType , params = Params , id = Id , desc = Desc , encoding = Enc , octets = Octets , lines = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_199_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 75).
yeccpars2_199_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_205_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 71).
yeccpars2_205_([__1 | __Stack]) ->
 [begin
   v ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 66).
yeccpars2_206_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 , __3 , __4 , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_207_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 77).
yeccpars2_207_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 213).
yeccpars2_208_([__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_209_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 206).
yeccpars2_209_([__2,__1 | __Stack]) ->
 [begin
   [ __1 , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 205).
yeccpars2_210_([__2,__1 | __Stack]) ->
 [begin
   [ __1 , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 201).
yeccpars2_211_([__3,__2,__1 | __Stack]) ->
 [begin
   build_fetch ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 176).
yeccpars2_212_([__2,__1 | __Stack]) ->
 [begin
   # imap_resp { cmd = v ( __1 ) , data = lists : flatten ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_214_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 172).
yeccpars2_214_([__2,__1 | __Stack]) ->
 [begin
   # imap_resp { cmd = v ( __1 ) , data = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 122).
yeccpars2_216_([__3,__2,__1 | __Stack]) ->
 [begin
   # imap_resp { tag = '*' , data = __3 , cmd = capability }
  end | __Stack].

-compile({inline,{yeccpars2_218_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 225).
yeccpars2_218_([__1 | __Stack]) ->
 [begin
   [ value_of ( __1 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 166).
yeccpars2_219_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_220_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 226).
yeccpars2_220_([__2,__1 | __Stack]) ->
 [begin
   [ value_of ( __1 ) | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_222_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 228).
yeccpars2_222_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_223_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 231).
yeccpars2_223_([__1 | __Stack]) ->
 [begin
   v ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_224_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 232).
yeccpars2_224_([__2,__1 | __Stack]) ->
 [begin
   [ v ( __1 ) , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_225_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 229).
yeccpars2_225_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 170).
yeccpars2_226_([__3,__2,__1 | __Stack]) ->
 [begin
   # imap_resp { cmd = v ( __1 ) , data = __3 , info = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_229_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 281).
yeccpars2_229_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_231_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 284).
yeccpars2_231_([__2,__1 | __Stack]) ->
 [begin
   { v ( __1 ) , value_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 285).
yeccpars2_232_([__3,__2,__1 | __Stack]) ->
 [begin
   [ { v ( __1 ) , value_of ( __2 ) } , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_233_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 282).
yeccpars2_233_([__3,__2,__1 | __Stack]) ->
 [begin
   lists : flatten ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 183).
yeccpars2_235_([__3,__2,__1 | __Stack]) ->
 [begin
   # folder { name = __3 , delim = value_of ( __2 ) , flags = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_236_,1}}).
-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 166).
yeccpars2_236_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].


-file("/sfe/erlang/releases/lib/erlmail-0.0.6/src/imap_parser.yrl", 382).
