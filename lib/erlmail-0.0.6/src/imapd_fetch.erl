%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP Server FETCH routines
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.6
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
-module(imapd_fetch).
-author('sjackson@simpleenigma.com').
-include("../include/erlmail.hrl").
-include("../include/imap.hrl").
-include("../include/mime.hrl").


-export([fetch/3]).
-export([items/1,items/2,tokens/1,tokens/2]).

-export([envelope/1]).
-export([value_or_nil/1,value_or_nil/2]).

%%-------------------------------------------------------------------------
%% @spec (MessageList::list(),Items::list(),State::imapd_fsm()) -> list()
%% @doc Takes a list of MessageName and retrieves Items for each message
%% @end
%%-------------------------------------------------------------------------
fetch(List,Items,State) -> fetch(List,Items,State,1,[]).

fetch([],_Items,_State,_SeqNum,Acc) -> lists:reverse(Acc);
fetch([{MessageName,UID}|T],Items,State,SeqNum,Acc) ->
	Resp = do_fetch(MessageName,Items,State),
	fetch(T,Items,State,SeqNum+1,[Resp#imap_resp{code=UID}|Acc]);
fetch([MessageName|T],Items,State,SeqNum,Acc) ->
	Resp = do_fetch(MessageName,Items,State),
	fetch(T,Items,State,SeqNum+1,[Resp#imap_resp{code=SeqNum}|Acc]).



do_fetch(MessageName,Items,#imapd_fsm{user = User} = _State) -> 
	Store = erlmail_util:get_app_env(store_type_message,mnesia_store),
	{UserName,DomainName} = User#user.name,
	Message = Store:select({MessageName,UserName,DomainName}),
	MIME = mime:decode(Message#message.message),
	Info = process(Items,Message,MIME),
	?D(Info),
	#imap_resp{tag='*',cmd = fetch, info = Info}.


process(Items,Message,MIME) -> process(Items,Message,MIME, <<>>).

process([],_Message,_MIME,Bin) -> 
	lists:flatten([40,string:strip(binary_to_list(Bin)),41]);

process([#imap_fetch_cmd{name = 'envelope'} = _Cmd|T],Message,MIME,Bin) ->
	Envelope = ["ENVELOPE",32,envelope(MIME),32],
	EnvelopeBin = list_to_binary(Envelope),
	process(T,Message,MIME,<<Bin/binary,EnvelopeBin/binary>>);

process([#imap_fetch_cmd{name = 'body'} = _Cmd|T],Message,MIME,Bin) ->
	BS = body(MIME),
	Body = ["BODY",32,BS,32],
	BodyBin = list_to_binary(Body),
	process(T,Message,MIME,<<Bin/binary,BodyBin/binary>>);

process([#imap_fetch_cmd{name = 'bodystructure'} = _Cmd|T],Message,MIME,Bin) ->
	BS = bodystructure(MIME),
	BodyStructure = ["BODYSTRUCTURE",32,BS,32],
	BodyStructureBin = list_to_binary(BodyStructure),
	process(T,Message,MIME,<<Bin/binary,BodyStructureBin/binary>>);

process([#imap_fetch_cmd{name='flags'} = _Cmd|T],Message,MIME,Bin) ->
	Flags = ["FLAGS",32,imapd_util:flags_resp(Message#message.flags),32],
	FlagsBin = list_to_binary(Flags),
	process(T,Message,MIME,<<Bin/binary,FlagsBin/binary>>);

process([#imap_fetch_cmd{name='internaldate'} = _Cmd|T],Message,MIME,Bin) ->
	InternalDate = ["INTERNALDATE",32,internaldate(Message),32],
	InternalDateBin = list_to_binary(InternalDate),
	process(T,Message,MIME,<<Bin/binary,InternalDateBin/binary>>);


process([#imap_fetch_cmd{name='rfc822.size'} = _Cmd|T],Message,MIME,Bin) ->
	Size = ["RFC822.SIZE",32,integer_to_list(length(Message#message.message)),32],
	SizeBin = list_to_binary(Size),
	process(T,Message,MIME,<<Bin/binary,SizeBin/binary>>);

process([#imap_fetch_cmd{name = 'rfc822.header'} = _Cmd|T],Message,MIME,Bin) ->
	H = MIME#mime.header_text,
	Size = length(H),
	Header = ["RFC822.HEADER",32,123,integer_to_list(Size),125,H,32],
	HeaderBin = list_to_binary(Header),
	process(T,Message,MIME,<<Bin/binary,HeaderBin/binary>>);

process([#imap_fetch_cmd{name = uid} = _Cmd|T],Message,MIME,Bin) ->
	UID = ["UID",32,integer_to_list(Message#message.uid),32],
	UIDBin = list_to_binary(UID),
	process(T,Message,MIME,<<Bin/binary,UIDBin/binary>>);

process([#imap_fetch_cmd{name = 'body.peek'} = _Cmd|T],Message,MIME,Bin) ->
	B = MIME#mime.header_text ++ MIME#mime.body_text,
	_Size = length(B),
	BodyPeek = [], % "BODY.PEEK[]",32,32,123,integer_to_list(Size),125,13,10,B,
	BodyPeekBin = list_to_binary(BodyPeek),
	process(T,Message,MIME,<<Bin/binary,BodyPeekBin/binary>>);

process([#imap_fetch_cmd{name = 'rfc822'} = _Cmd|T],Message,MIME,Bin) ->
	M = Message#message.message,
	Size = length(M),
	RFC822 = ["RFC822",32,123,integer_to_list(Size),125,13,10,Message#message.message,32],
	RFC822Bin = list_to_binary(RFC822),
	process(T,Message,MIME,<<Bin/binary,RFC822Bin/binary>>);


process([H|T],Message,MIME,Bin) ->
	?D({"Cannot Process: ",H}),
	process(T,Message,MIME,Bin).


envelope(MIME) -> envelope(MIME,[date,subject,from,sender,reply_to,to,cc,bcc,in_reply_to,message_id], <<>>).

envelope(_MIME,[],Bin) ->
	lists:flatten([40,string:strip(binary_to_list(Bin)),41]);

envelope(MIME, [H|T], Bin) when H =:= date; H =:= subject; H =:= in_reply_to; H =:= message_id ->
	String = case lists:keysearch(H,1,MIME#mime.header) of
		{value,{H,Value}} -> imapd_util:quote(Value,true);
		_ -> "NIL"
	end,
	NewBin = list_to_binary([String,32]),
	envelope(MIME,T,<<Bin/binary,NewBin/binary>>);
envelope(MIME, [H|T], Bin) when H =:= from; H =:= to; H =:= cc; H =:= bcc ->
	String = case lists:keysearch(H,1,MIME#mime.header) of
		{value,{H,Value}} -> string_to_address(Value);
		_ -> "NIL"
	end,
	NewBin = list_to_binary([String,32]),
	envelope(MIME,T,<<Bin/binary,NewBin/binary>>);
envelope(MIME, [H|T], Bin) when H =:= sender; H =:= reply_to ->
	String = case lists:keysearch(H,1,MIME#mime.header) of
		{value,{H,Value}} -> string_to_address(Value);
		_ -> 
			case lists:keysearch(from,1,MIME#mime.header) of
				{value,{from,Value}} -> string_to_address(Value);
				_ -> "NIL"
			end
	end,
	NewBin = list_to_binary([String,32]),
	envelope(MIME,T,<<Bin/binary,NewBin/binary>>);



envelope(MIME, [H|T], Bin) ->
	?D(H),
	envelope(MIME,T,Bin).


string_to_address(String) -> 
	AddressList = imapd_util:parse_addresses(String),
	Addresses = lists:map(fun(A) -> 
		[40,
		value_or_nil(A#address.addr_name,true),32,
		value_or_nil(A#address.addr_adl,true),32,
		value_or_nil(A#address.addr_mailbox,true),32,
		value_or_nil(A#address.addr_host,true),
		41]
		end,AddressList),
	lists:flatten([40,Addresses,41]). 




bodystructure(MIME) -> 
	?D(MIME),
	bodystructure(MIME,[]).
bodystructure(MIME,_Acc) ->
case MIME#mime.body of
		[#mime{}|_] = MIMEBody -> bodystructure(MIMEBody);
		TextBody -> 
			{Type,SubType,CharSet} = case mime:get_header('content-type',MIME) of
				[] -> {"text","plain","us-ascii"};
				ContentType ->
					case string:tokens(ContentType,[32,34,47,59,61]) of
						[T,ST] -> {T,ST,"us-ascii"};
						[T,ST,_,C] -> {T,ST,C};
						_Other -> 
							?D(_Other),
							{"text","plain","us-ascii"}
					end
					
			end,
			ContentId = mime:get_header('content-id',MIME,"NIL"),
			ContentDesc = mime:get_header('content-description',MIME,"NIL"),
			ContentTransEnc = mime:get_header('content-transfer-encoding',MIME,"7BIT"),
			Size = length(TextBody),
			Lines = length(string:tokens(TextBody,[13,10])),
			[40,
				imapd_util:quote(string:to_upper(Type),true),32,
				imapd_util:quote(string:to_upper(SubType),true),32,
				40,
					imapd_util:quote("CHARSET",true),32,
					imapd_util:quote(string:to_upper(CharSet),true),
				41,32,
				imapd_util:quote(string:to_upper(ContentId),true),32,
				imapd_util:quote(string:to_upper(ContentDesc),true),32,
				imapd_util:quote(string:to_upper(ContentTransEnc),true),32,
				integer_to_list(Size),32,
				integer_to_list(Lines),
			41]
	end.



body(MIME) -> 
%	?D(MIME),
	body(MIME,[]).
body(MIME,_Acc) ->
case MIME#mime.body of
		[#mime{}|_] = MIMEBody -> bodystructure(MIMEBody);
		TextBody -> 
			{Type,SubType,CharSet} = case mime:get_header('content-type',MIME) of
				[] -> {"text","plain","us-ascii"};
				ContentType ->
					case string:tokens(ContentType,[32,34,47,59,61]) of
						[T,ST] -> {T,ST,"us-ascii"};
						[T,ST,_,C] -> {T,ST,C};
						_Other -> 
							?D(_Other),
							{"text","plain","us-ascii"}
					end
					
			end,
			ContentId = mime:get_header('content-id',MIME,"NIL"),
			ContentDesc = mime:get_header('content-description',MIME,"NIL"),
			ContentTransEnc = mime:get_header('content-transfer-encoding',MIME,"7BIT"),
			Size = length(TextBody),
			Lines = length(string:tokens(TextBody,[13,10])),
			[40,
				imapd_util:quote(string:to_upper(Type),true),32,
				imapd_util:quote(string:to_upper(SubType),true),32,
				40,
					imapd_util:quote("CHARSET",true),32,
					imapd_util:quote(string:to_upper(CharSet),true),
				41,32,
				imapd_util:quote(string:to_upper(ContentId),true),32,
				imapd_util:quote(string:to_upper(ContentDesc),true),32,
				imapd_util:quote(string:to_upper(ContentTransEnc),true),32,
				integer_to_list(Size),32,
				integer_to_list(Lines),
			41]
	end.











%% @todo: impliment real timezone code
internaldate(Message) when is_record(Message,message) -> 
	case lists:keysearch(internaldate,1,Message#message.options) of
		{value,{internaldate,DateTime}} -> internaldate(DateTime);
		_ -> 
			?D(no_internaldate),
			mnesia_store:update(Message#message{options=[{internaldate,{date(),time()}}]}),
			internaldate({date(),time()})
	end;
internaldate({{Year,Month,Day},{Hour,Minute,Second}}) ->
	[34,integer_to_list(Day),45,month(Month),45,integer_to_list(Year),32,padded_integer_to_list(Hour),58,padded_integer_to_list(Minute),58,padded_integer_to_list(Second)," -0800",34].

month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

padded_integer_to_list(Integer) when Integer < 10 -> ["0",integer_to_list(Integer)];
padded_integer_to_list(Integer)                   -> integer_to_list(Integer).




items(String) ->	
	% Tokenize string
	items(String,[]).

items(_List,_Acc) -> ok.

%%-------------------------------------------------------------------------
%% @spec (String::string()) -> list()
%% @doc Takes a fetch string and returns a list of the data items.
%%      The data Items are return as a list of lower case atoms, or tuples
%% @end
%%-------------------------------------------------------------------------
tokens(String) -> 
	S1 = http_util:to_lower(String),
	S2 = string:strip(S1,both,40),
	S3 = string:strip(S2,both,41),
	tokens(list_to_binary(S3),[]).
%%-------------------------------------------------------------------------
%% @spec (String::string(),Acc::list()) -> list()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
tokens(<<>>,Acc) -> lists:ukeysort(2,Acc);
tokens(<<32,Rest/binary>>,Acc) ->
	tokens(Rest,Acc);
tokens(<<"all",Rest/binary>>,Acc) ->
	All = [
		#imap_fetch_cmd{name=flags,string="FLAGS"},
		#imap_fetch_cmd{name=internaldate,string="INTERNALDATE"},
		#imap_fetch_cmd{name='rfc822.size',string="RFC822.SIZE"},
		#imap_fetch_cmd{name=envelope,string="ENVELOPE"}
		],
	tokens(Rest,lists:append(All,Acc));
tokens(<<"fast",Rest/binary>>,Acc) ->
	Fast = [
		#imap_fetch_cmd{name=flags,string="FLAGS"},
		#imap_fetch_cmd{name=internaldate,string="INTERNALDATE"},
		#imap_fetch_cmd{name='rfc822.size',string="RFC822.SIZE"}
		],
	tokens(Rest,lists:append(Fast,Acc));
tokens(<<"full",Rest/binary>>,Acc) ->
	Full = [
		#imap_fetch_cmd{name=flags,string="FLAGS"},
		#imap_fetch_cmd{name=internaldate,string="INTERNALDATE"},
		#imap_fetch_cmd{name='rfc822.size',string="RFC822.SIZE"},
		#imap_fetch_cmd{name=envelope,string="ENVELOPE"},
		#imap_fetch_cmd{name=body,string="BODY"}
		],
	tokens(Rest,lists:append(Full,Acc));
tokens(<<"body.peek[",Rest/binary>>,Acc) ->
	String = binary_to_list(Rest),
	Pos = string:chr(String,93),
	{Sec,New} = lists:split(Pos,String),
	{Next,Start,Count} = case regexp:match(New,"^<([0-9]+\.[0-9]+)>") of
		{match,1,Length} -> 
			{Size,New2} = lists:split(Length,New),
			[S,C] = string:tokens(Size,[46,60,62]),
			{New2,list_to_integer(S),list_to_integer(C)};
		nomatch -> {New,0,0}
	end,
	Sections = body_sections(Sec),
	BodyPeek = #imap_fetch_cmd{
				name = 'body.peek',
				sections = Sections,
				start = Start,
				count = Count,
				string = "BODY.PEEK[" ++ String},
	tokens(list_to_binary(Next),[BodyPeek|Acc]);
tokens(<<"body[",Rest/binary>>,Acc) ->
	String = binary_to_list(Rest),
	Pos = string:chr(String,93),
	{Sec,New} = lists:split(Pos,String),
	{Next,Start,Count} = case regexp:match(New,"^<([0-9]+\.[0-9]+)>") of
		{match,1,Length} -> 
			{Size,New2} = lists:split(Length,New),
			[S,C] = string:tokens(Size,[46,60,62]),
			{New2,list_to_integer(S),list_to_integer(C)};
		nomatch -> {New,0,0}
	end,
	Sections = body_sections(Sec),
	Body = #imap_fetch_cmd{
			name = 'body.peek',
			sections = Sections,
			start = Start,
			count = Count,
			string = "BODY.PEEK[" ++ String},
	tokens(list_to_binary(Next),[Body|Acc]);

tokens(<<"bodystructure",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name=bodystructure,string="BODYSTRUCTURE"}|Acc]);
tokens(<<"body",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name=body,string="BODY"}|Acc]);
tokens(<<"envelope",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name=envelope,string="ENVELOPE"}|Acc]);
tokens(<<"flags",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name=flags,string="FLAGS"}|Acc]);
tokens(<<"internaldate",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name=internaldate,string="INTERNALDATE"}|Acc]);
tokens(<<"rfc822.header",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name='rfc822.header',string="RFC822.HEADER"}|Acc]);
tokens(<<"rfc822.size",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name='rfc822.size',string="RFC822.SIZE"}|Acc]);
tokens(<<"rfc822.text",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name='rfc822.text',string="RFC822.TEXT"}|Acc]);
tokens(<<"rfc822",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name=rfc822,string="RFC822"}|Acc]);
tokens(<<"uid",Rest/binary>>,Acc) ->
	tokens(Rest,[#imap_fetch_cmd{name=uid,string="UID"}|Acc]);

tokens(Bin,_Acc) -> 
	?D(Bin),
	{error,unknown_token}.


%%-------------------------------------------------------------------------
%% @spec (String::string()) -> list()
%% @doc Takes a BODY or BODY.PEEK string and returns a list of the data items.
%%      The data Items are return as a list of lower case atoms, or tuples
%% @end
%%-------------------------------------------------------------------------
body_sections(String) -> 
	Bin = list_to_binary(string:strip(http_util:to_lower(String),right,93)),
	body_sections(Bin,[],[]).
%%-------------------------------------------------------------------------
%% @spec (Bin::binary(),Part::list(),Acc::list()) -> list()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
body_sections(<<>>,_Part,Acc) -> lists:reverse(Acc);


body_sections(<<32,Rest/binary>>,Part,Acc) ->
	body_sections(Rest,Part,Acc);
body_sections(<<"header.fields.not",Rest/binary>>,Part,Acc) ->
	String = binary_to_list(Rest),
	Pos = string:chr(String,41),
	{F,Next} = lists:split(Pos,String),
	Fields = lists:map(fun(S) ->
		list_to_atom(http_util:to_lower(S))
		end,string:tokens(F,[32,40,41])),
	body_sections(list_to_binary(Next),[],[{'header.fields.not',Part,Fields}|Acc]);
body_sections(<<"header.fields",Rest/binary>>,Part,Acc) ->
	String = binary_to_list(Rest),
	Pos = string:chr(String,41),
	{F,Next} = lists:split(Pos,String),
	Fields = lists:map(fun(S) ->
		list_to_atom(http_util:to_lower(S))
		end,string:tokens(F,[32,40,41])),
	body_sections(list_to_binary(Next),[],[{'header.fields',Part,Fields}|Acc]);
body_sections(<<"header",Rest/binary>>,Part,Acc) ->
	body_sections(Rest,[],[{header,Part}|Acc]);
body_sections(<<"mime",Rest/binary>>,Part,Acc) ->
	body_sections(Rest,[],[{mime,Part}|Acc]);
body_sections(<<"text",Rest/binary>>,Part,Acc) ->
	body_sections(Rest,[],[{text,Part}|Acc]);

body_sections(Bin,_Part,Acc) ->
	String = binary_to_list(Bin),
	RegExp = "^([0-9]\.)+",
	case regexp:match(String,RegExp) of
		{match,1,Length} -> 
			{P,Next} = lists:split(Length,String),
			Part = lists:map(fun(S) ->
				list_to_integer(S)
				end,string:tokens(P,[46])),
			body_sections(list_to_binary(Next),Part,Acc);
		
		nomatch -> 
			?D(Bin),
			{error,unknown_body_section}
	end.

value_or_nil(Value) -> value_or_nil(Value,false).
value_or_nil([],_) -> "NIL";
value_or_nil(Value,Boolean) -> imapd_util:quote(Value,Boolean).