Nonterminals 
	form
	address_list address addr_name addr_adl addr_mailbox addr_host
	body body_ext_1part body_ext_mpart body_type_1part body_type_text 
	body_fields body_fld_lines body_fld_md5 body_fld_desc body_fld_dsp body_fld_enc body_fld_id body_fld_lang body_fld_loc body_fld_octets body_fld_param body_fld_param_list
	body_type_mpart body_type_basic
	capability capability_data capability_data_list cap_string
	continue_req
	envelope env_from env_sender env_reply_to env_to env_cc env_bcc env_in_reply_to env_message_id
	flag_list flag
	greeting
	mailbox_data mailbox_list mbx_list_flags mbx_list_flag mailbox
	message_data msg_att_list msg_att msg_att_dynamic msg_att_static
	media_subtype media_text
	nstring numbers
	response response_data response_done resp_text resp_text_code resp_text_code_list
	status_att status_att_list
	.
Terminals 
	string integer nil encoding media_type_str media_subtype_str
	response_code command
	'(' ')' '[' ']' '{' '}' '<' '>'
	.
Rootsymbol form.

form -> response : '$1'.


address_list -> nil : nil.
address_list -> '(' address ')' : 
	case '$2' of
		A when is_record(A,address) -> [A];
		L when is_list(L) -> L
	end.

address -> '(' addr_name addr_adl addr_mailbox addr_host ')' : 
	#address{addr_name='$2',addr_adl='$3',addr_mailbox='$4',addr_host='$5'}.
address -> '(' addr_name addr_adl addr_mailbox addr_host ')' address : 
	[#address{addr_name='$2',addr_adl='$3',addr_mailbox='$4',addr_host='$5'},'$7'].

addr_adl     -> nstring : '$1'.
addr_host    -> nstring : '$1'.
addr_mailbox -> nstring : '$1'.
addr_name    -> nstring : '$1'.

body -> '(' body_type_1part ')' : '$2'.
body -> '(' body_type_mpart ')' : 
	case '$2' of
		Part when is_record(Part,part) -> #body{type=basic,parts=Part};
		List when is_list(List) -> 
			case clean_parts(List) of
				{Parts,Type,[]} -> #body{type=Type,parts=Parts};
				{Parts,Type,{MD5,Dsp,Lang,Loc}} -> #body{type=Type,parts=Parts,md5=MD5,dsp=Dsp,lang=Lang,loc=Loc}
			end; 
		Other -> Other
	end.

body_ext_1part -> body_fld_md5 : {'$1',[],[],[]}.
body_ext_1part -> body_fld_md5 body_fld_dsp : {'$1','$2',[],[]}.
body_ext_1part -> body_fld_md5 body_fld_dsp body_fld_lang : {'$1','$2','$3',[]}.
body_ext_1part -> body_fld_md5 body_fld_dsp body_fld_lang body_fld_loc : {'$1','$2','$3','$4'}.

body_ext_mpart -> body_fld_param_list : {'$1',[],[],[]}.
body_ext_mpart -> body_fld_param_list body_fld_dsp : {'$1','$2',[],[]}.
body_ext_mpart -> body_fld_param_list body_fld_dsp body_fld_lang : {'$1','$2','$3',[]}.
body_ext_mpart -> body_fld_param_list body_fld_dsp body_fld_lang body_fld_loc : {'$1','$2','$3','$4'}.


body_fields -> body_fld_param_list body_fld_id body_fld_desc body_fld_enc body_fld_octets : {'$1','$2','$3','$4','$5'}.

body_fld_desc -> nstring : '$1'.
body_fld_dsp -> '(' string body_fld_param_list ')' : {v('$2'),'$3'}.
body_fld_dsp -> nil : nil.
body_fld_enc -> encoding : v('$1').
body_fld_id -> nstring : '$1'.
body_fld_lang -> nstring : '$1'.
body_fld_loc -> nstring : '$1'.
body_fld_lines -> integer : value_of('$1').
body_fld_md5 -> nstring : '$1'.
body_fld_octets -> integer : value_of('$1').

body_fld_param_list -> nil : nil.
body_fld_param_list -> '(' nil ')' : nil.
body_fld_param_list -> '(' body_fld_param ')' : clean_param('$2').

body_fld_param -> string : [value_of('$1')].
body_fld_param -> string body_fld_param : [value_of('$1')|'$2'].

body_type_1part -> body_type_text : '$1'.
body_type_1part -> body_type_basic : '$1'.
body_type_1part -> body_type_text body_ext_1part : 
	{MD5,Dsp,Lang,Loc} = '$2',
	'$1'#part{md5=MD5,dsp=Dsp,lang=Lang,loc=Loc}.
body_type_1part -> body_type_basic body_ext_1part : 
	{MD5,Dsp,Lang,Loc} = '$2',
	'$1'#part{md5=MD5,dsp=Dsp,lang=Lang,loc=Loc}.

body_type_basic -> media_text body_fields :
	{Type,SubType} = '$1',
	{Params,Id,Desc,Enc,Octets} = '$2',
	#part{type = Type,subtype = SubType,params = Params,id = Id,desc = Desc,encoding = Enc,octets = Octets}.

body_type_mpart -> body media_subtype : {'$1','$2'}.
body_type_mpart -> body media_subtype body_ext_mpart : {'$1','$2','$3'}.

body_type_mpart -> body body_type_mpart : ['$1','$2'].


body_type_text -> media_text body_fields body_fld_lines : 
	{Type,SubType} = '$1',
	{Params,Id,Desc,Enc,Octets} = '$2',
	#part{type = Type,subtype = SubType,params = Params,id = Id,desc = Desc,encoding = Enc,octets = Octets,lines = '$3'}.





cap_string -> string : '$1'.
cap_string -> command : '$1'.

capability -> cap_string : [value_of('$1')].
capability -> cap_string capability : [value_of('$1') | '$2'].

capability_data -> string command capability : 
	#imap_resp{tag='*',data='$3',cmd=capability}.	

capability_data_list -> '[' command capability ']' : {v('$2'),'$3'}.



continue_req -> string resp_text : #imap_resp{tag=v('$1'),status=v('$2')}.


envelope -> '(' string string env_from env_sender env_reply_to env_to env_cc env_bcc env_in_reply_to env_message_id ')' : 
	#envelope{date = value_of('$2'), subject = value_of('$3'), from = '$4', sender = '$5', reply_to = '$6', to = '$7', cc = '$8', bcc = '$9', in_reply_to = '$10', message_id = '$11'}.
envelope -> '(' string nil env_from env_sender env_reply_to env_to env_cc env_bcc env_in_reply_to env_message_id ')' : 
	#envelope{date = value_of('$2'), subject = nil, from = '$4', sender = '$5', reply_to = '$6', to = '$7', cc = '$8', bcc = '$9', in_reply_to = '$10', message_id = '$11'}.
envelope -> '(' nil string env_from env_sender env_reply_to env_to env_cc env_bcc env_in_reply_to env_message_id ')' : 
	#envelope{date = nil, subject = value_of('$3'), from = '$4', sender = '$5', reply_to = '$6', to = '$7', cc = '$8', bcc = '$9', in_reply_to = '$10', message_id = '$11'}.

env_bcc -> address_list : '$1'.
env_cc -> address_list : '$1'.
env_from -> address_list : '$1'.
env_in_reply_to -> nstring : '$1'.
env_message_id -> nstring : '$1'.
env_reply_to -> address_list : '$1'.
env_sender -> address_list : '$1'.
env_to -> address_list : '$1'.




flag_list -> '(' ')' : [].
flag_list -> '(' flag ')' : 
	if
		is_list('$2') -> lists:flatten('$2');
		true -> ['$2']
	end.

flag -> string : v('$1').
flag -> string flag : [v('$1'),'$2'].

greeting -> string response_code capability_data_list resp_text : 
	{Cmd,Data} = '$3',
	#imap_resp{tag=v('$1'),status=v('$2'),cmd=Cmd,data=Data,info=lists:flatten('$4')}.



mailbox -> string : value_of('$1').


mailbox_data -> command mailbox status_att_list :
	#imap_resp{cmd=v('$1'),data='$3',info='$2'}.
mailbox_data -> command mailbox_list : 
	#imap_resp{cmd=v('$1'),data='$2'}.
mailbox_data -> command : 
	#imap_resp{cmd=v('$1'),data=[]}.
mailbox_data -> command numbers : 
	#imap_resp{cmd=v('$1'),data=lists:flatten('$2')}.
mailbox_data -> integer string : 
	#imap_resp{cmd=v('$2'),data=value_of('$1')}.
mailbox_data -> string flag_list : 
	#imap_resp{data='$2',cmd=v('$1')}.

mailbox_list -> mbx_list_flags string mailbox : 
	#folder{name='$3',delim=value_of('$2'),flags='$1'}.

media_subtype -> media_subtype_str : v('$1').
media_subtype -> string : v('$1').
media_text -> media_type_str media_subtype : {v('$1'),'$2'}.



message_data -> integer command : {value_of('$1'),v('$2')}.
message_data -> integer command msg_att_list : 
	case '$3' of
		{_,Flags} -> #fetch{flags=Flags,seqnum=value_of('$1')};
		_ -> '$3'#fetch{seqnum=value_of('$1')}
	end.



msg_att_list -> nil : nil.
msg_att_list -> '(' msg_att ')' : build_fetch('$2').

msg_att -> msg_att_dynamic : '$1'.
msg_att -> msg_att_static : '$1'.
msg_att -> msg_att msg_att_dynamic : ['$1','$2'].
msg_att -> msg_att msg_att_static  : ['$1','$2'].


msg_att_dynamic -> string flag_list : {v('$1'),'$2'}.

msg_att_static -> string string : {v('$1'),value_of('$2')}.
msg_att_static -> string integer : {v('$1'),value_of('$2')}.
msg_att_static -> command integer : {v('$1'),value_of('$2')}.
msg_att_static -> string envelope : {v('$1'),'$2'}.
msg_att_static -> string body : {v('$1'),'$2'}.
msg_att_static -> string '{' integer '}' string : {v('$1'),value_of('$5')}.
msg_att_static -> string '[' string ']' '{' integer '}' string : {v('$1'),value_of('$8')}.
msg_att_static -> string '[' string ']' '<' integer '>' '{' integer '}' string : {v('$1'),value_of('$11')}.
msg_att_static -> string '[' integer ']' '{' integer '}' string : {v('$1'),value_of('$8')}.
msg_att_static -> string '[' integer ']' '<' integer '>' '{' integer '}' string : {v('$1'),value_of('$11')}.

nstring -> string : value_of('$1').
nstring -> nil : nil.
	
numbers -> integer : [value_of('$1')].
numbers -> integer numbers : [value_of('$1') | '$2'].

mbx_list_flags -> '(' ')' : [].
mbx_list_flags -> '(' mbx_list_flag ')' : '$2'.

mbx_list_flag -> string : v('$1').
mbx_list_flag -> string mbx_list_flag : [v('$1'), '$2'].


response -> greeting : '$1'.
response -> response_data : '$1'.
response -> capability_data : '$1'.
response -> response_done : '$1'.
response -> continue_req : '$1'.


response_data -> string response_code resp_text : 
	#imap_resp{tag=v('$1'),info=lists:flatten('$3'),status=v('$2')}.	
response_data -> string response_code resp_text_code_list: 
	#imap_resp{tag=v('$1'),status=v('$2'),code=clean_resp_code('$3')}.
response_data -> string response_code resp_text_code_list resp_text : 
	#imap_resp{tag=v('$1'),status=v('$2'),code=clean_resp_code('$3'),info=lists:flatten('$4')}.
response_data -> string mailbox_data : '$2'#imap_resp{tag=v('$1')}.
response_data -> string message_data : 
	case '$2' of
		Fetch when is_record(Fetch,fetch) -> #imap_resp{tag=v('$1'),data='$2'};
		{Data,Cmd} -> #imap_resp{tag=v('$1'),data=Data,cmd=Cmd};
		_ -> '$2'
	end.


response_done -> string response_code command resp_text : 
	#imap_resp{tag=v('$1'),status=v('$2'),info='$4',cmd=v('$3')}.
response_done -> string response_code resp_text_code_list command resp_text : 
	Cmd = v('$4'),
	if
		Cmd == append -> #imap_resp{tag=v('$1'),status=v('$2'),info='$5',cmd=Cmd};
		Cmd == copy -> #imap_resp{tag=v('$1'),status=v('$2'),info='$5',cmd=Cmd};
		true -> #imap_resp{tag=v('$1'),status=v('$2'),code=clean_resp_code('$3'),info='$5',cmd=Cmd}
	end.

resp_text_code_list  -> '[' ']' : [].
resp_text_code_list -> '[' resp_text_code ']' : '$2'.
resp_text_code -> string : v('$1').
resp_text_code -> integer : integer_to_list(v('$1')).
resp_text_code -> flag_list : '$1'.
resp_text_code -> string resp_text_code : [v('$1'), '$2'].


resp_text -> string : value_of('$1').
resp_text -> response_code : value_of('$1').
resp_text -> string resp_text : [value_of('$1'),[32],'$2'].



status_att_list -> '(' ')' : [].
status_att_list -> '(' status_att ')' : lists:flatten('$2').

status_att -> string integer : {v('$1'),value_of('$2')}.
status_att -> string integer status_att : [{v('$1'),value_of('$2')},'$3'].

Erlang code.
-include("imap.hrl").


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



