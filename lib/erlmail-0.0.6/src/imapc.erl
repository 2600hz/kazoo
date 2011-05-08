%%%---------------------------------------------------------------------------------------
%%% @author     Stuart Jackson <simpleenigma@gmail.com> [http://erlsoft.org]
%%% @copyright  2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc        IMAP Client API
%%% @reference  See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference  See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version    0.0.6
%%% @since      0.0.1
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
-module(imapc).
-author('simpleenigma@gmail.com').
-include("../include/erlmail.hrl").
-include("../include/imap.hrl").



-export([append/3,append/4,append/5,capability/1,capability/2]).
-export([check/1,check/2,connect/1,connect/2,close/1,close/2]).
-export([copy/3,copy/4]).
-export([login/3,logout/1,noop/1,noop/2,authenticate/2]).
-export([select/1,select/2,examine/1,examine/2]).
-export([create/2,delete/2,subscribe/2,unsubscribe/2,rename/3,list/3,lsub/3]).
-export([store/4,uid/3]).
-export([switch/2,move/3,expunge/1,status/3,status/2,search/2,fetch/3,sort/3,sort/4]).
-export([deletef/2,undeletef/2,draft/2,undraft/2,flag/2,unflag/2,seen/2,unseen/2]).
-export([info/1,info/2]).
-export([build/0,build/1]).

%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Mailbox::string(),Message::string()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends APPEND command to FSM: APPENDs Message to Mailbox. Tag is automaically generated
%% @end
%%-------------------------------------------------------------------------
append(Pid,Mailbox,Message) -> append(Pid,Mailbox,Message,[],imapc_util:tag()).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Mailbox::string(),Message::string(),Flags::list()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends APPEND command to FSM: APPENDs Message to Mailbox. Tag is automaically generated
%% @end
%%-------------------------------------------------------------------------
append(Pid,Mailbox,Message,[Atom|_Rest] = Flags) when is_atom(Atom) -> append(Pid,Mailbox,Message,Flags,imapc_util:tag());
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Mailbox::string(),Message::string(),Tag::sring()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends APPEND command to FSM: APPENDs Message to Mailbox. Flags is blank
%% @end
%%-------------------------------------------------------------------------
append(Pid,Mailbox,Message,Tag) -> append(Pid,Mailbox,Message,[],Tag).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Mailbox::string(),Message::string(),Flags::list(),Tag::string) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends APPEND command to FSM: APPENDs Message to Mailbox.
%% @end
%%-------------------------------------------------------------------------
append(Pid,Mailbox,Message,Flags,Tag) ->
	gen_fsm:sync_send_event(Pid,{append,Tag,Mailbox,Message,imapc_util:build_flags(Flags)}).

%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Method::string()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends AUTHENTICATE command to FSM. Tag is automaically generated
%% @end
%%-------------------------------------------------------------------------
authenticate(Pid,Method)     -> authenticate(Pid,Method,imapc_util:tag()).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Method::string(),Tag::string()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends AUTHENTICATE command to FSM.
%% @end
%%-------------------------------------------------------------------------
authenticate(Pid,Method,Tag) -> gen_fsm:sync_send_event(Pid,{authenticate,Tag,Method}).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends CAPABILITY command to FSM. Tag is automaically generated
%% @end
%%-------------------------------------------------------------------------
capability(Pid)     -> capability(Pid,imapc_util:tag()).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Tag::string()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends CAPABILITY command to FSM.
%% @end
%%-------------------------------------------------------------------------
capability(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{capability,Tag}).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends CHECK command to FSM. Tag is automaically generated
%% @end
%%-------------------------------------------------------------------------
check(Pid)     -> check(Pid,imapc_util:tag()).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Tag::string()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends CHECK command to FSM.
%% @end
%%-------------------------------------------------------------------------
check(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{check,Tag}).

%%-------------------------------------------------------------------------
%% @spec (Pid::pid()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends CLOSE command to FSM. Tag is automaically generated
%% @end
%%-------------------------------------------------------------------------
close(Pid)     -> close(Pid,imapc_util:tag()).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Tag::string()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends CLOSE command to FSM.
%% @end
%%-------------------------------------------------------------------------
close(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{close,Tag}).

%%-------------------------------------------------------------------------
%% @spec (IpAddress::tuple()) -> {ok,Fsm::pid()} | {error,Reason::atom()}
%% @doc  Connect to an IMAP server and initate a FSM. Default Port 143
%% @end
%%-------------------------------------------------------------------------
connect(IPAddress) -> connect(IPAddress,143).
%%-------------------------------------------------------------------------
%% @spec (IpAddress::tuple(),Port::integer()) -> {ok,Fsm::pid()} | {error,Reason::atom()}
%% @doc  Connect to an IMAP server and initate a FSM.
%% @end
%%-------------------------------------------------------------------------
connect(IPAddress,Port) -> imapc_fsm:start_link(IPAddress,Port).

%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Set::string(),DestMailbox::string()) -> {ok,Fsm::pid()} | {error,Reason::atom()}
%% @doc  Copy messages in Set from selcted Mailbox to Destination Mailbox. Tag is automatically generated.
%% @end
%%-------------------------------------------------------------------------
copy(Pid,Set,MailBox) -> copy(Pid,Set,MailBox,imapc_util:tag()).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Set::string(),DestMailbox::string(),Tag::string()) -> {ok,Fsm::pid()} | {error,Reason::atom()}
%% @doc  Copy messages in Set from selcted Mailbox to Destination Mailbox.
%% @end
%%-------------------------------------------------------------------------
copy(Pid,Set,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{copy,Tag,imapc_util:to_seq(Set),MailBox}).


































%%-------------------------------------------------------------------------
%% @spec (Pid::pid()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends NOOP command to FSM: No Operation. Tag is automaically generated
%% @end
%%-------------------------------------------------------------------------
noop(Pid)     -> noop(Pid,imapc_util:tag()).
%%-------------------------------------------------------------------------
%% @spec (Pid::pid(),Tag::string()) -> {ok,Reason::atom()} | {error,Reason::atom()}
%% @doc  Sends NOOP command to FSM: No Operation
%% @end
%%-------------------------------------------------------------------------
noop(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{noop,Tag}).


%%--------------------------------------------------------------------
%% Function: logout(Pid)
%%           Pid = pid()
%% Descrip.: Sends logout command to FSM, closes connection
%% Returns : ok
%%--------------------------------------------------------------------

logout(Pid)     -> logout(Pid,imapc_util:tag()).
logout(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{logout,Tag}).


%%--------------------------------------------------------------------
%% Function: login(Pid,UserName,Password)
%%           Pid      = pid()
%%           UserName = string()
%%           Password = string()
%% Descrip.: Sends login command to FSM, changes from not_authenticated to authtneicated state
%% Returns : ok
%%--------------------------------------------------------------------

login(Pid,UserName,Password)     -> login(Pid,UserName,Password,imapc_util:tag()).
login(Pid,UserName,Password,Tag) -> gen_fsm:sync_send_event(Pid,{login,Tag,UserName,Password}).



%%--------------------------------------------------------------------
%% Function: select(Pid)
%%           select(Pid,MailBox)
%%           Pid     = pid()
%%           MailBox = string(), Default "INBOX"
%% Descrip.: Sends select command to select specified mailbox name
%% Returns : ok
%%--------------------------------------------------------------------

select(Pid) -> select(Pid,"INBOX").
select(Pid,MailBox) -> select(Pid,MailBox,imapc_util:tag()).
select(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{select,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: examine(Pid)
%%           examine(Pid,MailBox)
%%           Pid     = pid()
%%           MailBox = string(), Default "INBOX"
%% Descrip.: Sends examine command to select (READ-ONLY) specified mailbox name
%% Returns : ok
%%--------------------------------------------------------------------

examine(Pid) -> examine(Pid,"INBOX").
examine(Pid,MailBox) -> examine(Pid,MailBox,imapc_util:tag()).
examine(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{examine,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: create(Pid,Mailbox)
%%           Pid     = pid()
%%           Mailbox = string()
%% Descrip.: Creates a mailbox in the authenticated state
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

create(Pid,MailBox) -> create(Pid,MailBox,imapc_util:tag()).
create(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{create,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: delete(Pid,Mailbox)
%%           Pid     = pid()
%%           Mailbox = string()
%% Descrip.: Deletes a mailbox in the authenticated state
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

delete(Pid,MailBox) -> delete(Pid,MailBox,imapc_util:tag()).
delete(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{delete,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: rename(Pid,OrgMailBox,NewMailBox)
%%           Pid     = pid()
%%           MailBox = string()
%% Descrip.: RENAMEs first OrgMailBOx to second NewMailBox
%% Returns : ok
%%--------------------------------------------------------------------

rename(Pid,OrgMailBox,NewMailBox) -> rename(Pid,OrgMailBox,NewMailBox,imapc_util:tag()).
rename(Pid,OrgMailBox,NewMailBox,Tag) ->  gen_fsm:sync_send_event(Pid,{rename,Tag,OrgMailBox,NewMailBox}).

%%--------------------------------------------------------------------
%% Function: subscribe(Pid,Mailbox)
%%           Pid     = pid()
%%           Mailbox = string()
%% Descrip.: Subscribes to a mailbox in the authenticated state
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

subscribe(Pid,MailBox) -> subscribe(Pid,MailBox,imapc_util:tag()).
subscribe(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{subscribe,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: unsubscribe(Pid,Mailbox)
%%           Pid     = pid()
%%           Mailbox = string()
%% Descrip.: Unsubscribes to a mailbox in the authenticated state
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

unsubscribe(Pid,MailBox) -> unsubscribe(Pid,MailBox,imapc_util:tag()).
unsubscribe(Pid,MailBox,Tag) -> gen_fsm:sync_send_event(Pid,{unsubscribe,Tag,MailBox}).

%%--------------------------------------------------------------------
%% Function: list(Pid,RefName,Mailbox)
%%           Pid     = pid()
%%           RefName = string()
%%           Mailbox = string()
%% Descrip.: lists mailboxes
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

list(Pid,RefName,Mailbox) -> list(Pid,RefName,Mailbox,imapc_util:tag()).
list(Pid,RefName,Mailbox,Tag) -> gen_fsm:sync_send_event(Pid,{list,Tag,imapd_util:quote(RefName,true),imapd_util:quote(Mailbox,true)}).

%%--------------------------------------------------------------------
%% Function: lsub(Pid,RefName,Mailbox)
%%           Pid     = pid()
%%           RefName = string()
%%           Mailbox = string()
%% Descrip.: Lists only subscribed mailboxes
%% Returns : {ok, Pid} | {error, Error}
%%--------------------------------------------------------------------

lsub(Pid,RefName,Mailbox) -> lsub(Pid,RefName,Mailbox,imapc_util:tag()).
lsub(Pid,RefName,Mailbox,Tag) -> gen_fsm:sync_send_event(Pid,{lsub,Tag,RefName,Mailbox}).

%%--------------------------------------------------------------------
%% Function: status(Pid,MailBox,StatusCodes)
%%           Pid         = pid()
%%           MailBox     = string()
%%           StatusCodes = term() - list of atoms [messages,recent,uidnext,uidvalidity,unseen]
%% Descrip.: Collects specified status codes
%% Returns : Status Information
%%--------------------------------------------------------------------

status(Pid,MailBox) -> status(Pid,MailBox,[messages,recent,uidnext,uidvalidity,unseen]).
status(Pid,MailBox,StatusCodes) -> status(Pid,MailBox,StatusCodes,imapc_util:tag()).
status(Pid,MailBox,StatusCodes,Tag) -> gen_fsm:sync_send_event(Pid,{status,Tag,MailBox,imapc_util:build_statuscodes(StatusCodes)}).





%%--------------------------------------------------------------------
%% Function: expunge(Pid)
%%           Pid     = pid()
%% Descrip.: perminately removes all messages with DELETED flag set
%% Returns : ok
%%--------------------------------------------------------------------

expunge(Pid)     -> expunge(Pid,imapc_util:tag()).
expunge(Pid,Tag) -> gen_fsm:sync_send_event(Pid,{expunge,Tag}).

%%--------------------------------------------------------------------
%% Function: search(Pid,Query)
%%           Pid   = pid()
%%           Query = Query Terms
%% Descrip.: Performs search query on selected mailbox
%% Returns : term(), list of integers
%%--------------------------------------------------------------------

search(Pid,Query) -> search(Pid,Query,imapc_util:tag()).
search(Pid,Query,Tag) -> gen_fsm:sync_send_event(Pid,{search,Tag,imapc_util:build_search(Query)}).



%%--------------------------------------------------------------------
%% Function: fetch(Pid,Set,Query)
%%           Pid   = pid()
%%           Set   = Integer or List of Integer
%%           Query = Fetch Query Terms
%% Descrip.: Performs search query on selected mailbox
%% Returns : term(), list of integers
%%--------------------------------------------------------------------

fetch(_Pid,[],_Query) -> [];
fetch(Pid,Set,Query) -> fetch(Pid,Set,Query,imapc_util:tag()).
fetch(Pid,Set,Query,Tag) -> 
	gen_fsm:sync_send_event(Pid,{fetch,Tag,imapc_util:to_seq(Set),imapc_util:build_fetch(Query)}).



%%--------------------------------------------------------------------
%% Function: store(Pid,Set,ItemName,Flags)
%%           Pid      = pid()
%%           Set      = Integer or List of Integer
%%           ItemName = Atom - one of [replace,add,remove,replace_silent,add_silent,remove_silent]
%%                      (delete is duplicate name for remove, including silent mode)
%%           Flags     = term() - List of atoms for flag names
%% Descrip.: Marks or unmarks all message in set with falgs from FlagList
%% Returns : ok
%%--------------------------------------------------------------------

store(_Pid,[],_ItemName,_Flags) -> [];
store(Pid,Set,ItemName,Flags) -> store(Pid,Set,ItemName,Flags,imapc_util:tag()).
store(Pid,Set,ItemName,Flags,Tag) -> 
	gen_fsm:sync_send_event(Pid,{store,Tag,imapd_util:list_to_seq(Set),imapc_util:build_store(ItemName),imapc_util:build_flags(Flags)}).



%%--------------------------------------------------------------------
%% Function: uid(Pid,Cmd,Opt)
%%           Pid      = pid()
%%           Set      = Integer or List of Integer
%%           ItemName = Atom - one of [replace,add,remove,replace_silent,add_silent,remove_silent]
%%                      (delete is duplicate name for remove, including silent mode)
%%           Flags     = term() - List of atoms for flag names
%% Descrip.: Marks or unmarks all message in set with DELETED flag
%% Returns : ok
%%--------------------------------------------------------------------

uid(Pid,fetch,{UIDSet,Query})          -> uid(Pid,fetch,{imapd_util:list_to_seq(UIDSet),imapc_util:build_fetch(Query)},imapc_util:tag());
uid(Pid,copy,{UIDSet,MailBox})         -> uid(Pid,copy, {imapd_util:list_to_seq(UIDSet),MailBox},imapc_util:tag());
uid(Pid,store,{UIDSet,ItemName,Flags}) -> uid(Pid,store,{imapd_util:list_to_seq(UIDSet),imapc_util:build_store(ItemName),imapc_util:build_flags(Flags)},imapc_util:tag());
uid(Pid,search,Query)                  -> uid(Pid,search,imapc_util:build_search(Query),imapc_util:tag()).

uid(Pid,Command,Args,Tag) -> gen_fsm:sync_send_event(Pid,{uid,Tag,Command,Args}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



























%%--------------------------------------------------------------------
%% Function: switch(Pid,MailBox)
%%           Pid     = pid()
%%           MailBox = string()
%% Descrip.: Closes any selected mailbox and selects specified mailbox
%% Returns : ok
%%--------------------------------------------------------------------

switch(Pid,MailBox) ->
	case info(Pid,state) of
		selected -> 
			Select = info(Pid,mailbox),
			if
				MailBox == Select -> ok;
				true -> 
					close(Pid),
					select(Pid,MailBox)
			end;
		_ -> select(Pid,MailBox)
	end.



%%--------------------------------------------------------------------
%% Function: move(Pid,MailBox,StatusCodes)
%%           Pid     = pid()
%%           Set     = list() of integer90
%%           MailBox = string()
%% Descrip.: Copies and deletes message set from selected mailbox to specified mailbox
%% Returns : ok
%%--------------------------------------------------------------------

move(Pid,Set,MailBox) ->
	copy(Pid,imapc_util:to_seq(Set),MailBox),
	delete(Pid,imapc_util:to_seq(Set)),
	ok.





%%--------------------------------------------------------------------
%% Function: sort(Pid,Order,Query,Charset)
%%           Pid     = pid()
%%           Order   = Order Terms
%%           Query   = Search Terms
%%           Charset = Charset (optional: default UTF-8)
%% Descrip.: Performs an ordered search query on selected mailbox
%% Returns : term(), list of integers
%%--------------------------------------------------------------------

sort(Pid,Order,Query) -> sort(Pid,Order,Query,"UTF-8"). 
sort(Pid,Order,Query,Charset) -> 
	Capability = info(Pid,capability),
	SortCap = lists:member("SORT",Capability),
	if
		SortCap -> imapc_fsm:sort(Pid,imapc_util:build_order(Order),imapc_util:build_search(Query),Charset);
		true    -> imapc_fsm:search(Pid,imapc_util:build_search(Query))
	end.



%%--------------------------------------------------------------------
%% Function: (un)deletef(Pid) 
%%           Pid     = pid()
%%           Set     = list() of integer()
%% Descrip.: Marks or unmarks all message in set with DELETED flag
%% Returns : ok
%%--------------------------------------------------------------------

deletef(Pid,Set)   -> store(Pid,Set,add_silent,[deleted]).
undeletef(Pid,Set) -> store(Pid,Set,remove_silent,[deleted]).

%%--------------------------------------------------------------------
%% Function: (un)draft(Pid)
%%           Pid     = pid()
%%           Set     = list() of integer()
%% Descrip.: Marks or unmarks all message in set with DRAFT flag
%% Returns : ok
%%--------------------------------------------------------------------

draft(Pid,Set)   -> store(Pid,Set,add_silent,[draft]).
undraft(Pid,Set) -> store(Pid,Set,remove_silent,[draft]).

%%--------------------------------------------------------------------
%% Function: (un)flag(Pid)
%%           Pid     = pid()
%%           Set     = list() of integer()
%% Descrip.: Marks or unmarks all message in set with FLAGGED flag
%% Returns : ok
%%--------------------------------------------------------------------

flag(Pid,Set)   -> store(Pid,Set,add_silent,[flagged]).
unflag(Pid,Set) -> store(Pid,Set,remove_silent,[flagged]).

%%--------------------------------------------------------------------
%% Function: (un)seen(Pid)
%%           Pid     = pid()
%%           Set     = list() of integer()
%% Descrip.: Marks or unmarks all message in set with SEEN flag
%% Returns : ok
%%--------------------------------------------------------------------

seen(Pid,Set)   -> store(Pid,Set,add_silent,[seen]).
unseen(Pid,Set) -> store(Pid,Set,remove_silent,[seen]).



%%--------------------------------------------------------------------
%% Function: info(Pid)
%%           info(Pid,Type)
%%           Pid  = pid()
%%           Type = atom(), Default all, options [all,state,mailbox,capability]
%% Descrip.: Returns current information ont eh state of the FSM
%% Returns : term()
%%--------------------------------------------------------------------
info(Pid) -> info(Pid,all).
info(Pid,Type) -> imapc_fsm:info(Pid,Type).

%%--------------------------------------------------------------------
%% Function: build(Type)
%%           Type = atom(), Default both, options [both,scanner,parser]
%% Descrip.: Returns current information ont eh state of the FSM
%% Returns : term() = results from yacc, leex or both
%%--------------------------------------------------------------------
-define(DEV_PATH,"/sfe/erlang/releases/lib/erlmail-" ++ ?ERLMAIL_VERSION).

%%-------------------------------------------------------------------------
%% @spec () -> term()
%% @doc  Build LEEX and YECC grammar files for IMAP client
%% @end
%%-------------------------------------------------------------------------
build() -> build(both).
%%-------------------------------------------------------------------------
%% @spec (Type::[parser | scanner | both]) -> Response::term()
%% @doc  Build LEEX and YECC grammar files for IMAP client
%% @end
%%-------------------------------------------------------------------------
build(parser)  -> yecc:file(?DEV_PATH ++ "/src/imap_parser.yrl",[{verbose,true}]);
build(scanner) -> leex:file(?DEV_PATH ++ "/src/imap_scan.xrl");
build(yecc)    -> yecc:file(?DEV_PATH ++ "/src/imap_parser.yrl",[{verbose,true}]);
build(leex)    -> leex:file(?DEV_PATH ++ "/src/imap_scan.xrl");
build(both)    -> 
	P = build(yecc),
	S = build(leex),
	{P,S}.