%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP server command processing
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
-module(imapd_cmd).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").
-include("../include/erlmail.hrl").

-export([command/1,command/2]).


%%-------------------------------------------------------------------------
%% @spec (State::imapd_fsm()) -> NewState::imapd_fsm()
%% @doc  Processes all IMAP commands and checks for extention processing
%% @end
%%-------------------------------------------------------------------------
command(#imapd_fsm{line = Line} = State) -> 
	Command = imapd_util:parse(Line,State),
	io:format("Command: ~p~n",[Command]),
	command(Command,State).


%%-------------------------------------------------------------------------
%% @spec (Comamnd::imap_cmd(),State::imapd_fsm()) -> NewState::imapd_fsm()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
%%%-------------------------
%%% CAPABILITY - Any State
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = capability = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	imapd_resp:respond([#imap_resp{tag = '*', cmd = capability, info = imapd_ext:capability()},
					  #imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = capability = Command, data = Data}, State) -> 
	imapd_util:out(Command, Data, State),
	imapd_resp:insert(#imap_resp{tag = Tag, status = bad}),
	imapd_resp:send(Tag,State),
	State;

%%%-------------------------
%%% NOOP - Any State
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = noop = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = noop = Command, data = Data}, State) -> 
	imapd_util:out(Command,Data,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;

%%%-------------------------
%%% LOGOUT - Any State
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = logout = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	imapd_resp:respond([#imap_resp{tag = '*', status = bye, info = "ErlMail terminating connection"},
						#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State),
	erlmail_store:close(),
	gen_tcp:close(State#imapd_fsm.socket),
	gen_fsm:send_all_state_event(self(),stop),
	State;
command(#imap_cmd{tag = Tag, cmd = logout = Command, data = Data}, State) -> 
	imapd_util:out(Command, Data, State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;

%%%-------------------------
%%% STARTTLS - Not Authenticated
%%%-------------------------

% @todo STARTTLS impliment command


%%%-------------------------
%%% AUTHENTICATE - Not Authenticated
%%%-------------------------

% @todo AUTHENTICATE impliment command


%%%-------------------------
%%% LOGIN - Not Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = login = Command, data = []},  State) -> 
	imapd_util:out(Command, State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = login = Command, data = {EmailAddress,Password}}, 
	    #imapd_fsm{state = not_authenticated, options = Options} = State) -> 
	case lists:keysearch(logindisabled, 1, Options) of
		{value, {logindisabled,true}} ->
			imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
			State;
		_ ->
			imapd_util:out(Command,EmailAddress,State),
			case erlmail_store:select(erlmail_util:split_email(EmailAddress)) of
				#user{password = Password} = User when Password /= [] -> 
					imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State),
					State#imapd_fsm{state = authenticated, user = User};
				_ -> 
					imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
					State
			end
	end;
command(#imap_cmd{tag = Tag, cmd = login = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= selected; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;

%%%-------------------------
%%% SELECT - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = select = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = select = Command}, 
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command, State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = select = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command, MailBoxName, State),
	case erlmail_store:select({MailBoxName, User#user.name}) of
		MailBoxStore when is_record(MailBoxStore, mailbox_store) ->
			% @todo: open mailbox in mailbox store or mark as active for this client. Set RW = true
			MailBox = imapd_util:mailbox_info(MailBoxStore),
			% @todo Clean up Flag processing - figure out where to store data
			% @todo Clean up PermanentFlag processing - figure out where to store data
			RespList = 
				[#imap_resp{tag = '*', status = MailBox#mailbox.exists, cmd = exists},
				 #imap_resp{tag = '*', status = MailBox#mailbox.recent, cmd = recent},
				 #imap_resp{tag = '*', cmd = flags, data = {flags,MailBox#mailbox.flags}},
				 #imap_resp{tag = '*', status = ok, code = {unseen,MailBox#mailbox.unseen}},
				 #imap_resp{tag = '*', status = ok, code = {uidvalidity,MailBoxStore#mailbox_store.uidvalidity}},
				 #imap_resp{tag = '*', status = ok, code = {uidnext,MailBoxStore#mailbox_store.uidnext}},
				 #imap_resp{tag = '*', status = ok, code = {permanentflags,[answered,flagged,draft,deleted,seen,'*']}},
				 #imap_resp{tag = Tag, status = ok, code = 'read-write', cmd = Command, info = "Completed"}
				],
			imapd_resp:respond(RespList,Tag,State),
			State#imapd_fsm{state = selected, mailbox = MailBoxStore,mailbox_rw = true};
		_ ->
			imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
			State#imapd_fsm{state = authenticated, mailbox = [], mailbox_rw = false}
	end;
%%%-------------------------
%%% EXAMINE - Authenticated
%%%-------------------------

command(#imap_cmd{tag = Tag, cmd = examine = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = examine = Command}, 
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command, State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = examine = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command, MailBoxName, State),
	case erlmail_store:select({MailBoxName, User#user.name}) of
		MailBoxStore when is_record(MailBoxStore, mailbox_store) ->
			MailBox = imapd_util:mailbox_info(MailBoxStore),
			% @todo Clean up Flag processing - figure out where to store data
			% @todo Clean up PermanentFlag processing - figure out where to store data
			RespList = 
				[#imap_resp{tag = '*', status = MailBox#mailbox.exists, cmd = exists},
				 #imap_resp{tag = '*', status = MailBox#mailbox.recent, cmd = recent},
				 #imap_resp{tag = '*', cmd = flags, data = {flags,MailBox#mailbox.flags}},
				 #imap_resp{tag = '*', status = ok, code = {unseen,MailBox#mailbox.unseen}},
				 #imap_resp{tag = '*', status = ok, code = {uidvalidity,MailBoxStore#mailbox_store.uidvalidity}},
				 #imap_resp{tag = '*', status = ok, code = {uidnext,MailBoxStore#mailbox_store.uidnext}},
				 #imap_resp{tag = '*', status = ok, code = {permanentflags,[answered,flagged,draft,deleted,seen,'*']}},
				 #imap_resp{tag = Tag, status = ok, code = 'read-write', cmd = Command, info = "Completed"}
				],
			imapd_resp:respond(RespList,Tag,State),
			State#imapd_fsm{state = selected, mailbox = MailBoxStore,mailbox_rw = false};
		_ ->
			imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
			State#imapd_fsm{state = authenticated, mailbox = [], mailbox_rw = false}
	end;
%%%-------------------------
%%% CREATE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = create = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = create = Command},
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = create = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = LoggedIn} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	User = erlmail_store:select(LoggedIn),
	case erlmail_store:select({MailBoxName,User#user.name}) of
		[] -> 
			% @todo CREATE Check for and clear trailing hierarchy seprator
			% @todo CREATE parent mailboxes
			UIDValidity = case lists:keysearch({uidvalidity,MailBoxName},1,User#user.options) of
				{value,{{uidvalidity,MailBoxName},Value}} -> 
					erlmail_store:update(User#user{options = lists:keydelete({uidvalidity,MailBoxName},1,User#user.options)}),
					Value + 1;
				_ -> 0
			end,
			{UserName,DomainName} = User#user.name,
			erlmail_store:insert(#mailbox_store{name={MailBoxName,UserName,DomainName}, uidvalidity=UIDValidity}),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State);
		MailBoxStore when is_record(MailBoxStore,mailbox_store) -> 
			imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		_ -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State)
	end,
	NewUser = erlmail_store:select(User),
	State#imapd_fsm{user = NewUser};
%%%-------------------------
%%% DELETE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = delete = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = delete = Command},
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = delete = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	case erlmail_store:select({MailBoxName,User#user.name}) of
		[] ->  imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		#mailbox_store{name = {"INBOX",_,_}} -> 
			 imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		MailBoxStore when is_record(MailBoxStore,mailbox_store) -> 
			% @todo DELETE messages and cleanup
			% @todo DELETE check for \noselect flag; error
			% @todo DELETE check for sub folders; remove mail and set \noselect leave folder
			erlmail_store:delete(MailBoxStore),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State);
		_ -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State)
	end,
	State;

%%%-------------------------
%%% RENAME - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = rename = Command, data = {[],[]}}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = rename = Command, data = {_,[]}}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = rename = Command},
        #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = rename = Command, data = {Src,Dst}},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,{Src,Dst},State),
	SrcMB = erlmail_store:select({string:strip(Src),User#user.name}),
	DstMB = erlmail_store:select({string:strip(Dst),User#user.name}),
	case {SrcMB,DstMB} of
		{[],_} -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		{_,DstMB} when is_record(DstMB,mailbox_store) -> 
			imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		{SrcMB,[]} ->
			% @todo RENAME any sub folders
			% @todo RENAME create any parent folders
			% @todo RENAME maintain list of Max UID for renamed folders incase of recreation
			% @todo RENAME INBOX special case. Move Mail, but do not delete INBOX. Leave subfolders alone
			{_SrcName,UserName,DomainName} = SrcMB#mailbox_store.name,
			NewMB = SrcMB#mailbox_store{name = {Dst,UserName,DomainName}},
			erlmail_store:insert(NewMB),
			erlmail_store:delete(SrcMB),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State);
		_ -> 	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State)
	end,
	State;

%%%-------------------------
%%% SUBSCRIBE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = subscribe = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = subscribe = Command},
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = subscribe = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	case erlmail_store:select({MailBoxName,User#user.name}) of
		[] -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		MailBox when is_record(MailBox,mailbox_store) ->
			NewMailBox = MailBox#mailbox_store{subscribed = true},
			erlmail_store:update(NewMailBox),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State);
		_ -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State)
	end,
	State;
%%%-------------------------
%%% UNSUBSCRIBE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = unsubscribe = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = unsubscribe = Command},
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = unsubscribe = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	case erlmail_store:select({MailBoxName,User#user.name}) of
		[] -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		MailBox when is_record(MailBox,mailbox_store) ->
			NewMailBox = MailBox#mailbox_store{subscribed = false},
			erlmail_store:update(NewMailBox),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State);
		_ -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State)
	end,
	State;
%%%-------------------------
%%% LIST - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = list = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = list = Command},
		#imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = list = Command, data = {_Reference,MailBoxName}},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	% @todo incorparete reference name. Simplier not to use at first
	Heirachy = imapd_util:heirachy_char(),
	case erlmail_store:mlist(MailBoxName,User#user.name,false) of
		List when is_list(List) -> 
			RespList = lists:map(fun(Name) -> 
				Info = {Command,Heirachy,Name},
				Data = {Command,[]},
				#imap_resp{tag = '*', cmd = Command, data = Data, info = Info}
				end,List),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"} | RespList],Tag,State);
		undefined -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		_ -> imapd_resp:respond([#imap_resp{tag = Tag, status = bado}],Tag,State)
	end,
	State;


%%%-------------------------
%%% LSUB - Authenticated
%%%-------------------------

command(#imap_cmd{tag = Tag, cmd = lsub = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = lsub = Command},
		#imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = lsub = Command, data = {_Reference,MailBoxName}},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	% @todo incorparete reference name. Simplier not to use at first
	Heirachy = imapd_util:heirachy_char(),
	case erlmail_store:mlist(MailBoxName,User#user.name,true) of
		List when is_list(List) -> 
			RespList = lists:map(fun(Name) -> 
				Info = {Command,Heirachy,Name},
				Data = {Command,[]},
				#imap_resp{tag = '*', cmd = Command, data = Data, info = Info}
				end,List),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"} | RespList],Tag,State);
		undefined -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		_ -> imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State)
	end,
	State;

%%%-------------------------
%%% STATUS - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = status = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = status = Command},
		#imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = status = Command, data = {MailBoxName,Flags}},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	case erlmail_store:select({string:strip(MailBoxName),User#user.name}) of
		[] -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		MailBox when is_record(MailBox,mailbox_store) -> 
			StatusFlags = imapd_util:status_flags(Flags),
			MailBoxInfo = imapd_util:mailbox_info(MailBox,StatusFlags),
			StatusInfo  = imapd_util:status_info(MailBoxInfo,StatusFlags),
			% @todo Process each flag and build data to return
			imapd_resp:respond([#imap_resp{tag = '*', cmd = Command, data = {status,MailBoxName,StatusInfo}},
							    #imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State);
		_ -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State)
	end,
	State;

%%%-------------------------
%%% APPEND - Authenticated
%%%-------------------------

% @todo APPEND impliment command


%%%-------------------------
%%% CHECK - Selected
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = check = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = check = Command, data = []},#imapd_fsm{state = selected} = State) -> 
	imapd_util:out(Command,State),
	erlmail_store:check(),
	imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = check = Command}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;

%%%-------------------------
%%% CLOSE - Selected
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = close = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = close = Command, data = []},
		#imapd_fsm{state = selected, mailbox = Selected} = State) -> 
	imapd_util:out(Command,State),
	case State#imapd_fsm.mailbox_rw of
		true ->
			MailBox = erlmail_store:select(Selected),
			imapd_util:expunge(MailBox);
		false -> ok
	end,
	imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State),
	State#imapd_fsm{mailbox = [], mailbox_rw = false, state = authenticated};
command(#imap_cmd{tag = Tag, cmd = close = Command}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;

%%%-------------------------
%%% EXPUNGE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = expunge = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = expunge = Command, data = []}, #imapd_fsm{mailbox = Selected} = State) -> 
	imapd_util:out(Command,State),
	MailBox = erlmail_store:select(Selected),
	{NewMailBox,RespList} = imapd_util:expunge(MailBox),
	erlmail_store:update(NewMailBox),
	imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd= Command, info = "Completed"} | RespList],Tag,State),
	State#imapd_fsm{mailbox = NewMailBox};
command(#imap_cmd{tag = Tag, cmd = expunge = Command}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;

%%%-------------------------
%%% SEARCH - Authenticated
%%%-------------------------

% @todo SEARCH impliment command

%%%-------------------------
%%% FETCH - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = fetch = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = fetch = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = fetch = Command, data = {Seq,Items}}, State) -> 
	imapd_util:out(Command,State),
	MailBox = gen_store:select(State#imapd_fsm.mailbox,State),
	Messages = imapd_util:seq_message_names(Seq,MailBox),
	RespList = imapd_fetch:fetch(Messages,Items,State),	
	imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd= Command, info = "Completed"} | RespList],Tag,State),
	State;

% @todo FETCH impliment command

%%%-------------------------
%%% STORE - Selected
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = store = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = store = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = store = Command, data = {Seq,Action,Flags}},#imapd_fsm{state = selected, mailbox = Selected} = State) -> 
	imapd_util:out(Command,{Seq,Action,Flags},State),
	Current = erlmail_store:select(Selected),
	Messages = imapd_util:seq_message_names(Seq,Current),
	RespList = imapd_util:store(Messages,State#imapd_fsm{mailbox=Current},Action,Flags),
	imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"} | RespList],Tag,State),
	State#imapd_fsm{mailbox = Current};

%%%-------------------------
%%% COPY - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = copy = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = copy = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = copy = Command, data = {Seq,MailBoxName}},#imapd_fsm{state = selected, mailbox = Selected, user = User} = State) -> 
	imapd_util:out(Command,{Seq,MailBoxName},State),
	Current = erlmail_store:select(Selected),
	Dest = erlmail_store:select({MailBoxName,User#user.name}),
	case Dest of
		[] -> imapd_resp:respond([#imap_resp{tag = Tag, status = no}],Tag,State);
		Dest when is_record(Dest,mailbox_store) ->
			Messages = imapd_util:seq_message_names(Seq,Current),
			imapd_util:copy(Dest,Messages,State),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State)
	end,
	State#imapd_fsm{mailbox = Current};

%%%-------------------------
%%% UID - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = uid = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = uid = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
	State;
command(#imap_cmd{tag = Tag, cmd = uid = Command, data = {fetch,Seq,Data}},
	#imapd_fsm{state = selected, mailbox = Selected} = State) -> 
%	?D(Data),
	Items = case lists:keysearch(uid,2,Data) of
		{value,_} -> Data;
		_ -> lists:ukeysort(2,lists:append([#imap_fetch_cmd{name=uid,string="UID"}],Data))
	end,
	imapd_util:out(Command,{fetch,Seq,Items},State),
	MailBox = gen_store:select(Selected,State),
	Messages = imapd_util:uidseq_message_names(Seq,MailBox),
%	?D(Messages),
	RespList = imapd_fetch:fetch(Messages,Items,State),
	imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"} | RespList],Tag,State),
	State#imapd_fsm{mailbox=MailBox};
command(#imap_cmd{tag = Tag, cmd = uid = Command, data = {copy, UIDSeq, DestMailBox}},
	#imapd_fsm{state = selected, mailbox = Selected, user = User} = State) -> 
	imapd_util:out(Command,{copy, UIDSeq, DestMailBox},State),
	Current = erlmail_store:select(Selected),
	Dest = erlmail_store:select({DestMailBox,User#user.name}),
	case Dest of
		[] -> 
			imapd_resp:respond([#imap_resp{tag = Tag, status = no, code = trycreate}],Tag,State);
		Dest when is_record(Dest,mailbox_store) ->
			Messages = imapd_util:uidseq_message_names(UIDSeq,Current),
			imapd_util:copy(Dest,Messages,State),
			imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}],Tag,State)
	end,
	State#imapd_fsm{mailbox=Current};


command(#imap_cmd{tag = Tag, cmd = uid = Command, data = {store, UIDSeq, Action, Flags}},
	#imapd_fsm{state = selected, mailbox = Selected} = State) -> 
	imapd_util:out(Command,{store, UIDSeq, Action, Flags},State),
	Current = gen_store:select(Selected,State),
	Messages = imapd_util:uidseq_message_names(UIDSeq,Current),
	RespList = imapd_util:store(Messages,State#imapd_fsm{mailbox=Current},Action,Flags),
	imapd_resp:respond([#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"} | RespList],Tag,State),
	State#imapd_fsm{mailbox=Current};









%%%-------------------------
%%% CHECK EXTENTIONS OR CATCH ALL ERROR
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = Command, data = Data} = Cmd, State) ->
	case imapd_ext:check(Command) of
		{ok,Module,Function} ->  
			RespList = Module:Function(Cmd, State),
			imapd_resp:respond(RespList,Tag,State);
		{error,_Reason} ->
			imapd_util:out(Command,Data,State),
			imapd_resp:respond([#imap_resp{tag = Tag, status = bad}],Tag,State),
			State
	end.