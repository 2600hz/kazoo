%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP Client FSM for ErlMail
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
-module(imapc_fsm).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").
-import(imapc_util).
-behavior(gen_fsm). % Finite State Machine

-define(TIMEOUT,   300000).

%% External Exports
-export([start_link/1,start_link/2]).
-export([set_socket/2,set_socket_opts/1]).

%% API functions

%% states
-export([]).

%% Testing Functions
-export(['IMAP_CMD'/2,'IMAP_CMD'/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Host)      -> start_link(Host,143).
start_link(Host,Port) -> gen_fsm:start_link(?MODULE, [Host,Port], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

set_socket_opts(Socket) -> inet:setopts(Socket, [{active, once}, binary]).



'IMAP_CMD'({socket_ready, Socket}, State) when is_port(Socket) ->
	set_socket_opts(Socket),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'IMAP_CMD', State#imapc_fsm{socket=Socket, peer=IP}, ?TIMEOUT};

'IMAP_CMD'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'IMAP_CMD', State, ?TIMEOUT}.

%%%----------------------------------------------------------------------
%%% NOOP Command - Any State
%%%----------------------------------------------------------------------
'IMAP_CMD'({noop,Tag}, _From, #imapc_fsm{socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "NOOP",
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	% %@todo process extra return values for more data. Example: Checking new mail
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,noop_successful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,noop_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,noop_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,noop_failed_unknown},'IMAP_CMD',State}
	end;
%%%----------------------------------------------------------------------
%%% CAPABILITY Command - Any State
%%%----------------------------------------------------------------------
'IMAP_CMD'({capability,Tag}, _From, #imapc_fsm{socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "CAPABILITY",
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{data = Data},#imap_resp{status = ok, tag = LowerTag}] -> {reply,{ok,capability_successful,Data},'IMAP_CMD',State#imapc_fsm{capability=Data}};
		[#imap_resp{},#imap_resp{status = no, tag = LowerTag}]            -> {reply,{error,capability_failed},'IMAP_CMD',State};
		[#imap_resp{},#imap_resp{status = bad, tag = LowerTag}]           -> {reply,{error,capability_bad_argument},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]                         -> {reply,{error,capability_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}]                        -> {reply,{error,capability_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,capability_failed_unknown},'IMAP_CMD',State}
	end;
	
%%%----------------------------------------------------------------------
%%% LOGOUT Command - Any State
%%%----------------------------------------------------------------------
'IMAP_CMD'({logout,Tag}, _From,  #imapc_fsm{socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "LOGOUT",
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = bye},#imap_resp{status = ok, tag = LowerTag}] -> {reply,{ok,logout_successful},'IMAP_CMD',State};
		[#imap_resp{},#imap_resp{status = no, tag = LowerTag}]             -> {reply,{error,logout_failed},'IMAP_CMD',State};
		[#imap_resp{},#imap_resp{status = bad, tag = LowerTag}]            -> {reply,{error,logout_bad_argument},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]                          -> {reply,{error,logout_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}]                         -> {reply,{error,logout_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,logout_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% LOGIN Command - Not Authenticated State
%%%----------------------------------------------------------------------
'IMAP_CMD'({login,Tag,UserName,Password}, _From, #imapc_fsm{socket = Socket,state = not_authenticated} = State) -> 
	Cmd = Tag ++ [32] ++ "LOGIN" ++ [32] ++ UserName ++ [32] ++ Password,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,login_successful},'IMAP_CMD',State#imapc_fsm{state=authenticated}};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,login_fail},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,login_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,logout_failed_unknown},'IMAP_CMD',State}
	end;
'IMAP_CMD'({login,_Tag,_UserName,_Password}, _From, State) -> 
	{reply,{error,wrong_state},'IMAP_CMD',State};
	

%%%----------------------------------------------------------------------
%%% AUTHENTICATE Command - Not Authenticated State - NOT IMPLIMENTED YET
%%%----------------------------------------------------------------------
%@todo impliment
'IMAP_CMD'({authenticate,_Tag,_AuthMech}, _From, State) -> 
	{reply,{error,not_implimented_use_login},'IMAP_CMD',State};

%%%----------------------------------------------------------------------
%%% STARTTLS Command - Not Authenticated State - NOT IMPLIMENTED YET 
%%%----------------------------------------------------------------------
%@todo impliment
'IMAP_CMD'({starttls,_Tag}, _From, State) -> 
	{reply,{error,not_implimented_use_login},'IMAP_CMD',State};


%%%----------------------------------------------------------------------
%%% SELECT Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({select,_Tag,_MailBox}, _From, #imapc_fsm{state=not_authenticated} = State) -> 
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({select,Tag,MailBox}, _From, #imapc_fsm{state=authenticated,socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "SELECT" ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	Resp = imapc_util:response(Socket,Tag),
	Last = lists:last(Resp),
	case Last of
		#imap_resp{status = ok, tag = LowerTag} -> 
			MB = imapc_util:mailbox(Resp,MailBox),
			{reply,{ok,select_successful,MB},'IMAP_CMD',State#imapc_fsm{state=selected,mailbox=MB}};
		#imap_resp{status = no, tag = LowerTag} -> {reply,{error,select_failed},'IMAP_CMD',State};
		#imap_resp{status = bad, tag = LowerTag} -> {reply,{error,select_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,select_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% EXAMINE Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({examine,_Tag,_MailBox}, _From, #imapc_fsm{state=not_authenticated} = State) -> 
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({examine,Tag,MailBox}, _From, #imapc_fsm{state=authenticated, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "EXAMINE" ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	Resp = imapc_util:response(Socket,Tag),
	Last = lists:last(Resp),
	case Last of
		#imap_resp{status = ok, tag = LowerTag} -> 
			MB = imapc_util:mailbox(Resp,MailBox),
			{reply,{ok,examine_successful,MB},'IMAP_CMD',State#imapc_fsm{state=selected,mailbox=MB}};
		#imap_resp{status = no, tag = LowerTag} -> {reply,{error,examine_failed},'IMAP_CMD',State};
		#imap_resp{status = bad, tag = LowerTag} -> {reply,{error,examine_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,examine_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% CREATE Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({create,_Tag,_MailBox}, _From, #imapc_fsm{state=not_authenticated} = State) ->  
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({create,Tag,MailBox}, _From, #imapc_fsm{state=authenticated,socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "CREATE" ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,create_successful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,create_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,create_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,create_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% DELETE Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({delete,_Tag,_MailBox}, _From, #imapc_fsm{state=not_authenticated} = State) ->  
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({delete,Tag,MailBox}, _From, #imapc_fsm{state=authenticated, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "DELETE" ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,delete_sucessful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,delete_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,delete_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,delete_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% RENAME Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({rename,_Tag,_MailBox,_NewMailBox}, _From, #imapc_fsm{state=not_authenticated} = State) ->  
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({rename,Tag,MailBox,NewMailBox}, _From, #imapc_fsm{state=authenticated, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "RENAME" ++ [32] ++ MailBox ++ [32] ++ NewMailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, cmd = rename},#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,rename_sucessful},'IMAP_CMD',State};
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,rename_sucessful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,rename_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,rename_bad_argument},'IMAP_CMD',State};
		_AnythingElse            -> 
			?D(_AnythingElse),
			{reply,{error,rename_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% SUBSCRIBE Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({subscribe,_Tag,_MailBox}, _From, #imapc_fsm{state=not_authenticated} = State) ->  
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({subscribe,Tag,MailBox}, _From, #imapc_fsm{state=authenticated, socket =Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "SUBSCRIBE" ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,subscribe_sucessful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,subscribe_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,subscribe_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,subscribe_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% UNSUBSCRIBE Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({unsubscribe,_Tag,_MailBox}, _From, #imapc_fsm{state = not_authenticated} = State) ->  
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({unsubscribe,Tag,MailBox}, _From, #imapc_fsm{state = authenticated, socket =Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "UNSUBSCRIBE" ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,unsubscribe_sucessful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,unsubscribe_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,unsubscribe_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,unsubscribe_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% LIST Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({list,_Tag,_RefName_MailBox}, _From, #imapc_fsm{state=not_authenticated} = State) ->   
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({list,Tag,RefName,MailBox}, _From, #imapc_fsm{socket =Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "LIST" ++ [32] ++ RefName ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	Resp = imapc_util:response(Socket,Tag),
	{List,Last} = lists:split(length(Resp) - 1,Resp),
	case Last of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,list_sucessful,imapc_util:reduce(List)},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,list_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,list_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,list_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% LSUB Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({lsub,_Tag,_RefName,_MailBox}, _From, #imapc_fsm{state=not_authenticated} = State) ->    
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({lsub,Tag,RefName,MailBox}, _From, #imapc_fsm{socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "LSUB" ++ [32] ++ RefName ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	Resp = imapc_util:response(Socket,Tag),
	{Lsub,Last} = lists:split(length(Resp) - 1,Resp),
	case Last of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,lsub_sucessful,imapc_util:reduce(Lsub)},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,lsub_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,lsub_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,lsub_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% STATUS Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({status,_Tag,_MailBox,_StatusCode}, _From, #imapc_fsm{state=not_authenticated} = State) ->     
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({status,Tag,MailBox,StatusCode}, _From, #imapc_fsm{socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "STATUS" ++ [32] ++ MailBox ++ [32] ++ StatusCode,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{data = Data},#imap_resp{status = ok, tag = LowerTag}]  -> 
			MB = imapc_util:status(Data,MailBox),
			{reply,{ok,status_sucessful,MB},'IMAP_CMD',State};
		[#imap_resp{},#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,status_failed},'IMAP_CMD',State};
		[#imap_resp{},#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,status_bad_argument},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]               -> {reply,{error,status_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}]              -> {reply,{error,status_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,status_failed_unknown},'IMAP_CMD',State}
	end;

%%%----------------------------------------------------------------------
%%% APPEND Command - Authenticated
%%%----------------------------------------------------------------------
'IMAP_CMD'({append,_Tag,_MailBox,_Message,_Flags}, _From, #imapc_fsm{state=not_authenticated} = State) ->      
	{reply,{error,wrong_state_not_authenticated},'IMAP_CMD',State};
'IMAP_CMD'({append,Tag,MailBox,Message,Flags}, _From, #imapc_fsm{socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "APPEND" ++ [32] ++ MailBox ++ [32] ++ Flags ++ [32] ++ [123] ++ integer_to_list(length(Message)) ++ [125],
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,append_sucessful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,append_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,append_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,append_failed_unknown},'IMAP_CMD',State}
	end;


%%%----------------------------------------------------------------------
%%% CHECK Command - Selected
%%%----------------------------------------------------------------------
'IMAP_CMD'({check,Tag}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "CHECK",
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,check_successful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,check_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,check_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,check_failed_unknown},'IMAP_CMD',State}
	end;
'IMAP_CMD'({check,_Tag}, _From, State) ->      
	{reply,{error,wrong_state},'IMAP_CMD',State};


%%%----------------------------------------------------------------------
%%% Close Command - Selected
%%%----------------------------------------------------------------------
'IMAP_CMD'({close,Tag}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "CLOSE",
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,close_successful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,close_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,close_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,close_failed_unknown},'IMAP_CMD',State}
	end;
'IMAP_CMD'({close,_Tag}, _From, State) ->      
	{reply,{error,wrong_state},'IMAP_CMD',State};

%%%----------------------------------------------------------------------
%%% EXPUNGE Command - Selected
%%%----------------------------------------------------------------------
'IMAP_CMD'({expunge,Tag}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "EXPUNGE",
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	Resp = imapc_util:response(Socket,Tag),
	case imapc_util:expunge_response(Resp,State) of
		{#imap_resp{status = ok, tag = LowerTag},NextState,ExpList}  -> {reply,{ok,expunge_successful,ExpList},'IMAP_CMD',NextState};
		{#imap_resp{status = no, tag = LowerTag},NextState}  -> {reply,{error,expunge_failed},'IMAP_CMD',NextState};
		{#imap_resp{status = bad, tag = LowerTag},NextState} -> {reply,{error,expunge_bad_argument},'IMAP_CMD',NextState};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,expunge_failed_unknown},'IMAP_CMD',State}
	end;
'IMAP_CMD'({expunge,_Tag}, _From, State) ->      
	{reply,{error,wrong_state},'IMAP_CMD',State};


%%%----------------------------------------------------------------------
%%% SEARCH Command - Selected
%%%----------------------------------------------------------------------
'IMAP_CMD'({search,Tag,Query}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "SEARCH" ++ [32] ++ Query,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{cmd = search, data=SearchList},#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,seach_successful,SearchList},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,search_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,seach_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,search_failed_unknown},'IMAP_CMD',State}
	end;
'IMAP_CMD'({search,_Tag,_Query}, _From, State) ->      
	{reply,{error,wrong_state},'IMAP_CMD',State};

%%%----------------------------------------------------------------------
%%% FETCH Command - Selected
%%%----------------------------------------------------------------------
%@todo some longer fetch responses are not being processes properly by the parser
'IMAP_CMD'({fetch,Tag,Set,Query}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "FETCH" ++ [32] ++ Set ++ [32] ++ Query,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	Resp = imapc_util:fetch_response(Socket,Tag),
	{Fetch,Last} = lists:split(length(Resp) - 1,Resp),
	case Last of
		[#imap_resp{status = ok, tag = LowerTag}]  -> 
			{reply,{ok,fetch_successful,imapc_util:reduce(Fetch)},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,fetch_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,fetch_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,fetch_failed_unknown},'IMAP_CMD',State}
	end;

	
'IMAP_CMD'({fetch,_Tag,_Set,_Query}, _From, State) ->      
	{reply,{error,wrong_state},'IMAP_CMD',State};


%%%----------------------------------------------------------------------
%%% STORE Command - Selected
%%%----------------------------------------------------------------------
'IMAP_CMD'({store,Tag,Set,ItemName,Flags}, From, #imapc_fsm{state=selected} = State) when is_integer(Set) -> 
	'IMAP_CMD'({store,Tag,integer_to_list(Set),ItemName,Flags}, From, State);
'IMAP_CMD'({store,Tag,Set,ItemName,Flags}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "STORE" ++ [32] ++ Set ++ [32] ++ ItemName ++ [32] ++ Flags,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	?D(Cmd),
	Resp = imapc_util:response(Socket,Tag),
	%% @todo Process rest of command for extra responses
	Last = lists:last(Resp),
	case Last of
		#imap_resp{status = ok, tag = LowerTag} -> {reply,{ok,store_successful},'IMAP_CMD',State};
		#imap_resp{status = no, tag = LowerTag} -> {reply,{error,store_failed},'IMAP_CMD',State};
		#imap_resp{status = bad, tag = LowerTag} -> {reply,{error,store_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,store_failed_unknown},'IMAP_CMD',State}
	end;
'IMAP_CMD'({store,_Tag,_Set,_ItemName,_Flags}, _From, State) ->      
	{reply,{error,wrong_state},'IMAP_CMD',State};

%%%----------------------------------------------------------------------
%%% COPY Command - Selected
%%%----------------------------------------------------------------------
'IMAP_CMD'({copy,Tag,Set,MailBox}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "COPY" ++ [32] ++ Set ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,copy_successful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,copy_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,copy_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,copy_failed_unknown},'IMAP_CMD',State}
	end;
'IMAP_CMD'({search,_Tag,_Set,_Mailbox}, _From, State) ->      
	{reply,{error,wrong_state},'IMAP_CMD',State};

%%%----------------------------------------------------------------------
%%% UID Command - Selected - UNTESTED @todo test this set of functions
%%%----------------------------------------------------------------------


'IMAP_CMD'({uid,Tag,fetch,{Set,Query}}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "UID FETCH" ++ [32] ++ Set ++ [32] ++ Query,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	Resp = imapc_util:fetch_response(Socket,Tag),
	{Fetch,Last} = lists:split(length(Resp) - 1,Resp),
	case Last of
		[#imap_resp{status = ok, tag = LowerTag}]  -> 
			{reply,{ok,uid_fetch_successful,imapc_util:reduce(Fetch)},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,uid_fetch_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,uid_fetch_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,uid_fetch_failed_unknown},'IMAP_CMD',State}
	end;


'IMAP_CMD'({uid,Tag,copy,{Set,MailBox}}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "UID COPY" ++ [32] ++ Set ++ [32] ++ MailBox,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,uid_copy_successful},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,uid_copy_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,uid_copy_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,uid_copy_failed_unknown},'IMAP_CMD',State}
	end;


'IMAP_CMD'({uid,Tag,store,{Set,ItemName,Flags}}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "UID STORE" ++ [32] ++ Set ++ [32] ++ ItemName ++ [32] ++ Flags,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	Resp = imapc_util:response(Socket,Tag),
	%% @todo Process rest of command for extra responses
	Last = lists:last(Resp),
	case Last of
		#imap_resp{status = ok, tag = LowerTag} -> {reply,{ok,uid_store_successful},'IMAP_CMD',State};
		#imap_resp{status = no, tag = LowerTag} -> {reply,{error,uid_store_failed},'IMAP_CMD',State};
		#imap_resp{status = bad, tag = LowerTag} -> {reply,{error,uid_store_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,uid_store_failed_unknown},'IMAP_CMD',State}
	end;



'IMAP_CMD'({uid,Tag,search,Query}, _From, #imapc_fsm{state=selected, socket = Socket} = State) -> 
	Cmd = Tag ++ [32] ++ "UID SEARCH" ++ [32] ++ Query,
	LowerTag = imapc_util:to_low_atom(Tag),
	imapc_util:write(Socket,Cmd),
	case imapc_util:response(Socket,Tag) of
		[#imap_resp{cmd = search, data=SearchList},#imap_resp{status = ok, tag = LowerTag}]  -> {reply,{ok,uid_seach_successful,SearchList},'IMAP_CMD',State};
		[#imap_resp{status = no, tag = LowerTag}]  -> {reply,{error,uid_search_failed},'IMAP_CMD',State};
		[#imap_resp{status = bad, tag = LowerTag}] -> {reply,{error,uid_seach_bad_argument},'IMAP_CMD',State};
		_AnythingElse -> 
			?D(_AnythingElse),
			{reply,{error,uid_search_failed_unknown},'IMAP_CMD',State}
	end;


'IMAP_CMD'({uid,_Tag,_Cmd,_Opts}, _From, State) ->      
	{reply,{error,wrong_state},'IMAP_CMD',State};






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------
%%% SORT Command - Selected - EXTENSION - NOT YET IMPLIMENTED
%%%----------------------------------------------------------------------
% @todo impliment
'IMAP_CMD'({sort,_Tag,_Order,_Query,_Charset}, From, #imapc_fsm{state=not_authenticated} = State) -> 
	gen_fsm:reply(From,{error,preauth}), % Send Preauth error 
	{reply,ok,'IMAP_CMD',State};
'IMAP_CMD'({sort,_Tag,_Order,_Query,_Charset}, From, #imapc_fsm{state=authenticated} = State) -> 
	gen_fsm:reply(From,{error,authenticated}), % Send selected error 
	{reply,ok,'IMAP_CMD',State};
'IMAP_CMD'({sort,Tag,Order,Query,Charset}, From, #imapc_fsm{state=selected} = State) -> 
	Cmd = Tag ++ [32] ++ "SORT" ++ [32,40] ++ Order ++ [41,32] ++ Charset ++ [32] ++ Query,
%	io:format("~p~n",[Cmd]),
	imapc_util:write(State#imapc_fsm.socket,Cmd),
	Resp = imapc_util:read(State#imapc_fsm.socket,Tag),
%	io:format("~p~n",[Resp]),
	First= lists:nth(1,Resp),
	Last = lists:last(Resp),
	if
		Last#imap_resp.status == ok -> gen_fsm:reply(From,First#imap_resp.data);
		true -> gen_fsm:reply(From,error)
	end,
	{reply,ok,'IMAP_CMD',State};

















'IMAP_CMD'(_Event, _From, State) -> 
	{reply,ok,'IMAP_CMD',State}.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([Host,Port]) ->
	?D("Initializing FSM ..."),
    process_flag(trap_exit, true),
	case gen_tcp:connect(Host,Port,[binary,{packet,0},{active,once}],infinity) of  %
		{ok,Socket} ->
			set_socket(self(),Socket),
			case imapc_util:response(Socket) of
				RespList when is_list(RespList) ->
					{ok, 'IMAP_CMD', #imapc_fsm{}};
				_R -> {stop, normal, #imapc_fsm{}}
			end;
		_Error -> {stop,conn_error}
			
	end.

handle_event(close, _AnyState, IMAPC) ->
    ok = gen_tcp:send(IMAPC#imapc_fsm.socket, "quit\r\n"),
    {stop, i_have_quit, []}.




handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_info({tcp, Socket, Bin}, StateName, #imapc_fsm{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName, #imapc_fsm{socket=Socket} = StateData) ->
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
	?D(_Info),
    {next_state, StateName, StateData}.

terminate(Reason,_StateName,_StateData) -> 
    {terminated, Reason}.
	
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
