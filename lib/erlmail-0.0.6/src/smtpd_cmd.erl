%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       SMTP server commands
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.5
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
-module(smtpd_cmd).
-author('sjackson@simpleenigma.com').
-include("../include/smtp.hrl").
-include("../include/erlmail.hrl").

-export([command/2]).
-export([store_message/2,store_message/4]).
-export([out/3,send/2]).

-export([check_user/1,check_user/2]).


command(Line,State) when is_binary(Line) -> command(parse(Line),State);

command({greeting,_},State) ->
	out(greeting,State),
	send(State,220,erlmail_util:get_app_env(server_smtp_greeting,"ErlMail http://erlsoft.org (NO UCE)")),
	State;

command({helo = Command,Domain},State) when is_list(Domain), length(Domain) > 0 -> 
	out(Command,Domain,State),
	send(State,250),
	State#smtpd_fsm{host=Domain};
command({ehlo = Command,Domain},State) when is_list(Domain), length(Domain) > 0 -> 
	out(Command,Domain,State),
	send(State,250),
	State#smtpd_fsm{host=Domain};
%% MAIL before HELO or EHLO
command({mail = Command,Param},#smtpd_fsm{host = undefined} = State) ->
	out(Command,Param,State),
	send(State,503),
	State;
command({mail = Command,Param},State) when length(Param) > 0 ->
	From = clean_email(Param),
	out(Command,From,State),
	send(State,250),
	State#smtpd_fsm{mail = From, rcpt = undefined, to = undefined, messagename = undefined, data = undefined};
%% RCPT before MAIL
command({rcpt = Command,Param},#smtpd_fsm{mail = undefined} = State) ->
	out(Command,Param,State),
	send(State,503),
	State;
%% Too many Rcpt
command({rcpt = Command,Param},#smtpd_fsm{rcpt = RcptList} = State) when is_list(RcptList), length(RcptList) >= 100 ->
	out(Command,Param,State),
	send(State,452,"Too many recipients"),
	State;
command({rcpt = Command,Param},#smtpd_fsm{relay = Relay} = State) ->
	To = clean_email(Param),
	out(Command,To,State),
	?D({relay,Relay}),
	case check_user(erlmail_util:split_email(To),Relay) of
		true ->
			NewRcptList = case State#smtpd_fsm.rcpt of
				undefined -> [To];
				RcptList -> [To|RcptList]
			end,
			send(State,250),
			?D({rcpt,NewRcptList}),
			State#smtpd_fsm{rcpt=NewRcptList};
		false ->
			send(State,550),
			State
	end;

command({data = Command,[]},#smtpd_fsm{rcpt = undefined} = State) ->
	out(Command,State),
	send(State,503),
	State;
command({data = Command,[]},State) ->
	out(Command,State),
	send(State,354),
	State#smtpd_fsm{data = <<>>};

command({noop = Command,[]},State) ->
	out(Command,State),
	send(State,250),
	State;
command({vrfy = Command,[]},State) ->
	out(Command,State),
	send(State,502),
	State;
command({expn = Command,[]},State) ->
	out(Command,State),
	send(State,502),
	State;
command({help = Command,_Param},State) ->
	out(Command,State),
	send(State,250,"http://erlsoft.org"),
	State;
command({quit = Command,[]},State) ->
	out(Command,State),
	send(State,221),
	gen_fsm:send_all_state_event(self(),stop),
	State;
command({rset = Command,[]},State) ->
	out(Command,State),
	send(State,250),
	State#smtpd_fsm{cmd = undefined, param = undefined, mail = undefined, rcpt = undefined, to = undefined, messagename = undefined, data = undefined};

%% Obsolete
command({send = Command,_Param},State) ->
	out(Command,State),
	send(State,502),
	State;
%% Obsolete
command({soml = Command,_Param},State) ->
	out(Command,State),
	send(State,502),
	State;
%% Obsolete
command({saml = Command,_Param},State) ->
	out(Command,State),
	send(State,502),
	State;




command({Command,Param},State) ->
	io:format("Unknown Command: ~p ~p~n",[Command,Param]),
	send(State,500),
	State.

check_user(Name) -> check_user(Name,undefined).
check_user(_Name,true) -> true;
check_user({_UserName,_DomainName} = Name,_Relay) ->
	case erlmail_store:status(Name) of
		{ok,User} when is_record(User,user) -> true;
		_Other -> false
	end.


%% @todo cehck relay state and store messages according to local or outgoing status. Only real differene is in the message name.

store_message(Message,State) when is_binary(Message) -> store_message(binary_to_list(Message),State);
store_message(Message,_State) when is_record(Message,message) ->
	case erlmail_antispam:pre_deliver(Message) of
		{ok,NewMessage} -> 
			erlmail_store:deliver(NewMessage),
			case erlmail_antispam:post_deliver(Message) of
				{ok,_M} -> ok;
				{error,Reason} -> {error,Reason}
			end;
		{error,Reason} -> {error,Reason}
	end;
store_message(Message,#smtpd_fsm{relay = _Relay} = State) ->
	lists:map(fun(To) -> 
		MessageName = erlmail_store:message_name(now()),
		store_message(MessageName,erlmail_util:split_email(To),Message,State)
		end,State#smtpd_fsm.rcpt).

store_message(MessageName,{UserName,DomainName},Message,#smtpd_fsm{relay = true} = State) -> 
	case check_user({UserName,DomainName}) of
		true ->
			store_message(#message{
				name={MessageName,UserName,DomainName},
				message=Message},State);
		false ->
			?D({relay,non_local}),
			SmtpOut = #outgoing_smtp{rcpt = erlmail_util:combine_email(UserName,DomainName)},
			store_message(#message{
				name={MessageName,SmtpOut,now()},
				message=Message},State)
	end;
store_message(MessageName,{UserName,DomainName},Message,#smtpd_fsm{relay = false} = State) -> 
	store_message(#message{
		name={MessageName,UserName,DomainName},
		message=Message},State).











send(State,Code) -> send(State,Code,resp(Code)).
send(State,Code,[]) -> send(State,Code,resp(Code));
send(State,Code,Message) when is_record(State,smtpd_fsm) -> send(State#smtpd_fsm.socket,Code,Message);
send(Socket,Code,Message) ->
	Last = string:right(Message,2),
	Msg = case Last of
		?CRLF -> [integer_to_list(Code),32,Message];
		_      -> [integer_to_list(Code),32,Message,?CRLF]
	end,
	gen_tcp:send(Socket,Msg).





parse(Bin)  when is_binary(Bin) -> parse(binary_to_list(Bin));
parse(Line) when is_list(Line)  ->
	case string:chr(Line,32) of
		0 -> {list_to_atom(http_util:to_lower(Line)),[]};
		Pos ->
			{Command,RespText} = lists:split(Pos-1,Line),
			{list_to_atom(http_util:to_lower(Command)),string:strip(RespText)}
	end.


out(Command,State) -> io:format("~p ~p~n",[State#smtpd_fsm.addr,Command]).
out(Command,Param,State) -> io:format("~p ~p ~p~n",[State#smtpd_fsm.addr,Command,Param]).



clean_email(String) -> 
	case regexp:match(String,"<(.*)>") of
		{match,Start,Length} -> string:substr(String,Start+1,Length-2);
		{error,Reason} -> {error,Reason}
	end.




resp(211) -> "System Status"; % Need more info
resp(214) -> "For help please go to http://erlsoft.org/modules/erlmail/";
resp(220) -> "ErlMail (NO UCE)";
resp(221) -> "SMTP server closing transmission channel";
resp(250) -> "Requested mail action okay, completed";
resp(251) -> "User not local";
resp(252) -> "Cannot VRFY user, but will accept message and attempt deliver";
resp(354) -> "Mail input accepted";
resp(421) -> "Service Not avaiable, closing transmission channel";
resp(450) -> "Requestion action not taken: mailbox unavailable";
resp(451) -> "Requestion action aborted: local error in processing";
resp(452) -> "Requestion action not taken: insufficient system storage";
resp(500) -> "Syntax error, command unrecognized";
resp(501) -> "Syntax error in parameters or arguments";
resp(502) -> "Command not implimented";
resp(503) -> "Bad sequence of commands";
resp(504) -> "Command parameter not implimented";
resp(511) -> "No mailbox here by that name";
resp(550) -> "Reqested action not taken: mailbox unavailable";
resp(551) -> "User not local";
resp(552) -> "Requestion action not taken: insufficient system storage";
resp(553) -> "Requestion action not taken: mailbox name not allowed";
resp(554) -> "Transaction Failed".