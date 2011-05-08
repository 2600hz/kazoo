%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       SMTP Client FSM for ErlMail
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
-module(smtpc_fsm).
-author('sjackson@simpleenigma.com').
-behavior(gen_fsm). % Finite State Machine
-include("../include/smtp.hrl").

%% External Exports
-export([start/1,start/2,start_link/1,start_link/2]).
%% API functions
-export([]).
%% states
-export([smtp_cmd/3]).
%% Testing Functions
-export([]).
%% gen_server callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
% Manual Start
start(Host)      -> start(Host,25).
start(Host,Port) -> gen_fsm:start(?MODULE, [Host,Port], []).

% Supervised Start
start_link(Host)      -> start_link(Host,25).
start_link(Host,Port) -> gen_fsm:start_link(?MODULE, [Host,Port], []).

%%%----------------------------------------------------------------------
%%% State functions
%%%----------------------------------------------------------------------
	
	
%%%----------------------------------------------------------------------
%%% HELO Command
%%%----------------------------------------------------------------------
smtp_cmd({helo, Name}, From, State)->
    Msg = "HELO" ++ [32] ++ Name,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
		gen_fsm:reply(From,{250,Resp}),
	    {reply,ok,smtp_cmd,State#smtpc{state=mail}};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% EHLO Command
%%%----------------------------------------------------------------------
smtp_cmd({ehlo, Name}, From, State) ->
    Msg = "EHLO" ++ [32] ++ Name,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
	    Strs = [string:sub_string(X,5) || X <- string:tokens(Resp, "\r\n")],
	    {reply, ok, smtp_cmd, State#smtpc{features = tl(Strs),state=mail}};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% ETRN Command
%%%----------------------------------------------------------------------
smtp_cmd({etrn, Queue}, From, State) ->
    Msg = "ETRN" ++ [32] ++ Queue,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
		gen_fsm:reply(From,{250,Resp}),
	    {reply, ok, smtp_cmd, State};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% ETRN Command
%%%----------------------------------------------------------------------
smtp_cmd({expn, Alias}, From, State) ->
    Msg = "EXPN" ++ [32] ++ Alias,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
		gen_fsm:reply(From,{250,Resp}),
	    {reply, ok, smtp_cmd, State};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% HELP Command
%%%----------------------------------------------------------------------
smtp_cmd(help, From, State) ->
    Msg = "HELP",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{214, Resp} -> 
		gen_fsm:reply(From,{214,Resp}),
	    {reply, ok, smtp_cmd, State};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% MAIL Command
%%%----------------------------------------------------------------------
smtp_cmd({mail, Address}, From, State) ->
    Msg = "MAIL FROM:<" ++ Address ++ ">",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
		gen_fsm:reply(From,{250,Resp}),
	    {reply, ok, smtp_cmd, State#smtpc{state=rcpt}};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% NOOP Command
%%%----------------------------------------------------------------------
smtp_cmd(noop, From, State) ->
    Msg = "NOOP",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
		gen_fsm:reply(From,{250,Resp}),
	    {reply, ok, smtp_cmd, State};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% QUIT Command
%%%----------------------------------------------------------------------
smtp_cmd(quit, From, State) ->
    Msg = "QUIT",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{221, Resp} -> 
		gen_tcp:close(State#smtpc.socket),
		gen_fsm:reply(From,{221,Resp}),
	    {reply, ok, smtp_cmd, State};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% RSET Command
%%%----------------------------------------------------------------------
smtp_cmd(rset, From, State) ->
    Msg = "RSET",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
		gen_fsm:reply(From,{250,Resp}),
	    {reply, ok, smtp_cmd, State#smtpc{state=mail}};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% RCPT Command
%%%----------------------------------------------------------------------
smtp_cmd({rcpt, Address}, From, State) ->
    Msg = "RCPT TO:<" ++ Address ++ ">",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
		gen_fsm:reply(From,{250,Resp}),
	    {reply, ok, smtp_cmd, State#smtpc{state=rcpt}};
	{511, Resp} -> 
		gen_fsm:reply(From,{511,Resp}),
	    {reply, ok, smtp_cmd, State#smtpc{state=rcpt}};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% VRFY Command
%%%----------------------------------------------------------------------
smtp_cmd({vrfy, Address}, From, State) ->
    Msg = "VRFY " ++ Address,
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{250, Resp} -> 
		gen_fsm:reply(From,{250,Resp}),
	    {reply, ok, smtp_cmd, State#smtpc{state=rcpt}};
	{511, Resp} -> 
		gen_fsm:reply(From,{511,Resp}),
	    {reply, ok, smtp_cmd, State#smtpc{state=rcpt}};
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% DATA Command
%%%----------------------------------------------------------------------
smtp_cmd({data,Message}, From, State) ->
    Msg = "DATA",
    write(State#smtpc.socket, Msg),
    case read(State#smtpc.socket) of
	{354, _Resp} -> 
	    write(State#smtpc.socket, Message ++ ?SMTP_DATA_END),
    	case read(State#smtpc.socket) of
			{250, DataResp} -> 
				gen_fsm:reply(From,{250,DataResp}),
	    		{reply, ok, smtp_cmd, State#smtpc{state=mail}};
			{Code, DataResp} -> 
				gen_fsm:reply(From,{Code,DataResp}),
			    {reply,ok,smtp_cmd, State};
			Error -> 
				{stop, Error, [], []}
			end;
	{Code, Resp} -> 
		gen_fsm:reply(From,{Code,Resp}),
	    {reply,ok,smtp_cmd, State};
	Error -> 
	    {stop, Error, [], []}
    end;
%%%----------------------------------------------------------------------
%%% INFO Command 
%%%----------------------------------------------------------------------
smtp_cmd({info,Info}, From, State) -> 
	case Info of
		all -> gen_fsm:reply(From,State);
		features -> gen_fsm:reply(From,State#smtpc.features);
		state -> gen_fsm:reply(From,State#smtpc.state);
		type -> gen_fsm:reply(From,State#smtpc.type);
		scoket -> gen_fsm:reply(From,State#smtpc.socket);
		_Other -> gen_fsm:reply(From,State)
	end,
	{reply,ok,smtp_cmd,State}.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([Server,Port]) ->
	case gen_tcp:connect(Server,Port,[binary,{packet,0},{active,once}]) of 
		{ok,Socket} ->
			case read(Socket) of
				{220, Resp} -> {ok, smtp_cmd, #smtpc{socket=Socket,type=smtp_type(Resp)}};
				{error,Reason} -> {error,Reason}
			end;
		{error,Reason} -> {error,Reason}
	end.

handle_event(close, _AnyState, State) ->
    ok = gen_tcp:send(State#smtpc.socket, "quit\r\n"),
    {stop, i_have_quit, []}.

handle_sync_event(rset, _From, _AnyState, State) ->
	{reply, ok, smtp_cmd, State}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason,_StateName,_StateData) -> 
    {terminated, Reason}.
	
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: read(Socket)
%%         : read(Socket,Encrypt)
%%         : read(Socket,Encrypt,Acc)
%%           Socket : TCP Socket
%%           Encrypt: Atom - Encryption Type - Unused
%%           Acc    : List - Accumulator
%% Descrip.: Reads from Socket until CRLF is received
%% Returns : List
%%--------------------------------------------------------------------
read(Socket) -> read(Socket, <<>>).
read(Socket,Packet) ->
	receive
		{tcp,Socket,Bin} ->
			NewPacket = <<Packet/binary,Bin/binary>>,
			String = binary_to_list(NewPacket),
			set_socket_opts(Socket),
			case string:right(String,2) of
				?CRLF -> 
					Line = string:left(String,length(String) - 2),
					parse(Line);
				_ -> read(Socket,NewPacket)
			end;
		{tcp_closed, Socket} -> {error,socket_closed}
	end.

%%--------------------------------------------------------------------
%% Function: parse(Line)
%%           Line: List - one line of SMTP data
%% Descrip.: Separates a line of SMTP data into a command and the data
%% Returns : {Command,Response}
%%--------------------------------------------------------------------
parse(Line) ->
	{CodeString,RespText} = lists:split(3,Line),
	{list_to_integer(CodeString),string:strip(RespText)}.
	
smtp_type(RespText) ->
	case string:str(http_util:to_lower(RespText),"esmtp") of
		0 -> smtp;
		_ -> esmtp
	end.

%%--------------------------------------------------------------------
%% Function: write(Socket,Msg)
%%         : write(Socket,Msg,Encrypt)
%%           Socket : TCP Socket
%%           Msg    : List - Message
%%           Encrypt: Atom - Encryption Type - Unused
%% Descrip.: Write message to socket, adds CRLF is needed
%% Returns : List
%%--------------------------------------------------------------------
write(Socket,Msg) -> 
	Last = string:right(Msg,2),
	case Last of
		?CRLF -> gen_tcp:send(Socket,Msg);
		_      -> gen_tcp:send(Socket,Msg ++ ?CRLF)
	end.

set_socket_opts(Socket) -> inet:setopts(Socket, [{active, once}, binary]).