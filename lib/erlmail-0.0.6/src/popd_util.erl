%%%---------------------------------------------------------------------------------------
%%% @author     Stuart Jackson <simpleenigma@gmail.com> [http://erlsoft.org]
%%% @copyright  2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc        POP server utility functions
%%% @reference  See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference  See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version    0.0.6
%%% @since      0.0.6
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
-module(popd_util).
-author('simpleenigma@gmail.com').
-include("../include/pop.hrl").



-export([parse/2,send/2]).


parse(Line,_State) -> 
	{Cmd,Data} = erlmail_util:split_at(Line,32),
	#popd_cmd{line = Line, cmd = erlmail_util:to_lower_atom(Cmd), data = Data}.


%%-------------------------------------------------------------------------
%% @spec (Message::string(),Socket::port()) -> ok | {error,string()}
%% @doc Sends a Message to Socket adds CRLF if needed.
%% @end
%%-------------------------------------------------------------------------
send([],_State) -> ok;
send(ok,State) -> send("+OK",State);
send(error,State) -> send("-ERR",State);
send(Msg,State)  when is_record(State,popd_fsm) -> send(Msg,State#popd_fsm.socket);
send(Message,Socket) ->
	Msg = case string:right(Message,2) of
		?CRLF -> [Message];
		_     -> [Message,?CRLF]
	end,
	gen_tcp:send(Socket,Msg).




























