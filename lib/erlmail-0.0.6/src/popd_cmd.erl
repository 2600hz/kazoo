%%%---------------------------------------------------------------------------------------
%%% @author     Stuart Jackson <simpleenigma@gmail.com> [http://erlsoft.org]
%%% @copyright  2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc        POP server commands 
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
-module(popd_cmd).
-author('simpleenigma@gmail.com').
-include("../include/pop.hrl").

-export([command/1,command/2]).

%%-------------------------------------------------------------------------
%% @spec (State::popd_fsm()) -> NewState::popd_fsm()
%% @doc  Processes all POP commands and checks for extention processing
%% @end
%%-------------------------------------------------------------------------
command(#popd_fsm{line = Line} = State) -> 
	Command = popd_util:parse(Line,State),
	io:format("Command: ~p~n",[Command]),
	command(Command,State).




command(#popd_cmd{cmd = quit} = _Cmd,
		#popd_fsm{state = authorization} = State) ->
	popd_util:send(ok,State),
	gen_tcp:close(State#popd_fsm.socket),
	gen_fsm:send_all_state_event(self(),stop),
	State;
command(#popd_cmd{cmd = quit} = _Cmd,
		State) ->
	popd_util:send(ok,State),
	% Clean up routines to delete messages
	gen_tcp:close(State#popd_fsm.socket),
	gen_fsm:send_all_state_event(self(),stop),
	State;
command(#popd_cmd{cmd = _Command} = _Cmd,State) ->
	popd_util:send(error,State),
	State.