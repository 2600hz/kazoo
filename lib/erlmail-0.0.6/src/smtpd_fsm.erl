%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       SMTP Server FSM for ErlMail
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.3
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
-module(smtpd_fsm).
-author('sjackson@simpleenigma.com').
-behaviour(gen_fsm).
-include("../include/smtp.hrl").

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2,
    'WAIT_FOR_CMD'/2
]).



%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #smtpd_fsm{}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
	{ok,DNSBL} = erlmail_antispam:dnsbl(IP),
	?D({relay,IP,smtpd_queue:checkip(IP)}),
	NewState = State#smtpd_fsm{socket=Socket, addr=IP, options = DNSBL, relay = smtpd_queue:checkip(IP)},
	NextState = smtpd_cmd:command({greeting,IP},NewState),
    {next_state, 'WAIT_FOR_CMD', NextState, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_CMD'({data, Data}, #smtpd_fsm{buff = Buff} = State) ->
	NewBuff = <<Buff/binary,Data/binary>>,
	case end_of_cmd(NewBuff) of
		0 -> {next_state, 'WAIT_FOR_CMD', State#smtpd_fsm{buff = NewBuff}, ?TIMEOUT};
		Pos -> 
			<<Line:Pos/binary,13,10,NextBuff/binary>> = NewBuff,
			NextState = smtpd_cmd:command(Line,State#smtpd_fsm{line = Line}),
			case NextState#smtpd_fsm.data of
				undefined -> {next_state, 'WAIT_FOR_CMD', NextState#smtpd_fsm{buff = NextBuff}, ?TIMEOUT};
				<<>> -> {next_state, 'WAIT_FOR_DATA', NextState#smtpd_fsm{buff = NextBuff}, ?TIMEOUT}
			end
			
	end;

'WAIT_FOR_CMD'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_CMD'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_CMD', State, ?TIMEOUT}.




%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #smtpd_fsm{buff = Buff} = State) ->
	NewBuff = <<Buff/binary,Data/binary>>,
	case end_of_data(NewBuff) of
		0 -> {next_state, 'WAIT_FOR_DATA', State#smtpd_fsm{buff = NewBuff}, ?TIMEOUT};
		Pos -> % wait for end of data and return data in state.
			<<Message:Pos/binary,13,10,46,13,10,NextBuff/binary>> = NewBuff,
			% @todo Check return value of store_message to see if it fails
			smtpd_cmd:store_message(Message,State),
			smtpd_cmd:out(data,"complete",State),
			smtpd_cmd:send(State,250),
			NextState = State#smtpd_fsm{cmd   = undefined,
			                       param = undefined,
								   mail  = undefined,
								   rcpt  = undefined,
								   to    = undefined,
								   messagename = undefined,
								   data  = undefined},
			{next_state, 'WAIT_FOR_CMD', NextState#smtpd_fsm{buff = NextBuff}, ?TIMEOUT}
	end;

'WAIT_FOR_DATA'(timeout, State) ->
%    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
	{stop, normal, State};
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #smtpd_fsm{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName, #smtpd_fsm{socket=Socket, addr=_Addr} = StateData) ->
%    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #smtpd_fsm{socket=Socket} = _State) ->
	% @todo: close conenctions to message store
    (catch gen_tcp:close(Socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

end_of_cmd(Bin) ->
	List = binary_to_list(Bin),
	case string:str(List,?CRLF) of
		0 -> 0;
		Pos -> Pos - 1
	end.

end_of_data(Bin) ->
	List = binary_to_list(Bin),
	case string:str(List,?SMTP_DATA_END) of
		0 -> 0;
		Pos -> Pos - 1
	end.