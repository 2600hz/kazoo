%%% Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimer.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%%% EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc erlang wrapper for syslog port

-module(syslog).

-behaviour(gen_server).

-define(DRV_NAME, "syslog_drv").

% API
-export([
	start/0,
	start_link/0,
	open/3,
	log/2
]).

% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {port}).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open(Ident, Logopt, Facility) ->
	gen_server:call(?MODULE, {open, Ident, logopt(Logopt), facility(Facility)}).

log(Priority, Message) ->
	gen_server:call(?MODULE, {log, priorities(Priority), Message}).

%%% API %%%

init([]) ->
	erl_ddll:start(),
	Path = case code:priv_dir(syslog) of
		{error, _} ->
			case load_path(?DRV_NAME++".so") of
				{error, _} ->
					error;
				{ok, P} ->
					P
			end;
		P ->
			P
	end,

	case Path of
		error ->
			{stop, no_driver};
		Path ->
			case erl_ddll:load_driver(Path, ?DRV_NAME) of
				ok ->
					Port = open_port({spawn, ?DRV_NAME}, [binary]),
					{ok, #state{port = Port}};
				{error, Error} ->
					error_logger:format("Error loading driver: " ++ erl_ddll:format_error(Error), []),
					{stop, bad_driver}
			end
	end.

handle_call({log, Priority, Message}, _From, #state{port = Port} = State) ->
	port_command(Port, erlang:term_to_binary({log, Priority, lists:flatten(Message)})),
	Reply = receive
		{Port, {data, Bin}} ->
			binary_to_term(Bin)
	after 1000 -> timeout
	end,
	{reply, Reply, State};
handle_call({open, Ident, Logopt, Facility}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({open, Ident, Logopt, Facility})),
	Reply = receive
		{Port, {data, Bin}} ->
			binary_to_term(Bin)
	after 1000 -> timeout
	end,
	{reply, Reply, State};
handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_, _, _) ->
	ok.

%%% internal functions %%%

priorities(emerg)   -> 0;
priorities(alert)   -> 1;
priorities(crit)    -> 2;
priorities(err)     -> 3;
priorities(warning) -> 4;
priorities(notice)  -> 5;
priorities(info)    -> 6;
priorities(debug)   -> 7;
priorities(N)       -> N.

facility(kern)      -> 0;
facility(user)      -> 8;
facility(mail)      -> 16;
facility(daemon)    -> 24;
facility(auth)      -> 32;
facility(syslog)    -> 40;
facility(lpr)       -> 48;
facility(news)      -> 56;
facility(uucp)      -> 64;
facility(cron)      -> 72;
facility(authpriv)  -> 80;
facility(ftp)       -> 88;
facility(netinfo)   -> 96;
facility(remoteauth)-> 104;
facility(install)   -> 112;
facility(ras)       -> 120;
facility(local0)    -> 16 * 8;
facility(local1)    -> 17 * 8;
facility(local2)    -> 18 * 8;
facility(local3)    -> 19 * 8;
facility(local4)    -> 20 * 8;
facility(local5)    -> 21 * 8;
facility(local6)    -> 22 * 8;
facility(local7)    -> 23 * 8;
facility(N)         -> N.

openlog_opt(pid)    -> 1;
openlog_opt(cons)   -> 2;
openlog_opt(odelay) -> 4;
openlog_opt(ndelay) -> 8;
openlog_opt(perror) -> 20;
openlog_opt(N)      -> N.

logopt([Queue]) -> openlog_opt(Queue);
logopt([Tail|Queue]) -> 
	openlog_opt(Tail) bor logopt(Queue);
logopt([]) -> 0;
logopt(N) -> openlog_opt(N).

load_path(File) ->
	case lists:zf(fun(Ebin) ->
					Priv = Ebin ++ "/../priv/",
					case file:read_file_info(Priv ++ File) of
						{ok, _} -> {true, Priv};
						_ -> false
					end
			end, code:get_path()) of
		[Dir|_] ->
			{ok, Dir};
		[] ->
			error_logger:format("Error: ~s not found in code path\n", [File]),
			{error, enoent}
	end.

-ifdef(TEST).

logopt_test() ->
	{ok, _} = syslog:start(),
	11 = logopt([1,2,8]).

-endif.
