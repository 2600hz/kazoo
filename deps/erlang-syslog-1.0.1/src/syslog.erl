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

%% these constants must match those in syslog_drv.c
-define(SYSLOGDRV_OPEN,  1).
-define(SYSLOGDRV_CLOSE, 2).

%% API
-export([
         start/0,
         start_link/0,
         stop/0,
         open/3,
         log/3,
         log/4,
         close/1
        ]).

%% gen_server callbacks
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

stop() ->
    gen_server:cast(?MODULE, stop).

open(Ident, Logopt, Facility) ->
    case gen_server:call(?MODULE, {open, Ident, logopt(Logopt), facility(Facility)}) of
        {error, badarg} ->
            erlang:error(badarg);
        Else ->
            Else
    end.

log(_Log, _Priority, []) ->
    ok;
log(Log, Priority, Message) ->
    NumPri = priorities(Priority),
    %% encode the priority value as a 4-byte integer in network order, and
    %% add a 0 byte to the end of the command data to act as a NUL character
    true = erlang:port_command(Log, [<<NumPri:32/big>>, Message, <<0:8>>]),
    ok.
log(Log, Priority, FormatStr, FormatArgs) ->
    log(Log, Priority, io_lib:format(FormatStr, FormatArgs)).

close(Log) ->
    try erlang:port_call(Log, ?SYSLOGDRV_CLOSE, <<>>) of
        Result ->
            Result
    after
        port_close(Log)
    end.


%%% API %%%

init([]) ->
    process_flag(trap_exit, true),
    erl_ddll:start(),
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    LoadResult = case erl_ddll:load_driver(PrivDir, ?DRV_NAME) of
                     ok -> ok;
                     {error, already_loaded} -> ok;
                     {error, LoadError} ->
                         LoadErrorStr = erl_ddll:format_error(LoadError),
                         ErrStr = lists:flatten(
                                    io_lib:format("could not load driver ~s: ~p",
                                                  [?DRV_NAME, LoadErrorStr])),
                         {stop, ErrStr}
                 end,
    case LoadResult of
        ok ->
            Port = erlang:open_port({spawn, ?DRV_NAME}, [binary]),
            {ok, #state{port = Port}};
        Error ->
            Error
    end.

handle_call({open, Ident, Logopt, Facility}, {Pid,_}, #state{port = Port} = State) ->
    Ref = make_ref(),
    Args = term_to_binary({Ident, Logopt, Facility, term_to_binary(Ref)}),
    Reply = try erlang:port_control(Port, ?SYSLOGDRV_OPEN, Args) of
                <<>> ->
                    receive
                        {Ref, {ok, Log}=Result} ->
                            erlang:port_connect(Log, Pid),
                            unlink(Log),
                            Result;
                        {Ref, Result} ->
                            Result
                    end;
                BinError ->
                    binary_to_term(BinError)
            catch
                _:Reason ->
                    {error, Reason}
            end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    erlang:port_close(Port),
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
priorities(N) when is_integer(N) -> N;
priorities(_) -> erlang:error(badarg).

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
facility(N) when is_integer(N) -> N;
facility(_) -> erlang:error(badarg).

openlog_opt(pid)    -> 1;
openlog_opt(cons)   -> 2;
openlog_opt(odelay) -> 4;
openlog_opt(ndelay) -> 8;
openlog_opt(perror) -> 20;
openlog_opt(N) when is_integer(N) -> N;
openlog_opt(_) -> erlang:error(badarg).

logopt([Queue]) -> openlog_opt(Queue);
logopt([Tail|Queue]) ->
    openlog_opt(Tail) bor logopt(Queue);
logopt([]) -> 0;
logopt(N) -> openlog_opt(N).


-ifdef(TEST).

logopt_test() ->
    11 = logopt([1,2,8]),
    1 = logopt(pid),
    try
        foo = logopt(foo)
    catch
        error:badarg ->
            ok;
        Reason ->
            throw(Reason)
    end.

closed_test() ->
    {ok, _} = syslog:start(),
    try
        {ok, Log} = open("test", pid, local0),
        Self = self(),
        {connected,Self} = erlang:port_info(Log, connected),
        ok = close(Log),
        try
            close(Log)
        catch
            error:badarg ->
                ok;
            Reason1 ->
                throw(Reason1)
        end,
        try
            ok = log(Log, 8, "writing to closed log")
        catch
            error:badarg ->
                ok;
            Reason2 ->
                throw(Reason2)
        end
    after
        syslog:stop()
    end.

-endif.
