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

%% this constant must match the same in syslog_drv.c
-define(SYSLOGDRV_OPEN,  1).

%% API
-export([
         start/0,
         start_link/0,
         stop/0,
         open/3,
         log/3,
         log/4,
         close/1,
         priority/1,
         facility/1,
         openlog_opt/1,
         openlog_opts/1,
         load/0,
         unload/0
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

-record(state, {}).

-type priority() :: emerg | alert | crit | err |
                    warning | notice | info | debug | non_neg_integer().
-type facility() :: kern | user | mail | daemon | auth | syslog |
                    lpr | news | uucp | cron | authpriv | ftp |
                    netinfo | remoteauth | install | ras |
                    local0 | local1 | local2 | local3 |
                    local4 | local5 | local6 | local7 | non_neg_integer().
-type openlog_opt() :: pid | cons | odelay | ndelay | perror | pos_integer().
-export_type([priority/0, facility/0, openlog_opt/0]).

%%% API %%%

-spec start() ->
    {ok, pid()} | ignore | {error, any()}.

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec start_link() ->
    {ok, pid()} | ignore | {error, any()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() ->
    ok.

stop() ->
    gen_server:cast(?MODULE, stop).

-spec open(Ident :: string(),
           Logopt :: list(openlog_opt()),
           Facility :: facility()) ->
    {ok, port()} |
    {error, any()}.

open(Ident, Logopt, Facility) ->
    Log = erlang:open_port({spawn, ?DRV_NAME}, [binary]),
    Args = term_to_binary({Ident, openlog_opts(Logopt), facility(Facility)}),
    try erlang:port_control(Log, ?SYSLOGDRV_OPEN, Args) of
        <<>> ->
            {ok, Log};
        BinError ->
            binary_to_term(BinError)
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec log(Log :: port(),
          Priority :: priority(),
          Message :: iolist()) ->
    ok.

log(_Log, _Priority, []) ->
    ok;
log(Log, Priority, Message) ->
    NumPri = priority(Priority),
    %% encode the priority value as a 4-byte integer in network order, and
    %% add a 0 byte to the end of the command data to act as a NUL character
    true = erlang:port_command(Log, [<<NumPri:32/big>>, Message, <<0:8>>]),
    ok.

-spec log(Log :: port(),
          Priority :: priority(),
          FormatStr :: string(),
          FormatArgs :: list()) ->
    ok.

log(Log, Priority, FormatStr, FormatArgs) ->
    log(Log, Priority, io_lib:format(FormatStr, FormatArgs)).

-spec close(Log :: port()) ->
    ok.

close(Log) ->
    true = erlang:port_close(Log),
    ok.

-spec priority(N :: priority() | non_neg_integer()) ->
    non_neg_integer().

priority(emerg)     -> 0;
priority(alert)     -> 1;
priority(crit)      -> 2;
priority(err)       -> 3;
priority(warning)   -> 4;
priority(notice)    -> 5;
priority(info)      -> 6;
priority(debug)     -> 7;
priority(N) when is_integer(N), N >= 0 -> N;
priority(_) -> erlang:error(badarg).

-spec facility(N :: facility() | non_neg_integer()) ->
    non_neg_integer().

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
facility(N) when is_integer(N), N >= 0 -> N;
facility(_) -> erlang:error(badarg).

-spec openlog_opt(N :: openlog_opt() | pos_integer()) ->
    pos_integer().

openlog_opt(pid)    -> 1;
openlog_opt(cons)   -> 2;
openlog_opt(odelay) -> 4;
openlog_opt(ndelay) -> 8;
openlog_opt(perror) -> 20;
openlog_opt(N) when is_integer(N), N >= 1 -> N;
openlog_opt(_) -> erlang:error(badarg).

-spec openlog_opts(N :: list(openlog_opt() | pos_integer()) |
                        openlog_opt() | pos_integer()) ->
    pos_integer().

openlog_opts([Queue]) -> openlog_opt(Queue);
openlog_opts([Tail|Queue]) ->
    openlog_opt(Tail) bor openlog_opts(Queue);
openlog_opts([]) -> 0;
openlog_opts(N) -> openlog_opt(N).

-spec load() ->
    ok | {error, string()}.

load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    case erl_ddll:load_driver(PrivDir, ?DRV_NAME) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, LoadError} ->
            LoadErrorStr = erl_ddll:format_error(LoadError),
            ErrStr = lists:flatten(
                io_lib:format("could not load driver ~s: ~p",
                              [?DRV_NAME, LoadErrorStr])),
            {error, ErrStr}
    end.

-spec unload() ->
    ok | {error, string()}.

unload() ->
    case erl_ddll:unload_driver(?DRV_NAME) of
        ok -> ok;
        {error, UnloadError} ->
            UnloadErrorStr = erl_ddll:format_error(UnloadError),
            ErrStr = lists:flatten(
                io_lib:format("could not unload driver ~s: ~p",
                              [?DRV_NAME, UnloadErrorStr])),
            {error, ErrStr}
    end.

%%% gen_server callbacks %%%

init([]) ->
    case load() of
        ok ->
            {ok, #state{}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%% internal functions %%%

-ifdef(TEST).

openlog_opts_test() ->
    11 = openlog_opts([1,2,8]),
    1 = openlog_opts(pid),
    try
        foo = openlog_opts(foo)
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
