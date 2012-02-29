%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc A memory-limited info/error/warning event handler.
%%
%% Replace the OTP default error_logger's event handler (which
%% can cause memory use problems when handling very large messages)
%% with a handler that will use a limited amount of RAM but is
%% otherwise equivalent.
%%
%% If the SASL error logger's <tt>sasl_error_logger</tt> configuration
%% parameter is set to the <tt>{file, FileName}</tt> form, then this
%% module will attempt to emulate the SASL error logger's
%% logging-to-file behavior.  However, the interpretation of the
%% <tt>errlog_type</tt> configuration parameter differs with respect to
%% the handling of SASL progress reports: if the <tt>errlog_type</tt>
%% value is <tt>error</tt>, then SASL error-level progress reports will be 
%% written to the file.
%% <ul>
%% <li> <b>NOTE:</b> The log file's filehandle will be re-opened once
%%                   per second, which will allow log file rotation schemes
%%                   to rotate the log file safely without undue worry about
%%                   losing log file entries or worrying about sending a
%%                   SIGHUP signal to the owner process before rotation. </li>
%% </ul>

-module(riak_err_handler).

-behaviour(gen_event).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SUPERVISOR_REPORT, "SUPERVISOR REPORT").
-define(CRASH_REPORT, "CRASH REPORT").
-define(PROGRESS_REPORT, "PROGRESS REPORT").

%% External exports
-export([add_sup_handler/0,
         set_term_max_size/1, set_fmt_max_bytes/1, reopen_log_file/0,
         get_state/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

%% Functions perhaps useful to the wider Erlang world.
-export([format_event/3, limited_fmt/4]).

-record(state, {
          term_max_size :: pos_integer(),
          fmt_max_bytes :: pos_integer(),
          log_path      :: undefined | string(),
          log_fh        :: undefined | file:io_device(),
          errlog_type   :: error | progress | all,
          conslog_type  :: error | all,
          conslog_sasl  :: boolean()
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc Add a supervised handler to the OTP kernel's
%%      <tt>error_logger</tt> event server.
-spec add_sup_handler() -> term().
add_sup_handler() ->
    gen_event:add_sup_handler(error_logger, ?MODULE, []).

%% @doc Change the internal value of <tt>term_max_size</tt>.
-spec set_term_max_size(pos_integer()) -> ok.
set_term_max_size(Num) ->
    gen_event:call(error_logger, ?MODULE, {set_term_max_size, Num}, infinity).

%% @doc Change the internal value of <tt>fmt_max_bytes</tt>.
-spec set_fmt_max_bytes(pos_integer()) -> ok.
set_fmt_max_bytes(Num) ->
    gen_event:call(error_logger, ?MODULE, {set_fmt_max_bytes, Num}, infinity).

%% @doc Tell our error handler to reopen the <tt>sasl_error_logger</tt> file's
%%      file handle (e.g., to assist log file rotation schemes).
-spec reopen_log_file() -> ok.
reopen_log_file() ->
    gen_event:call(error_logger, riak_err_handler, reopen_log_file, infinity).

%% @doc Debugging: get internal state record.
-spec get_state() -> #state{}.
get_state() ->
    gen_event:call(error_logger, ?MODULE, {get_state}, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%% @hidden
-spec init([]) -> {ok, #state{}}.
init([]) ->
    TermMaxSize = get_int_env(term_max_size, 10*1024),
    FmtMaxBytes = get_int_env(fmt_max_bytes, 12*1024),
    {LogPath, LogFH} = case application:get_env(sasl, sasl_error_logger) of
                           {ok, {file, Path}} ->
                               FH = open_log_file(Path),
                               {Path, FH};
                           _ ->
                               {undefined, undefined}
                       end,
    ErrlogType = case application:get_env(sasl, errlog_type) of
                     {ok, ErrVal} -> ErrVal;
                     _            -> all
                 end,
    ConslogType = get_env_or_arg(console_error_type, all),
    ConslogSASL = get_env_or_arg(console_sasl_reports, false),
    {ok, #state{term_max_size = TermMaxSize,
                fmt_max_bytes = FmtMaxBytes,
                log_path = LogPath,
                log_fh = LogFH,
                errlog_type = ErrlogType,
                conslog_type = ConslogType,
                conslog_sasl = ConslogSASL}}.

%% @hidden
-spec handle_event({atom(), pid(), {pid(), string() | atom(), any()}}, #state{}) -> {ok, #state{}}.
handle_event(Event, #state{errlog_type = ErrlogType, conslog_type = ConslogType,
                           log_fh = LogFH, term_max_size = TermMaxSize,
                           fmt_max_bytes = FmtMaxBytes,
                           conslog_sasl = ConslogSASL} = State) ->
    {ErrorP, ReportStr, Formatted} =
        format_event(Event, TermMaxSize, FmtMaxBytes),
    case {is_sasl_reportstr(ReportStr), ConslogSASL,
          should_log_it(ConslogType, ErrorP, x)} of
        {true, true, true} ->
            io:put_chars(Formatted);
        {false, _, true} ->
            io:put_chars(Formatted);
        _ ->
            ok
    end,
    case should_log_it(ErrlogType, ErrorP, ReportStr) of
        true when LogFH /= undefined ->
            case file:write(LogFH, Formatted) of
              ok -> ok;
              {error, _Reason} -> io:format("Couldn't log: " ++ Formatted)
            end;
        _ ->
            ok
    end,
    {ok, State}.

%% @hidden
-spec handle_call(term(), #state{}) -> {ok, ok, #state{}}.
handle_call(reopen_log_file, State) ->
    case State#state.log_fh of
        undefined ->
            {ok, ok, State};
        _ ->
            catch file:close(State#state.log_fh),
            FH = open_log_file(State#state.log_path),
            {ok, ok, State#state{log_fh = FH}}
    end;
handle_call({set_term_max_size, Num}, State) ->
    {ok, ok, State#state{term_max_size = Num}};
handle_call({set_fmt_max_bytes, Num}, State) ->
    {ok, ok, State#state{fmt_max_bytes = Num}};
handle_call({get_state}, State) ->
    {ok, State, State};
handle_call(_Request, State) ->
    Reply = nosupported,
    {ok, Reply, State}.

%% @hidden
-spec handle_info(term(), #state{}) -> {ok, #state{}}.
handle_info(_Info, State) ->
    {ok, State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec format_event(tuple(), integer(), integer()) ->
      {boolean(), string(), iolist()}.
%% @doc Given an error_logger-style Event tuple, format it with the
%%      constraints of TermMaxSize and FmtMaxSize.
%%
%% See {@link limited_fmt/3} for the meaning of TermMaxSize and FmtMaxSize.
format_event(Event, TermMaxSize, FmtMaxBytes) ->
    %% Case clauses appear the same order as error_logger_tty_h:write_event/1.
    {ReportStr, Pid, MsgStr, ErrorP} =
        case Event of
            {_Type, GL, _Msg} when node(GL) /= node() ->
                {ignore, ignore, ignore, false};
            {error, _GL, {Pid1, Fmt, Args}} ->
                {"ERROR REPORT", Pid1, limited_fmt(Fmt, Args, TermMaxSize, FmtMaxBytes), true};
            %% SLF: non-standard string below.
            {emulator, _GL, Chars} ->
                {"ERROR REPORT", emulator, Chars, true};
            {info, _GL, {Pid1, Info, _}} ->
                {"INFO REPORT", Pid1, limited_str(Info, FmtMaxBytes), false};
            {error_report, _GL, {Pid1, std_error, Rep}} ->
                {"ERROR REPORT", Pid1, limited_str(Rep, FmtMaxBytes), true};
            {error_report, _GL, Other} ->
                perhaps_a_sasl_report(error_report, Other, FmtMaxBytes);
            {info_report, _GL, {Pid1, std_info, Rep}} ->
                {"INFO REPORT", Pid1, limited_str(Rep, FmtMaxBytes), false};
            {info_report, _GL, Other} ->
                perhaps_a_sasl_report(info_report, Other, FmtMaxBytes);
            {info_msg, _GL, {Pid1, Fmt, Args}} ->
                {"INFO REPORT", Pid1, limited_fmt(Fmt, Args, TermMaxSize, FmtMaxBytes), false};
            {warning_report, _GL, {Pid1, std_warning, Rep}} ->
                {"WARNING REPORT", Pid1, limited_str(Rep, FmtMaxBytes), true};
            {warning_report, _GL, _Other} ->
                {ignore, ignore, ignore, false};
            {warning_msg, _GL, {Pid1, Fmt, Args}} ->
                {"WARNING REPORT", Pid1, limited_fmt(Fmt, Args, TermMaxSize, FmtMaxBytes), true};
            %% This type is allegedly ignored, so whatever.
            _E ->
                {"ODD REPORT", "blahblah", limited_fmt("odd ~p", [_E], TermMaxSize, FmtMaxBytes), false}
        end,
    if ReportStr == ignore ->
            {false, "", ""};
       true ->
            Time = riak_err_stdlib:write_time(riak_err_stdlib:maybe_utc(erlang:localtime()), ReportStr),
            NodeSuffix = other_node_suffix(Pid),
            {ErrorP, ReportStr, io_lib:format("~s~s~s",
                                              [Time, MsgStr, NodeSuffix])}
    end.

%% @doc Format Fmt and Args similar to what io_lib:format/2 does but with 
%%      limits on how large the formatted string may be.
%%
%% If the Args list's size is larger than TermMaxSize, then the
%% formatting is done by trunc_io:print/2, where FmtMaxBytes is used
%% to limit the formatted string's size.
-spec limited_fmt(string(), list(), integer(), integer()) -> iolist().
limited_fmt(Fmt, Args, TermMaxSize, FmtMaxBytes) ->
    TermSize = erts_debug:flat_size(Args),
    if TermSize > TermMaxSize ->
            ["Oversize args for format \"", Fmt, "\": \n",
             [
              begin
                  {Str, _} = trunc_io:print(lists:nth(N, Args), FmtMaxBytes),
                  ["  arg", integer_to_list(N), ": ", Str, "\n"]
              end || N <- lists:seq(1, length(Args))
             ]];
       true ->
            io_lib:format(Fmt, Args)
    end.

limited_str(Term, FmtMaxBytes) ->
    {Str, _} = trunc_io:print(Term, FmtMaxBytes),
    Str.

other_node_suffix(Pid) when node(Pid) =/= node() ->
    "** at node " ++ atom_to_list(node(Pid)) ++ " **\n";
other_node_suffix(_) ->
    "".

perhaps_a_sasl_report(error_report, {Pid, Type, Report}, FmtMaxBytes) ->
    case riak_err_stdlib:is_my_error_report(Type) of
        true ->
            {sasl_type_to_report_head(Type), Pid,
             sasl_limited_str(Type, Report, FmtMaxBytes), true};
        false ->
            {ignore, ignore, ignore, false}
    end;
perhaps_a_sasl_report(info_report, {Pid, Type, Report}, FmtMaxBytes) ->
    case riak_err_stdlib:is_my_info_report(Type) of
        true ->
            {sasl_type_to_report_head(Type), Pid,
             sasl_limited_str(Type, Report, FmtMaxBytes), false};
        false ->
            {ignore, ignore, ignore, false}
    end;
perhaps_a_sasl_report(_, _, _) ->
    {ignore, ignore, ignore, false}.

sasl_type_to_report_head(supervisor_report) ->
    "SUPERVISOR REPORT";
sasl_type_to_report_head(crash_report) ->
    "CRASH REPORT";
sasl_type_to_report_head(progress) ->
    ?PROGRESS_REPORT.

sasl_limited_str(supervisor_report, Report, FmtMaxBytes) ->
    Name = riak_err_stdlib:sup_get(supervisor, Report),
    Context = riak_err_stdlib:sup_get(errorContext, Report),
    Reason = riak_err_stdlib:sup_get(reason, Report),
    Offender = riak_err_stdlib:sup_get(offender, Report),
    FmtString = "     Supervisor: ~p~n     Context:    ~p~n     Reason:     "
        "~s~n     Offender:   ~s~n~n",
    {ReasonStr, _} = trunc_io:print(Reason, FmtMaxBytes),
    {OffenderStr, _} = trunc_io:print(Offender, FmtMaxBytes),
    io_lib:format(FmtString, [Name, Context, ReasonStr, OffenderStr]);
sasl_limited_str(progress, Report, FmtMaxBytes) ->
    [begin
         {Str, _} = trunc_io:print(Data, FmtMaxBytes),
         io_lib:format("    ~16w: ~s~n", [Tag, Str])
     end || {Tag, Data} <- Report];
sasl_limited_str(crash_report, Report, FmtMaxBytes) ->
    riak_err_stdlib:proc_lib_format(Report, FmtMaxBytes).

get_int_env(Name, Default) ->
    to_integer(get_env_or_arg(Name, Default)).

get_env_or_arg(Name, Default) ->
    case application:get_env(riak_err, Name) of
        {ok, Val} ->
            Val;
        _ ->
            get_env_or_arg2(Name, Default)
    end.            

get_env_or_arg2(Name, Default) when is_atom(Name) ->
    get_env_or_arg2(atom_to_list(Name), Default);
get_env_or_arg2(Name, Default) ->
    case init:get_argument(riak_err) of
        {ok, ListOfLists} ->
            find_in_pairs(lists:append(ListOfLists), Name, Default);
        error ->
            Default
    end.

find_in_pairs([Key, Value|_], Key, _Default) ->
    Value;
find_in_pairs([_K, _V|Tail], Key, Default) ->
    find_in_pairs(Tail, Key, Default);
find_in_pairs(_, _, Default) ->
    Default.

to_integer(X) when is_list(X) ->
    list_to_integer(X);
to_integer(X) when is_integer(X) ->
    X.

open_log_file(Path) ->
    {ok, FH} = file:open(Path, [append, raw, binary]),
    FH.

should_log_it(progress, _, ?PROGRESS_REPORT) ->
    true;
should_log_it(progress, _, _) ->
    false;
should_log_it(error, ErrorP, _) ->
    ErrorP;
should_log_it(all, _, _) ->
    true.

is_sasl_reportstr(?SUPERVISOR_REPORT) ->
    true;
is_sasl_reportstr(?CRASH_REPORT) ->
    true;
is_sasl_reportstr(?PROGRESS_REPORT) ->
    true;
is_sasl_reportstr(_) ->
    false.

-ifdef(TEST).

format_event_test() ->
    Nd = node(),
    P = self(),
    Own = [{own, foo}, {bar, baz}],
    Link = [{link, foo2}, {bar2, baz2}],

    {false, "ODD REPORT", _} = fe_wrap({foo, asdf, asdf}, 1024, 1024),
    {true, "ERROR REPORT", _} =
        fe_wrap({error, Nd, {P, "~s", ["!"]}},
                1024, 1024),
    {true, "ERROR REPORT", _} =
        fe_wrap({emulator, Nd, "!"},
                1024, 1024),
    {true, "ERROR REPORT", _} =
        fe_wrap({error_report, Nd, {P, std_error, ["report!"]}},
                1024, 1024),
    {true, ?SUPERVISOR_REPORT, _} =
        fe_wrap({error_report, Nd, {P, supervisor_report, ["report!"]}},
                1024, 1024),
    {true, ?CRASH_REPORT, _} =
        fe_wrap({error_report, Nd, {P, crash_report, [Own, Link]}},
                1024, 1024),
    {false, _, _} =
        fe_wrap({error_report, Nd, {P, someone_else_report, ["report!"]}},
                1024, 1024),
    {false, "INFO REPORT", _} =
        fe_wrap({info_report, Nd, {P, std_info, ["report!"]}},
                1024, 1024),
    {false, ?PROGRESS_REPORT, _} =
        fe_wrap({info_report, Nd, {P, progress, ["report!"]}},
                1024, 1024),
    {false, _, _} =
        fe_wrap({info_report, Nd, {P, someone_else_report, ["report!"]}},
                1024, 1024),
    {false, "INFO REPORT", _} =
        fe_wrap({info_msg, Nd, {P, "~s", ["!"]}},
                1024, 1024),
    {true, "WARNING REPORT", _} =
        fe_wrap({warning_report, Nd, {P, std_warning, ["report!"]}},
                1024, 1024),
    {false, _, _} = 
        fe_wrap({error_report, Nd, {P, someone_else_report, ["report!"]}},
                1024, 1024),
    {true, "WARNING REPORT", _} =
        fe_wrap({warning_msg, Nd, {P, "~s", ["!"]}},
                1024, 1024),
    {false, "ODD REPORT", _} =
        fe_wrap({moo},
                1024, 1024),

    %% Check that the truncation flavors are all working
    Word = "YOYOYO",
    Atom = foo,
    LongStr = "Here's a big string with a sentinel word at the end: " ++ Word,
    {true, _, Str1} = fe_wrap({error, Nd, {P, "atom ~p, string ~s\n",
                                           [Atom, LongStr]}},
                              1024, 1024),
    nomatch = re:run(Str1, "Oversize"),
    {match, _} = re:run(Str1, Word),
    {true, _, Str2} = fe_wrap({error, Nd, {P, "atom ~p, string ~s\n",
                                           [Atom, LongStr]}},
                              20, 1024),
    {match, _} = re:run(Str2, "Oversize"),
    {match, _} = re:run(Str2, Word),
    {true, _, Str3} = fe_wrap({error, Nd, {P, "atom ~p, string ~s\n",
                                           [Atom, LongStr]}},
                              20, 20),
    {match, _} = re:run(Str3, "Oversize"),
    nomatch = re:run(Str3, Word),
    {true, _, Str4} = fe_wrap({error, Nd, {P, "atom ~p, string ~s\n",
                                           [Atom, LongStr]}},
                              1024, 20),
    nomatch = re:run(Str4, "Oversize"),
    {match, _} = re:run(Str4, Word),

    %% [io:format(user, "Res: ~s\n", [XX]) || XX <- [Str1, Str2, Str3, Str4]],
    ok.

fe_wrap(A, B, C) ->
    {X, Y, Z} = format_event(A, B, C),
    {X, Y, lists:flatten(Z)}.

-endif. % TEST
