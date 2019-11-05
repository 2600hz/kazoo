%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-, Jeremy Raymond
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author Jeremy Raymond <jeraymond@gmail.com>
%%%
%%% @doc The amqp_cron_task module provides different methods for scheduling
%%% a task to be executed periodically in the future. The supported methods
%%% are one shot, sleeper, and cron mode.
%%%
%%% A one-shot schedule executes a task once after sleeping a specified
%%% number of milliseconds or at a given datetime.
%%%
%%% <code>
%%% {oneshot, 60000} % execute task once after waiting a minute<br />
%%% {oneshot, {{2012, 2, 23}, {1, 0, 0}}} % execute task on Feb 23, 2012 at 1 am
%%% </code>
%%%
%%% A sleeper mode schedule repeatedly executes a task then sleeps for a
%%% specified number of milliseconds before repeating the task.
%%%
%%% <code>{sleeper, 5000} % execute task then wait 5 seconds before the
%%% next execution</code>
%%%
%%% A cron mode schedule acts similarly to Unix cron. The schedule is
%%% defined by the cron tuple
%%%
%%% <code>{cron, {Minute, Hour, DayOfMonth, Month, DayOfWeek}}</code>
%%%
%%% The valid range of values for these fields are
%%%
%%% <pre>
%%% Field         Valid Range
%%% ------------  -------------------
%%% minute        0 - 59
%%% hour          0 - 23
%%% day of month  1 - 31
%%% month         1 - 12
%%% day of week   0 - 6 (Sunday is 0) </pre>
%%%
%%% The semantics of these fields align with Unix cron. Each field
%%% specifies which values in the range are valid for task execution. The
%%% values can be given as a range, a list or the atom 'all'.
%%%
%%% <pre>
%%% Field Spec                     Example            Unix Cron
%%% -----------------------------  -----------------  ---------
%%% all                            all                *
%%% {integer(), integer{}}         {1, 5}             1-5
%%% [integer()]                    [1, 3, 7]          1,3,7
%%%
%%% # old range and list format is also supported
%%% {range, integer(), integer()}  {range, 1, 5}      1-5
%%% {list, [integer()]}            {list, [1, 3, 7]}  1,3,7</pre>
%%%
%%% If the day of month is set to a day which does not exist in the current
%%% month (such as 31 for February) the day is skipped. Setting day of month
%%% to 31 does _not_ mean the last day of the month. This aligns with Unix
%%% cron.
%%%
%%% Specified dates and times are all handled in UTC.
%%%
%%% When a task takes longer than the time to the next valid period (or
%%% periods) the overlapped periods are skipped.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_cron_task).
-behaviour(gen_server).

%% API
-export([start_link/2, status/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([sleeper/0, cron/0, execargs/0, status/0, schedule/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(SERVER, ?MODULE).

-record(state, {schedule :: schedule()
               ,exec :: execargs()
               ,task_pid :: pid()
               ,status :: status() | 'undefined'
               ,next
               }).
-type state() :: #state{}.

-define(DAY_IN_SECONDS, 86400).
-define(HOUR_IN_SECONDS, 3600).
-define(MINUTE_IN_SECONDS, 60).

-type schedule() :: oneshot() | sleeper() | cron().
%% A cron schedule.

-type oneshot() :: {'oneshot', Millis::pos_integer() | kz_time:datetime()}.
%% Schedule a task once after a delay or on a particular date.

-type sleeper() :: {'sleeper', Millis::pos_integer()}.
%% Repeating schedule sleeping between executions.

-type cron() :: {'cron', {Minute :: cronspec()
                         ,Hour :: cronspec()
                         ,DayOfMonth :: cronspec()
                         ,Month :: cronspec()
                         ,DayOfWeek :: cronspec()
                         }}.
%% Unix like cron schedule representing the five cron fields:
%% minute, hour, day of month, month, day of week.

-type cronspec() :: 'all' | [rangespec() | listspec()].
%% Cron field value. Atom all for all values (e.g. *) or one of rangespec()
%% or listspec().

-type rangespec() :: {'range', Min :: integer(), Max :: integer()}
                   | {Min :: integer(), Max :: integer()}.
%% Represents a cron range (e.g. 1-5).

-type listspec() :: {'list', Values :: [integer()]} | [integer()] | integer().
%% Represents a cron list (e.g. 1,3,7)

-type status() :: 'waiting' | 'running' | 'done' | 'error'.
%% Task execution status.

-type execargs() :: mfargs() | funcargs().
%% Task execution type.

-type mfargs() :: {Module :: atom(), Function :: atom(), Args :: [term()]}.
%% Function execution definition.

-type funcargs() :: {Function :: fun(), Args :: [term()]}.
%% Anonymous function execution definition.

%% Date and time.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates a linked process which schedules the function in the
%% specified module with the given arguments to be run according
%% to the given schedule.
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_link(Schedule, Exec) -> kz_types:startlink_ret() when
      Schedule :: schedule(),
      Exec :: execargs().

start_link(Schedule, Exec) ->
    gen_server:start_link(?SERVER, [{Schedule, Exec}], []).

%%------------------------------------------------------------------------------
%% @doc Gets the current status of the task and the trigger time. If running
%% the trigger time denotes the time the task started. If waiting the
%% time denotes the next time the task will run. If done the time the
%% task ran. If error the cause of the error.
%%
%% @end
%%------------------------------------------------------------------------------
-spec status(pid()) -> {Status, ScheduleTime, TaskPid} when
      Status :: status(),
      ScheduleTime :: kz_time:datetime() | pos_integer() | {'error', Reason},
      Reason :: any(),
      TaskPid :: pid().

status(Pid) ->
    gen_server:call(Pid, 'status').

%%------------------------------------------------------------------------------
%% @doc Stops the task.
%% @end
%%------------------------------------------------------------------------------
-spec stop(pid()) -> 'ok'.
stop(Pid) ->
    gen_server:cast(Pid, 'stop').

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([{schedule(), execargs()}]) -> {'ok', state()}.
init([{Schedule, Exec}]) ->
    Self = self(),
    Pid = spawn_link(fun() ->
                             case Schedule of
                                 {'oneshot', _} ->
                                     oneshot(Schedule, Exec, Self);
                                 _ ->
                                     run_task(Schedule, Exec, Self)
                             end
                     end),
    {'ok', #state{schedule = Schedule
                 ,exec = Exec
                 ,task_pid = Pid
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('status', _From, State) ->
    Status = State#state.status,
    Next = State#state.next,
    TaskPid = State#state.task_pid,
    {'reply', {Status, Next, TaskPid}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'error', Message}, State) ->
    {'noreply', State#state{status = 'error', next = Message}};
handle_cast({'done', Schedule}, State) ->
    {'noreply', State#state{status = 'done', next = Schedule}};
handle_cast({'waiting', NextValidDateTime}, State) ->
    {'noreply', State#state{status = 'waiting', next = NextValidDateTime}};
handle_cast({'running', NextValidDateTime}, State) ->
    {'noreply', State#state{status = 'running', next = NextValidDateTime}};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, State) ->
    exit(State#state.task_pid, 'kill'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
oneshot({'oneshot', Millis}, Exec, ParentPid) when is_integer(Millis) ->
    gen_server:cast(ParentPid, {'waiting', Millis}),
    sleep_accounting_for_max(Millis),
    gen_server:cast(ParentPid, {'running', Millis}),
    apply_task(Exec),
    gen_server:cast(ParentPid, {'done', Millis});
oneshot({'oneshot', DateTime}, Exec, ParentPid) ->
    CurrentDateTime = calendar:universal_time(),
    CurrentSeconds = calendar:datetime_to_gregorian_seconds(CurrentDateTime),
    ScheduleSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    WaitSeconds = ScheduleSeconds - CurrentSeconds,
    case WaitSeconds > 0 of
        'true' ->
            gen_server:cast(ParentPid, {'waiting', DateTime}),
            sleep_accounting_for_max(WaitSeconds * 1000),
            gen_server:cast(ParentPid, {'running', DateTime}),
            apply_task(Exec),
            gen_server:cast(ParentPid, {'done', DateTime});
        'false' ->
            Format = "Schedule datetime ~p is in the past",
            Message = lists:flatten(io_lib:format(Format, [DateTime])),
            error_logger:error_report(Message),
            gen_server:cast(ParentPid, {'error', Message})
    end.

run_task({'sleeper', Millis} = Sleeper, Exec, ParentPid) ->
    gen_server:cast(ParentPid, {'running', Millis}),
    apply_task(Exec),
    gen_server:cast(ParentPid, {'waiting', Millis}),
    sleep_accounting_for_max(Millis),
    run_task(Sleeper, Exec, ParentPid);
run_task(Schedule, Exec, ParentPid) ->
    CurrentDateTime = calendar:universal_time(),
    NextValidDateTime = next_valid_datetime(Schedule, CurrentDateTime),
    SleepFor = time_to_wait_millis(CurrentDateTime, NextValidDateTime),
    gen_server:cast(ParentPid, {'waiting', NextValidDateTime}),
    sleep_accounting_for_max(SleepFor),
    gen_server:cast(ParentPid, {'running', NextValidDateTime}),
    apply_task(Exec),
    run_task(Schedule, Exec, ParentPid).

-spec apply_task(execargs()) -> any().
apply_task(Exec) ->
    try
        case Exec of
            {M, F, A} ->
                apply(M, F, A);
            {F, A} ->
                apply(F, A)
        end
    catch
        ?STACKTRACE(Error, Reason, ST)
        Format = "Task ~p in process ~p with value:~n~p",
        Message = lists:flatten(
                    io_lib:format(Format, [Error, self(), {Reason, ST}])
                   ),
        error_logger:error_report(Message)
        end.

-spec time_to_wait_millis(kz_time:datetime(), kz_time:datetime()) -> integer().
time_to_wait_millis(CurrentDateTime, NextDateTime) ->
    CurrentSeconds = calendar:datetime_to_gregorian_seconds(CurrentDateTime),
    NextSeconds = calendar:datetime_to_gregorian_seconds(NextDateTime),
    SecondsToSleep = NextSeconds - CurrentSeconds,
    SecondsToSleep * 1000.

-spec next_valid_datetime(cron(), kz_time:datetime()) -> kz_time:datetime().
next_valid_datetime({'cron', _Schedule} = Cron, DateTime) ->
    DateTime1 = advance_seconds(DateTime, ?MINUTE_IN_SECONDS),
    {{Y, Mo, D}, {H, M, _}} = DateTime1,
    DateTime2 = {{Y, Mo, D}, {H, M, 0}},
    next_valid_datetime('not_done', Cron, DateTime2).

-spec next_valid_datetime('done' | 'not_done', cron(), kz_time:datetime()) -> kz_time:datetime().
next_valid_datetime('done', _, DateTime) ->
    DateTime;
next_valid_datetime('not_done', {'cron', Schedule} = Cron, DateTime) ->
    {MinuteSpec, HourSpec, DayOfMonthSpec, MonthSpec, DayOfWeekSpec} = Schedule,
    {{Year, Month, Day},  {Hour, Minute, _}} = DateTime,
    {Done, Time} =
        case value_valid(MonthSpec, 1, 12, Month) of
            'false' ->
                case Month of
                    12 ->
                        {'not_done', {{Year + 1, 1, 1}, {0, 0, 0}}};
                    Month ->
                        {'not_done', {{Year, Month + 1, 1}, {0, 0, 0}}}
                end;
            'true' ->
                DayOfWeek = case calendar:day_of_the_week(Year, Month, Day) of
                                7 ->
                                    0; % we want 0 to be Sunday not 7
                                DOW ->
                                    DOW
                            end,
                DOMValid = value_valid(DayOfMonthSpec, 1, 31, Day),
                DOWValid = value_valid(DayOfWeekSpec, 0, 6, DayOfWeek),
                case (((DayOfMonthSpec =/= 'all')
                       andalso
                         (DayOfWeekSpec =/= 'all')
                       andalso
                         (DOMValid
                          orelse DOWValid))
                      orelse (DOMValid
                              andalso DOWValid))
                of
                    'false' ->
                        Temp1 = advance_seconds(DateTime, ?DAY_IN_SECONDS),
                        {{Y, M, D}, {_, _, _}} = Temp1,
                        {'not_done', {{Y, M, D}, {0, 0, 0}}};
                    'true' ->
                        case value_valid(HourSpec, 0, 23, Hour) of
                            'false' ->
                                Temp3 = advance_seconds(DateTime, ?HOUR_IN_SECONDS),
                                {{Y, M, D}, {H, _, _}} = Temp3,
                                {'not_done', {{Y, M, D}, {H, 0, 0}}};
                            'true' ->
                                case value_valid(MinuteSpec, 0, 59, Minute) of
                                    'false' ->
                                        {'not_done', advance_seconds(DateTime, ?MINUTE_IN_SECONDS)};
                                    'true' ->
                                        {'done', DateTime}
                                end
                        end
                end
        end,
    next_valid_datetime(Done, Cron, Time).

-spec value_valid(cronspec(), integer(), integer(), integer()) -> boolean().
value_valid(Spec, Min, Max, Value) when Value >= Min, Value =< Max->
    case Spec of
        'all' -> 'true';
        Spec ->
            ValidValues = extract_integers(Spec, Min, Max),
            lists:any(fun(Item) ->
                              Item == Value
                      end, ValidValues)
    end.

-spec advance_seconds(kz_time:datetime(), integer()) -> kz_time:datetime().
advance_seconds(DateTime, Seconds) ->
    Seconds1 = calendar:datetime_to_gregorian_seconds(DateTime) + Seconds,
    calendar:gregorian_seconds_to_datetime(Seconds1).

-spec extract_integers([rangespec()|listspec()], integer(), integer()) ->
                              [integer()].
extract_integers(Spec, Min, Max) when Min < Max ->
    extract_integers(Spec, Min, Max, []).

-spec extract_integers(Spec, Min, Max, Acc) -> Integers when
      Spec :: [rangespec()|listspec()],
      Min :: integer(),
      Max :: integer(),
      Acc :: list(),
      Integers :: [integer()].
extract_integers([], Min, Max, Acc) ->
    Integers = lists:sort(sets:to_list(sets:from_list(lists:flatten(Acc)))),
    lists:foreach(
      fun
          (Int) when Int < Min ->
                         throw({'error', {'out_of_range', {'min', Min}, {'value', Int}}});
          (Int) when Int > Max ->
                         throw({'error', {'out_of_range', {'max', Max}, {'value', Int}}});
          (_Int) ->
                         'ok'
                 end, Integers),
    Integers;
extract_integers(Spec, Min, Max, Acc) ->
    [H|T] = Spec,
    Values = case H of
                 {'range', Lower, Upper} when Lower < Upper ->
                     lists:seq(Lower, Upper);
                 {'list', List} ->
                     List;
                 {Lower, Upper} when Lower < Upper ->
                     lists:seq(Lower, Upper);
                 List when is_list(List) ->
                     List;
                 Integer when is_integer(Integer) ->
                     [Integer]
             end,
    extract_integers(T, Min, Max, [Values|Acc]).

-define(LONG_SLEEP_TIME, 100000000).

sleep_accounting_for_max(TimeInMillis) ->
    case TimeInMillis > ?LONG_SLEEP_TIME of
        'false' -> timer:sleep(TimeInMillis);
        'true' ->
            timer:sleep(TimeInMillis rem ?LONG_SLEEP_TIME),
            long_sleep(TimeInMillis div ?LONG_SLEEP_TIME)
    end.

long_sleep(0) -> 'ok';
long_sleep(Chunks) ->
    timer:sleep(?LONG_SLEEP_TIME),
    long_sleep(Chunks - 1).
