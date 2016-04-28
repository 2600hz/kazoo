-module(eflame2).

%% Public API
-export([%% 1st phase: generate a binary trace
         write_trace/4, write_trace/6,
         %% The following forms used with experimental VM only!
         write_trace_exp/4, write_trace_exp/6,

         %% 2nd phase: convert binary trace to an ASCII trace
         format_trace/1, format_trace/2]).
-export([help/0, custom_trace_flags/0]).
-compile(export_all). %% SLF debugging

-record(state, {
          output_path="",
          pid,
          last_ts,
          count=0,
          acc=[]}). % per-process state

%% For help & use recommendations, run help().

write_trace(Mode, BinaryFile, PidSpec, SleepMSecs) when is_number(SleepMSecs) ->
    write_trace(Mode, BinaryFile, PidSpec, timer, sleep, [SleepMSecs]).

write_trace(Mode, BinaryFile, PidSpec, M, F, A) ->
    write_trace2(normal, Mode, BinaryFile, PidSpec, M, F, A).

write_trace_exp(Mode, BinaryFile, PidSpec, SleepMSecs) when is_number(SleepMSecs) ->
    write_trace_exp(Mode, BinaryFile, PidSpec, timer, sleep, [SleepMSecs]).

write_trace_exp(Mode, BinaryFile, PidSpec, M, F, A) ->
    write_trace2(backtrace, Mode, BinaryFile, PidSpec, M, F, A).

write_trace2(Style, Mode, BinaryFile, PidSpec, M, F, A) ->
    {ok, Tracer} = start_tracer(BinaryFile),
    io:format(user, "Tracer ~p\n", [Tracer]),
    io:format(user, "self() ~p\n", [self()]),

    start_trace(Style, Tracer, PidSpec, Mode),
    Return = (catch erlang:apply(M, F, A)),
    stop_trace(Tracer, PidSpec),

    %% ok = file:write_file(OutputFile, Bytes),
    Return.

start_trace(normal, _Tracer, PidSpec, Mode) ->
    MatchSpec = [{'_',[],[{message,{process_dump}}]}],
    start_trace2(_Tracer, PidSpec, Mode, MatchSpec);
start_trace(backtrace, _Tracer, PidSpec, Mode) ->
    io:format("\n\nYEAH, using the new hackery!\n\n"),
    MatchSpec = [{'_',[],[{message,{process_backtrace}}]}],
    start_trace2(_Tracer, PidSpec, Mode, MatchSpec).

start_trace2(_Tracer, PidSpec, Mode, MatchSpec) ->
    Verb = case is_list(Mode) andalso lists:member(global_and_local_calls, Mode) of
               true ->
                   {v_global_and_local_calls, dbg:tpl('_', '_', MatchSpec)};
               false ->
                   if Mode == 'global_and_local_calls' orelse
                      Mode == 'global_and_local_calls_plus_new_procs'  ->
                           {v_global_and_local_calls, dbg:tpl('_', '_', MatchSpec)};
                      true ->
                           {v_global_calls_only, dbg:tp('_', '_', MatchSpec)}
                   end
           end,
    io:format("Starting tracer results: ~p\n", [Verb]),
    if is_list(PidSpec) ->
            [dbg:p(PS, trace_flags(Mode)) || PS <- PidSpec];
       true ->
            dbg:p(PidSpec, trace_flags(Mode))
    end,
    ok.

stop_trace(Tracer, _PidSpec) ->
    _X00 = dbg:flush_trace_port(),
    (catch dbg:ctp()),
    (catch dbg:stop()),
    (catch dbg:stop_clear()),
    (exit(Tracer, normal)),
    ok.

format_trace(BinaryFile) ->
    format_trace(BinaryFile, BinaryFile ++ ".out").

format_trace(BinaryFile, OutFile) ->
    Acc = exp1_init(OutFile),
    dbg:trace_client(file, BinaryFile, {fun exp1/2, Acc}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exp0({trace_ts, Pid, call, {M,F,A}, BIN, _TS}, _Acc) ->
    Stak = lists:filter(fun("unknown function") -> false;
                           (_)                  -> true
                        end, stak(BIN)), % Thank you, Mats!
    MFA_str = lists:flatten(io_lib:format("~w:~w/~w", [M, F, A])),
    Total0 = Stak ++ [MFA_str],
    Total = stak_trim(Total0),
    io:format("~w;~s\n", [Pid, intercalate(";", Total)]);
exp0(end_of_trace, _Acc) ->
    io:format("End of trace found, hooray!\n");
exp0(Else, _Acc) ->
    io:format("?? ~P\n", [Else, 10]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exp1_init(OutputPath) ->
    #state{output_path=OutputPath}.

exp1(end_of_trace = _Else, #state{output_path=OutputPath} = OuterS) ->
    (catch erlang:delete(hello_world)),
    PidStates = get(),
    {ok, FH} = file:open(OutputPath, [write, raw, binary, delayed_write]),
    io:format("\n\nWriting to ~s for ~w processes... ", [OutputPath, length(PidStates)]),
    [
        [begin
             Pid_str0 = lists:flatten(io_lib:format("~w", [Pid])),
             Size = length(Pid_str0),
             Pid_str = [$(, lists:sublist(Pid_str0, 2, Size-2), $)],
             Time_str = integer_to_list(Time),
             file:write(FH, [Pid_str, $;, intersperse($;, lists:reverse(Stack)), 32, Time_str, 10])
         end || {Stack, Time} <- Acc]
     || {Pid, #state{acc=Acc} = _S} <- PidStates],
    file:close(FH),
    io:format("finished!\n"),
    OuterS;
exp1(T, #state{output_path=OutputPath} = S) ->
    trace_ts = element(1, T),
    Pid = element(2, T),
    PidState = case erlang:get(Pid) of
                   undefined ->
                       io:format("~p ", [Pid]),
                       #state{output_path=OutputPath};
                   SomeState ->
                       SomeState
               end,
    NewPidState = exp1_inner(T, PidState),
    erlang:put(Pid, NewPidState),
    S.

exp1_inner({trace_ts, _Pid, InOut, _MFA, _TS}, #state{last_ts=undefined} = S)
  when InOut == in; InOut == out ->
    exp1_hello_world(),
    %% in & out, without call context, don't help us
    S;
exp1_inner({trace_ts, _Pid, Return, _MFA, _TS}, #state{last_ts=undefined} = S)
  when Return == return_from; Return == return_to ->
    exp1_hello_world(),
    %% return_from and return_to, without call context, don't help us
    S;
exp1_inner({trace_ts, Pid, call, MFA, BIN, TS},
     #state{last_ts=LastTS, acc=Acc, count=Count} = S) ->
  try
    exp1_hello_world(),
    %% Calculate time elapsed, TS-LastTs.
    %% 0. If Acc is empty, then skip step #1.
    %% 1. Credit elapsed time to the stack on the top of Acc.
    %% 2. Push a 0 usec item with this stack onto Acc.
    Stak = lists:filter(fun(<<"unknown function">>) -> false;
                           (_)                      -> true
                        end, stak_binify(BIN)),
    Stack0 = stak_trim(Stak),
    MFA_bin = mfa_binify(MFA),
    Stack1 = [MFA_bin|lists:reverse(Stack0)],
    Acc2 = case Acc of
               [] ->
                   [{Stack1, 0}];
               [{LastStack, LastTime}|Tail] ->
                   USec = timer:now_diff(TS, LastTS),
%                   io:format("Stack1: ~p ~p\n", [Stack1, USec]),
                   [{Stack1, 0},
                    {LastStack, LastTime + USec}|Tail]
           end,
    %% TODO: more state tracking here.
    S#state{pid=Pid, last_ts=TS, count=Count+1, acc=Acc2}
  catch XX:YY ->
            io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, XX, YY, erlang:get_stacktrace()]),
            S
  end;
exp1_inner({trace_ts, _Pid, return_to, MFA, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Calculate time elapsed, TS-LastTs.
    %% 1. Credit elapsed time to the stack on the top of Acc.
    %% 2. Push a 0 usec item with the "best" stack onto Acc.
    %%    "best" = MFA exists in the middle of the stack onto Acc,
    %%    or else MFA exists at the top of a stack elsewhere in Acc.
    [{LastStack, LastTime}|Tail] = Acc,
    MFA_bin = mfa_binify(MFA),
    BestStack = lists:dropwhile(fun(SomeMFA) when SomeMFA /= MFA_bin -> true;
                                   (_)                               -> false
                                end, find_matching_stack(MFA_bin, Acc)),
    USec = timer:now_diff(TS, LastTS),
    Acc2 = [{BestStack, 0},
            {LastStack, LastTime + USec}|Tail],
%    io:format(user, "return-to: ~p\n", [lists:sublist(Acc2, 4)]),
    S#state{last_ts=TS, acc=Acc2}
  catch XX:YY ->
            io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, XX, YY, erlang:get_stacktrace()]),
            S
  end;
    
exp1_inner({trace_ts, _Pid, gc_start, _Info, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Push a 0 usec item onto Acc.
    [{LastStack, LastTime}|Tail] = Acc,
    NewStack = [<<"GARBAGE-COLLECTION">>|LastStack],
    USec = timer:now_diff(TS, LastTS),
    Acc2 = [{NewStack, 0},
            {LastStack, LastTime + USec}|Tail],
%    io:format(user, "GC 1: ~p\n", [lists:sublist(Acc2, 4)]),
    S#state{last_ts=TS, acc=Acc2}
  catch _XX:_YY ->
            %% io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, _XX, _YY, erlang:get_stacktrace()]),
            S
  end;
exp1_inner({trace_ts, _Pid, gc_end, _Info, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Push the GC time onto Acc, then push 0 usec item from last exec
    %% stack onto Acc.
    [{GCStack, GCTime},{LastExecStack,_}|Tail] = Acc,
    USec = timer:now_diff(TS, LastTS),
    Acc2 = [{LastExecStack, 0}, {GCStack, GCTime + USec}|Tail],
%    io:format(user, "GC 2: ~p\n", [lists:sublist(Acc2, 4)]),
    S#state{last_ts=TS, acc=Acc2}
  catch _XX:_YY ->
            %% io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, _XX, _YY, erlang:get_stacktrace()]),
            S
  end;

exp1_inner({trace_ts, _Pid, out, MFA, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Push a 0 usec item onto Acc.
    %% The MFA reported here probably doesn't appear in the stacktrace
    %% given to us by the last 'call', so add it here.
    [{LastStack, LastTime}|Tail] = Acc,
    MFA_bin = mfa_binify(MFA),
    NewStack = [<<"SLEEP">>,MFA_bin|LastStack],
    USec = timer:now_diff(TS, LastTS),
    Acc2 = [{NewStack, 0},
            {LastStack, LastTime + USec}|Tail],
    S#state{last_ts=TS, acc=Acc2}
  catch XX:YY ->
            io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, XX, YY, erlang:get_stacktrace()]),
            S
  end;
exp1_inner({trace_ts, _Pid, in, MFA, TS}, #state{last_ts=LastTS, acc=Acc} = S) ->
  try
    %% Push the Sleep time onto Acc, then push 0 usec item from last
    %% exec stack onto Acc.
    %% The MFA reported here probably doesn't appear in the stacktrace
    %% given to us by the last 'call', so add it here.
    MFA_bin = mfa_binify(MFA),
    [{SleepStack, SleepTime},{LastExecStack,_}|Tail] = Acc,
    USec = timer:now_diff(TS, LastTS),
    Acc2 = [{[MFA_bin|LastExecStack], 0}, {SleepStack, SleepTime + USec}|Tail],
    S#state{last_ts=TS, acc=Acc2}
  catch XX:YY ->
            io:format(user, "~p: ~p:~p @ ~p\n", [?LINE, XX, YY, erlang:get_stacktrace()]),
            S
  end;

exp1_inner(end_of_trace = _Else, #state{pid=Pid, output_path=OutputPath, acc=Acc} = S) ->
    {ok, FH} = file:open(OutputPath, [write, raw, binary, delayed_write]),
    io:format("Writing to ~s ... ", [OutputPath]),
    [begin
         Pid_str = io_lib:format("~w", [Pid]),
         Time_str = integer_to_list(Time),
         file:write(FH, [Pid_str, $;, intersperse($;, lists:reverse(Stack)), 32, Time_str, 10])
     end || {Stack, Time} <- Acc],
    file:close(FH),
    io:format("finished\n"),
    S;
exp1_inner(_Else, S) ->
    io:format("?? ~P\n", [_Else, 10]),
    S.

exp1_hello_world() ->
    case erlang:get(hello_world) of
        undefined ->
            io:format("Hello, world, I'm ~p and I'm running....\n", [self()]),
            erlang:put(hello_world, true);
        _ ->
            ok
    end.

find_matching_stack(MFA_bin, [{H,_Time}|_] = Acc) ->
    case lists:member(MFA_bin, H) of
        true ->
            H;
        false ->
            find_matching_stack2(MFA_bin, Acc)
    end.

find_matching_stack2(MFA_bin, [{[MFA_bin|_StackTail]=Stack,_Time}|_]) ->
    Stack;
find_matching_stack2(MFA_bin, [_H|T]) ->
    find_matching_stack2(MFA_bin, T);
find_matching_stack2(_MFA_bin, []) ->
    [<<"FIND-MATCHING-FAILED">>].    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_tracer(BinaryFile) ->
    dbg:tracer(port, dbg:trace_port(file, BinaryFile)).

help() ->
    io:format("
    Usage: 1st phase: Generate a binary trace

        ~s:write_trace(Mode, BinaryFile, PidSpec, SleepMSecs)
        ~s:write_trace(Mode, BinaryFile, PidSpec, M, F, A)
        %% The following forms used with experimental VM only!
        ~s:write_trace_exp(Mode, BinaryFile, PidSpec, SleepMSecs)
        ~s:write_trace_exp(Mode, BinaryFile, PidSpec, M, F, A)

    Mode = 'global_calls' | 'global_calls_plus_new_procs' |
           'global_and_local_calls' | 'global_and_local_calls_plus_new_procs' |
           erlang_trace_flags()
    erlang_trace_flags() = See OTP docs for erlang:trace() BIF

    PidSpec = 'all' | 'existing' | 'new' | pid() | [pid()]

    To trace current pid and execution of {M,F,A} only, we suggest:
        ~s:write_trace(Mode, BinaryFile, self(), M, F, A)

    To trace all pids and gather traces on all pids for some time, we suggest:
        ~s:write_trace(Mode, BinaryFile, PidSpec, SleepMSecs)

    Usage: 2nd phase: Convert a binary trace to an ASCII trace

        ~s:format_trace(BinaryFile).  % output = input ++ \".out\"
      or else
        ~s:format_trace(BinaryFile, OutputFile).

        Remember to wait for 'finished!' message!
", [?MODULE || _ <- lists:seq(1, 8)]).

trace_flags(Mode) when Mode == global_calls; Mode == global_and_local_calls ->
    [call, arity, return_to, timestamp, running];
trace_flags(Mode) when Mode == global_calls_plus_new_procs; Mode == global_and_local_calls_plus_new_procs ->
    [call, arity, return_to, timestamp, running, set_on_spawn];
trace_flags(Mode) when is_list(Mode) ->
    io:format(user, "\nWARNING: we assume that you know what you're doing "
              "when\nspecifying trace_flags(Mode=~w)\n", [Mode]),
    Mode -- custom_trace_flags().

custom_trace_flags() ->
    [global_and_local_calls, global_calls_only].

entry_to_iolist({M, F, A}) ->
    [atom_to_binary(M, utf8), <<":">>, atom_to_binary(F, utf8), <<"/">>, integer_to_list(A)];
entry_to_iolist(A) when is_atom(A) ->
    [atom_to_binary(A, utf8)].

intercalate(Sep, Xs) -> lists:concat(intersperse(Sep, Xs)).

intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].

stak_trim([<<"proc_lib:init_p_do_apply/3">>,<<"gen_fsm:decode_msg/9">>,<<"gen_fsm:handle_msg/7">>,<<"gen_fsm:loop/7">>|T]) ->
    stak_trim([<<"GEN-FSM">>|T]);
stak_trim([<<"GEN-FSM">>,<<"gen_fsm:decode_msg/9">>,<<"gen_fsm:handle_msg/7">>,<<"gen_fsm:loop/7">>|T]) ->
    stak_trim([<<"GEN-FSM">>|T]);
stak_trim(Else) ->
    Else.

stak_binify(Bin) when is_binary(Bin) ->
    [list_to_binary(X) || X <- stak(Bin)];
stak_binify(X) ->
    list_to_binary(io_lib:format("~w", [X])).

mfa_binify({M,F,A}) ->
    list_to_binary(io_lib:format("~w:~w/~w", [M, F, A]));
mfa_binify(X) ->
    list_to_binary(io_lib:format("~w", [X])).

%% Borrowed from redbug.erl

stak(Bin) ->
  lists:foldl(fun munge/2,[],string:tokens(binary_to_list(Bin),"\n")).

munge(I,Out) ->
  case I of %% lists:reverse(I) of
    "..."++_ -> ["truncated!!!"|Out];
    _ ->
      case string:str(I, "Return addr") of
        0 ->
          case string:str(I, "cp = ") of
            0 -> Out;
            _ -> [mfaf(I)|Out]
          end;
        _ ->
          case string:str(I, "erminate process normal") of
            0 -> [mfaf(I)|Out];
            _ -> Out
          end
      end
  end.

mfaf(I) ->
  [_, C|_] = string:tokens(I,"()+"),
  string:strip(C).
