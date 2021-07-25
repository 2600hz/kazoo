%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2021-, Ooma Inc.
%%% @doc Allows tracing strategy state changes in acdc_queue_manager
%%%
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_manager_diag).
-behaviour(gen_server).

%% API
-export([start_link/3
        ,stop/1
        ]).
-export([send_diagnostics/2]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("acdc.hrl").

-define(MESSAGE_LINE_LENGTH, 100).

-record(state, {account_id            :: kz_term:ne_binary()
               ,queue_id              :: kz_term:ne_binary()
               ,manager               :: kz_term:api_pid()
               ,observer              :: kz_term:api_pid()
               ,observer_group_leader :: kz_term:api_pid()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_term:ne_binary(), kz_term:ne_binary(), pid()) -> kz_types:startlink_ret().
start_link(AccountId, QueueId, Observer) ->
    gen_server:start_link(?MODULE, {AccountId, QueueId, Observer}, []).

%%------------------------------------------------------------------------------
%% @doc Stops the server
%% @end
%%------------------------------------------------------------------------------
-spec stop(kz_types:server_ref()) -> 'ok'.
stop(Srv) ->
    gen_server:call(Srv, 'stop').

%%------------------------------------------------------------------------------
%% @doc Send a diagnostics message to `Srv'
%% @end
%%------------------------------------------------------------------------------
-spec send_diagnostics(kz_types:server_ref(), iolist()) -> 'ok'.
send_diagnostics(Srv, Message) ->
    gen_server:cast(Srv, {'send_diagnostics', Message}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init({kz_term:ne_binary(), kz_term:ne_binary(), pid()}) -> {'ok', state()} | {'stop', any()}.
init({AccountId, QueueId, Observer}) ->
    kz_util:put_callid(<<AccountId/binary, "-", QueueId/binary, "-", (kz_term:to_binary(Observer))/binary>>),

    InitFuns = [{fun set_up_group_leader/1, [Observer], #state.observer_group_leader}
               ,{fun monitor_manager/2, [AccountId, QueueId], #state.manager}
               ,{fun print_header/0, []}
               ],
    case lists:foldl(fun init_fold/2, [], InitFuns) of
        {'error', _}=E -> {'stop', E};
        StateAssignments ->
            lager:debug("started"),

            {'ok', lists:foldl(fun({Index, Value}, State) -> setelement(Index, State, Value) end
                              ,#state{account_id=AccountId
                                     ,queue_id=QueueId
                                     }
                              ,StateAssignments
                              )}
    end.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('stop', _From, State) ->
    {'stop', 'normal', 'ok', State};

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
    {'reply', 'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'send_diagnostics', Message}, State) ->
    print(lists:flatten(Message)),
    {'noreply', State};

handle_cast(_Message, State) ->
    lager:debug("unhandled cast: ~p", [_Message]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', _, 'process', Manager, Reason}, #state{manager=Manager}=State) ->
    %% Stop the diagnostics process if the manager goes down
    print(io_lib:format("manager pid ~p went down (~p)", [Manager, Reason])),
    {'stop', 'normal', State};
handle_info({'DOWN', _, 'process', Pid, Reason}, #state{observer=Observer
                                                       ,observer_group_leader=GroupLeader
                                                       }=State) when Observer =:= Pid; GroupLeader =:= Pid ->
    %% Stop the diagnostics process if the observer goes down to avoid wasting resources for
    %% diagnostics
    lager:debug("observer pid ~p went down (~p)", [Pid, Reason]),
    {'stop', 'normal', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{manager=Manager}) ->
    catch acdc_queue_manager:remove_diagnostics_receiver(Manager, self()),

    catch print(io_lib:format("stopping diagnostics: ~p", [_Reason])),
    lager:debug("terminating: ~p", [_Reason]).

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
%% @doc Fold over a list of initialization funs, short-circuiting to return an
%% error if one occurs during any of those funs.
%% @end
%%------------------------------------------------------------------------------
init_fold(_, {'error', _}=E) -> E;
init_fold({Fun, Args}, StateAssignments) ->
    apply(Fun, Args),
    StateAssignments;
init_fold({Fun, Args, StateAssignment}, StateAssignments) ->
    case apply(Fun, Args) of
        {'error', _}=E -> E;
        InitFunResult ->
            [{StateAssignment, InitFunResult} | StateAssignments]
    end.

%%------------------------------------------------------------------------------
%% @doc Set the group leader of the diagnostics process to the same one as the
%% observer and monitor both the observer and group leader processes.
%% @end
%%------------------------------------------------------------------------------
set_up_group_leader(Observer) ->
    case get_observer_group_leader(Observer) of
        'undefined' -> {'error', 'unknown_group_leader'};
        GroupLeader ->
            group_leader(GroupLeader, self()),
            monitor('process', Observer),
            monitor('process', GroupLeader),
            GroupLeader
    end.

%%------------------------------------------------------------------------------
%% @doc Find the `acdc_queue_manager' process for the queue to be observed and
%% monitor it. Also inform the manager process that it should send diagnostics
%% to this process.
%% @end
%%------------------------------------------------------------------------------
monitor_manager(AccountId, QueueId) ->
    case acdc_queues_sup:find_queue_supervisor(AccountId, QueueId) of
        'undefined' -> {'error', 'manager_process_not_found'};
        QueueSup ->
            Manager = acdc_queue_sup:manager(QueueSup),
            monitor_manager(Manager)
    end.

%%------------------------------------------------------------------------------
%% @doc Inform a manager process that it should send diagnostics to this
%% process.
%% @end
%%------------------------------------------------------------------------------
monitor_manager(Manager) ->
    try acdc_queue_manager:add_diagnostics_receiver(Manager, self()) of
        'ok' ->
            monitor('process', Manager),
            Manager
    catch
        'error':E -> {'error', {E, erlang:get_stacktrace()}}
    end.

%%------------------------------------------------------------------------------
%% @doc Get the group leader of the process identified by `Observer'.
%% @end
%%------------------------------------------------------------------------------
-spec get_observer_group_leader(pid()) -> kz_term:api_pid().
get_observer_group_leader(Observer) ->
    case process_info(Observer, 'group_leader') of
        'undefined' -> 'undefined';
        {'group_leader', GroupLeader} -> GroupLeader
    end.

%%------------------------------------------------------------------------------
%% @doc Print the diagnostics output column headers to the console.
%% @end
%%------------------------------------------------------------------------------
-spec print_header() -> 'ok'.
print_header() ->
    Format = create_format_of_length(?MESSAGE_LINE_LENGTH, "|  Timestamp  | ~-{{lineLength}}s |~n"),
    io:format(Format, ["Message"]),
    print_separator().

%%------------------------------------------------------------------------------
%% @doc Print a diagnostics output message row separator to the console.
%% @end
%%------------------------------------------------------------------------------
-spec print_separator() -> 'ok'.
print_separator() ->
    Format = create_format_of_length(?MESSAGE_LINE_LENGTH, "|-------------|-~-{{lineLength}}s-|~n"),
    io:format(Format
             ,[lists:foldl(fun(_, Acc) -> "-" ++ Acc end, "", lists:seq(1, ?MESSAGE_LINE_LENGTH))]
             ).

%%------------------------------------------------------------------------------
%% @doc Print the specified diagnostics message to the console.
%% @end
%%------------------------------------------------------------------------------
-spec print(string()) -> 'ok'.
print(Message) ->
    print(Message, 'true').

-spec print(string(), boolean()) -> 'ok'.
print("", _) -> print_separator();
print(Message, ShouldPrintTimestamp) ->
    case ShouldPrintTimestamp of
        'true' -> io:format("| ~b ", [kz_time:current_tstamp()]);
        'false' -> io:format("|             ")
    end,
    {Part, Rest} = split_next_part(Message),
    Format = create_format_of_length(?MESSAGE_LINE_LENGTH, "| ~-{{lineLength}}s |~n"),
    io:format(Format, [Part]),
    print(Rest, 'false').

%%------------------------------------------------------------------------------
%% @doc Split the diagnostics message into a chunk that can fit on the current
%% line of the console and the rest of the message.
%% @end
%%------------------------------------------------------------------------------
-spec split_next_part(string()) -> {string(), string()}.
split_next_part(Message) ->
    split_next_part(Message, "", 'false', ?MESSAGE_LINE_LENGTH).

-spec split_next_part(string(), string(), boolean(), non_neg_integer()) -> {string(), string()}.
split_next_part("", Part, _, _) -> {Part, ""};
split_next_part(Rest, Part, 'true', 0) -> {Part, Rest};
split_next_part(Rest, _, 'false', 0) ->
    %% Blank line, ignore
    split_next_part(Rest);
split_next_part([$\n|Rest], Part, _, _) -> {Part, Rest};
split_next_part([Ch|Rest], Part, IsNotBlankLine, RemainingCharCount) ->
    IsNotBlankLine1 = IsNotBlankLine
        orelse Ch =/= 32,
    split_next_part(Rest, [Part, Ch], IsNotBlankLine1, RemainingCharCount - 1).

%%------------------------------------------------------------------------------
%% @doc Replace `{{lineLength}}' placeholders in a format string with the
%% defined diagnostics log line length.
%% @end
%%------------------------------------------------------------------------------
-spec create_format_of_length(pos_integer(), string()) -> string().
create_format_of_length(Length, Format) ->
    re:replace(Format, "{{lineLength}}", kz_term:to_binary(Length), [{'return', 'list'}]).
