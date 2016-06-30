%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%  Run tasks without CSV input file, scheduled by kz_tasks.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_task_noinput_worker).

%% API
-export([start/5]).

-include("kz_tasks.hrl").

-record(state, { task_id :: kz_tasks:task_id()
               , module :: module()
               , function :: atom()
               , fassoc :: kz_csv:fassoc()
               , extra_args :: kz_proplist()
               , total_failed = 0 :: non_neg_integer()
               , total_succeeded = 0 :: non_neg_integer()
               }).
-type state() :: #state{}.

-define(OUT(TaskId), <<"/tmp/task_out.", (TaskId)/binary, ".csv">>).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(kz_tasks:task_id(), module(), atom(), kz_proplist(), ne_binaries()) -> 'ok'.
start(TaskId, Module, Function, ExtraArgs, OrderedFields) ->
    _ = kz_util:put_callid(TaskId),
    case init(TaskId, Module, Function, ExtraArgs, OrderedFields) of
        {'ok', State} ->
            lager:debug("worker for ~s started", [TaskId]),
            loop('init', State);
        {'error', _R} ->
            kz_tasks:worker_error(TaskId),
            lager:debug("worker exiting now: ~p", [_R])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec init(kz_tasks:task_id(), module(), atom(), kz_proplist(), ne_binaries()) -> {'ok', state()} |
                                                                                  {'error', any()}.
init(TaskId, Module, Function, ExtraArgs, _) ->
    case
        kz_util:try_load_module(Module) =:= Module
        andalso write_output_csv_header(TaskId, Module, Function)
    of
        'false' ->
            lager:error("failed loading module '~p' for task ~s", [Module, TaskId]),
            {'error', 'badmodule'};
        {'error', _R}=Error ->
            lager:error("failed to write CSV header in ~s", [?OUT(TaskId)]),
            Error;
        'ok' ->
            State = #state{ task_id = TaskId
                          , module = Module
                          , function = Function
                          , extra_args = ExtraArgs
                          },
            {'ok', State}
    end.

%% @private
-spec loop(task_iterator(), state()) -> any().
loop(IterValue, State=#state{task_id = TaskId
                            ,module = Module
                            ,function = Function
                            ,extra_args = ExtraArgs
                            ,total_failed = TotalFailed
                            ,total_succeeded = TotalSucceeded
                            }) ->
    case is_task_successful(TaskId, Module, Function, ExtraArgs, IterValue) of
        'stop' ->
            TaskRev = upload_output(TaskId),
            _ = kz_tasks:worker_finished(TaskId, TaskRev, TotalSucceeded, TotalFailed),
            'stop';
        {IsSuccessful, Written, {_PrevRow, NewIterValue}} ->
            NewState = state_after_writing(IsSuccessful, Written, State),
            loop(NewIterValue, NewState)
    end.

%% @private
-spec state_after_writing(boolean(), non_neg_integer(), state()) -> state().
state_after_writing('true', Written, State) ->
    new_state_after_writing(Written, 0, State);
state_after_writing('false', Written, State) ->
    new_state_after_writing(0, Written, State).

%% @private
-spec new_state_after_writing(non_neg_integer(), non_neg_integer(), state()) -> state().
new_state_after_writing(WrittenSucceeded, WrittenFailed, State) ->
    NewTotalSucceeded = WrittenSucceeded + State#state.total_succeeded,
    NewTotalFailed = WrittenFailed + State#state.total_failed,
    S = State#state{total_succeeded = NewTotalSucceeded
                   ,total_failed = NewTotalFailed
                   },
    _ = kz_tasks:worker_maybe_send_update(State#state.task_id, NewTotalSucceeded, NewTotalFailed),
    _ = kz_tasks:worker_pause(),
    S.

%% @private
-spec is_task_successful(kz_tasks:task_id(), module(), atom(), list(), task_iterator()) ->
                                {boolean(), non_neg_integer(), task_iterator()} |
                                'stop'.
is_task_successful(TaskId, Module, Function, ExtraArgs, IterValue) ->
    try Module:Function(ExtraArgs, IterValue) of
        'stop' -> 'stop';
        {'ok', _Data}=NewIterValue ->
            %% For initialisation steps. Skeeps writing a CSV output row.
            {'true', 0, NewIterValue};
        {[_|_]=NewRowOrRows, _Data}=NewIterValue ->
            Written = store_return(TaskId, NewRowOrRows),
            {'true', Written, NewIterValue};
        {?NE_BINARY=NewRow, _Data}=NewIterValue ->
            Written = store_return(TaskId, NewRow),
            {'true', Written, NewIterValue};
        {Error, _Data}=NewIterValue ->
            Written = store_return(TaskId, Error),
            {'false', Written, NewIterValue}
    catch
        _E:_R ->
            kz_util:log_stacktrace(),
            Written = store_return(TaskId, ?WORKER_TASK_FAILED),
            {'false', Written, 'stop'}
    end.

%% @private
-spec store_return(kz_tasks:task_id(), task_return()) -> pos_integer().
store_return(TaskId, Rows=[_List|_]) when is_list(_List) ->
    lists:sum([store_return(TaskId, Row) || Row <- Rows]);
store_return(TaskId, Reason) ->
    Data = [reason(Reason), $\n],
    _ = kz_util:write_file(?OUT(TaskId), Data, ['append']),
    1.

%% @private
-spec reason(task_return()) -> iodata().
reason([_|_]=Row) ->
    kz_csv:row_to_iolist(Row);
reason(?NE_BINARY=Reason) ->
    kz_csv:row_to_iolist([Reason]);
reason(_) -> <<>>.

%% @private
-spec upload_output(kz_tasks:task_id()) -> ne_binary().
upload_output(TaskId) ->
    {'ok', Out} = file:read_file(?OUT(TaskId)),
    {'ok', TaskRev} = kz_tasks:worker_upload_result(TaskId, Out),
    kz_util:delete_file(?OUT(TaskId)),
    TaskRev.

%% @private
-spec write_output_csv_header(kz_tasks:task_id(), module(), atom()) -> 'ok' |
                                                                       {'error', any()}.
write_output_csv_header(TaskId, Module, Function) ->
    HeaderRHS = kz_tasks:get_output_header(Module, Function),
    Data = [kz_csv:row_to_iolist(HeaderRHS), $\n],
    file:write_file(?OUT(TaskId), Data).

%%% End of Module.
