%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%  Run tasks scheduled by kz_tasks.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_task_worker).

%% API
-export([start/6]).

-include("kz_tasks.hrl").

-record(state, { task_id :: kz_tasks:task_id()
               , api = kz_json:object()
               , module :: module()
               , function :: atom()
               , fassoc :: kz_csv:fassoc()
               , extra_args :: kz_proplist()
               , total_failed = 0 :: non_neg_integer()
               , total_succeeded = 0 :: non_neg_integer()
               }).
-type state() :: #state{}.

-define(IN, 'csv_in').
-define(OUT(TaskId), <<"/tmp/task_out.", (TaskId)/binary, ".csv">>).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(kz_tasks:task_id(), kz_json:object(), module(), atom(), kz_proplist(), ne_binaries()) ->
                   'ok'.
start(TaskId, API, Module, Function, ExtraArgs, OrderedFields) ->
    _ = kz_util:put_callid(TaskId),
    case init(TaskId, API, Module, Function, ExtraArgs, OrderedFields) of
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
-spec init(kz_tasks:task_id(), kz_json:object(), module(), atom(), kz_proplist(), ne_binaries()) ->
                  {'ok', state()} |
                  {'error', any()}.
init(TaskId, API, Module, Function, ExtraArgs, OrderedFields) ->
    case
        kz_util:try_load_module(Module) =:= Module
        andalso kz_datamgr:fetch_attachment(?KZ_TASKS_DB, TaskId, ?KZ_TASKS_ATTACHMENT_NAME_IN)
    of
        'false' ->
            lager:error("failed loading module '~p' for task ~s", [Module, TaskId]),
            {'error', 'badmodule'};
        {'error', _R}=Error ->
            lager:error("failed loading attachment ~s from ~s/~s: ~p"
                       ,[?KZ_TASKS_ATTACHMENT_NAME_IN, ?KZ_TASKS_DB, TaskId, _R]),
            Error;
        {'ok', CSV} ->
            {Header, CSVRest} = kz_csv:take_row(CSV),
            case write_output_csv_header(TaskId, Module, Function, Header) of
                {'error', _R}=Error ->
                    lager:error("failed to write CSV header in ~s", [?OUT(TaskId)]),
                    Error;
                'ok' ->
                    Verifier = build_verifier(Module),
                    FAssoc = kz_csv:associator(Header, OrderedFields, Verifier),
                    State = #state{ task_id = TaskId
                                  , api = API
                                  , module = Module
                                  , function = Function
                                  , fassoc = FAssoc
                                  , extra_args = ExtraArgs
                                  },
                    _ = put(?IN, CSVRest),
                    {'ok', State}
            end
    end.

%% @private
-spec build_verifier(module()) -> kz_csv:verifier().
build_verifier(Module) ->
    fun (Field, Value) ->
            try Module:Field(Value) of
                'true' -> 'true';
                'false' ->
                    lager:error("'~s' failed to validate with ~s:~s/1"
                               ,[Value, Module, Field]),
                    'false'
            catch
                _:'undef' ->
                    %% If a verifier does not exist then it always passes
                    'true'
            end
    end.

%% @private
-spec loop(task_iterator(), state()) -> any().
loop(IterValue, State=#state{task_id = TaskId
                            ,api = API
                            ,module = Module
                            ,function = Function
                            ,fassoc = FAssoc
                            ,extra_args = ExtraArgs
                            }) ->
    case kz_csv:take_row(get(?IN)) of
        'eof' -> teardown(State, API, IterValue);
        {Row, CSVRest} ->
            case is_task_successful(TaskId, Module, Function, ExtraArgs, FAssoc, Row, IterValue) of
                'stop' -> teardown(State, API, IterValue);
                {IsSuccessful, Written, 'stop'} ->
                    NewState = state_after_writing(IsSuccessful, Written, State),
                    teardown(NewState, API, IterValue);
                {IsSuccessful, Written, NewIterValue} ->
                    NewState = state_after_writing(IsSuccessful, Written, State),
                    _ = put(?IN, CSVRest),
                    loop(NewIterValue, NewState)
            end
    end.

-spec teardown(state(), kz_json:object(), any()) -> 'stop'.
teardown(State, API, IterValue) ->
    TaskId = State#state.task_id,
    _ = kz_tasks:worker_finished(TaskId
                                ,State#state.total_succeeded
                                ,State#state.total_failed
                                ,?OUT(TaskId)
                                ),
    _ = erase(?IN),
    _ = kz_tasks:cleanup_task(State#state.module, API, IterValue),
    'stop'.

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
-spec is_task_successful(kz_tasks:task_id(), module(), atom(), list()
                        ,kz_csv:fassoc(), kz_csv:row(), task_iterator()) ->
                                {boolean(), non_neg_integer(), task_iterator()} |
                                'stop'.
is_task_successful(TaskId, Module, Function, ExtraArgs, FAssoc, RawRow, IterValue) ->
    try FAssoc(RawRow) of
        {'true', RowArgs} ->
            Args = [ExtraArgs, IterValue | RowArgs],
            try apply(Module, Function, Args) of
                'stop' -> 'stop';
                {'ok', NewIterValue} ->
                    %% For initialisation steps. Skeeps writing a CSV output row.
                    {'true', 0, NewIterValue};
                {[_|_]=NewRowOrRows, NewIterValue} ->
                    Written = store_return(TaskId, RawRow, NewRowOrRows),
                    {'true', Written, NewIterValue};
                {?NE_BINARY=NewRow, NewIterValue} ->
                    Written = store_return(TaskId, RawRow, NewRow),
                    {'true', Written, NewIterValue};
                {Error, NewIterValue} ->
                    Written = store_return(TaskId, RawRow, Error),
                    {'false', Written, NewIterValue};
                'ok'=NewRowOrRows=NewIterValue ->
                    Written = store_return(TaskId, RawRow, NewRowOrRows),
                    {'true', Written, NewIterValue};
                NewRow=NewIterValue ->
                    Written = store_return(TaskId, RawRow, NewRow),
                    {'false', Written, NewIterValue}
            catch
                _E:_R ->
                    kz_util:log_stacktrace(),
                    lager:debug("args: ~p", [Args]),
                    Written = store_return(TaskId, RawRow, ?WORKER_TASK_FAILED),
                    {'false', Written, 'stop'}
            end;
        'false' ->
            lager:error("verifier failed on ~p", [RawRow]),
            Written = store_return(TaskId, RawRow, ?WORKER_TASK_TYPE),
            %% Stop on crashes, but only skip typefailed rows.
            {'false', Written, IterValue}
    catch
        _:_R ->
            ST = erlang:get_stacktrace(),
            lager:error("verifier crashed: ~p", [_R]),
            kz_util:log_stacktrace(ST),
            Written = store_return(TaskId, RawRow, ?WORKER_TASK_MAYBE_OK),
            {'false', Written, 'stop'}
    end.

%% @private
-spec store_return(kz_tasks:task_id(), kz_csv:row(), task_return()) -> 'ok'.
store_return(TaskId, Row, Rows=[_List|_]) when is_list(_List) ->
    lists:sum([store_return(TaskId, Row, R) || R <- Rows]);
store_return(TaskId, Row, Reason) ->
    Data = [kz_csv:row_to_iolist(Row), $,, reason(Reason), $\n],
    kz_util:write_file(?OUT(TaskId), Data, ['append']),
    1.

%% @private
-spec reason(task_return()) -> iodata().
reason([_|_]=Row) ->
    kz_csv:row_to_iolist(Row);
reason(?NE_BINARY=Reason) ->
    kz_csv:row_to_iolist([Reason]);
reason(_) -> <<>>.

%% @private
-spec write_output_csv_header(kz_tasks:task_id(), module(), atom(), kz_csv:row()) ->
                                     'ok' | {'error', any()}.
write_output_csv_header(TaskId, Module, Function, HeaderRow) ->
    HeaderRHS = kz_tasks:get_output_header(Module, Function),
    Data = [kz_csv:row_to_iolist(HeaderRow ++ HeaderRHS), $\n],
    file:write_file(?OUT(TaskId), Data).

%%% End of Module.
