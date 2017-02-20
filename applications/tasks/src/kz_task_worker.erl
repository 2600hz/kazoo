%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2017, 2600Hz INC
%%% @doc
%%%  Run tasks scheduled by kz_tasks.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_task_worker).

%% API
-export([start/3]).

-include("tasks.hrl").

-record(state, {task_id :: kz_tasks:id()
               ,api = kz_json:object()
               ,fassoc :: kz_csv:fassoc()
               ,extra_args :: map()
               ,total_failed = 0 :: non_neg_integer()
               ,total_succeeded = 0 :: non_neg_integer()
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
-spec start(kz_tasks:id(), kz_json:object(), map()) -> ok.
start(TaskId, API, ExtraArgs) ->
    _ = kz_util:put_callid(TaskId),
    case init(TaskId, API, ExtraArgs) of
        {'ok', State} ->
            lager:debug("worker for ~s started", [TaskId]),
            loop('init', State);
        {'error', _R} ->
            kz_tasks_scheduler:worker_error(TaskId),
            lager:debug("worker exiting now: ~p", [_R])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec init(kz_tasks:id(), kz_json:object(), map()) -> {ok, state()} |
                                                      {error, any()}.
init(TaskId, API, ExtraArgs) ->
    case kz_datamgr:fetch_attachment(?KZ_TASKS_DB, TaskId, ?KZ_TASKS_ANAME_IN) of
        {'error', _R}=Error ->
            lager:error("failed loading attachment ~s from ~s/~s: ~p"
                       ,[?KZ_TASKS_ANAME_IN, ?KZ_TASKS_DB, TaskId, _R]
                       ),
            Error;
        {'ok', CSV} ->
            init_from_csv(TaskId, API, ExtraArgs, CSV)
    end.

-spec init_from_csv(kz_tasks:id(), kz_json:object(), map(), binary()) ->
                           {'ok', state()} |
                           {'error', any()}.
init_from_csv(TaskId, API, ExtraArgs, CSV) ->
    {CSVHeader, CSVRest} = kz_csv:take_row(CSV),
    case write_output_csv_header(TaskId, API, CSVHeader) of
        {'error', _R}=Error ->
            lager:error("failed to write CSV header in ~s", [?OUT(TaskId)]),
            Error;
        'ok' ->
            Verifier = build_verifier(API),
            TaskFields = kz_tasks:mandatory(API) ++ kz_tasks:optional(API),

            FAssoc = kz_csv:associator(CSVHeader, TaskFields, Verifier),
            State = #state{task_id = TaskId
                          ,api = API
                          ,fassoc = FAssoc
                          ,extra_args = ExtraArgs
                          },
            _ = put(?IN, CSVRest),
            {'ok', State}
    end.

%% @private
-spec build_verifier(kz_json:object()) -> kz_csv:verifier().
build_verifier(API) ->
    fun (Field, 'undefined') ->
            %% Always validate empty optional fields.
            %% Always deny empty mandatory fields.
            not lists:member(Field, kz_tasks:mandatory(API));
        (Field, Value) ->
            case tasks_bindings:apply(API, Field, [Value]) of
                ['true'] -> 'true';
                ['false'] ->
                    lager:error("'~s' failed to validate with ~s/1", [Value, Field]),
                    'false';
                %% If a verifier does not exist then it always passes
                [{'EXIT',{'undef',_}}] -> 'true';
                _ -> 'true'
            end
    end.

%% @private
-spec loop(kz_tasks:iterator(), state()) -> any().
loop(IterValue, State=#state{task_id = TaskId
                            ,api = API
                            ,fassoc = FAssoc
                            ,extra_args = ExtraArgs
                            }) ->
    case kz_csv:take_row(get(?IN)) of
        'eof' -> teardown(State, API, IterValue);
        {Row, CSVRest} ->
            case is_task_successful(TaskId, API, ExtraArgs, FAssoc, Row, IterValue) of
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
    _ = kz_tasks_scheduler:worker_finished(TaskId
                                          ,State#state.total_succeeded
                                          ,State#state.total_failed
                                          ,?OUT(TaskId)
                                          ),
    _ = erase(?IN),
    _ = kz_tasks_scheduler:cleanup_task(API, IterValue),
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
    _ = kz_tasks_scheduler:worker_maybe_send_update(State#state.task_id
                                                   ,NewTotalSucceeded
                                                   ,NewTotalFailed
                                                   ),
    _ = kz_tasks_scheduler:worker_pause(),
    S.

%% @private
-spec is_task_successful(kz_tasks:id(), kz_json:object(), map()
                        ,kz_csv:fassoc(), kz_csv:row(), kz_tasks:iterator()) ->
                                {boolean(), non_neg_integer(), kz_tasks:iterator()} |
                                stop.
is_task_successful(TaskId, API, ExtraArgs, FAssoc, RawRow, IterValue) ->
    try FAssoc(RawRow) of
        {ok, RowArgs} ->
            Args = [ExtraArgs, IterValue, RowArgs],
            case tasks_bindings:apply(API, Args) of
                ['stop'] -> 'stop';
                [{'EXIT', {_Error, _ST}}] ->
                    lager:error("args: ~p", [Args]),
                    lager:error("error: ~p", [_Error]),
                    kz_util:log_stacktrace(_ST),
                    Written = store_return(TaskId, RawRow, ?WORKER_TASK_FAILED),
                    {'false', Written, 'stop'};
                [{NewRowOrRows, NewIterValue}] when is_list(NewRowOrRows) ->
                    Written = store_return(TaskId, RawRow, NewRowOrRows),
                    {'true', Written, NewIterValue};
                [{Error, NewIterValue}] ->
                    lager:error("~p", [Error]),
                    Written = store_return(TaskId, RawRow, Error),
                    {'false', Written, NewIterValue};
                [NewRowOrRows=NewIterValue] when is_list(NewRowOrRows) ->
                    Written = store_return(TaskId, RawRow, NewRowOrRows),
                    {'true', Written, NewIterValue};
                NewRow=NewIterValue ->
                    Written = store_return(TaskId, RawRow, NewRow),
                    {'false', Written, NewIterValue}
            end;
        {error, Field} ->
            lager:error("verifier ~s failed on ~p", [Field, RawRow]),
            Written = store_return(TaskId, RawRow, <<"bad ", Field/binary>>),
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
-spec store_return(kz_tasks:id(), kz_csv:row(), kz_tasks:return()) -> pos_integer().
store_return(TaskId, Row, Rows=[_List|_]) when is_list(_List) ->
    lists:sum([store_return(TaskId, Row, R) || R <- Rows]);
store_return(TaskId, Row, Reason) ->
    Data = [kz_csv:row_to_iolist(Row), $,, reason(Reason), $\n],
    kz_util:write_file(?OUT(TaskId), Data, ['append']),
    1.

%% @private
-spec reason(kz_tasks:return()) -> iodata().
reason([_|_]=Row) ->
    kz_csv:row_to_iolist(Row);
reason(?NE_BINARY=Reason) ->
    kz_csv:row_to_iolist([Reason]);
reason(_) -> <<>>.

%% @private
-spec write_output_csv_header(kz_tasks:id(), kz_json:object(), kz_csv:row()) ->
                                     ok | {error, any()}.
write_output_csv_header(TaskId, API, HeaderRow) ->
    HeaderRHS = kz_tasks_scheduler:get_output_header(API),
    Data = [kz_csv:row_to_iolist(HeaderRow ++ HeaderRHS), $\n],
    file:write_file(?OUT(TaskId), Data).

%%% End of Module.
