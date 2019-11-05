%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Run tasks scheduled by kz_tasks.
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_task_worker).

%% API
-export([start/3]).

-include("tasks.hrl").

-record(state, {task_id :: kz_tasks:id()
               ,api :: kz_json:object()
               ,verifier :: kz_csv:mapped_row_verifier()
               ,input_header :: kz_csv:row()
               ,output_header :: kz_tasks:output_header()
               ,extra_args :: kz_tasks:extra_args()
               ,total_failed = 0 :: non_neg_integer()
               ,total_succeeded = 0 :: non_neg_integer()
               ,columns = sets:new() :: kz_tasks:columns()
               }).
-type state() :: #state{}.

-define(IN, 'csv_in').
-define(OUT(TaskId), kz_tasks_scheduler:output_path(TaskId)).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(kz_tasks:id(), kz_json:object(), kz_tasks:extra_args()) -> ok.
start(TaskId, API, ExtraArgs) ->
    _ = kz_log:put_callid(TaskId),
    case init(TaskId, API, ExtraArgs) of
        {'ok', State} ->
            lager:debug("worker for ~s started", [TaskId]),
            loop('init', State);
        {'error', _R} ->
            kz_tasks_scheduler:worker_error(TaskId),
            lager:debug("worker exiting now: ~p", [_R])
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(kz_tasks:id(), kz_json:object(), kz_tasks:extra_args()) -> {ok, state()} |
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

-spec init_from_csv(kz_tasks:id(), kz_json:object(), kz_tasks:extra_args(), binary()) ->
                           {'ok', state()} |
                           {'error', any()}.
init_from_csv(TaskId, API, ExtraArgs, CSV) ->
    {InputHeader, CSVRest} = kz_csv:take_row(CSV),
    OutputHeader = output_csv_header(API, InputHeader),
    case write_output_csv_header(TaskId, OutputHeader) of
        {'error', _R}=Error ->
            lager:error("failed to write CSV header in ~s", [?OUT(TaskId)]),
            Error;
        'ok' ->
            State = #state{task_id = TaskId
                          ,api = API
                          ,verifier = build_verifier(API)
                          ,extra_args = ExtraArgs
                          ,input_header = InputHeader
                          ,output_header = OutputHeader
                          },
            _ = put(?IN, CSVRest),
            {'ok', State}
    end.

-spec build_verifier(kz_json:object()) -> kz_csv:mapped_row_verifier().
build_verifier(API) ->
    Mandatory = kz_tasks:mandatory(API),
    fun (Field, 'undefined') ->
            %% Always validate empty optional fields.
            %% Always deny empty mandatory fields.
            not lists:member(Field, Mandatory);
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

-spec loop(kz_tasks:iterator(), state()) -> any().
loop(IterValue, State=#state{api = API
                            ,input_header = InputHeader
                            }) ->
    case kz_csv:take_mapped_row(InputHeader, get(?IN)) of
        'eof' -> teardown(API, IterValue, State);
        {MappedRow0, CSVRest} ->
            MappedRow = pad_mapped_row(API, MappedRow0),
            case is_task_successful(MappedRow, IterValue, State) of
                'stop' -> teardown(API, IterValue, State);
                {IsSuccessful, Columns, Written, 'stop'} ->
                    NewState = state_after_writing(IsSuccessful, Columns, Written, State),
                    teardown(API, IterValue, NewState);
                {IsSuccessful, Columns, Written, NewIterValue} ->
                    NewState = state_after_writing(IsSuccessful, Columns, Written, State),
                    _ = put(?IN, CSVRest),
                    loop(NewIterValue, NewState)
            end
    end.

-spec pad_mapped_row(kz_json:object(), kz_csv:mapped_row()) -> kz_csv:mapped_row().
pad_mapped_row(API, MappedRow) ->
    MissingFields = kz_tasks:possible_fields(API) -- maps:keys(MappedRow),
    F = fun (Field, Row) -> Row#{Field => undefined} end,
    lists:foldl(F, MappedRow, MissingFields).

-spec teardown(kz_json:object(), any(), state()) -> 'stop'.
teardown(API, IterValue, #state{task_id = TaskId
                               ,total_succeeded = TotalSucceeded
                               ,total_failed = TotalFailed
                               ,columns = Columns
                               }) ->
    _ = kz_tasks_scheduler:finish_task(API, IterValue),
    _ = kz_tasks_scheduler:worker_finished(TaskId
                                          ,TotalSucceeded
                                          ,TotalFailed
                                          ,Columns
                                          ),
    _ = erase(?IN),
    _ = kz_tasks_scheduler:cleanup_task(API, IterValue),
    'stop'.

-spec state_after_writing(boolean(), kz_tasks:columns(), non_neg_integer(), state()) -> state().
state_after_writing('true', Columns, Written, State) ->
    new_state_after_writing(Columns, Written, 0, State);
state_after_writing('false', Columns, Written, State) ->
    new_state_after_writing(Columns, 0, Written, State).

-spec new_state_after_writing(kz_tasks:columns(), non_neg_integer(), non_neg_integer(), state()) -> state().
new_state_after_writing(NewColumns, WrittenSucceeded, WrittenFailed
                       ,State=#state{task_id = Taskid
                                    ,total_succeeded = TotalSucceeded
                                    ,total_failed = TotalFailed
                                    ,columns = Columns
                                    }) ->
    S = State#state{total_succeeded = NewTotalSucceeded = WrittenSucceeded + TotalSucceeded
                   ,total_failed = NewTotalFailed = WrittenFailed + TotalFailed
                   ,columns = sets:union(Columns, NewColumns)
                   },
    _ = kz_tasks_scheduler:worker_maybe_send_update(Taskid, NewTotalSucceeded, NewTotalFailed),
    _ = kz_tasks_scheduler:worker_pause(),
    S.

-spec is_task_successful(kz_csv:mapped_row(), kz_tasks:iterator(), state()) ->
                                {boolean(), kz_tasks:columns(), non_neg_integer(), kz_tasks:iterator()} |
                                stop.
is_task_successful(MappedRow
                  ,IterValue
                  ,State=#state{api = API
                               ,verifier = Verifier
                               ,extra_args = ExtraArgs
                               }) ->
    try kz_csv:verify_mapped_row(Verifier, MappedRow) of
        [] ->
            Args = [ExtraArgs, IterValue, MappedRow],
            case tasks_bindings:apply(API, Args) of
                ['stop'] -> 'stop';
                [{'EXIT', {_Error, _ST}}] ->
                    lager:error("args: ~p", [Args]),
                    lager:error("error: ~p", [_Error]),
                    kz_log:log_stacktrace(_ST),
                    {Columns,Written} = store_return(State, MappedRow, ?WORKER_TASK_FAILED),
                    {'false', Columns, Written, 'stop'};
                [{[_|_]=NewRowOrRows, NewIterValue}] ->
                    {Columns,Written} = store_return(State, MappedRow, NewRowOrRows),
                    {'true', Columns, Written, NewIterValue};
                [{#{?OUTPUT_CSV_HEADER_ERROR := ?NE_BINARY}=NewMappedRowOrMappedRows, NewIterValue}] ->
                    {Columns,Written} = store_return(State, MappedRow, NewMappedRowOrMappedRows),
                    {'false', Columns, Written, NewIterValue};
                [{#{}=NewMappedRowOrMappedRows, NewIterValue}] ->
                    {Columns,Written} = store_return(State, MappedRow, NewMappedRowOrMappedRows),
                    {'true', Columns, Written, NewIterValue};
                [{'ok', NewIterValue}] ->
                    {'true', State#state.columns, 1, NewIterValue};
                [{Error, NewIterValue}] ->
                    lager:error("~p", [Error]),
                    {Columns,Written} = store_return(State, MappedRow, Error),
                    {'false', Columns, Written, NewIterValue};
                [[_|_]=NewRowOrRows=NewIterValue] ->
                    {Columns,Written} = store_return(State, MappedRow, NewRowOrRows),
                    {'true', Columns, Written, NewIterValue};
                [#{?OUTPUT_CSV_HEADER_ERROR := ?NE_BINARY}=NewMappedRowOrMappedRows=NewIterValue] ->
                    {Columns,Written} = store_return(State, MappedRow, NewMappedRowOrMappedRows),
                    {'false', Columns, Written, NewIterValue};
                [#{}=NewMappedRowOrMappedRows=NewIterValue] ->
                    {Columns,Written} = store_return(State, MappedRow, NewMappedRowOrMappedRows),
                    {'true', Columns, Written, NewIterValue};
                NewRow=NewIterValue ->
                    {Columns,Written} = store_return(State, MappedRow, NewRow),
                    {'false', Columns, Written, NewIterValue}
            end;
        Fields ->
            PPFields = kz_binary:join(Fields, $\s),
            lager:error("verifier ~s failed on ~p", [PPFields, maps:with(Fields, MappedRow)]),
            {Columns,Written} = store_return(State, MappedRow, <<"bad field(s) ", PPFields/binary>>),
            %% Stop on crashes, but only skip typefailed rows.
            {'false', Columns, Written, IterValue}
    catch
        ?STACKTRACE(_, _R, ST)
        lager:error("verifier crashed: ~p", [_R]),
        kz_log:log_stacktrace(ST),
        {Columns,Written} = store_return(State, MappedRow, ?WORKER_TASK_MAYBE_OK),
        {'false', Columns, Written, 'stop'}
        end.

-spec store_return(state(), kz_csv:mapped_row(), kz_tasks:return()) -> {kz_tasks:columns(), pos_integer()}.
store_return(State, MappedRow, Rows=[_List|_]) when is_list(_List) ->
    {ListOfColumns, ListOfWritten} =
        lists:unzip([store_return(State, MappedRow, Row) || Row <- Rows]),
    {sets:union(ListOfColumns), lists:sum(ListOfWritten)};

store_return(#state{task_id = TaskId
                   ,output_header = {'replace', Header}
                   }, _InputMappedRow, OutputMappedRow)
  when is_map(OutputMappedRow) ->
    write_row(TaskId, Header, OutputMappedRow);
store_return(#state{task_id = TaskId
                   ,output_header = {'replace', Header}
                   }, InputMappedRow, Reason) ->
    MappedRow = InputMappedRow#{?OUTPUT_CSV_HEADER_ERROR => Reason},
    write_row(TaskId, Header, MappedRow);
store_return(#state{task_id = TaskId
                   ,output_header = Header
                   }, InputMappedRow, OutputMappedRow)
  when is_map(OutputMappedRow) ->
    MappedRow = maps:merge(InputMappedRow, OutputMappedRow),
    write_row(TaskId, Header, MappedRow);
store_return(#state{task_id = TaskId
                   ,output_header = Header
                   }, InputMappedRow, Reason) ->
    Data = kz_csv:mapped_row_to_iolist(Header, InputMappedRow),
    IOList = [Data, $,, reason(Reason)],
    Written = write_row(TaskId, IOList),
    {columns(InputMappedRow), Written}.

-spec write_row(kz_tasks:id(), kz_csv:header(), kz_csv:mapped_row()) -> {kz_tasks:columns(), 1}.
write_row(TaskId, Header, MappedRow) ->
    IOList = kz_csv:mapped_row_to_iolist(Header, MappedRow),
    Written = write_row(TaskId, IOList),
    {columns(MappedRow), Written}.

-spec write_row(kz_tasks:id(), iodata()) -> 1.
write_row(TaskId, IOList) ->
    kz_util:write_file(?OUT(TaskId), [IOList,$\n], ['append']),
    1.

-spec columns(kz_csv:mapped_row()) -> kz_tasks:columns().
columns(MappedRow) ->
    Found = [K || {K,V} <- maps:to_list(MappedRow),
                  V =/= undefined
            ],
    sets:from_list(Found).

-spec reason(kz_tasks:return()) -> iodata().
reason([_|_]=Row) ->
    kz_csv:row_to_iolist(Row);
reason(?NE_BINARY=Reason) ->
    kz_csv:row_to_iolist([Reason]);
reason(_) -> <<>>.

-spec write_output_csv_header(kz_tasks:id(), kz_tasks:output_header()) -> 'ok' | {'error', file:posix()}.
write_output_csv_header(TaskId, {'replace', Header}) ->
    write_output_csv_header(TaskId, Header);
write_output_csv_header(TaskId, Header) ->
    Data = [kz_csv:row_to_iolist(Header), $\n],
    file:write_file(?OUT(TaskId), Data).

-spec output_csv_header(kz_json:object(), kz_csv:row()) -> kz_tasks:output_header().
output_csv_header(API, HeaderRow) ->
    case kz_tasks_scheduler:get_output_header(API) of
        H={'replace', _FullHeader} -> H;
        HeaderRHS -> HeaderRow ++ HeaderRHS
    end.

%%% End of Module.
