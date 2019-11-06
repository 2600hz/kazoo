%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Run tasks without CSV input file, scheduled by kz_tasks.
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_task_worker_noinput).

%% API
-export([start/3]).

-include("tasks.hrl").

-record(state, {task_id :: kz_tasks:id()
               ,api :: kz_json:object()
               ,output_header :: kz_tasks:output_header()
               ,extra_args :: kz_tasks:extra_args()
               ,total_failed = 0 :: non_neg_integer()
               ,total_succeeded = 0 :: non_neg_integer()
               ,columns = sets:new() :: kz_tasks:columns()
               }).
-type state() :: #state{}.

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
-spec init(kz_tasks:id(), kz_json:object(), kz_tasks:extra_args()) -> {'ok', state()} |
                                                                      {'error', any()}.
init(TaskId, API, ExtraArgs) ->
    Header = kz_tasks_scheduler:get_output_header(API),
    case write_output_csv_header(TaskId, Header) of
        {'error', _R}=Error ->
            lager:error("failed to write CSV header in ~s", [?OUT(TaskId)]),
            Error;
        'ok' ->
            State = #state{task_id = TaskId
                          ,api = API
                          ,extra_args = ExtraArgs
                          ,output_header = Header
                          },
            {'ok', State}
    end.

-spec loop(kz_tasks:iterator(), state()) -> any().
loop(IterValue, State=#state{task_id = TaskId}) ->
    case is_task_successful(IterValue, State) of
        'stop' ->
            _ = kz_tasks_scheduler:worker_finished(TaskId
                                                  ,State#state.total_succeeded
                                                  ,State#state.total_failed
                                                  ,State#state.columns
                                                  ),
            'stop';
        {IsSuccessful, Columns, Written, 'stop'} ->
            NewState = state_after_writing(IsSuccessful, Columns, Written, State),
            _ = kz_tasks_scheduler:worker_finished(TaskId
                                                  ,NewState#state.total_succeeded
                                                  ,NewState#state.total_failed
                                                  ,NewState#state.columns
                                                  ),
            'stop';
        {IsSuccessful, Columns, Written, NewIterValue} ->
            NewState = state_after_writing(IsSuccessful, Columns, Written, State),
            loop(NewIterValue, NewState)
    end.

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

store_file(#state{task_id = TaskId}, File) ->
    lager:info("storing ~s to task doc ~s", [File, TaskId]),
    {'ok', CSV} = file:read_file(File),
    case kz_tasks_scheduler:attempt_upload(TaskId, filename:basename(File), CSV, File) of
        'ok' -> 'true';
        {'error', _E} ->
            lager:error("failed to upload ~s: ~p", [File, _E]),
            'false'
    end.

-spec is_task_successful(kz_tasks:iterator(), state()) ->
                                {boolean(), kz_tasks:columns(), non_neg_integer(), kz_tasks:iterator()} |
                                'stop'.
is_task_successful(IterValue
                  ,State=#state{api = API
                               ,extra_args = ExtraArgs
                               }
                  ) ->
    case tasks_bindings:apply(API, [ExtraArgs, IterValue]) of
        ['stop'] -> 'stop';
        [{'EXIT', {_Error, _ST=[_|_]}}] ->
            lager:error("error: ~p", [_Error]),
            kz_log:log_stacktrace(_ST),
            {Columns, Written} = store_return(State, ?WORKER_TASK_FAILED),
            {'false', Columns, Written, 'stop'};
        [{'ok', NewIterValue}] ->
            %% For initialisation steps. Skips writing a CSV output row.
            {'true', State#state.columns, 0, NewIterValue};
        [{'file', FileForUpload}] ->
            %% Upload file to task attachments
            IsSuccessful = store_file(State, FileForUpload),
            {IsSuccessful, State#state.columns, 0, 'stop'};
        [{[_|_]=NewRowOrRows, NewIterValue}] ->
            {Columns, Written} = store_return(State, NewRowOrRows),
            {'true', Columns, Written, NewIterValue};
        [{#{}=NewMappedRow, NewIterValue}] ->
            {Columns, Written} = store_return(State, NewMappedRow),
            {'true', Columns, Written, NewIterValue};
        [{?NE_BINARY=NewRow, NewIterValue}] ->
            {Columns, Written} = store_return(State, NewRow),
            {'true', Columns, Written, NewIterValue};
        [{Error, NewIterValue}] ->
            {Columns, Written} = store_return(State, Error),
            {'false', Columns, Written, NewIterValue}
    end.

-spec store_return(state(), kz_tasks:return()) -> {kz_tasks:columns(), pos_integer()}.
store_return(State, Rows=[_Row|_]) when is_list(_Row);
                                        is_map(_Row) ->
    {ListOfColumns, ListOfWritten} =
        lists:unzip([store_return(State, Row) || Row <- Rows]),
    {sets:union(ListOfColumns), lists:sum(ListOfWritten)};
store_return(#state{task_id = TaskId
                   ,output_header = OutputHeader
                   }
            ,Reason
            ) ->
    Data = [reason(OutputHeader, Reason)],
    kz_util:write_file(?OUT(TaskId), Data, ['append']),
    NewColumns = columns(OutputHeader, Reason),
    {NewColumns, 1}.

-spec reason(kz_tasks:output_header(), kz_tasks:return()) -> iodata().
reason(Header, MappedRow) when is_map(MappedRow) ->
    kz_csv:mapped_row_to_iolist(Header, MappedRow);
reason(_, [_|_]=Row) ->
    kz_csv:row_to_iolist(Row);
reason(_, ?NE_BINARY=Reason) ->
    kz_csv:row_to_iolist([Reason]);
reason(_, _) -> <<>>.

-spec columns(kz_tasks:output_header(), kz_tasks:return()) -> kz_tasks:columns().
columns(_, MappedRow) when is_map(MappedRow) ->
    Found = [K || {K,V} <- maps:to_list(MappedRow),
                  V =/= 'undefined'
            ],
    sets:from_list(Found);
columns(Header, [_|_]=Row) ->
    Found = [K || {K, V} <- lists:zip(Header, Row),
                  V =/= 'undefined'
            ],
    sets:from_list(Found);
columns(_, _) ->
    sets:new().

-spec write_output_csv_header(kz_tasks:id(), kz_csv:row()) ->
                                     'ok' | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
write_output_csv_header(TaskId, Header) ->
    Data = [kz_csv:row_to_iolist(Header)],
    file:write_file(?OUT(TaskId), Data).

%%% End of Module.
