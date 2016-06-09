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
-export([start/5]).

-include("tasks.hrl").

-record(state, { task_id :: kz_tasks:task_id()
               , module :: module()
               , function :: atom()
               , fassoc :: kz_csv:fassoc()
               , extra_args :: kz_proplist()
               , total_failed = 0 :: non_neg_integer()
               , total_succeeded = 0 :: non_neg_integer()
               }).
-type state() :: #state{}.

-define(IN, 'csv_in').
-define(OUT, 'txt_out').


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(kz_tasks:task_id(), module(), atom(), kz_proplist(), ne_binaries()) -> any().
start(TaskId, Module, Function, ExtraArgs, OrderedFields) ->
    _ = kz_util:put_callid(TaskId),
    case init(TaskId, Module, Function, ExtraArgs, OrderedFields) of
        {'ok', State} ->
            lager:debug("worker for ~s started", [TaskId]),
            loop(State);
        {'error', _R} ->
            kz_tasks:worker_error(TaskId),
            lager:debug("worker exiting now: ~p", [_R])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec init(kz_tasks:task_id(), module(), atom(), kz_proplist(), ne_binaries()) -> any().
init(TaskId, Module, Function, ExtraArgs, OrderedFields) ->
    case
        kz_util:try_load_module(Module) == Module andalso
        kz_datamgr:fetch_attachment(?KZ_TASKS_DB, TaskId, ?KZ_TASKS_ATTACHMENT_NAME_IN)
    of
        'false' ->
            lager:error("failed loading module '~p' for task ~s", [Module, TaskId]),
            {'error', 'badmodule'};
        {'error', Reason} ->
            lager:error("failed loading attachment ~s from ~s/~s: ~p"
                       ,[?KZ_TASKS_ATTACHMENT_NAME_IN, ?KZ_TASKS_DB, TaskId, Reason]),
            {'error', Reason};
        {'ok', CSV} ->
            Verify = build_verifier(Module),
            {Header, CSVRest} = kz_csv:take_row(CSV),
            FAssoc = kz_csv:associator(Header, OrderedFields, Verify),
            State = #state{ task_id = TaskId
                          , module = Module
                          , function = Function
                          , fassoc = FAssoc
                          , extra_args = ExtraArgs
                          },
            _ = put(?IN, CSVRest),
            _ = put(?OUT, []),
            {'ok', State}
    end.

%% @private
-spec build_verifier(module()) -> kz_csv:verifier().
build_verifier(Module) ->
    fun (Field, Value) ->
            case Module:Field(Value) of
                'true' -> 'true';
                'false' ->
                    lager:error("'~s' failed to validate with ~s:~s/1"
                               ,[Value, Module, Field]),
                    'false'
            end
    end.

%% @private
loop(State=#state{task_id = TaskId
                 ,module = Module
                 ,function = Function
                 ,fassoc = FAssoc
                 ,extra_args = ExtraArgs
                 ,total_failed = TotalFailed
                 ,total_succeeded = TotalSucceeded
                 }) ->
    case kz_csv:take_row(get(?IN)) of
        'eof' ->
            kz_tasks:worker_finished(TaskId, TotalSucceeded, TotalFailed),
            %%FIXME: when this goes over the wire shmem is of no help!
            %%Need to have some bucket to stream to!
            <<",",Bin/binary>> = iolist_to_binary(lists:reverse(get(?OUT))),
            kz_tasks:worker_result(TaskId, <<"{",Bin/binary,"}">>),
            _ = erase(?OUT),
            _ = erase(?IN),
            'stop';
        {Row, CSVRest} ->
            NewState =
                case is_task_successful(Module, Function, ExtraArgs, FAssoc, Row) of
                    'false' ->
                        State#state{total_failed = TotalFailed + 1
                                   };
                    'true' ->
                        State#state{total_succeeded = TotalSucceeded + 1
                                   }
                end,
            _ = maybe_send_update(NewState),
            _ = put(?IN, CSVRest),
            loop(NewState)
    end.

%% @private
-spec is_task_successful(module(), atom(), list(), kz_csv:fassoc(), kz_csv:row()) -> boolean().
is_task_successful(Module, Function, ExtraArgs, FAssoc, RawRow) ->
    try FAssoc(RawRow) of
        {'true', Args} ->
            try
                TaskReturn = apply(Module, Function, [ExtraArgs|Args]),
                store_error(TaskReturn, RawRow),
                'ok' == TaskReturn
            catch
                _E:_R ->
                    kz_util:log_stacktrace(),
                    store_error(<<"error">>, RawRow),
                    'false'
            end;
        'false' ->
            lager:error("verifier failed on ~p", [RawRow]),
            store_error(<<"typecheck">>, RawRow),
            'false'
    catch
        _:_R ->
            lager:error("verifier crashed: ~p", [_R]),
            store_error(<<"internal">>, RawRow),
            'false'
    end.

%% @private
-spec store_error(task_return(), kz_csv:row()) -> 'ok'.
store_error(Reason, Row) ->
    Error = [<<",\"">>, kz_csv:row_to_iolist(Row), <<"\":\"">>, reason(Reason), <<"\"">>
            ],
    _ = put(?OUT, [Error | get(?OUT)]),
    'ok'.

%% @private
-spec reason(task_return()) -> binary().
reason(?NE_BINARY=Reason) -> Reason;
reason(_) -> <<>>.

%% @private
-spec maybe_send_update(state()) -> 'ok'.
maybe_send_update(#state{task_id = TaskId
                        ,total_failed = TotalFailed
                        ,total_succeeded = TotalSucceeded
                        })
  when (TotalFailed + TotalSucceeded) rem 1000 == 0 ->
    kz_tasks:worker_update_processed(TaskId, TotalSucceeded, TotalFailed);
maybe_send_update(_) ->
    'ok'.

%%% End of Module.
