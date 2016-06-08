%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Run tasks scheduled by parent.
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
               , total_succeeded = 0 :: non_neg_integer()
               }).

-define(IN, 'csv_in').
-define(OUT, 'json_out').


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(kz_tasks:task_id()
           ,module()
           ,atom()
           ,kz_proplist()
           ,ne_binaries()
           ) -> any().
start(TaskId, Module, Function, ExtraArgs, OrderedFields) ->
    _ = kz_util:put_callid(TaskId),
    case init(TaskId, Module, Function, ExtraArgs, OrderedFields) of
        {'ok', State} ->
            lager:debug("worker for ~s started", [TaskId]),
            loop(State);
        {'error', _R} ->
            kz_tasks:worker_finished(TaskId, 0),
            lager:debug("worker exiting now: ~p", [_R])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec init(kz_tasks:task_id()
          ,module()
          ,atom()
          ,kz_proplist()
          ,ne_binaries()
          ) -> any().
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
                 ,total_succeeded = TotalSucceeded
                 }) ->
    case kz_csv:take_row(get(?IN)) of
        'eof' ->
            kz_tasks:worker_finished(TaskId, TotalSucceeded),
            %%FIXME: when this goes over the wire shmem is of no help!
            %%Need to have some bucket to stream to!
            kz_tasks:worker_result(TaskId, iolist_to_binary(lists:reverse(get(?OUT)))),
            _ = erase(?OUT),
            _ = erase(?IN),
            'stop';
        {Row, CSVRest} ->
            Success = try_apply(Module, Function, ExtraArgs, FAssoc, Row),
            NewState = State#state{total_succeeded = TotalSucceeded + Success
                                  },
            _ = put(?IN, CSVRest),
            loop(NewState)
    end.

%% @private
-spec try_apply(module(), atom(), list(), kz_csv:fassoc(), kz_csv:row()) -> non_neg_integer().
try_apply(Module, Function, ExtraArgs, FAssoc, RawRow) ->
    try FAssoc(RawRow) of
        {'true', Args} ->
            try
                apply(Module, Function, [ExtraArgs|Args]),
                store_error(<<>>, RawRow),
                1
            catch
                _E:_R ->
                    ST = erlang:get_stacktrace(),
                    kz_util:log_stacktrace(ST),
                    %% hd * hd: removes try_apply/5 then loop/1
                    Reason = io_lib:format("~p1000", [hd(hd(ST))]),
                    store_error(Reason, RawRow),
                    0
            end;
        'false' ->
            lager:error("verifier failed on ~p", [RawRow]),
            store_error(<<"typecheck">>, RawRow),
            0
    catch
        _:_R ->
            lager:error("verifier crashed: ~p", [_R]),
            store_error(<<"internal">>, RawRow),
            0
    end.

%% @private
-spec store_error(binary(), kz_csv:row()) -> 'ok'.
store_error(Reason, Row) ->
    Error = [<<"{\"">>, kz_csv:row_to_iolist(Row), <<"\":\"">>
            ,Reason, <<"\"},">>
            ],
    _ = put(?OUT, [Error | get(?OUT)]).

%%% End of Module.
