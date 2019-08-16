%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Utilities for tasks validation and stuff.
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tasks).

-export([all/0
        ,all/1
        ,read/1
        ,new/7
        ,new_id/0
        ]).

-export([mandatory/1
        ,optional/1
        ,possible_fields/1
        ,input_mime/1
        ]).

-export([from_json/1
        ,to_json/1
        ,to_public_json/1
        ,task_by_id/1
        ]).

-export([is_processing/1
        ,status/1
        ]).

-include("kazoo_tasks.hrl").

-define(TASK_ID_SIZE, 15).
-type id() :: <<_:(8*2*?TASK_ID_SIZE)>>.

-type task() :: #{worker_pid => kz_term:api_pid()
                 ,worker_node => kz_term:api_ne_binary()
                 ,account_id => kz_term:ne_binary()
                 ,auth_account_id => kz_term:ne_binary()
                 ,id => id()
                 ,category => kz_term:ne_binary()
                 ,action => kz_term:ne_binary()
                 ,file_name => kz_term:api_ne_binary()
                 ,created => kz_time:gregorian_seconds() %% Time of task creation (PUT)
                 ,started => kz_time:api_seconds() %% Time of task start (PATCH)
                 ,finished => kz_time:api_seconds() %% Time of task finish (> started)
                 ,total_rows => kz_term:api_pos_integer() %% CSV rows (undefined for a noinput task)
                 ,total_rows_failed => kz_term:api_non_neg_integer() %% Rows that crashed or didn't return ok
                 ,total_rows_succeeded => kz_term:api_non_neg_integer() %% Rows that returned 'ok'
                 ,was_stopped => kz_term:api_boolean() %% true if stopped, undefined of false otherwise
                 }.

-type input() :: kz_term:api_ne_binary() | kz_json:objects().

-type output_header() :: kz_csv:header() | {'replace', kz_csv:header()}.

-type columns() :: sets:set(kz_term:ne_binary()).

-type help_error() :: {'error', 'unknown_category_action' | 'unknown_category'}.

-type return() :: 'ok' | kz_term:api_ne_binary() |
                  kz_csv:row() | [kz_csv:row()] |
                  kz_csv:mapped_row() | [kz_csv:mapped_row()].

-type iterator() :: 'init' | 'stop' | any().

-type extra_args() :: #{account_id => kz_term:ne_binary()
                       ,auth_account_id => kz_term:ne_binary()
                       }.

-type args() :: map().

-export_type([id/0
             ,input/0
             ,output_header/0
             ,columns/0
             ,help_error/0
             ,return/0
             ,iterator/0
             ,extra_args/0
             ,args/0
             ,task/0
             ]).

-define(API_MANDATORY, <<"mandatory">>).
-define(API_OPTIONAL, <<"optional">>).
-define(API_INPUT_MIME, <<"expected_content">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec mandatory(kz_json:object()) -> kz_term:ne_binaries().
mandatory(APIJObj) ->
    kz_json:get_list_value(?API_MANDATORY, APIJObj, []).

-spec optional(kz_json:object()) -> kz_term:ne_binaries().
optional(APIJObj) ->
    kz_json:get_list_value(?API_OPTIONAL, APIJObj, []).

-spec possible_fields(kz_json:object()) -> kz_term:ne_binaries().
possible_fields(APIJObj) ->
    mandatory(APIJObj) ++ optional(APIJObj).

-spec input_mime(kz_json:object()) -> kz_term:ne_binary().
input_mime(APIJObj) ->
    kz_json:get_ne_binary_value(?API_INPUT_MIME, APIJObj, ?NIL_MIME).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec all() -> kz_json:objects().
all() -> view([]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec all(kz_term:ne_binary()) -> kz_json:objects().
all(?MATCH_ACCOUNT_RAW(AccountId)) ->
    view([{'startkey', [AccountId, kz_time:now_s(), kz_json:new()]}
         ,{'endkey', [AccountId]}
         ,'descending'
         ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec read(id()) -> {'ok', kz_json:object()} |
                    {'error', 'not_found'}.
read(TaskId=?NE_BINARY) ->
    case task_by_id(TaskId) of
        [Task] -> {'ok', to_public_json(Task)};
        [] -> {'error', 'not_found'}
    end.

%%------------------------------------------------------------------------------
%% @doc Verify a task previous to its creation in DB.
%% @end
%%------------------------------------------------------------------------------
-spec new(kz_term:ne_binary(), kz_term:ne_binary()
         ,kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_pos_integer(), input(), kz_term:api_binary()) ->
                 {'ok', kz_json:object()} |
                 help_error() |
                 {'error', kz_json:object()}.
new(?MATCH_ACCOUNT_RAW(AuthAccountId), ?MATCH_ACCOUNT_RAW(AccountId)
   ,Category=?NE_BINARY, Action=?NE_BINARY
   ,TotalRows, Input, CSVName
   )
  when is_integer(TotalRows), TotalRows > 0;
       TotalRows =:= 'undefined', Input =:= 'undefined' ->
    case help(Category, Action) of
        {'error', _R}=E ->
            lager:debug("checking task ~s ~s failed: ~p", [Category, Action, _R]),
            E;
        API ->
            lager:debug("task ~s ~s matched api ~s", [Category, Action, kz_json:encode(API)]),
            case find_input_errors(API, Input) of
                Errors when Errors =/= #{} ->
                    JObj = kz_json:from_list(props:filter_empty(maps:to_list(Errors))),
                    {'error', JObj};
                _ ->
                    InputName = case kz_term:is_empty(CSVName) of
                                    'true' -> 'undefined';
                                    'false' -> kz_term:to_binary(CSVName)
                                end,
                    lager:debug("creating ~s.~s task (~p)", [Category, Action, TotalRows]),
                    lager:debug("using auth ~s and account ~s", [AuthAccountId, AccountId]),
                    TaskId = new_id(),
                    Task = #{worker_pid => 'undefined'
                            ,worker_node => 'undefined'
                            ,account_id => AccountId
                            ,auth_account_id => AuthAccountId
                            ,id => TaskId
                            ,category => Category
                            ,action => Action
                            ,file_name => InputName
                            ,created => kz_time:now_s()
                            ,started => 'undefined'
                            ,finished => 'undefined'
                            ,total_rows => TotalRows
                            ,total_rows_failed => 'undefined'
                            ,total_rows_succeeded => 'undefined'
                            },
                    {'ok', _JObj} = Ok = save_new_task(Task),
                    lager:debug("task ~s created, rows: ~p", [TaskId, TotalRows]),
                    Ok
            end
    end.

-spec new_id() -> id().
new_id() ->
    kz_binary:rand_hex(?TASK_ID_SIZE).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec help(kz_term:ne_binary(), kz_term:ne_binary()) ->
                  kz_json:object() |
                  {'error', 'unknown_category_action'}.
help(Category, Action) ->
    Req = props:filter_undefined(
            [{<<"Category">>, Category}
            ,{<<"Action">>, Action}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case kz_amqp_worker:call(Req
                            ,fun kapi_tasks:publish_lookup_req/1
                            ,fun kapi_tasks:lookup_resp_v/1
                            )
    of
        {'ok', JObj} ->
            Help = kz_json:get_value([<<"Help">>, Category, Action], JObj),
            case kz_term:is_empty(Help) of
                'false' -> Help;
                'true' ->
                    lager:debug("help resp is empty"),
                    {'error', 'unknown_category_action'}
            end;
        {'timeout', _Resp} ->
            lager:debug("help resp timed out"),
            {'error', 'unknown_category_action'};
        {'error', _E} ->
            lager:debug("help resp errored out: ~p", [_E]),
            {'error', 'unknown_category_action'}
    end.

-spec find_input_errors(kz_json:object(), input()) -> map().
find_input_errors(API, 'undefined') ->
    find_API_errors(API, [], 'false');

find_input_errors(API, Input=?NE_BINARY) ->
    {Fields, InputData} = kz_csv:take_row(Input),
    Errors = find_API_errors(API, Fields, 'true'),
    %% Stop here if there is no Mandatory fields to check against.
    case mandatory(API) of
        [] -> Errors;
        Mandatory ->
            IsMandatory = [lists:member(Field, Mandatory) || Field <- Fields],
            Unsets =
                fun (Row, Es) ->
                        case are_mandatories_unset(IsMandatory, Row) of
                            'false' -> Es;
                            'true' -> [iolist_to_binary(kz_csv:row_to_iolist(Row)) | Es]
                        end
                end,
            case kz_csv:fold(InputData, Unsets, []) of
                [] -> Errors;
                MMVs -> Errors#{?KZ_TASKS_INPUT_ERROR_MMV => lists:reverse(MMVs)}
            end
    end;

find_input_errors(API, InputRecord=[_|_]) ->
    %%NOTE: assumes first record has all the fields that all the other records will ever need set,
    %%NOTE: assumes all records have all the same fields defined.
    Fields = kz_json:get_keys(hd(InputRecord)),
    find_API_errors(API, Fields, 'true').

-spec find_API_errors(kz_json:object(), kz_term:ne_binaries(), boolean()) -> map().
find_API_errors(API, Fields, HasInputData) ->
    Mandatory = mandatory(API),
    Routines =
        [fun (Errors) ->
                 case Mandatory -- Fields of
                     [] -> Errors;
                     Missing -> Errors#{?KZ_TASKS_INPUT_ERROR_MMF => Missing}
                 end
         end
        ,fun (Errors) ->
                 RequestedMIME = input_mime(API),
                 APIRequiresInputData = ?NIL_MIME /= RequestedMIME,
                 case APIRequiresInputData xor HasInputData of
                     'false' -> Errors;
                     'true' ->  Errors#{?KZ_TASKS_INPUT_ERROR_MIME => RequestedMIME}
                 end
         end
        ],
    lists:foldl(fun (F, Errors) -> F(Errors) end, #{}, Routines).

-spec are_mandatories_unset(nonempty_list(boolean()), nonempty_list(kz_term:ne_binary())) -> boolean().
are_mandatories_unset(IsMandatory, Row) ->
    MapF = fun (Mandatory, Value) ->
                   Mandatory
                       andalso 'undefined' == Value
           end,
    RedF = fun erlang:'or'/2,
    lists:foldl(RedF, 'false', lists:zipwith(MapF, IsMandatory, Row)).


-spec view(list()) -> kz_json:objects().
view(ViewOptions) ->
    case kz_datamgr:get_results(?KZ_TASKS_DB, ?KZ_TASKS_BY_CREATED, ViewOptions) of
        {'ok', []} -> [];
        {'ok', JObjs} -> [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("error viewing tasks (~p): ~p", [ViewOptions, _R]),
            []
    end.

-spec save_new_task(task()) -> {'ok', kz_json:object()} |
                               {'error', any()}.
save_new_task(Task = #{id := _TaskId}) ->
    case kz_datamgr:save_doc(?KZ_TASKS_DB, to_json(Task)) of
        {'ok', Doc} -> {'ok', to_public_json(from_json(Doc))};
        {'error', _R}=E ->
            lager:error("failed to save ~s in ~s: ~p", [_TaskId, ?KZ_TASKS_DB, _R]),
            E
    end.

-spec task_by_id(id()) -> [task()].
task_by_id(TaskId) ->
    case kz_datamgr:open_cache_doc(?KZ_TASKS_DB, TaskId) of
        {'ok', JObj} -> [from_json(JObj)];
        {'error', _R} ->
            lager:error("failed to open ~s in ~s: ~p", [TaskId, ?KZ_TASKS_DB, _R]),
            []
    end.


-spec from_json(kz_json:object()) -> task().
from_json(Doc) ->
    #{worker_pid => 'undefined'
     ,worker_node => kzd_task:node(Doc)
     ,account_id => kzd_task:account_id(Doc)
     ,auth_account_id => kzd_task:auth_account_id(Doc)
     ,id => kzd_task:id(Doc)
     ,category => kzd_task:category(Doc)
     ,action => kzd_task:action(Doc)
     ,file_name => kzd_task:file_name(Doc)
     ,created => kz_doc:created(Doc)
     ,started => kzd_task:start_timestamp(Doc)
     ,finished => kzd_task:end_timestamp(Doc)
     ,total_rows => kzd_task:total_count(Doc)
     ,total_rows_failed => kzd_task:failure_count(Doc)
     ,total_rows_succeeded => kzd_task:success_count(Doc)
     ,was_stopped => ?STATUS_STOPPED =:= kzd_task:status(Doc)
     }.

-spec to_json(task()) -> kz_json:object().
to_json(#{id := TaskId
         ,worker_node := Node
         ,account_id := AccountId
         ,auth_account_id := AuthAccountId
         ,category := Category
         ,action := Action
         ,file_name := InputName
         ,created := Created
         ,started := Started
         ,finished := Finished
         ,total_rows := TotalRows
         ,total_rows_failed := TotalFailed
         ,total_rows_succeeded := TotalSucceeded
         } = Task) ->
    kz_json:from_list(
      [{<<"_id">>, TaskId}
      ,{?PVT_TYPE, kzd_task:type()}
      ,{?PVT_WORKER_NODE, Node}
      ,{?PVT_ACCOUNT_ID, AccountId}
      ,{?PVT_AUTH_ACCOUNT_ID, AuthAccountId}
      ,{?PVT_CATEGORY, Category}
      ,{?PVT_ACTION, Action}
      ,{?PVT_FILENAME, InputName}
      ,{?PVT_CREATED, Created}
      ,{?PVT_MODIFIED, kz_time:now_s()}
      ,{?PVT_STARTED_AT, Started}
      ,{?PVT_FINISHED_AT, Finished}
      ,{?PVT_TOTAL_ROWS, TotalRows}
      ,{?PVT_TOTAL_ROWS_FAILED, TotalFailed}
      ,{?PVT_TOTAL_ROWS_SUCCEEDED, TotalSucceeded}
      ,{?PVT_STATUS, status(Task)}
      ]).

-spec to_public_json(task()) -> kz_json:object().
to_public_json(Task) ->
    Doc = to_json(Task),
    kz_json:from_list_recursive(
      [{<<"_read_only">>
       ,[{<<"id">>, kzd_task:id(Doc)}
        ,{<<"node">>, kzd_task:node(Doc)}
        ,{<<"account_id">>, kzd_task:account_id(Doc)}
        ,{<<"auth_account_id">>, kzd_task:auth_account_id(Doc)}
        ,{<<"category">>, kzd_task:category(Doc)}
        ,{<<"action">>, kzd_task:action(Doc)}
        ,{<<"file_name">>, kzd_task:file_name(Doc)}
        ,{<<"created">>, kz_doc:created(Doc)}
        ,{<<"start_timestamp">>, kzd_task:start_timestamp(Doc)}
        ,{<<"end_timestamp">>, kzd_task:end_timestamp(Doc)}
        ,{<<"total_count">>, kzd_task:total_count(Doc)}
        ,{<<"failure_count">>, kzd_task:failure_count(Doc)}
        ,{<<"success_count">>, kzd_task:success_count(Doc)}
        ,{<<"status">>, kzd_task:status(Doc)}
        ]
       }
      ]).


%%------------------------------------------------------------------------------
%% @doc Whether task has been started and is still running.
%% @end
%%------------------------------------------------------------------------------
-spec is_processing(task()) -> boolean().
is_processing(#{started := Started
               ,finished := Finished
               }=Task)
  when Started  =/= 'undefined',
       Finished =:= 'undefined' ->
    not maps:get('was_stopped', Task, 'false');
is_processing(_) ->
    'false'.

-spec status(task()) -> kz_term:ne_binary().
status(#{was_stopped := 'true'}) ->
    ?STATUS_STOPPED;

status(#{started := 'undefined'}) ->
    ?STATUS_PENDING;
status(#{started := Started
        ,finished := 'undefined'
        })
  when Started =/= 'undefined' ->
    ?STATUS_EXECUTING;

%% For tasks with CSV input
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := 0
        ,total_rows_succeeded := TotalRows
        })
  when Finished =/= 'undefined',
       is_integer(TotalRows), TotalRows > 0 ->
    ?STATUS_SUCCESS;
%% Jobs like `compact_node' return a different number of rows compared to the number
%% of succeeded rows.
%% total_rows = number of nodes that were provided within the input csv file.
%% total_rows_succeeded = number of dbs compacted on all the nodes provided within the input csv file.
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := 0
        ,total_rows_succeeded := TotalRowsSucceeded
        })
  when Finished =/= 'undefined',
       is_integer(TotalRows),
       is_integer(TotalRowsSucceeded),
       TotalRows > 0,
       TotalRowsSucceeded >= TotalRows ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := TotalRows
        ,total_rows_succeeded := 0
        })
  when Finished =/= 'undefined',
       is_integer(TotalRows), TotalRows > 0 ->
    ?STATUS_FAILURE;
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished =/= 'undefined',
       is_integer(TotalRows), is_integer(TotalFailed), is_integer(TotalSucceeded),
       TotalRows > TotalFailed, TotalRows > TotalSucceeded ->
    ?STATUS_PARTIAL;

%% For noinput tasks
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := 0
        ,total_rows_succeeded := 0
        })
  when Finished =/= 'undefined' ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := 0
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished =/= 'undefined',
       is_integer(TotalSucceeded), TotalSucceeded > 0 ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := 0
        })
  when Finished =/= 'undefined',
       is_integer(TotalFailed), TotalFailed > 0 ->
    ?STATUS_FAILURE;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished =/= 'undefined',
       is_integer(TotalFailed), is_integer(TotalSucceeded) ->
    ?STATUS_PARTIAL;

status(_Task) ->
    lager:error("impossible task ~p", [_Task]),
    %% Probably due to worker killed (due to e.g. OOM).
    ?STATUS_BAD.
