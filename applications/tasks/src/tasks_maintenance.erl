%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(tasks_maintenance).

-export([help/0, help/1, help/2]).
-export([tasks/0, tasks/1]).
-export([add/5, add/4]).
-export([task/1, task_input/1, task_output/1]).
-export([start/1, restart/1]).
-export([remove/1]).

-export([start_cleanup_pass/0
        ,cleanup_soft_deletes/1
        ]).

-export([register_views/0]).

-include("tasks.hrl").


%%% API

-spec help() -> 'no_return'.
help() ->
    print_json(kz_tasks_help:help()).

-spec help(kz_term:text()) -> 'no_return'.
help(Category) ->
    case kz_tasks_help:help(Category) of
        {'ok', JObj} -> print_json(JObj);
        {'error', Reason} -> print_error(Reason)
    end.

-spec help(kz_term:text(), kz_term:text()) -> 'no_return'.
help(Category, Action) ->
    case kz_tasks_help:help(Category, Action) of
        {'ok', JObj} -> print_json(JObj);
        {'error', Reason} -> print_error(Reason)
    end.

-spec add(kz_term:text(), kz_term:text(), kz_term:text(), kz_term:text(), kz_term:text()) -> 'no_return'.
add(AuthAccount, Account, Category, Action, CSVFile) ->
    AuthAccountId = kz_util:format_account_id(AuthAccount),
    AccountId = kz_util:format_account_id(Account),
    case file:read_file(CSVFile) of
        {'ok', CSVBin} ->
            case kz_csv:count_rows(CSVBin) of
                0 -> print_error(<<"Empty CSV or some row(s) longer than others or header missing">>);
                TotalRows ->
                    CSVName = filename:basename(CSVFile),
                    new_task(AuthAccountId, AccountId, Category, Action, TotalRows, CSVBin, CSVName)
            end;
        {'error', Reason} ->
            print_error(Reason)
    end.

-spec add(kz_term:text(), kz_term:text(), kz_term:text(), kz_term:text()) -> 'no_return'.
add(AuthAccount, Account, Category, Action) ->
    AuthAccountId = kz_util:format_account_id(AuthAccount),
    AccountId = kz_util:format_account_id(Account),
    case kz_tasks:new(AuthAccountId
                     ,AccountId
                     ,Category
                     ,Action
                     ,'undefined'
                     ,'undefined'
                     ,'undefined'
                     )
    of
        {'ok', TaskJObj} -> print_json(TaskJObj);
        {'error', Reason} -> handle_new_task_error(Reason, Category, Action)
    end.

-spec tasks() -> 'no_return'.
tasks() ->
    Tasks = kz_tasks:all(),
    print_json(Tasks).

-spec tasks(kz_term:text()) -> 'no_return'.
tasks(Account) ->
    AccountId = kz_util:format_account_id(Account),
    Tasks = kz_tasks:all(AccountId),
    print_json(Tasks).

-spec task(kz_term:text()) -> 'no_return'.
task(TaskId) ->
    case kz_tasks:read(TaskId) of
        {'ok', JObj} -> print_json(JObj);
        {'error', Reason} -> print_error(Reason)
    end.

-spec task_input(kz_term:text()) -> 'no_return'.
task_input(TaskId) ->
    attachment(TaskId, ?KZ_TASKS_ANAME_IN).

-spec task_output(kz_term:text()) -> 'no_return'.
task_output(TaskId) ->
    attachment(TaskId, ?KZ_TASKS_ANAME_OUT).

-spec start(kz_term:text()) -> 'no_return'.
start(TaskId) ->
    case kz_tasks_scheduler:start(TaskId) of
        {'ok', StartedTask} -> print_json(StartedTask);
        {'error', Reason} -> print_error(Reason)
    end.

-spec restart(kz_term:text()) -> 'no_return'.
restart(TaskId) ->
    case kz_tasks_scheduler:restart(TaskId) of
        {'ok', RestartedTask} -> print_json(RestartedTask);
        {'error', Reason} -> print_error(Reason)
    end.

-spec remove(kz_term:text()) -> 'no_return'.
remove(TaskId) ->
    case kz_tasks_scheduler:remove(TaskId) of
        {'ok', RemovedTask} -> print_json(RemovedTask);
        {'error', Reason} -> print_error(Reason)
    end.

-spec start_cleanup_pass() -> no_return.
start_cleanup_pass() ->
    _ = kz_tasks_trigger:browse_dbs_for_triggers(?MODULE),
    no_return.

-spec cleanup_soft_deletes(kz_term:text()) -> no_return.
cleanup_soft_deletes(Account) ->
    _ = kt_cleanup:cleanup_soft_deletes(Account),
    no_return.

%%% Internals

-spec print_json(kz_json:json_term()) -> 'no_return'.
print_json(Data) ->
    'ok' = io:fwrite(io_lib:format("~ts\n", [kz_json:encode(Data)])),
    'no_return'.

-spec print_error(any()) -> 'no_return'.
print_error(Reason)
  when is_atom(Reason); is_binary(Reason) ->
    io:format("ERROR: ~s\n", [Reason]),
    'no_return';
print_error(Reason) ->
    io:format("ERROR: ~p\n", [Reason]),
    'no_return'.

-spec attachment(kz_tasks:id(), kz_term:ne_binary()) -> no_return.
attachment(TaskId, AName) ->
    case kz_tasks:read(TaskId) of
        {'ok', _JObj} ->
            case kz_datamgr:fetch_attachment(?KZ_TASKS_DB, TaskId, AName) of
                {'ok', AttachBin} ->
                    io:fwrite(AttachBin),
                    'no_return';
                {'error', Reason} -> print_error(Reason)
            end;
        {'error', Reason} -> print_error(Reason)
    end.

-spec new_task(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pos_integer(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                      'no_return'.
new_task(AuthAccountId, AccountId, Category, Action, TotalRows, CSVBin, CSVName) ->
    case kz_tasks:new(AuthAccountId, AccountId, Category, Action, TotalRows, CSVBin, CSVName) of
        {'ok', TaskJObj} ->
            TaskId = kz_json:get_value([<<"_read_only">>, <<"id">>], TaskJObj),
            case kz_datamgr:put_attachment(?KZ_TASKS_DB
                                          ,TaskId
                                          ,?KZ_TASKS_ANAME_IN
                                          ,CSVBin
                                          ,[{'content_type', <<"text/csv">>}]
                                          )
            of
                {'ok', _} -> print_json(TaskJObj);
                {'error', Reason} -> print_error(Reason)
            end;
        {'error', Reason} ->
            handle_new_task_error(Reason, Category, Action)
    end.

-spec handle_new_task_error(atom() | kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
handle_new_task_error('unknown_category_action', Category, Action) ->
    print_error(<<"No such category / action: ", Category/binary, " ", Action/binary>>);
handle_new_task_error(JObj, _, _) ->
    print_json(kz_json:from_list([{<<"errors">>, JObj}])).

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder(?APP).

%%% End of Module
