%%%-------------------------------------------------------------------
%%% @Copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kazoo_tasks_maintenance).

-export([help/0, help/1, help/2]).
-export([tasks/0, tasks/1]).
-export([add/4, add/3]).
-export([task/1, task_input/1, task_output/1]).
-export([start/1]).
-export([remove/1]).

-include_lib("kazoo_tasks/src/kz_tasks.hrl").


%%% API

-spec help() -> 'no_return'.
help() ->
    print_json(kz_tasks:help()).

-spec help(text()) -> 'no_return'.
help(Category) ->
    case kz_tasks:help(Category) of
        {'ok', JObj} -> print_json(JObj);
        {'error', Reason} -> print_error(Reason)
    end.

-spec help(text(), text()) -> 'no_return'.
help(Category, Action) ->
    case kz_tasks:help(Category, Action) of
        {'ok', JObj} -> print_json(JObj);
        {'error', Reason} -> print_error(Reason)
    end.

-spec add(text(), text(), text(), text()) -> 'no_return'.
add(Account, Category, Action, CSVFile) ->
    AccountId = kz_util:format_account_id(Account),
    case file:read_file(CSVFile) of
        {'ok', CSVBin} ->
            case kz_csv:count_rows(CSVBin) of
                0 -> print_error(<<"Empty CSV or some row(s) longer than others or header missing">>);
                TotalRows ->
                    new_task(AccountId, Category, Action, TotalRows, CSVBin)
            end;
        {'error', Reason} ->
            print_error(Reason)
    end.

-spec add(text(), text(), text()) -> 'no_return'.
add(Account, Category, Action) ->
    AccountId = kz_util:format_account_id(Account),
    case kz_tasks:new(AccountId, Category, Action, 'undefined', 'undefined') of
        {'ok', TaskJObj} -> print_json(TaskJObj);
        {'error', Reason} -> handle_new_task_error(Reason, Category, Action)
    end.

-spec tasks() -> 'no_return'.
tasks() ->
    Tasks = kz_tasks:all(),
    print_json(Tasks).

-spec tasks(text()) -> 'no_return'.
tasks(Account) ->
    AccountId = kz_util:format_account_id(Account),
    Tasks = kz_tasks:all(AccountId),
    print_json(Tasks).

-spec task(text()) -> 'no_return'.
task(TaskId) ->
    case kz_tasks:read(TaskId) of
        {'ok', JObj} -> print_json(JObj);
        {'error', Reason} -> print_error(Reason)
    end.

-spec task_input(text()) -> 'no_return'.
task_input(TaskId) ->
    attachment(TaskId, ?KZ_TASKS_ATTACHMENT_NAME_IN).

-spec task_output(text()) -> 'no_return'.
task_output(TaskId) ->
    attachment(TaskId, ?KZ_TASKS_ATTACHMENT_NAME_OUT).

-spec start(text()) -> 'no_return'.
start(TaskId) ->
    case kz_tasks:start(TaskId) of
        {'ok', StartedTask} -> print_json(StartedTask);
        {'error', Reason} -> print_error(Reason)
    end.

-spec remove(text()) -> 'no_return'.
remove(TaskId) ->
    case kz_tasks:remove(TaskId) of
        {'ok', RemovedTask} -> print_json(RemovedTask);
        {'error', Reason} -> print_error(Reason)
    end.

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

-spec attachment(kz_tasks:task_id(), ne_binary()) -> 'no_return'.
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

-spec new_task(ne_binary(), ne_binary(), ne_binary(), pos_integer(), ne_binary()) -> 'no_return'.
new_task(AccountId, Category, Action, TotalRows, CSVBin) ->
    case kz_tasks:new(AccountId, Category, Action, TotalRows, CSVBin) of
        {'ok', TaskJObj} ->
            TaskId = kz_json:get_value([<<"_read_only">>, <<"id">>], TaskJObj),
            case kz_datamgr:put_attachment(?KZ_TASKS_DB
                                          ,TaskId
                                          ,?KZ_TASKS_ATTACHMENT_NAME_IN
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

-spec handle_new_task_error(atom() | kz_json:object(), ne_binary(), ne_binary()) -> 'no_return'.
handle_new_task_error('no_categories', _, _) ->
    _ = kz_util:spawn(fun kz_tasks:help/0),
    print_error(<<"No APIs known yet: please try again in a second.">>);
handle_new_task_error('unknown_category', Category, _) ->
    print_error(<<"No such category: ", Category/binary>>);
handle_new_task_error('unknown_action', _, Action) ->
    print_error(<<"No such action: ", Action/binary>>);
handle_new_task_error(JObj, _, _) ->
    print_json(kz_json:from_list([{<<"errors">>, JObj}])).

%%% End of Module
