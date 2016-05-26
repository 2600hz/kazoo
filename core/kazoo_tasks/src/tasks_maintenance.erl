%%%-------------------------------------------------------------------
%%% @Copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(tasks_maintenance).

-export([tasks/0, tasks/1]).
-export([task/1]).
-export([add/4]).
-export([start/1]).
-export([remove/1]).

-include("tasks.hrl").


%%% API

-spec tasks() -> 'no_return'.
tasks() ->
    Tasks = kz_tasks:all(),
    print_json(Tasks).

-spec tasks(text() | ne_binary()) -> 'no_return'.
tasks(Account) ->
    AccountId = kz_util:format_account_id(Account),
    Tasks = kz_tasks:all(AccountId),
    print_json(Tasks).

-spec task(text() | kz_tasks:task_id()) -> 'no_return'.
task(TaskId) ->
    case kz_tasks:read(TaskId) of
        {'ok', JObj} -> print_json(JObj);
        {'error', Reason} -> print_error(Reason)
    end.

-spec add(text() | ne_binary(), text() | module(), text() | atom(), text() | list()) -> 'no_return'.
add(AccountId=?NE_BINARY, M, F, A)
  when is_atom(M),
       is_atom(F),
       is_list(A) ->
    case kz_tasks:new(AccountId, M, F, A) of
        {'ok', NewTask} -> print_json(NewTask);
        {'error', Reason} -> print_error(Reason)
    end;
add(AccountId, M=?NE_BINARY, F, A) ->
    add(AccountId, kz_util:to_atom(M, 'true'), F, A);
add(AccountId, M, F=?NE_BINARY, A) ->
    add(AccountId, M, kz_util:to_atom(F, 'true'), A);
add(AccountId, M, F, BinA=?NE_BINARY) ->
    try
        {'ok', Tokens, _} = erl_scan:string(binary_to_list(<<BinA/binary, ".">>)),
        {'ok', Exprs} = erl_parse:parse_exprs(Tokens),
        Bindings = [],
        {'value', A, Bindings} = erl_eval:exprs(Exprs, Bindings),
        add(AccountId, M, F, A)
    catch
        _E:_R ->
            print_error({'not_a_list', BinA})
    end.

-spec start(text() | kz_tasks:task_id()) -> 'no_return'.
start(TaskId) ->
    case kz_tasks:start(TaskId) of
        {'ok', StartedTask} -> print_json(StartedTask);
        {'error', Reason} -> print_error(Reason)
    end.

-spec remove(text() | kz_tasks:task_id()) -> 'no_return'.
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
print_error(Reason) ->
    io:format("ERROR: ~p\n", [Reason]),
    'no_return'.

%%% End of Module
