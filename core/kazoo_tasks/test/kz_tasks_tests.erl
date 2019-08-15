%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tasks_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_tasks/include/tasks.hrl").

-define(ACCOUNT_ID, <<"account_id_11cf2770fc736e56a4cb5">>).
-define(AUTH_ACCOUNT_ID, <<"auth_account_id_8d593d7d19bd0b12">>).
-define(CATEGORY, <<"number_management">>).
-define(ACTION_WITH_INPUT, <<"assign_to">>).
-define(ACTION_WITHOUT_INPUT, <<"dump_available">>).
-define(INPUT_FILE_NAME, <<"my_input.csv">>).


is_processing_test_() ->
    [?_assert(kz_tasks:is_processing(task(executing)))
    ,?_assert(kz_tasks:is_processing(task(executing_noinput)))
    ,?_assert(not kz_tasks:is_processing(task(stopped)))
    ,?_assert(not kz_tasks:is_processing(task(stopped_noinput)))
    ,?_assert(not kz_tasks:is_processing(task(pending)))
    ,?_assert(not kz_tasks:is_processing(task(pending_noinput)))
    ,?_assert(not kz_tasks:is_processing(task(success)))
    ,?_assert(not kz_tasks:is_processing(task(success_noinput)))
    ,?_assert(not kz_tasks:is_processing(task(partial)))
    ,?_assert(not kz_tasks:is_processing(task(partial_noinput)))
    ,?_assert(not kz_tasks:is_processing(task(failure)))
    ,?_assert(not kz_tasks:is_processing(task(failure_noinput)))
    ,?_assert(not kz_tasks:is_processing(task(bad)))
    ,?_assert(not kz_tasks:is_processing(task(bad_noinput)))
    ].

status_test_() ->
    [?_assertEqual(?STATUS_EXECUTING, kz_tasks:status(task(executing)))
    ,?_assertEqual(?STATUS_EXECUTING, kz_tasks:status(task(executing_noinput)))
    ,?_assertEqual(?STATUS_STOPPED, kz_tasks:status(task(stopped)))
    ,?_assertEqual(?STATUS_STOPPED, kz_tasks:status(task(stopped_noinput)))
    ,?_assertEqual(?STATUS_PENDING, kz_tasks:status(task(pending)))
    ,?_assertEqual(?STATUS_PENDING, kz_tasks:status(task(pending_noinput)))
    ,?_assertEqual(?STATUS_SUCCESS, kz_tasks:status(task(success)))
    ,?_assertEqual(?STATUS_SUCCESS, kz_tasks:status(task(success_noinput)))
    ,?_assertEqual(?STATUS_PARTIAL, kz_tasks:status(task(partial)))
    ,?_assertEqual(?STATUS_PARTIAL, kz_tasks:status(task(partial_noinput)))
    ,?_assertEqual(?STATUS_FAILURE, kz_tasks:status(task(failure)))
    ,?_assertEqual(?STATUS_FAILURE, kz_tasks:status(task(failure_noinput)))
    ,?_assertEqual(?STATUS_BAD, kz_tasks:status(task(bad)))
    ,?_assertEqual(?STATUS_BAD, kz_tasks:status(task(bad_noinput)))
    ].


%% Internals

noinput(Type) ->
    (task(Type))#{action => ?ACTION_WITHOUT_INPUT
                 ,file_name => undefined
                 ,total_rows => undefined
                 }.

task(bad) ->
    #{worker_pid => self()
     ,worker_node => kz_term:to_binary(node())
     ,account_id => ?ACCOUNT_ID
     ,auth_account_id => ?AUTH_ACCOUNT_ID
     ,id => kz_tasks:new_id()
     ,category => ?CATEGORY
     ,action => ?ACTION_WITH_INPUT
     ,file_name => ?INPUT_FILE_NAME
     ,created => 63669093916
     ,started => 63669094000
     ,finished => 63669094010
     ,total_rows => 42
     ,total_rows_failed => undefined
     ,total_rows_succeeded => 0
     };

task(executing_noinput) -> noinput(executing);
task(stopped_noinput) -> noinput(stopped);
task(pending_noinput) -> noinput(pending);
task(success_noinput) -> noinput(success);
task(partial_noinput) -> noinput(partial);
task(failure_noinput) -> noinput(failure);
task(bad_noinput) -> noinput(bad);

task(failure) ->
    #{worker_pid => self()
     ,worker_node => kz_term:to_binary(node())
     ,account_id => ?ACCOUNT_ID
     ,auth_account_id => ?AUTH_ACCOUNT_ID
     ,id => kz_tasks:new_id()
     ,category => ?CATEGORY
     ,action => ?ACTION_WITH_INPUT
     ,file_name => ?INPUT_FILE_NAME
     ,created => 63669093916
     ,started => 63669094000
     ,finished => 63669094010
     ,total_rows => 42
     ,total_rows_failed => 42
     ,total_rows_succeeded => 0
     };

task(partial) ->
    #{worker_pid => self()
     ,worker_node => kz_term:to_binary(node())
     ,account_id => ?ACCOUNT_ID
     ,auth_account_id => ?AUTH_ACCOUNT_ID
     ,id => kz_tasks:new_id()
     ,category => ?CATEGORY
     ,action => ?ACTION_WITH_INPUT
     ,file_name => ?INPUT_FILE_NAME
     ,created => 63669093916
     ,started => 63669094000
     ,finished => 63669094010
     ,total_rows => 42
     ,total_rows_failed => 1
     ,total_rows_succeeded => 41
     };

task(success) ->
    #{worker_pid => self()
     ,worker_node => kz_term:to_binary(node())
     ,account_id => ?ACCOUNT_ID
     ,auth_account_id => ?AUTH_ACCOUNT_ID
     ,id => kz_tasks:new_id()
     ,category => ?CATEGORY
     ,action => ?ACTION_WITH_INPUT
     ,file_name => ?INPUT_FILE_NAME
     ,created => 63669093916
     ,started => 63669094000
     ,finished => 63669094010
     ,total_rows => 42
     ,total_rows_failed => 0
     ,total_rows_succeeded => 42
     };

task(pending) ->
    #{worker_pid => undefined
     ,worker_node => undefined
     ,account_id => ?ACCOUNT_ID
     ,auth_account_id => ?AUTH_ACCOUNT_ID
     ,id => kz_tasks:new_id()
     ,category => ?CATEGORY
     ,action => ?ACTION_WITH_INPUT
     ,file_name => ?INPUT_FILE_NAME
     ,created => 63669093916
     ,started => undefined
     ,finished => undefined
     ,total_rows => 42
     ,total_rows_failed => undefined
     ,total_rows_succeeded => undefined
     };

task(stopped) ->
    #{worker_pid => self()
     ,worker_node => kz_term:to_binary(node())
     ,account_id => ?ACCOUNT_ID
     ,auth_account_id => ?AUTH_ACCOUNT_ID
     ,id => kz_tasks:new_id()
     ,category => ?CATEGORY
     ,action => ?ACTION_WITH_INPUT
     ,file_name => ?INPUT_FILE_NAME
     ,created => 63669093916
     ,started => 63669094000
     ,finished => undefined
     ,total_rows => 42
     ,total_rows_failed => 0
     ,total_rows_succeeded => 10
     ,was_stopped => true
     };

task(executing) ->
    #{worker_pid => self()
     ,worker_node => kz_term:to_binary(node())
     ,account_id => ?ACCOUNT_ID
     ,auth_account_id => ?AUTH_ACCOUNT_ID
     ,id => kz_tasks:new_id()
     ,category => ?CATEGORY
     ,action => ?ACTION_WITH_INPUT
     ,file_name => ?INPUT_FILE_NAME
     ,created => 63669093916
     ,started => 63669094000
     ,finished => undefined
     ,total_rows => 42
     ,total_rows_failed => 0
     ,total_rows_succeeded => 10
     }.
