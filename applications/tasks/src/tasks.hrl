-ifndef(KZ_TASKS_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_tasks/include/tasks.hrl").

-define(CONFIG_CAT, <<"tasks">>).
-define(APP, 'tasks').
-define(APP_NAME, <<"tasks">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(TASKS, ['kt_skel'
               ,'kt_numbers'
               ]).

-define(API_MANDATORY, <<"mandatory">>).
-define(API_OPTIONAL, <<"optional">>).
-define(API_INPUT_MIME, <<"expected_content">>).

-define(STATUS_EXECUTING, <<"executing">>).
-define(STATUS_FAILURE, <<"failure">>).
-define(STATUS_PARTIAL, <<"partial">>).
-define(STATUS_PENDING, <<"pending">>).
-define(STATUS_SUCCESS, <<"success">>).
-define(STATUS_BAD, <<"internal_error">>).

-define(WORKER_TASK_FAILED, <<"applier crashed">>).
-define(WORKER_TASK_TYPE, <<"bad input">>).
-define(WORKER_TASK_MAYBE_OK, <<"internal">>).

-define(OUTPUT_CSV_HEADER_ROW, [<<"error">>]).

-define(KZ_TASKS_HRL, 'true').
-endif.
