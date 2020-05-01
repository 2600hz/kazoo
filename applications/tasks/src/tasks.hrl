-ifndef(KZ_TASKS_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_tasks/include/tasks.hrl").

-define(APP, 'tasks').
-define(APP_NAME, <<"tasks">>).
-define(APP_VERSION, <<"4.0.0">> ).
-define(CONFIG_CAT, ?APP_NAME).

-define(WORKER_TASK_FAILED, <<"applier crashed">>).
-define(WORKER_TASK_MAYBE_OK, <<"internal">>).

-define(OUTPUT_CSV_HEADER_ERROR, <<"error">>).

-define(KZ_TASKS_HRL, 'true').
-endif.
