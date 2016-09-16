-ifndef(KZ_TASKS_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_tasks/include/tasks.hrl").

-define(CONFIG_CAT, <<"tasks">>).
-define(APP, 'tasks').
-define(APP_NAME, <<"tasks">>).
-define(APP_VERSION, <<"4.0.0">> ).

%%TODO: do not hardcode ?TASKS list.
-define(TASKS, ['kt_numbers'
               ,'kt_services'
                %% ,'kt_skel'
               ]).

-define(WORKER_TASK_FAILED, <<"applier crashed">>).
-define(WORKER_TASK_TYPE, <<"bad input">>).
-define(WORKER_TASK_MAYBE_OK, <<"internal">>).

-define(OUTPUT_CSV_HEADER_ROW, [<<"error">>]).

-define(TRIGGER_MINUTE, <<"tasks.triggers.minute">>).
-define(TRIGGER_HOUR, <<"tasks.triggers.hour">>).
-define(TRIGGER_DAY, <<"tasks.triggers.day">>).

-define(KZ_TASKS_HRL, 'true').
-endif.
