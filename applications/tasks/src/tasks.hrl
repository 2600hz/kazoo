-ifndef(KZ_TASKS_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_tasks/include/tasks.hrl").

-define(APP, 'tasks').
-define(APP_NAME, <<"tasks">>).
-define(APP_VERSION, <<"4.0.0">> ).
-define(CONFIG_CAT, ?APP_NAME).

%%TODO: do not hardcode ?TASKS list.
-define(TASKS, ['kt_numbers'
               ,'kt_services'
                %% ,'kt_skel'
               ]).

-define(WORKER_TASK_FAILED, <<"applier crashed">>).
-define(WORKER_TASK_TYPE, <<"bad input">>).
-define(WORKER_TASK_MAYBE_OK, <<"internal">>).

-define(OUTPUT_CSV_HEADER_ROW, [<<"error">>]).

-define(TRIGGER_ACCOUNT, <<"tasks.triggers.account">>).
-define(TRIGGER_ACCOUNT_MOD, <<"tasks.triggers.account_mod">>).
-define(TRIGGER_DAY, <<"tasks.triggers.day">>).
-define(TRIGGER_HOUR, <<"tasks.triggers.hour">>).
-define(TRIGGER_MINUTE, <<"tasks.triggers.minute">>).
-define(TRIGGER_OTHER, <<"tasks.triggers.other">>).
-define(TRIGGER_SYSTEM, <<"tasks.triggers.system">>).

-define(TRIGGER_ALL_DBS, [?TRIGGER_ACCOUNT
                         ,?TRIGGER_ACCOUNT_MOD
                         ,?TRIGGER_SYSTEM
                         ,?TRIGGER_OTHER
                         ]).

-define(KZ_TASKS_HRL, 'true').
-endif.
