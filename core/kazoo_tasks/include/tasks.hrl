-ifndef(KAZOO_TASKS_HRL).

-define(KZ_TASKS_BY_ACCOUNT, <<"tasks/listing_by_account">>).
-define(KZ_TASKS_BY_CREATED, <<"tasks/listing_by_created">>).

-define(KZ_TASKS_ANAME_IN, <<"in.csv">>).
-define(KZ_TASKS_ANAME_OUT, <<"out.csv">>).

%% These are returned to user.
-define(KZ_TASKS_INPUT_ERROR_MMV, <<"missing_mandatory_values">>).
-define(KZ_TASKS_INPUT_ERROR_MMF, <<"missing_mandatory_fields">>).
-define(KZ_TASKS_INPUT_ERROR_MIME, <<"expected_content">>).

-define(STATUS_STOPPED, <<"stopped">>).
-define(STATUS_EXECUTING, <<"executing">>).
-define(STATUS_FAILURE, <<"failure">>).
-define(STATUS_PARTIAL, <<"partial">>).
-define(STATUS_PENDING, <<"pending">>).
-define(STATUS_SUCCESS, <<"success">>).
-define(STATUS_BAD, <<"internal_error">>).

-define(NIL_MIME, <<"none">>).

-define(TRIGGER_ACCOUNT, <<"tasks.triggers.account">>).
-define(TRIGGER_ACCOUNT_MOD, <<"tasks.triggers.account_mod">>).
-define(TRIGGER_DAILY, <<"tasks.triggers.day">>).
-define(TRIGGER_HOURLY, <<"tasks.triggers.hour">>).
-define(TRIGGER_MINUTELY, <<"tasks.triggers.minute">>).
-define(TRIGGER_OTHER, <<"tasks.triggers.other">>).
-define(TRIGGER_SYSTEM, <<"tasks.triggers.system">>).

-define(TRIGGER_ALL_DBS, [?TRIGGER_ACCOUNT
                         ,?TRIGGER_ACCOUNT_MOD
                         ,?TRIGGER_SYSTEM
                         ,?TRIGGER_OTHER
                         ]).

-define(KAZOO_TASKS_HRL, 'true').
-endif.
