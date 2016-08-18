-ifndef(KAZOO_TASKS_HRL).

-define(KZ_TASKS_BY_ACCOUNT, <<"tasks/listing_by_account">>).

-define(KZ_TASKS_ATTACHMENT_NAME_IN, <<"in.csv">>).
-define(KZ_TASKS_ATTACHMENT_NAME_OUT, <<"out.csv">>).

%% These are returned to user.
-define(KZ_TASKS_INPUT_ERROR_MMV, <<"missing_mandatory_values">>).
-define(KZ_TASKS_INPUT_ERROR_MMF, <<"missing_mandatory_fields">>).
-define(KZ_TASKS_INPUT_ERROR_UF, <<"unknown_fields">>).
-define(KZ_TASKS_INPUT_ERROR_MIME, <<"expected_content">>).

-define(STATUS_EXECUTING, <<"executing">>).
-define(STATUS_FAILURE, <<"failure">>).
-define(STATUS_PARTIAL, <<"partial">>).
-define(STATUS_PENDING, <<"pending">>).
-define(STATUS_SUCCESS, <<"success">>).
-define(STATUS_BAD, <<"internal_error">>).

-define(KAZOO_TASKS_HRL, 'true').
-endif.
