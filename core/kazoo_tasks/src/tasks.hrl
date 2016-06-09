-ifndef(KZ_TASKS_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_tasks/include/kazoo_tasks.hrl").

-define(APP_NAME, <<"kazoo_tasks">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(PVT_ACCOUNT_ID, <<"pvt_account_id">>).
-define(PVT_ACTION, <<"pvt_action">>).
-define(PVT_CATEGORY, <<"pvt_category">>).
-define(PVT_CREATED, <<"pvt_created">>).
-define(PVT_FINISHED_AT, <<"pvt_ended_at">>).
-define(PVT_STARTED_AT, <<"pvt_started_at">>).
-define(PVT_STATUS, <<"pvt_status">>).
-define(PVT_TOTAL_ROWS, <<"pvt_total_rows">>).
-define(PVT_TOTAL_ROWS_FAILED, <<"pvt_total_rows_failed">>).
-define(PVT_TOTAL_ROWS_SUCCEEDED, <<"pvt_total_rows_succeeded">>).
-define(PVT_TYPE, <<"pvt_type">>).
-define(PVT_WORKER_NODE, <<"pvt_worker_node">>).
-define(PVT_MODIFIED, <<"pvt_modified">>).

-define(API_MANDATORY, <<"mandatory">>).
-define(API_OPTIONAL, <<"optional">>).

-define(STATUS_EXECUTING, <<"executing">>).
-define(STATUS_FAILURE, <<"failure">>).
-define(STATUS_PARTIAL, <<"partial">>).
-define(STATUS_PENDING, <<"pending">>).
-define(STATUS_SUCCESS, <<"success">>).

-define(KZ_TASKS_HRL, 'true').
-endif.
