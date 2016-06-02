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
-define(PVT_ERROR, <<"pvt_error">>).
-define(PVT_FAILED_AT, <<"pvt_failed_at">>).
-define(PVT_FINISHED_AT, <<"pvt_finished_at">>).
-define(PVT_IS_SUCCESS, <<"pvt_is_success">>).
-define(PVT_IS_TERMINATED, <<"pvt_is_terminated">>).
-define(PVT_RAN_FOR, <<"pvt_ran_for">>).
-define(PVT_STARTED_AT, <<"pvt_started_at">>).
-define(PVT_SUBMITTED_AT, <<"pvt_submitted_at">>).
-define(PVT_TYPE, <<"pvt_type">>).
-define(PVT_WORKER_NODE, <<"pvt_worker_node">>).

-define(API_MANDATORY, <<"mandatory">>).
-define(API_OPTIONAL, <<"optional">>).

-define(KZ_TASKS_HRL, 'true').
-endif.
