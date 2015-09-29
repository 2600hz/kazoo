-ifndef(CIRCLEMAKER_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CONFIG_CAT, <<"circlemaker">>).
-define(WORKER_POOL, 'circlemaker_worker_pool').

-define(APP_VERSION, <<"1.0.0">>).
-define(APP_NAME, <<"circlemaker">>).

-define(ETS_SESSION_TIMEOUT, 'cm_session_timeout').
-define(ETS_INTERIM_UPDATE, 'cm_interim_update').
-define(ETS_DELAY_ACCOUNTING, 'cm_delay_accounting').
-define(ETS_DEVICE_INFO, 'cm_device_info').
-define(ETS_LOOPBACK_CHANNELS, 'cm_loopback_channels').
-define(ETS_ORIG_INBOUND_LEG, 'cm_orig_inbound_leg').
-define(ETS_CACHED_CHANNEL_FS_STATUS, 'cm_cached_channel_fs_status').
-define(ETS_CACHED_CHANNEL_TYPE, 'cm_cached_channel_type').

-define(CCV, <<"Custom-Channel-Vars">>).

-define(CIRCLEMAKER_HRL, 'true').
-endif.
