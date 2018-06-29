-ifndef(KAZOO_APPS_HRL).

-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-define(APP_NAME, <<"kazoo_apps">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(APP, 'kazoo_apps').

-define(KAPPS_CONFIG_CACHE, 'kapps_config_cache').
-define(KAPPS_GETBY_CACHE, 'kapps_getby_cache').

-define(DEFAULT_KAPPS, ['blackhole'
                       ,'callflow'
                       ,'cdr'
                       ,'conference'
                       ,'crossbar'
                       ,'fax'
                       ,'hangups'
                       ,'media_mgr'
                       ,'milliwatt'
                       ,'omnipresence'
                       ,'pivot'
                       ,'registrar'
                       ,'reorder'
                       ,'stepswitch'
                       ,'sysconf'
                       ,'tasks'
                       ,'teletype'
                       ,'trunkstore'
                       ,'webhooks'
                       ]).

-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(SEARCH_VIEW_FILE, <<"views/search.json">>).

-define(KAZOO_APPS_HRL, 'true').
-endif.
