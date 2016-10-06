-ifndef(KAZOO_APPS_HRL).

-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-define(APP_NAME, <<"kazoo_apps">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(APP, kazoo_apps).

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
                       ,'teletype'
                       ,'trunkstore'
                       ,'webhooks'
                       ]).

-define(KAZOO_APPS_HRL, 'true').
-endif.
