-ifndef(WHISTLE_APPS_HRL).

-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_system_config.hrl").
-include_lib("whistle/include/kz_system_config.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(DEFAULT_WHAPPS, ['registrar'
                         ,'reorder'
                         ,'stepswitch'
                         ,'sysconf'
                         ,'media_mgr'
                         ,'callflow'
                         ,'notify'
                         ,'cdr'
                         ,'crossbar'
                         ,'trunkstore'
                         ,'conference'
                         ,'fax'
                         ,'hangups'
                         ,'omnipresence'
                         ,'milliwatt'
                         ,'pivot'
                         ,'camper'
                         ,'frontier'
                         ,'doodle'
                        ]).

-define(WHAPPS_GETBY_CACHE, 'whapps_getby_cache').

-define(WHISTLE_APPS_HRL, 'true').
-endif.
