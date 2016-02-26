-ifndef(WHISTLE_APPS_HRL).

-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_system_config.hrl").
-include_lib("whistle/include/kz_system_config.hrl").

-define(DEFAULT_WHAPPS, ['blackhole'
                         ,'callflow'
                         ,'cdr'
                         ,'conference'
                         ,'crossbar'
                         ,'doodle'
                         ,'ecallmgr'
                         ,'fax'
                         ,'hangups'
                         ,'hotornot'
                         ,'konami'
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

-define(WHAPPS_GETBY_CACHE, 'whapps_getby_cache').

-define(WHISTLE_APPS_HRL, 'true').
-endif.
