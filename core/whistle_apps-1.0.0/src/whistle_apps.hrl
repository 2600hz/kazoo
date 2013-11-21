-ifndef(WHISTLE_APPS_HRL).

-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_system_config.hrl").

-define(WHAPPS_CONFIG_CACHE, 'whapps_config_cache').

-define(WHAPPS_AMQP_POOL, 'whapps_amqp_pool').

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
                        ]).

-define(WHISTLE_APPS_HRL, 'true').
-endif.
