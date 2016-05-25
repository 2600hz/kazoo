-ifndef(KAZOO_APPS_HRL).

-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-define(DEFAULT_KAPPS, ['blackhole'
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
                         ,'jonny5'
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
