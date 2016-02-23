-ifndef(WHISTLE_CONFIG_HRL).

-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_system_config.hrl").

-define(WHAPPS_CONFIG_CACHE, 'whapps_config_cache').
-define(CONFIG_FILE_ENV, "KAZOO_CONFIG").
-define(CONFIG_FILE, "/etc/kazoo/config.ini").

-define(DEFAULT_DEFAULTS, []).
-define(SECTION_DEFAULTS, [{'amqp', [{'uri', "amqp://guest:guest@localhost:5672"}
                                     ,{'use_federation', 'false'}
                                    ]
                           }
                           ,{'bigcouch', [{'ip', "localhost"}
                                          ,{'port', 5984}
                                          ,{'username', ""}
                                          ,{'password', ""}
                                          ,{'admin_port', 5986}
                                          ,{'cookie', 'monster'}
                                          ,{'compact_automatically', 'true'}
                                         ]
                            }
                           ,{'ecallmgr', [{'cookie', 'change_me'}]}
                           ,{'whistle_apps', [{'cookie', 'change_me'}]}
                           ,{'log', [{'syslog', 'info'}
                                     ,{'console', 'notice'}
                                     ,{'file', 'error'}
                                    ]}
                          ]).
-type section() :: 'bigcouch' | 'amqp' |
                   'whistle_apps' | 'ecallmgr' |
                   'zone' | 'log'.

-define(WHISTLE_CONFIG_HRL, 'true').
-endif.
