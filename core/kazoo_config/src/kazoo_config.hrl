-ifndef(KAZOO_CONFIG_HRL).

-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

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
                           ,{'kazoo_apps', [{'cookie', 'change_me'}]}
                           ,{'log', [{'syslog', 'info'}
                                     ,{'console', 'notice'}
                                     ,{'file', 'error'}
                                    ]}
                          ]).
-type section() :: 'amqp' |
                   'bigcouch' |
                   'ecallmgr' |
                   'log' |
                   'kazoo_apps' |
                   'zone'.

-define(KAZOO_CONFIG_HRL, 'true').
-endif.
