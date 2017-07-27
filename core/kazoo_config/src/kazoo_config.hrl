-ifndef(KAZOO_CONFIG_HRL).

-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").
-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-define(CONFIG_FILE_ENV, "KAZOO_CONFIG").
-define(CONFIG_FILE, "/etc/kazoo/config.ini").
-define(V4_CONFIG_FILE, "/etc/kazoo/core/config.ini").

-define(APP, kazoo_config).
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).

-define(SETTINGS_KEY, '$_App_Settings').

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
                          ,{'log', [{'syslog', 'info'}
                                   ,{'console', 'notice'}
                                   ,{'file', 'error'}
                                   ]}
                          ,{'kazoo_apps', [{'cookie', 'change_me'}]}
                          ]).

-type section() :: 'amqp' |
                   'bigcouch' |
                   'ecallmgr' |
                   'log' |
                   'kazoo_apps' |
                   'zone'.

-ifdef(TEST).
-define(TEST_CAT, <<"test_category">>).
-define(TEST_CAT_EMPTY, <<"test_category_empty">>).

%% AccountIds for different test situation
-define(NOT_CUSTOMIZED_ALL_ACCOUNTS, <<"087ca0424ec8acf3acc1f87ac81f28e1">>).
-define(CUSTOMIZED_RESELLER, <<"123ca0424ec8acf3acc1f87ac81f2123">>).
-define(CUSTOMIZED_RESELLER_UNDEFINED, <<"7cc16a9cd8bcfeea4732986790e930a4">>).
-define(CUSTOMIZED_SUBACCOUNT_UNDEFINED, <<"ce22d0d38c8b1e16e4e68ec941eed319">>).
-define(CUSTOMIZED_SUBACCOUNT, <<"5bb4a2b6323bd02a77f1049669073643">>).

-define(A_MASTER_ACCOUNT_ID, <<"6b71cb72c876b5b1396a335f8f8a2594">>).
-define(A_MASTER_ACCOUNT_DB, <<"account%2F6b%2F71%2Fcb72c876b5b1396a335f8f8a2594">>).

-define(AN_ACCOUNT_ID, <<"009afc511c97b2ae693c6cc4920988e8">>).
-define(AN_ACCOUNT_DB, <<"account%2F00%2F9a%2Ffc511c97b2ae693c6cc4920988e8">>).

-define(AN_ACCOUNT_USER_ID, <<"8e248327b85591955749e53ea45b6baa">>).

-endif.

-ifdef(TEST).
-define(LOG_ERROR(F,A), io:format(user, "\n" ++ "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_WARN(F,A), io:format(user, "\n" ++ "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_DEBUG(F,A), io:format(user, "\n" ++ "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_DEBUG(F), io:format(user, "\n" ++ "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE])).
-else.
-define(LOG_ERROR(F,A), lager:error(F,A)).
-define(LOG_WARN(F,A), lager:warning(F,A)).
-define(LOG_DEBUG(F,A), lager:debug(F,A)).
-define(LOG_DEBUG(F), lager:debug(F)).
-endif.

-define(KAZOO_CONFIG_HRL, 'true').
-endif.
