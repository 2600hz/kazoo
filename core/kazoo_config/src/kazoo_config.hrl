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

%% Account Ids to emulate not_found
-define(NO_CONFIG, <<"175d1885aa75d6f243e5f1c57c5b3e9b">>).
-define(NOT_CUSTOMIZED_ALL_ACCOUNTS, <<"087ca0424ec8acf3acc1f87ac81f28e1">>).

%% AccountIds for different reseller scenario
-define(CUSTOMIZED_RESELLER, <<"123ca0424ec8acf3acc1f87ac81f2123">>). %% do not use this for hierarchy_merge
-define(CUSTOMIZED_RESELLER_UNDEFINED, <<"7cc16a9cd8bcfeea4732986790e930a4">>).
-define(CUSTOMIZED_RESELLER_HIER, <<"f3b9104a6dc3ab36fbcf413444c30a12">>). %% for hierarchy_merge tests only
-define(NOT_CUSTOMIZED_RESELLER, <<"7714f8d8936380295c0129c9351a2038">>).
-define(SELF_RESELLER, <<"62135a704ba5401c85ce549933340c17">>).

%% AccountIds for different configured sub-account (not reseller) scenario
%% (parent of another test account)
-define(CUSTOMIZED_SUBACCOUNT_1_UNDEFINED, <<"ce22d0d38c8b1e16e4e68ec941eed319">>).
-define(CUSTOMIZED_SUBACCOUNT_1, <<"5bb4a2b6323bd02a77f1049669073643">>).

%% AccountIds for different sub-sub-account (not reseller) scenario
%% (child of a non reseller parent and a reseller)
%% (for hierarchy_merge tests)
%% Customized Sub-Sub-Account and customized parent scenario
-define(CUST_A_CUST_P_CUST_R, <<"94b2fdbd4b92c8c1d3c3331aff97e16a">>).
-define(CUST_A_CUST_P_404_R, <<"9854f203fdef737fe533a119e69da0d8">>).
-define(CUST_A_CUST_P_EMPTY_R, <<"1e4b041dd12e66f0d90e777cb5a5a31c">>).

%% Customized Sub-Sub-Account and not customized parent scenario
-define(CUST_A_404_P_CUST_R, <<"c300f5f4ebe4cbeeeeb49482bd0b5fd3">>).
-define(CUST_A_404_P_404_R, <<"97b53cf174c7dd378f4aff4b687f09df">>).


-define(CUSTOMIZED_SUBACCOUNT_2, <<"94b2fdbd4b92c8c1d3c3331aff97e16a">>).

-define(A_MASTER_ACCOUNT_ID, <<"6b71cb72c876b5b1396a335f8f8a2594">>).

-define(AN_ACCOUNT_ID, <<"009afc511c97b2ae693c6cc4920988e8">>).

-define(AN_ACCOUNT_USER_ID, <<"8e248327b85591955749e53ea45b6baa">>).

-define(LOG_DEBUG(F,A), io:format(user, "\n" ++ "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_DEBUG(F), io:format(user, "\n" ++ "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE])).

-else.

-define(LOG_DEBUG(F,A), lager:debug(F,A)).
-define(LOG_DEBUG(F), lager:debug(F)).
-endif.

-define(KAZOO_CONFIG_HRL, 'true').
-endif.
