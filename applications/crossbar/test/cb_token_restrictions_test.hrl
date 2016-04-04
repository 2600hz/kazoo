-ifndef(CB_TOKEN_RESTRICTIONS_TEST_HRL).

-include("crossbar_types.hrl").

-define(AUTH_ACCOUNT_ID, <<"6eb37425cd30bf7e5b1785abdcbc2c1d">>).
-define(ACCOUNT_ID, <<"c959c8f96db9891f99e6f655d1aeb503">>).
-define(DEVICE_ID, <<"d60fb8c1f0930f2d0d9c9f878f271432">>).

-define(ARGS_EMPTY, <<"/">>).
-define(ARGS_SINGLE, <<"*">>).
-define(ARGS_ANY, <<"#">>).
-define(ARGS_EXACT, ?DEVICE_ID).
-define(ARGS_ANY_TWO, <<"*/*">>).
-define(ARGS_ANY_THREE, <<"*/*/*">>).
-define(ARGS_EXACT_THEN_ANY, <<?DEVICE_ID/binary, "/#">>).

-define(HTTP_ANY, [?CATCH_ALL]).
-define(HTTP_GET_ONLY, [?HTTP_GET]).
-define(HTTP_POST_ONLY, [?HTTP_POST]).
-define(HTTP_GET_POST, [?HTTP_GET, ?HTTP_POST]).

-define(ACCOUNT_AUTH, <<"{AUTH_ACCOUNT_ID}">>).
-define(ACCOUNT_DESCENDANT, <<"{DESCENDANT_ACCOUNT_ID}">>).

-define(ACCOUNTS_AUTH, [?ACCOUNT_AUTH]).
-define(ACCOUNTS_DESCENDANT, [?ACCOUNT_DESCENDANT]).
-define(ACCOUNTS_ANY, [?CATCH_ALL]).
-define(ACCOUNTS_EXACT, [?ACCOUNT_ID]).

-define(ENDPOINT_DEVICE
        ,wh_json:from_list(
           [{<<"allowed_accounts">>, ?ACCOUNTS_AUTH}
            ,{<<"rules">>, wh_json:new()}
           ]
          )
       ).

-define(DEVICE_RULES
        ,wh_json:from_list([{<<"devices">>, [?ENDPOINT_DEVICE]}])
       ).

-define(ALLOW_ALL_RULE_RESTRICTIONS
        ,wh_json:from_list(
           [{?CATCH_ALL
             ,[wh_json:from_list(
                 [{<<"allowed_accounts">>, ?ACCOUNTS_ANY}
                  ,{<<"rules">>, wh_json:from_list([{?ARGS_ANY, ?HTTP_ANY}])}
                 ]
                )
              ]
            }
           ]
          )
       ).

-define(DENY_API_ENDPOINT_RESTRICTIONS
        ,wh_json:from_list(
           [{<<"accounts">>
             ,[wh_json:from_list(
                 [{<<"allowed_accounts">>, ?ACCOUNTS_ANY}
                  ,{<<"rules">>, wh_json:from_list([{?ARGS_ANY, ?HTTP_ANY}])}
                 ]
                )
              ]
            }
           ]
          )
       ).

-define(ALLOW_API_ENDPOINT_RESTRICTIONS
        ,wh_json:from_list(
           [{<<"devices">>
             ,[wh_json:from_list(
                 [{<<"allowed_accounts">>, ?ACCOUNTS_ANY}
                  ,{<<"rules">>, wh_json:from_list([{?ARGS_ANY, ?HTTP_ANY}])}
                 ]
                )
              ]
            }
           ]
          )
       ).

-define(ALLOW_ACCOUNTS_RESTRICTIONS(AccountId)
        ,wh_json:from_list(
           [{<<"devices">>
             ,[wh_json:from_list(
                 [{<<"allowed_accounts">>
                   ,case AccountId of 'undefined' -> 'undefined'; _ -> [AccountId] end
                  }
                  ,{<<"rules">>, wh_json:from_list([{?ARGS_ANY, ?HTTP_ANY}])}
                 ]
                )
              ]
            }
           ]
          )
       ).

-define(ARGUMENTS_RESTRICTIONS(Arg)
        ,wh_json:from_list(
           [{<<"devices">>
             ,[wh_json:from_list(
                 [{<<"allowed_accounts">>, [?ACCOUNT_ID]}
                  ,{<<"rules">>, wh_json:from_list([{Arg, ?HTTP_ANY}])}
                 ]
                )
              ]
            }
           ]
          )
       ).

-define(HTTP_VERB_RESTRICTIONS(Verbs)
        ,wh_json:from_list(
           [{<<"devices">>
             ,[wh_json:from_list(
                 [{<<"allowed_accounts">>, [?ACCOUNT_ID]}
                  ,{<<"rules">>, wh_json:from_list([{?ARGS_ANY, Verbs}])}
                 ]
                )
              ]
            }
           ]
          )
       ).

-define(CB_TOKEN_RESTRICTIONS_TEST_HRL, 'true').
-endif.
