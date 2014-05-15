-ifndef(REG_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"registrar">>).
-define(APP_VERSION, <<"0.4.2">>).

-define(CONFIG_CAT, <<"registrar">>).

-record(auth_user, {realm
                    ,username
                    ,password
                    ,account_id
                    ,account_db
                    ,authorizing_type
                    ,authorizing_id
                    ,method
                    ,owner_id
                    ,suppress_unregister_notifications
                    ,register_overwrite_notify
                    ,account_realm
                    ,account_name
                    ,nonce
                    ,a3a8_kc
                    ,a3a8_sres
                    ,primary_number
                   }).
-type auth_user() :: #auth_user{}.

-define(GSM_AUTH_METHOD, <<"gsm">>).
-define(ANY_AUTH_METHOD, _ ).

-define(REG_HRL, 'true').
-endif.
