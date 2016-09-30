-ifndef(KZ_VOICEMAIL_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(VMBOX_CB_LIST, <<"vmboxes/crossbar_listing">>).

-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CF_CONFIG_CAT, <<"callflow">>).
-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_RETENTION_DURATION, <<"message_retention_duration">>).

-define(RETENTION_PATH, [?KEY_VOICEMAIL, ?KEY_RETENTION_DURATION]).
-define(RETENTION_DAYS,
        kapps_config:get_integer(?CF_CONFIG_CAT, ?RETENTION_PATH, 93)  %% 93 days(3 months)
       ).

-define(TIME_BETWEEN_ACCOUNT_CRAWLS,
        kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_interaccount_delay_ms">>], ?MILLISECONDS_IN_SECOND)).

-type db_ret() :: 'ok' | {'ok', kz_json:object() | kz_json:objects()} | {'error', any()}.
-type vm_folder() :: ne_binary() | {ne_binary(), boolean()}.
-type message() :: ne_binary() | kz_json:object().
-type messages() :: ne_binaries() | kz_json:objects().
-type count_result() :: {non_neg_integer(), non_neg_integer()}.
-type update_funs() :: [fun((kz_json:object()) -> kz_json:object())].

-type next_account() :: {ne_binary(), gregorian_seconds(), gregorian_seconds()}.

-define(MODB_MSG_ID(Year, Month, Id),
        <<(kz_util:to_binary(Year))/binary
          ,(kz_util:pad_month(Month))/binary
          ,"-"
          ,(Id)/binary
        >>).

-define(KZ_VOICEMAIL_HRL, 'true').
-endif.
