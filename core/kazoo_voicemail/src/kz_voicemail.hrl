-ifndef(KZ_VOICEMAIL_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(VMBOX_CB_LIST, <<"vmboxes/crossbar_listing">>).

-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(VM_CONFIG_CAT, <<"callflow">>).
-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_RETENTION_DURATION, <<"message_retention_duration">>).
-define(KEY_VM_DELETE_NOTIFY_AMPQ, <<"vm_delete_amqp">>).

-define(VM_DELETE_NOTIFY_AMPQ_PATH, [?KEY_VOICEMAIL, ?KEY_VM_DELETE_NOTIFY_AMPQ]).
-define(SEND_DELETE_NOTIFY_AMPQ,
        kapps_config:get_boolean(?VM_CONFIG_CAT, ?VM_DELETE_NOTIFY_AMPQ_PATH, false)
       ).

-define(RETENTION_PATH, [?KEY_VOICEMAIL, ?KEY_RETENTION_DURATION]).
-define(RETENTION_DAYS,
        kapps_config:get_integer(?VM_CONFIG_CAT, ?RETENTION_PATH, 93)  %% 93 days(3 months)
       ).

-define(TIME_BETWEEN_ACCOUNT_CRAWLS,
        kapps_config:get_integer(?VM_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_interaccount_delay_ms">>], 2 * ?MILLISECONDS_IN_SECOND)).

-type db_ret() :: 'ok' | {'ok', kz_json:object() | kz_json:objects()} | {'error', any()}.
-type vm_folder() :: kz_term:ne_binary() | {kz_term:ne_binary(), boolean()}.
-type message() :: kz_term:ne_binary() | kz_json:object().
-type messages() :: kz_term:ne_binaries() | kz_json:objects().
-type update_funs() :: [fun((kz_json:object()) -> kz_json:object())].

-type bulk_map() :: #{succeeded => kz_term:ne_binaries() | kz_json:objects()
                     ,failed => [{kz_term:ne_binary(), kz_term:ne_binary()}]
                     ,to_update_map => #{kz_term:ne_binary() => kz_json:objects()}
                     ,enforce_set => sets:set()
                     }.

-type next_account() :: {kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()}.

-type vm_delete_reason() :: 'dtmf' | 'delete_after_notify' | 'crossbar_action'.

-define(KZ_VOICEMAIL_HRL, 'true').
-endif.
