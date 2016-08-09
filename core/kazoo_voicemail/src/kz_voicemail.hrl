-ifndef(KZ_VOICEMAIL_HRL).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").
-include_lib("kazoo/src/kz_json.hrl").

-define(VMBOX_CB_LIST, <<"vmboxes/crossbar_listing">>).

-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CF_CONFIG_CAT, <<"callflow">>).
-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_RETENTION_DURATION, <<"message_retention_duration">>).

-define(RETENTION_PATH, [?KEY_VOICEMAIL, ?KEY_RETENTION_DURATION]).
-define(RETENTION_DURATION
       ,kapps_config:get_integer(?CF_CONFIG_CAT, ?RETENTION_PATH, 93)  %% 93 days(3 months)
       ).

-define(RETENTION_DAYS(Duration)
       ,?SECONDS_IN_DAY * Duration + ?SECONDS_IN_HOUR
       ).

-define(KZ_VOICEMAIL_HRL, 'true').
-endif.
