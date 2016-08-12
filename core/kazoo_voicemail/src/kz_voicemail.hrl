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

-define(RETRY_CONFLICT(F), kvm_util:retry_conflict(fun() -> F end)).

-define(RETENTION_PATH, [?KEY_VOICEMAIL, ?KEY_RETENTION_DURATION]).
-define(RETENTION_DURATION
       ,kapps_config:get_integer(?CF_CONFIG_CAT, ?RETENTION_PATH, 93)  %% 93 days(3 months)
       ).

-define(RETENTION_DAYS(Duration)
       ,?SECONDS_IN_DAY * Duration + ?SECONDS_IN_HOUR
       ).

-record(bulk_res, {succeeded = []  :: ne_binaries()
                  ,failed = [] :: kz_json:objects()
                  ,moved = [] :: ne_binaries()
                  }).

-type update_funs() :: [fun((kz_json:object()) -> kz_json:object())].

-type db_ret() :: 'ok' | {'ok', kz_json:object() | kz_json:objects()} | {'error', any()}.
-type vm_folder() :: ne_binary() | {ne_binary(), boolean()}.

-define(CHANGE_VMBOX_FUNS(AccountId, NewBoxId, NBoxJ, OldBoxId)
       ,[fun(DocJ) -> kzd_box_message:set_source_id(NewBoxId, DocJ) end
        ,fun(DocJ) -> kzd_box_message:apply_folder(?VM_FOLDER_NEW, DocJ) end
        ,fun(DocJ) -> kzd_box_message:change_message_name(NBoxJ, DocJ) end
        ,fun(DocJ) -> kzd_box_message:change_to_sip_field(AccountId, NBoxJ, DocJ) end
        ,fun(DocJ) -> kzd_box_message:add_message_history(OldBoxId, DocJ) end
        ]).

-define(KZ_VOICEMAIL_HRL, 'true').
-endif.
