-ifndef(KAPI_CONF_HRL).

-define(DOC_EDITED, <<"doc_edited">>).
-define(DOC_CREATED, <<"doc_created">>).
-define(DOC_DELETED, <<"doc_deleted">>).

-define(DB_CREATED, <<"db_created">>).
-define(DB_DELETED, <<"db_deleted">>).

-define(KAPI_CONF_CATEGORY, <<"configuration">>).

-define(DOC_ACTIONS, [?DOC_CREATED, ?DOC_EDITED, ?DOC_DELETED]).
-define(DOC_TYPES, [kz_account:type()
                    ,kzd_callflow:type()
                    ,kz_device:type()
                    ,kzd_fax_box:type()
                    ,kzd_media:type()
                    ,kzd_user:type()
                    ,kzd_voicemail_box:type()
                   ]).

-define(KAPI_CONF_HRL, 'true').
-endif.
