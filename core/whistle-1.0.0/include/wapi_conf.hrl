-ifndef(WAPI_CONF_HRL).

-define(DOC_EDITED, <<"doc_edited">>).
-define(DOC_CREATED, <<"doc_created">>).
-define(DOC_DELETED, <<"doc_deleted">>).

-define(DOC_ACTIONS, [?DOC_CREATED, ?DOC_EDITED, ?DOC_DELETED]).
-define(DOC_TYPES, [kz_account:type()
                    ,kzd_callflow:type()
                    ,kz_device:type()
                    ,kzd_fax_box:type()
                    ,kzd_media:type()
                    ,kzd_user:type()
                    ,kzd_voicemail_box:type()
                    ,<<"*">>
                   ]).

-define(WAPI_CONF_HRL, 'true').
-endif.
