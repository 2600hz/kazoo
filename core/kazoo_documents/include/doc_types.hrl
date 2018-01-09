-ifndef(KAZOO_DOC_TYPES_HRL).

-define(DOC_TYPES, [kz_account:type()
                   ,kzd_callflow:type()
                   ,kz_device:type()
                   ,kzd_fax_box:type()
                   ,kzd_media:type()
                   ,kzd_user:type()
                   ,kzd_voicemail_box:type()
                   ,kzd_fax:type()
                   ,kzd_box_message:type()
                   ,kzd_call_recordings:type()
                   ]).

-define(DOC_MODB_TYPES, [kzd_fax:type()
                        ,kzd_box_message:type()
                        ]).

-define(KAZOO_DOC_TYPES_HRL, 'true').
-endif.
