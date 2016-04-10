-ifndef(KZ_DOCUMENTS_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(FAX_SETTINGS_KEY, <<"fax_settings">>).
-define(FAX_TIMEZONE_KEY, <<"fax_timezone">>).


-define(DEFAULT_TIMEZONE
        ,whapps_config:get(<<"accounts">>, <<"default_timezone">>, <<"America/Los_Angeles">>)
       ).

-define(DEFAULT_FAX_SETTINGS,
        whapps_config:get(<<"fax">>, ?FAX_SETTINGS_KEY, wh_json:from_list(
                            [{<<"override_fax_identity">>, 'true'}
                             ,{<<"override_callee_number">>, 'false'}
                            ]))).

-define(SYSTEM_FAX_SETTINGS, wh_json:set_value(?FAX_TIMEZONE_KEY, ?DEFAULT_TIMEZONE, ?DEFAULT_FAX_SETTINGS)).


-define(KZ_DOCUMENTS_HRL, 'true').
-endif.
