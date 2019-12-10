-ifndef(KZ_DOCUMENTS_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(APP, 'kazoo_documents').
-define(APP_NAME, (atom_to_binary(?APP, 'utf8'))).
-define(APP_VERSION, <<"4.0.0">>).

-define(FAX_SETTINGS_KEY, <<"fax_settings">>).
-define(FAX_TIMEZONE_KEY, <<"fax_timezone">>).

-define(DEFAULT_FAX_SETTINGS
       ,kapps_config:get_json(<<"fax">>, ?FAX_SETTINGS_KEY, kz_json:from_list(
                                                              [{<<"override_fax_identity">>, 'true'}
                                                              ,{<<"override_callee_number">>, 'false'}
                                                              ])
                             )
       ).

-define(SYSTEM_FAX_SETTINGS
       ,kz_json:set_value(?FAX_TIMEZONE_KEY, kzd_accounts:default_timezone(), ?DEFAULT_FAX_SETTINGS)
       ).

-ifdef(TEST).
-define(PROPERTIES_JOBJ, kz_json:from_list_recursive(
                           [{<<"key1">>, [{<<"type">>, <<"string">>}]}
                           ,{<<"key2">>, [{<<"type">>, <<"integer">>}]}
                           ,{<<"key3">>, [{<<"type">>, <<"string">>}
                                         ,{<<"default">>, <<"value3">>}
                                         ]}
                           ])).
-define(SCHEMA_JOBJ
       ,kz_json:from_list(
          [{<<"$schema">>, <<"http://json-schema.org/draft-04/schema#">>}
          ,{<<"properties">>, ?PROPERTIES_JOBJ}
          ,{<<"required">>, [<<"key1">>, <<"key2">>]}
          ]
         )
       ).
-endif.

-define(KZ_DOCUMENTS_HRL, 'true').
-endif.
