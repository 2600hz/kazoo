-ifndef(FAX_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"fax">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(CACHE_NAME, 'fax_cache').
-define(FAX_WORKER_POOL, 'fax_worker_pool').

-define(FAX_CHANNEL_DESTROY_PROPS, [<<"Ringing-Seconds">>, <<"Billing-Seconds">>
                                   ,<<"0">>, <<"Duration-Seconds">>
                                   ,<<"User-Agent">>
                                   ,<<"Hangup-Code">>, <<"Hangup-Cause">>
                                   ,{<<"Custom-Channel-Vars">>, [<<"Resource-ID">>, <<"Ecallmgr-Node">>
                                                                ,<<"Call-ID">>, <<"Fetch-ID">>
                                                                ]}
                                   ]).

-record(fax_storage, {
          id :: api_binary()
                     ,attachment_id :: api_binary()
                     ,db :: api_binary()
         }).
-type fax_storage() :: #fax_storage{}.

-type fax_job() :: kz_json:object().
-type fax_jobs() :: [fax_job()].

-define(FAX_OUTGOING, <<"outgoing">>).
-define(FAX_INCOMING, <<"incoming">>).

-define(FAX_START, <<"start">>).
-define(FAX_ACQUIRE, <<"acquire">>).
-define(FAX_PREPARE, <<"prepare">>).
-define(FAX_ORIGINATE, <<"originate">>).
-define(FAX_NEGOTIATE, <<"negotiate">>).
-define(FAX_SEND, <<"send">>).
-define(FAX_RECEIVE, <<"receive">>).
-define(FAX_END, <<"end">>).
-define(FAX_ERROR, <<"error">>).

-define(FAX_STATE_LIST, [?FAX_START, ?FAX_PREPARE, ?FAX_SEND, ?FAX_RECEIVE, ?FAX_END, ?FAX_ERROR]).

-define(OPENXML_MIME_PREFIX, "application/vnd.openxmlformats-officedocument.").
-define(OPENOFFICE_MIME_PREFIX, "application/vnd.oasis.opendocument.").
-define(OPENOFFICE_COMPATIBLE(CT)
       ,(CT =:= <<"application/msword">>
             orelse CT =:= <<"application/vnd.ms-excel">>
             orelse CT =:= <<"application/vnd.ms-powerpoint">>
        )).

-define(DEFAULT_ALLOWED_CONTENT_TYPES, [<<"application/pdf">>
                                       ,<<"image/tiff">>
                                       ,kz_json:from_list([{<<"prefix">>, <<"image">>}])
                                       ,kz_json:from_list([{<<"prefix">>, <<?OPENXML_MIME_PREFIX>>}])
                                       ,kz_json:from_list([{<<"prefix">>, <<?OPENOFFICE_MIME_PREFIX>>}])
                                       ,<<"application/msword">>
                                       ,<<"application/vnd.ms-excel">>
                                       ,<<"application/vnd.ms-powerpoint">>
                                       ]).

-define(DEFAULT_DENIED_CONTENT_TYPES
       ,[kz_json:from_list([{<<"prefix">>, <<"image/">>}])]
       ).

-define(SMTP_MSG_MAX_SIZE, kapps_config:get_integer(?CONFIG_CAT, <<"smtp_max_msg_size">>, 10485670)).
-define(SMTP_EXTENSIONS, [{"SIZE", kz_util:to_list(?SMTP_MSG_MAX_SIZE)}]).
-define(SMTP_CALLBACK_OPTIONS, {'callbackoptions', ['extensions', ?SMTP_EXTENSIONS]}).
-define(SMTP_PORT, kapps_config:get_integer(?CONFIG_CAT, <<"smtp_port">>, 19025)).

-define(FAX_EXTENSION, <<"tiff">>).

-define(FAX_OUTBOUND_SERVER(AccountId), <<"fax_outbound_", AccountId/binary>>).

-define(FAX_HRL, 'true').
-endif.
