-ifndef(FAX_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(CONFIG_CAT, <<"fax">>).

-define(APP_NAME, <<"fax">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(FAX_CACHE, 'fax_cache').

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

-define(FAX_OUTGOING, <<"outgoing">>).
-define(FAX_INCOMING, <<"incoming">>).

-define(FAX_START, <<"start">>).
-define(FAX_PREPARE, <<"prepare">>).
-define(FAX_SEND, <<"send">>).
-define(FAX_RECEIVE, <<"receive">>).
-define(FAX_END, <<"end">>).
-define(FAX_ERROR, <<"error">>).

-define(FAX_STATE_LIST, [?FAX_START, ?FAX_PREPARE, ?FAX_SEND, ?FAX_RECEIVE, ?FAX_END, ?FAX_ERROR]).

-define(OPENXML_MIME_PREFIX, "application/vnd.openxmlformats-officedocument.").
-define(OPENOFFICE_MIME_PREFIX, "application/vnd.oasis.opendocument.").
-define(OPENOFFICE_COMPATIBLE(CT), (
                                CT =:= <<"application/msword">>
                                orelse CT =:= <<"application/vnd.ms-excel">>
                                orelse CT =:= <<"application/vnd.ms-powerpoint">>
                               )).

-define(DEFAULT_ALLOWED_CONTENT_TYPES, [
                                        <<"application/pdf">>
                                        ,<<"image/tiff">>
                                        ,{[{<<"prefix">>, <<"image">>}]}
                                        ,{[{<<"prefix">>, <<?OPENXML_MIME_PREFIX>>}]}
                                        ,{[{<<"prefix">>, <<?OPENOFFICE_MIME_PREFIX>>}]}
                                        ,<<"application/msword">>
                                        ,<<"application/vnd.ms-excel">>
                                        ,<<"application/vnd.ms-powerpoint">>
                                       ]).

-define(FAX_HRL, 'true').
-endif.
