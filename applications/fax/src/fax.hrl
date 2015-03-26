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

-define(FAX_HRL, 'true').
-endif.
