-ifndef(FAX_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
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

-define(FAX_HRL, 'true').
-endif.
