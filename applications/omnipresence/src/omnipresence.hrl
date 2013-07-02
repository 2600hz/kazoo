-ifndef(OMNIPRESENCE_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"omnipresence">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(CONFIG_CAT, <<"omnipresence">>).

-define(PRESENCE_HANGUP, <<"terminated">>).
-define(PRESENCE_RINGING, <<"early">>).
-define(PRESENCE_ANSWERED, <<"confirmed">>).

-define(OMNIPRESENCE_HRL, 'true').
-endif.
