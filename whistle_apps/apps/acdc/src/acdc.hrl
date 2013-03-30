-ifndef(ACDC_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(CONFIG_CAT, <<"acdc">>).

-define(APP_NAME, <<"acdc">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(ACDC_CACHE, acdc_cache).

-define(ABANDON_TIMEOUT, 'timeout').
-define(ABANDON_EXIT, 'member_exit').
-define(ABANDON_HANGUP, 'member_hangup').

-define(PRESENCE_GREEN, <<"terminated">>).
-define(PRESENCE_RED_FLASH, <<"early">>).
-define(PRESENCE_RED_SOLID, <<"confirmed">>).

-define(ENDPOINT_UPDATE_REG(AcctId, EPId), {'p', 'l', {'endpoint_update', AcctId, EPId}}).
-define(ENDPOINT_CREATED(EP), {'endpoint_created', EP}).
-define(ENDPOINT_EDITED(EP), {'endpoint_edited', EP}).
-define(ENDPOINT_DELETED(EP), {'endpoint_deleted', EP}).

-define(OWNER_UPDATE_REG(AcctId, OwnerId), {'p', 'l', {'owner_update', AcctId, OwnerId}}).

-define(NEW_CHANNEL_REG(AcctId, User), {'p', 'l', {'new_channel', AcctId, User}}).
-define(NEW_CHANNEL_FROM(CallId), {'call_from', CallId}).
-define(NEW_CHANNEL_TO(CallId), {'call_to', CallId}).

-type abandon_reason() :: ?ABANDON_TIMEOUT | ?ABANDON_EXIT |
                          ?ABANDON_HANGUP.

-define(ACDC_HRL, 'true').
-endif.
