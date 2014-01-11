-ifndef(BLACKHOLE_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"blackhole">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(BLACKHOLE_HRL, 'true').
-endif.
