-ifndef(ACDC_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"acdc">>).
-define(APP_VERSION, <<"0.2.0">>).

-define(ACDC_HRL, true).

-define(ACDC_CACHE, acdc_cache).

-endif.
