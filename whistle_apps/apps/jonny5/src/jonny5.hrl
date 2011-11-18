-include_lib("couchbeam/include/couchbeam.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"jonny5">>).
-define(APP_VERSION, <<"0.2.0">>).
-define(BLACKLIST_SERVER, blacklist_server).

-type call_types() :: 'per_min' | 'two_way' | 'inbound'.

%% tracked in tenths of a cent
-define(DOLLAR_TO_UNIT, 1000).

-define(DOLLARS_TO_UNITS(F), round(F * ?DOLLAR_TO_UNIT)). % convert $1.00 to 1,000
-define(UNITS_TO_DOLLARS(U), (U / ?DOLLAR_TO_UNIT)).

%% How much to charge a per_min call at the outset
-define(PER_MIN_MIN, 2 * ?DOLLAR_TO_UNIT). % 2.00, aka 2000 units
