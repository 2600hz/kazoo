-ifndef(JONNY5_HRL).

-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(JONNY5_CACHE, jonny5_cache).

-define(JONNY5_HRL, true).

-define(APP_VERSION, <<"2.0.0">>).
-define(APP_NAME, <<"jonny5">>).

-record(limits, {twoway_trunks = -1
                 ,inbound_trunks = 0
                 ,resource_consuming_calls = -1
                 ,calls = -1
                 ,allow_prepay = true
                 ,allow_postpay = false
                 ,max_postpay_amount = 0.0
                 ,reserve_amount = 0.0
                }).

-endif.
