-ifndef(JONNY5_HRL).

-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(JONNY5_CACHE, jonny5_cache).

-define(DEFAULT_RATE, 0.5).

-define(APP_VERSION, <<"2.0.0">>).
-define(APP_NAME, <<"jonny5">>).

-record(limits, {account_id = undefined
                 ,account_db = undefined
                 ,enabled = true
                 ,twoway_trunks = -1
                 ,inbound_trunks = 0
                 ,resource_consuming_calls = -1
                 ,calls = -1
                 ,allow_prepay = true
                 ,allow_postpay = false
                 ,max_postpay_amount = 0.0
                 ,reserve_amount = 0.0
                 ,allotments = wh_json:new()
                 ,soft_limit_inbound = false
                 ,soft_limit_outbound = false
                }).
-type j5_limits() :: #limits{}.

-define(JONNY5_HRL, true).
-endif.
