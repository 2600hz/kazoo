-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(ROUTES_DB, <<"offnet">>).
-define(LIST_ROUTES_BY_NUMBER, {<<"routes">>, <<"listing_by_number">>}).
-define(LIST_ROUTE_DUPS, {<<"routes">>, <<"listing_by_assignment">>}).
-define(LIST_ROUTE_ACCOUNTS, {<<"routes">>, <<"listing_by_account">>}).

-define(RESOURCES_DB, <<"offnet">>).
-define(LIST_RESOURCES_BY_ID, {<<"resources">>, <<"listing_by_id">>}).

-define(DEFAULT_PROGRESS_TIMEOUT, 8).
-define(DEFAULT_GRACE_PERIOD, 3).

-define(APP_NAME, <<"stepswitch">>).
-define(APP_VERSION, <<"0.2.0">>).

-record(gateway, {
           resource_id = 'undefined'
          ,server = 'undefined'
          ,realm = 'undefined'
          ,username = 'undefined'
          ,password = 'undefined'
          ,route = 'undefined'
          ,prefix = <<>>
          ,suffix = <<>>
          ,codecs = []
          ,bypass_media = 'undefined'
          ,caller_id_type = 'undefined'
          ,sip_headers = 'undefined'
          ,progress_timeout = ?DEFAULT_PROGRESS_TIMEOUT :: pos_integer()
         }).

-record(resrc, {
           id = <<>> :: binary()
          ,rev = <<>> :: binary()
          ,weight_cost = 1 :: 1..100
          ,grace_period = ?DEFAULT_GRACE_PERIOD :: non_neg_integer()
          ,flags = [] :: list()
          ,rules = [] :: list()
          ,gateways = [] :: list()
          ,is_emergency = 'true' :: boolean()
         }).

-type endpoint() :: {1..100, non_neg_integer(), ne_binary(), [#gateway{},...] | [], boolean()}.
-type endpoints() :: [] | [endpoint()].
