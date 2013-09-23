-ifndef(STEPSWITCH_HRL).
-include_lib("rabbitmq_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(ROUTES_DB, <<"offnet">>).
-define(RESOURCES_DB, <<"offnet">>).

-define(LIST_ROUTES_BY_NUMBER, <<"routes/listing_by_number">>).
-define(LIST_ROUTE_DUPS, <<"routes/listing_by_assignment">>).
-define(LIST_ROUTE_ACCOUNTS, <<"routes/listing_by_account">>).
-define(LIST_RESOURCES_BY_ID, <<"resources/listing_by_id">>).

-define(SUCCESSFUL_HANGUP_CAUSES, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>, <<"SUCCESS">>]).

-define(APP_NAME, <<"stepswitch">>).
-define(APP_VERSION, <<"0.5.0">>).

-define(STEPSWITCH_CACHE, stepswitch_cache).
-define(STEPSWITCH_CNAM_POOL, stepswitch_cnam_pool).

-record(gateway, {
           resource_id = 'undefined'
          ,server = 'undefined'
          ,realm = 'undefined'
          ,username = 'undefined'
          ,password = 'undefined'
          ,route = whapps_config:get_binary(<<"stepswitch">>, <<"default_route">>)
          ,prefix = whapps_config:get_binary(<<"stepswitch">>, <<"default_prefix">>, <<>>)
          ,suffix = whapps_config:get_binary(<<"stepswitch">>, <<"default_suffix">>, <<>>)
          ,codecs = whapps_config:get(<<"stepswitch">>, <<"default_codecs">>, [])
          ,bypass_media = whapps_config:get_is_true(<<"stepswitch">>, <<"default_bypass_media">>, 'false')
          ,caller_id_type = whapps_config:get_binary(<<"stepswitch">>, <<"default_caller_id_type">>, <<"external">>)
          ,t38_setting = whapps_config:get_is_true(<<"stepswitch">>, <<"default_t38_settings">>, 'false')
          ,sip_headers = 'undefined'
          ,sip_interface = 'undefined'
          ,progress_timeout = whapps_config:get_integer(<<"stepswitch">>, <<"default_progress_timeout">>, 8) :: pos_integer()
          ,invite_format = <<"route">>
          ,endpoint_type = <<"sip">> % could be freetdm as well
          ,endpoint_options = wh_json:new()
          ,format_from_uri = false :: boolean()
         }).

-record(resrc, {
           id = <<>> :: binary()
          ,rev = <<>> :: binary()
          ,weight_cost = whapps_config:get_integer(<<"stepswitch">>, <<"default_weight">>, 1) :: 1..100
          ,grace_period = whapps_config:get_integer(<<"stepswitch">>, <<"default_weight">>, 3) :: non_neg_integer()
          ,flags = [] :: list()
          ,rules = [] :: list()
          ,gateways = [] :: list()
          ,is_emergency = 'true' :: boolean()
         }).

-type endpoint() :: {1..100, non_neg_integer(), ne_binary(), [#gateway{},...] | [], boolean()}.
-type endpoints() :: [] | [endpoint()].

-define(STEPSWITCH_HRL, true).
-endif.
