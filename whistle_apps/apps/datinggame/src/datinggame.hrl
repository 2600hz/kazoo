-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(dg_agent, {
          id = <<>> :: binary()
          ,call_id = <<>> :: binary()
          ,control_queue = <<>> :: binary()
          ,signed_in = erlang:now() :: integer()
          ,skills = wh_json:new() :: json_object()
         }).
