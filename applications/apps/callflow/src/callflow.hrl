-include_lib("whistle/include/whistle_types.hrl").
-include_lib("whistle/include/whistle_amqp.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").

-define ( CALLFLOW_DB, "callflow" ).

-define ( DIALPLAN_MAP, [
   { <<"tone">>, <<"tones">> }
] ).

-type cf_exe_response() :: tuple(stop) | tuple (continue) | tuple(continue, integer()) | tuple(heartbeat).

-record (cf_call, {
             amqp_h = <<>> :: binary()                            %% The AMPQ host the ctrl_q and bdst_q exist in
            ,amqp_q = <<>> :: binary()                            %% The AMPQ queue that we consume on
            ,ctrl_q = <<>> :: binary()                            %% The control queue for this request
            ,bdst_q = <<>> :: binary()                            %% The broadcast queue the request was recieved on
            ,call_id = <<>> :: binary()                           %% The call-id of this request
            ,cf_pid = undefined :: pid() | undefined              %% PID of the callflow tree processor, who we should pass control back to
            ,from_number = <<>>
            ,from_domain = <<>>
            ,to_number = <<>>
            ,to_domain = <<>>
            ,route_request = undefined :: proplist() | undefined  %% The initial route request that spawned this call flow
           }).
