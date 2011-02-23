-include("../../../utils/src/whistle_types.hrl").

-define ( CALLFLOW_DB, "callflow" ).

-record ( cf_call, {
            amqp_h = <<>> :: binary()                             %% The AMPQ host the ctrl_q and bdst_q exist in
            ,ctrl_q = <<>> :: binary()                            %% The control queue for this request
            ,bdst_q = <<>> :: binary()                            %% The broadcast queue the request was recieved on
            ,call_id = <<>> :: binary()                           %% The call-id of this request
            ,cf_pid = undefined :: pid() | undefined              %% PID of the callflow tree processor, who we should pass control back to
            ,route_request = undefined :: proplists() | undefined %% The initial route request that spawned this call flow
           }).
