-include_lib("whistle/include/whistle_types.hrl").
-include_lib("whistle/include/whistle_amqp.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("cf_amqp.hrl").

-type cf_exe_response() :: tuple(stop) | tuple (continue) | tuple(continue, integer()) | tuple(heartbeat).

-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"0.7.4">> ).

-define ( DIALPLAN_MAP, [
   { <<"tone">>, <<"tones">> }
] ).

-define(CALLFLOW_DB, "callflows").
-define(VIEW_FILE, <<"views/callflows.json">>).
-define(VIEW_BY_URI, {?CALLFLOW_DB, <<"listing_by_uri">>}).

-define(DEFAULT_TIMEOUT, 20).
-define(ANY_DIGIT, [
                     <<"1">>, <<"2">>, <<"3">>
                    ,<<"4">>, <<"5">>, <<"6">>    
                    ,<<"7">>, <<"8">>, <<"9">>    
                    ,<<"*">>, <<"0">>, <<"#">>
                   ]).

-record (cf_call, {
             amqp_q = <<>> :: binary()                              %% The AMPQ queue that we consume on
            ,ctrl_q = <<>> :: binary()                              %% The control queue for this request
            ,bdst_q = <<>> :: binary()                              %% The broadcast queue the request was recieved on
            ,call_id = <<>> :: binary()                             %% The call-id of this request
            ,cf_pid = undefined :: pid() | undefined                %% PID of the callflow tree processor, who we should pass control back to
            ,cf_responder = undefined :: pid() | undefined          %% PID of the callflow responder that won this route_request
            ,account_db = undefined :: binary() | undefined         %% The database name of the account that this callflow belongs to
            ,authorizing_id = undefined :: binary() | undefined     %% The ID of the record that authorized this call
            ,flow_id = undefined :: binary() | undefined            %% The ID of the callflow that was intially executed (does not reflect branches, or hunts)
            ,cid_name = <<>> :: binary()                            %% The CID name provided on the route req
            ,cid_number = <<>> :: binary()                          %% The CID number provided on the route req
            ,destination = <<>> :: binary()
            ,dest_number = <<>> :: binary()
            ,dest_realm = <<>> :: binary()
            ,from = <<>> :: binary()                                %% Result of sip_from_user + @ + sip_from_host
            ,from_number = <<>>  :: binary()                        %% SIP from user
            ,from_realm = <<>> :: binary()                          %% SIP from host
            ,to = <<>> :: binary()                                  %% Result of sip_to_user + @ + sip_to_host
            ,to_number = <<>> :: binary()                           %% SIP to user
            ,to_realm = <<>> :: binary()                            %% SIP to host
            ,channel_vars = undefined :: json_object() | undefined  %% Any custom channel vars that where provided with the route request
           }).
