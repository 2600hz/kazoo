-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("cf_amqp.hrl").

-type cf_exe_response() :: tuple(stop) | tuple (continue) | tuple(continue, integer()) | tuple(heartbeat).
-type cf_api_error() :: tuple(error, channel_hungup | channel_unbridge | execution_failure | timeout).
-type cf_api_std_return() :: cf_api_error() | tuple(ok, json_object()).
-type cf_api_bridge_return() :: cf_api_error() | tuple(ok, json_object()) | tuple(fail, json_object()).
-type cf_api_binary() :: binary() | undefined.

-define(APP_NAME, <<"callflow">>).
-define(APP_VERSION, <<"0.8.2">> ).

-define(DIALPLAN_MAP, [{ <<"tone">>, <<"tones">> }]).

-define(LIST_BY_NUMBER, {<<"callflow">>, <<"listing_by_number">>}).
-define(LIST_BY_PATTERN, {<<"callflow">>, <<"listing_by_pattern">>}).

-define(NO_MATCH_CF, <<"no_match">>).

-define(DEFAULT_TIMEOUT, <<"20">>).
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
            ,flow_id = undefined :: binary() | undefined            %% The ID of the callflow that was intially executed (does not reflect branches, or hunts)
            ,cid_name = <<>> :: binary()                            %% The CID name provided on the route req
            ,cid_number = <<>> :: binary()                          %% The CID number provided on the route req
            ,request = <<>> :: binary()                             %% The request of sip_request_user + @ + sip_request_host
            ,request_user = <<>> :: binary()                        %% SIP request user
            ,request_realm = <<>> :: binary()                       %% SIP request host
            ,from = <<>> :: binary()                                %% Result of sip_from_user + @ + sip_from_host
            ,from_user = <<>>  :: binary()                          %% SIP from user
            ,from_realm = <<>> :: binary()                          %% SIP from host
            ,to = <<>> :: binary()                                  %% Result of sip_to_user + @ + sip_to_host
            ,to_user = <<>> :: binary()                             %% SIP to user
            ,to_realm = <<>> :: binary()                            %% SIP to host
            ,no_match = false :: boolean()                          %% Boolean flag, set when the no_match callflow is used
            ,inception = undefined :: binary() | undefined          %% Origin of the call <<"on-net">> | <<"off-net">>
            ,account_db = undefined :: binary() | undefined         %% The database name of the account that authorized this call
            ,account_id = undefined :: binary() | undefined         %% The account id that authorized this call
            ,authorizing_id = undefined :: binary() | undefined     %% The ID of the record that authorized this call
            ,owner_id = undefined :: binary() | undefined           %% The ID of the that owns the authorizing endpoint
            ,channel_vars = undefined :: json_object() | undefined  %% Any custom channel vars that where provided with the route request
            ,last_action = undefined :: undefined | atom()          %% Previous action
            ,capture_group = undefined :: undefined | binary()      %% If the callflow was found using a pattern this is the capture group
            ,inception_during_transfer = false :: boolean()         %% If the hunt for this callflow was intiated during transfer
           }).
