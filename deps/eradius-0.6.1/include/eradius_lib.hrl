-define(BYTE, integer-unit:8).    % Nice syntactic sugar...

%%- cmds
-define(RAccess_Request,       1).
-define(RAccess_Accept,        2).
-define(RAccess_Reject,        3).
-define(RAccounting_Request,   4).
-define(RAccounting_Response,  5).
-define(RAccess_Challenge,    11).
-define(RDisconnect_Request,  40).
-define(RDisconnect_Ack,      41).
-define(RDisconnect_Nak,      42).
-define(RCoa_Request,         43).
-define(RCoa_Ack,             44).
-define(RCoa_Nak,             45).

%%- attribs
-define(RUser_Name,        1).
-define(RUser_Passwd,      2).
-define(RNAS_Ip_Address,   4).
-define(RReply_Msg,       18).
-define(RState,           24).
-define(RClass,           25).
-define(RVendor_Specific, 26).
-define(RSession_Timeout, 27).
-define(RIdle_Timeout,    28).
-define(RStatus_Type,     40).
-define(RSession_Id,      44).
-define(RSession_Time,    46).
-define(RTerminate_Cause, 49).
-define(REAP_Message,     79).
-define(RMessage_Authenticator, 80).

%%- attribute values
-define(RStatus_Type_Start,  1).
-define(RStatus_Type_Stop,   2).
-define(RStatus_Type_Update, 3).  % Interim-Update
-define(RStatus_Type_On,     7).
-define(RStatus_Type_Off,    8).

%%- Terminate Cause values
-define(RTCUser_Request,     1).
-define(RTCIdle_Timeout,     4).
-define(RTCSession_Timeout,  5).
-define(RTCAdmin_Reset,      6).
-define(RTCAdmin_Reboot,     7).
-define(RTCNAS_Request,     10).
-define(RTCNAS_Reboot,      11).

-record(nas_counter, {
          key                          :: term(),
          requests                 = 0 :: non_neg_integer(),
          replies                  = 0 :: non_neg_integer(),
          dupRequests              = 0 :: non_neg_integer(),
          malformedRequests        = 0 :: non_neg_integer(),
          accessRequests           = 0 :: non_neg_integer(),
          accessAccepts            = 0 :: non_neg_integer(),
          accessRejects            = 0 :: non_neg_integer(),
          accessChallenges         = 0 :: non_neg_integer(),
          accountRequests          = 0 :: non_neg_integer(),
          accountResponses         = 0 :: non_neg_integer(),
          noRecords                = 0 :: non_neg_integer(),
          badAuthenticators        = 0 :: non_neg_integer(),
          packetsDropped           = 0 :: non_neg_integer(),
          unknownTypes             = 0 :: non_neg_integer(),
          handlerFailure           = 0 :: non_neg_integer(),
          coaRequests              = 0 :: non_neg_integer(),
          coaAcks                  = 0 :: non_neg_integer(),
          coaNaks                  = 0 :: non_neg_integer(),
          discRequests             = 0 :: non_neg_integer(),
          discAcks                 = 0 :: non_neg_integer(),
          discNaks                 = 0 :: non_neg_integer()
         }).

-record(server_counter, {
          key                  :: term(),
          upTime               :: erlang:timestamp(),
          resetTime            :: erlang:timestamp(),
          invalidRequests = 0  :: non_neg_integer(),
          discardNoHandler = 0 :: non_neg_integer()
}).

-record(nas_prop, {
    server_ip     :: inet:ip_address(),
    server_port   :: eradius_server:port_number(),
    nas_id        :: term(),
    nas_ip        :: inet:ip_address(),
    nas_port      :: eradius_server:port_number(),
    secret        :: eradius_lib:secret(),
    handler_nodes = local :: list(atom()) | local
}).

-record(radius_request, {
    reqid = 0        :: byte(),
    cmd = request    :: eradius_lib:command(),
    attrs = []       :: eradius_lib:attribute_list(),
    secret           :: eradius_lib:secret(),
    authenticator    :: eradius_lib:authenticator(),
    msg_hmac = false :: boolean(),
    eap_msg = <<>>   :: binary()
}).
