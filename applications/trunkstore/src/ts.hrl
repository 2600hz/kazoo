-ifndef(TS_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(APP_NAME, <<"trunkstore">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

%% couch params for the trunk store and its views
-define(TS_DB, <<"ts">>).

-define(CACHE_NAME, 'trunkstore_cache').

%% Account views
-define(TS_VIEW_DIDLOOKUP, <<"trunkstore/lookup_did">>).

%% just want to deal with binary K/V pairs
-type active_calls() :: [{binary(), 'flat_rate' | 'per_min'}].

-record(ts_callflow_state, {aleg_callid :: kz_term:api_ne_binary()
                           ,bleg_callid :: kz_term:api_ne_binary()
                           ,acctid = <<>> :: binary()
                           ,acctdb = <<>> :: binary()
                           ,route_req_jobj = kz_json:new() :: kapi_route:req()
                           ,ep_data = kz_json:new() :: kz_json:object() %% data for the endpoint, either an actual endpoint or an offnet request
                           ,amqp_worker :: kz_term:api_pid()
                           ,callctl_q :: kz_term:api_ne_binary()
                           ,call_cost = 0.0 :: float()
                           ,failover :: kz_term:api_object()
                           ,kapps_call :: kapps_call:call()
                           }).

-record(route_flags, {callid = <<>> :: binary()                      % unique call ID
                     ,to_user = <<>> :: binary()                    % usually a DID
                     ,to_domain = <<>> :: binary()
                     ,from_user = <<>> :: binary()
                     ,from_domain = <<>> :: binary()
                     ,auth_user = <<>> :: kz_term:api_binary()      % what username did we authenticate with
                     ,auth_realm = <<>> :: kz_term:api_binary()     % what realm did we auth with
                     ,direction = <<>> :: binary()                  % what direction is the call (relative to client)
                     ,server_id = <<>> :: binary()                  % Server of the DID
                     ,failover = {} :: tuple()                      % Failover information {type, value}. Type=(sip|e164), Value=("sip:user@domain"|"+1234567890")
                     ,allow_payphone = 'false' :: boolean()
                     ,caller_id = {} :: tuple()                     % Name and Number for Caller ID - check DID, then server, then account, then what we got from ecallmgr
                     ,caller_id_e911 = {} :: tuple()                % CallerID for E911 calls - Check DID, then server, then account
                     ,inbound_format = <<>> :: binary()             % how does the server want the number? "E.164" | "NPANXXXXXX" | "1NPANXXXXXX" | "USERNAME"
                     ,media_handling = <<>> :: binary()             % are we in the media path or not "process" | "bypass"
                     ,progress_timeout = 'none' :: 'none' | integer()   %% for inbound with failover, how long do we wait
                     ,force_outbound :: boolean() %% if true, and call is outbound, don't try to route through our network; force over a carrier
                     ,codecs = [] :: list()                         % what codecs to use (t38, g729, g711, etc...)
                     ,rate = 0.0 :: float()                 % rate for the inbound leg, per minute
                     ,rate_increment = 60 :: integer()      % time, in sec, to bill per
                     ,rate_minimum = 60 :: integer()        % time, in sec, to bill as a minimum
                     ,surcharge = 0.0 :: float()            % rate to charge up front
                     ,rate_name = <<>> :: binary()          % name of the rate
                     ,route_options = [] :: list()                  % options required to be handled by carriers
                     ,flat_rate_enabled = 'true' :: boolean()
                     ,account_doc_id = <<>> :: binary()             % doc id of the account
                     ,diverted_account_doc_id = <<>> :: binary()    % if an outbound call routes to a known DID, route internally rather than over a carrier; for billing
                     ,routes_generated = kz_json:new() :: kz_json:object() | kz_json:objects()           % the routes generated during the routing phase
                     ,scenario = 'inbound' :: 'inbound' | 'outbound' | 'inbound_failover' | 'outbound_inbound' | 'outbound_inbound_failover' % what scenario have we routed over
                     }).

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).

-define(TS_HRL, 'true').
-endif.
