-include_lib("couchbeam/include/couchbeam.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"trunkstore">>).
-define(APP_VERSION, <<"0.9.0">>).

%% couch params for the trunk store and its views
-define(TS_DB, <<"ts">>).

%% cdr doc store
-define(TS_CDR_PREFIX, <<"ts_cdr">>).

-define(TS_VIEW_IPAUTH, <<"LookUpIPAuth/LookUpIPAuth">>).
-define(TS_VIEW_USERAUTH, <<"LookUpUserAuth/LookUpUserAuth">>).
-define(TS_VIEW_USERAUTHREALM, <<"LookUpUserAuth/LookUpUserAuthRealm">>).
-define(TS_VIEW_DIDLOOKUP, <<"LookUpDID/LookUpDID">>).
-define(TS_VIEW_CARRIERIP, <<"LookUpCarrierIP/LookUpCarrierIP">>).

-define(EOD, end_of_day).
-define(MILLISECS_PER_DAY, 1000 * 60 * 60 * 24).

%% couch params for the routing table and its views
-define(TS_RATES_DB, <<"ts_rates">>).
-define(TS_CARRIERS_DOC, <<"carriers">>).

-define(DEFAULT_PROGRESS_TIMEOUT, 6). % seconds to timeout if no progress

-define(INBOUND_FORMATS, [<<"E.164">>, <<"NPANXXXXXX">>, <<"1NPANXXXXXX">>, <<"e164">>, <<"npan">>, <<"1npan">>]).

% just want to deal with binary K/V pairs
%%-type proplist() :: list(tuple(binary(), binary())) | [].
-type active_calls() :: list(tuple(binary(), flat_rate | per_min)) | [].

-record(route_flags, {
	  callid = <<>> :: binary()                      % unique call ID
	  ,to_user = <<>> :: binary()                    % usually a DID
	  ,to_domain = <<>> :: binary()
          ,from_user = <<>> :: binary()
          ,from_domain = <<>> :: binary()
	  ,auth_user = <<>> :: binary() | undefined      % what username did we authenticate with
          ,auth_realm = <<>> :: binary() | undefined     % what realm did we auth with
	  ,direction = <<>> :: binary()                  % what direction is the call (relative to client)
	  ,server_id = <<>> :: binary()                  % Server of the DID
	  ,failover = {} :: tuple()                      % Failover information {type, value}. Type=(sip|e164), Value=("sip:user@domain"|"+1234567890")
	  ,allow_payphone = false :: boolean()
	  ,caller_id = {} :: tuple()                     % Name and Number for Caller ID - check DID, then server, then account, then what we got from ecallmgr
          ,caller_id_e911 = {} :: tuple()                % CallerID for E911 calls - Check DID, then server, then account
          ,inbound_format = <<>> :: binary()             % how does the server want the number? "E.164" | "NPANXXXXXX" | "1NPANXXXXXX" | "USERNAME"
          ,media_handling = <<>> :: binary()             % are we in the media path or not "process" | "bypass"
	  ,progress_timeout = none :: none | integer()   %% for inbound with failover, how long do we wait
          ,force_outbound = undefined :: undefined | boolean() %% if true, and call is outbound, don't try to route through our network; force over a carrier
          ,codecs = [] :: list()                         % what codecs to use (t38, g729, g711, etc...)
	  ,rate = 0.0 :: float()                 % rate for the inbound leg, per minute
	  ,rate_increment = 60 :: integer()      % time, in sec, to bill per
          ,rate_minimum = 60 :: integer()        % time, in sec, to bill as a minimum
	  ,surcharge = 0.0 :: float()            % rate to charge up front
          ,rate_name = <<>> :: binary()          % name of the rate
	  ,route_options = [] :: list()                  % options required to be handled by carriers
          ,flat_rate_enabled = true :: boolean()
	  ,account_doc_id = <<>> :: binary()             % doc id of the account
	  ,diverted_account_doc_id = <<>> :: binary()    % if an outbound call routes to a known DID, route internally rather than over a carrier; for billing
          ,routes_generated = ?EMPTY_JSON_OBJECT :: json_object() | json_objects()           % the routes generated during the routing phase
	  ,scenario = inbound :: inbound | outbound | inbound_failover | outbound_inbound | outbound_inbound_failover % what scenario have we routed over
	 }).


-define(TS_COUCH_DESIGN_DOCS, ["filter.json", "lookupuserauth.json", "lookupmonitor.json", "lookupipauth.json", "lookupdid.json", "lookupuser.json", "ts_cdr.json"]).
-define(TS_COUCH_BASE_DOCS, ["carriers.json"]).
