-include_lib("couchbeam/include/couchbeam.hrl").

%% couch params for the trunk store and its views
-define(TS_DB, "ts").

%% cdr doc store
-define(TS_CDR_DB, "ts_cdr").

-define(TS_VIEW_IPAUTH, {"LookUpIPAuth", "LookUpIPAuth"}).
-define(TS_VIEW_USERAUTH, {"LookUpUserAuth","LookUpUserAuth"}).
-define(TS_VIEW_DIDLOOKUP, {"LookUpDID","LookUpDID"}).
-define(TS_VIEW_CARRIERIP, {"LookUpCarrierIP","LookUpCarrierIP"}).

%% couch params for the routing table and its views
-define(TS_RATES_DOC, "rates").
-define(TS_CARRIERS_DOC, "carriers").

-define(INBOUND_FORMATS, [<<"E.164">>, <<"NPANXXXXXX">>, <<"1NPANXXXXXX">>]).

% just want to deal with binary K/V pairs
-type proplist() :: list(tuple(binary(), binary())) | [].
-type active_calls() :: list(tuple(binary(), flat_rate | per_min)) | [].

-record(route_flags, {
	  callid = <<>> :: binary()                      % unique call ID
	  ,to_user = <<>> :: binary()                    % usually a DID
	  ,to_domain = <<>> :: binary()
          ,from_user = <<>> :: binary()
          ,from_domain = <<>> :: binary()
	  ,auth_user = <<>> :: binary()                  % what username did we authenticate with
          ,auth_realm = <<>> :: binary()                 % what realm did we auth with
	  ,direction = <<>> :: binary()                  % what direction is the call (relative to client)
	  ,server_id = <<>> :: binary()                  % Server of the DID
	  ,failover = {} :: tuple()                      % Failover information {type, value}. Type=(sip|e164), Value=("sip:user@domain"|"+1234567890")
	  ,allow_payphone = false :: boolean()
	  ,caller_id = {} :: tuple()                     % Name and Number for Caller ID - check DID, then server, then account, then what we got from ecallmgr
          ,caller_id_e911 = {} :: tuple()                % CallerID for E911 calls - Check DID, then server, then account
          ,inbound_format = <<>> :: binary()             % how does the server want the number? "E.164" | "NPANXXXXXX" | "1NPANXXXXXX" | "USERNAME"
          ,codecs = [] :: list()                         % what codecs to use (t38, g729, g711, etc...)
	  ,rate = 0.0 :: float()                 % rate for the inbound leg, per minute
	  ,rate_increment = 60 :: integer()      % time, in sec, to bill per
          ,rate_minimum = 60 :: integer()        % time, in sec, to bill as a minimum
	  ,surcharge = 0.0 :: float()            % rate to charge up front
          ,rate_name = <<>> :: binary()          % name of the rate
	  ,route_options = [] :: list()                  % options required to be handled by carriers
          ,flat_rate_enabled = false :: boolean()
	  ,account_doc_id = <<>> :: binary()             % doc id of the account
          ,routes_generated = [] :: proplist()           % the routes generated during the routing phase
	 }).


-define(TS_COUCH_DESIGN_DOCS, ["filter.json", "lookupuserauth.json", "lookupmonitor.json", "lookupipauth.json", "lookupdid.json"]).
-define(TS_COUCH_BASE_DOCS, ["carriers.json", "rates.json"]).
