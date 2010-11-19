-include_lib("couchbeam/include/couchbeam.hrl").

%% couch params for the trunk store and its views
-define(TS_DB, "ts").

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

-record(route_flags, {
	  to_user = <<>> :: binary()
	  ,to_domain = <<>> :: binary()
          ,from_user = <<>> :: binary()
          ,from_domain = <<>> :: binary()
	  ,auth_user = <<>> :: binary()                  % what username did we authenticate with
	  ,direction = <<>> :: binary()                  % what direction is the call (relative to client)
	  ,server_id = <<>> :: binary()                  % Server of the DID
          ,credit_available = 0.0 :: float()             % How much credit is left on the account/server/DID
	  ,failover = {} :: tuple()                      % Failover information {type, value}. Type=(sip|e164), Value=("sip:user@domain"|"+1234567890")
	  ,allow_payphone = false :: boolean()
	  ,caller_id = {} :: tuple()                     % Name and Number for Caller ID - check DID, then server, then account, then what we got from ecallmgr
          ,caller_id_e911 = {} :: tuple()                % CallerID for E911 calls - Check DID, then server, then account
	  ,trunks = 0 :: integer()                       % Trunks available
	  ,trunks_in_use = 0 :: integer()                % How many trunks are in use in the system
	  ,flat_rate_enabled = false :: boolean()        % is this a flat-rate eligible call - only if trunks are available
          ,inbound_format = <<>> :: binary()             % how does the server want the number? "E.164" | "NPANXXXXXX" | "1NPANXXXXXX"
          ,codecs = [] :: list()                         % what codecs to use (t38, g729, g711, etc...)
	  ,inbound_rate = 0.0 :: float()                 % rate for the inbound leg, per minute
	  ,inbound_rate_increment = 60 :: integer()      % time, in sec, to bill per
          ,inbound_rate_minimum = 60 :: integer()        % time, in sec, to bill as a minimum
	  ,inbound_surcharge = 0.0 :: float()            % rate to charge up front
	  ,outbound_rate = 0.0 :: float()                % rate for the outbound leg, per minute
	  ,outbound_rate_increment = 60 :: integer()     % time, in sec, to bill per
          ,outbound_rate_minimum = 60 :: integer()       % time, in sec, to bill as a minimum
	  ,outbound_surcharge = 0.0 :: float()           % rate to charge up front
	  ,route_options = [] :: list()                  % options required to be handled by carriers
	  ,account_doc = [] :: proplist()                % the full Couch document
	 }).
