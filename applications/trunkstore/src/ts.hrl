-include_lib("couchbeam/include/couchbeam.hrl").
-define(COUCH_PARAMS, #couchdb_params{
	  host = "66.170.17.98"
	  ,port = 5984
	 }).

%% couch params for the trunk store and its views
-define(TS_DB, "ts").

-define(TS_VIEW_IPAUTH, "LookUpIPAuth/LookUpIPAuth").
-define(TS_VIEW_USERAUTH, "LookUpUserAuth/LookUpUserAuth").
-define(TS_VIEW_DIDLOOKUP, "LookUpDID/LookUpDID").
-define(TS_VIEW_CARRIERIP, "LookUpCarrierIP/LookUpCarrierIP").

%% couch params for the routing table and its views
-define(TS_RATES_DOC, "rates").
-define(TS_CARRIERS_DOC, "carriers").

% just want to deal with binary K/V pairs
-type proplist() :: list(tuple(binary(), binary())) | [].

-record(route_flags, {
	  to_user = <<>> :: binary()
	  ,to_domain = <<>> :: binary()
          ,from_user = <<>> :: binary()
          ,from_domain = <<>> :: binary()
	  ,direction = <<>> :: binary()                  % what direction is the call (relative to client)
	  ,server_id = <<>> :: binary()                  % Server of the DID
          ,credit_available = 0.0 :: float()             % How much credit is left on the account/server/DID
	  ,failover = [] :: proplist()                   % Failover information
	  ,payphone = false :: boolean()
	  ,callerid = {} :: tuple()                      % Name and Number for Caller ID
          ,callerid_e911 = {} :: tuple()                 % CallerID for E911 calls
          ,callerid_default = {} :: tuple()              % Override Name and Number for Caller ID
	  ,fax = [] :: proplist()                        % Fax properties
	  ,flat_rate_enabled = false :: boolean()        % is this a flat-rate eligible call
          ,codecs = [] :: list()                         % what codecs to use (t38, g729, g711, etc...)
	  ,inbound_rate = 0.0 :: float()                 % rate for the inbound leg
	  ,inbound_rate_increment = 60 :: integer()      % time, in sec, to bill per
          ,inbound_rate_minimum = 60 :: integer()        % time, in sec, to bill as a minimum
	  ,inbound_surcharge = 0.0 :: float()            % rate to charge up front
	  ,outbound_rate = 0.0 :: float()                % rate for the outbound leg
	  ,outbound_rate_increment = 60 :: integer()     % time, in sec, to bill per
          ,outbound_rate_minimum = 60 :: integer()       % time, in sec, to bill as a minimum
	  ,outbound_surcharge = 0.0 :: float()           % rate to charge up front
	  ,route_options = [] :: list()                  % options required to be handled by carriers
	 }).
