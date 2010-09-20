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
-define(TS_ROUTE_DB, "ts_route").
-define(TS_VIEW_RATES, "LookupFinancials/LookupRates").

% just want to deal with binary K/V pairs
-type proplist() :: list(tuple(binary(), binary())) | [].

-record(route_flags, {
	  to_user = <<>> :: binary()
	  ,to_domain = <<>> :: binary()
          ,from_user = <<>> :: binary()
          ,from_domain = <<>> :: binary()
	  ,server_id = <<>> :: binary()                  % Server of the DID
	  ,failover = [] :: proplist()                   % Failover information
	  ,e911 = [] :: proplist()                       % E911 info for the DID, if configured
	  ,e911_default = [] :: proplist()               % E911 info for server
	  ,payphone = false :: boolean()
	  ,callerid = {} :: tuple()                      % Name and Number for Caller ID
          ,callerid_default = {} :: tuple()              % Name and Number for Caller ID for the server
	  ,fax = [] :: proplist()                        % Fax properties
	  ,flat_rate_enabled = false :: boolean()        % is this a flat-rate eligible call
          ,codecs = [] :: list()                         % what codecs to use (Fax(t38) mostly)
	 }).
