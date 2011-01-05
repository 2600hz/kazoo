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
	  ,to_user = <<>> :: binary()
	  ,to_domain = <<>> :: binary()
          ,from_user = <<>> :: binary()
          ,from_domain = <<>> :: binary()
	  ,auth_user = <<>> :: binary()                  % what username did we authenticate with
	  ,direction = <<>> :: binary()                  % what direction is the call (relative to client)
	  ,server_id = <<>> :: binary()                  % Server of the DID
	  ,failover = {} :: tuple()                      % Failover information {type, value}. Type=(sip|e164), Value=("sip:user@domain"|"+1234567890")
	  ,allow_payphone = false :: boolean()
	  ,caller_id = {} :: tuple()                     % Name and Number for Caller ID - check DID, then server, then account, then what we got from ecallmgr
          ,caller_id_e911 = {} :: tuple()                % CallerID for E911 calls - Check DID, then server, then account
          ,inbound_format = <<>> :: binary()             % how does the server want the number? "E.164" | "NPANXXXXXX" | "1NPANXXXXXX"
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


-define(TS_COUCH_DESIGN_DOCS, [{<<"_design/filter">>, [{<<"filters">>, <<"{\"by_doc\": \"function(doc, req) { if (req.query.name == doc._id) {   return true; } else {   return false; }}\"}">>}]}
			       ,{<<"design/LookUpUserAuth">>, [{<<"language">>, <<"javascript">>}
							       ,{<<"views">>, <<"{\"LookUpUserAuth\": {\"map\": \"function(doc) { if(doc.type != 'sys_info' ) return; if(doc.servers) { var srvs = Iterator(doc.servers); for (var srv in srvs)  { if (srv[1].auth) { emit(srv[1].auth.auth_user, srv[1].auth); } } }}\"} }">>}
							      ]}
			       ,{<<"design/LookUpMonitor">>, [{<<"language">>, <<"javascript">>}
							      ,{<<"views">>, <<"{\"LookUpMonitor\": { \"map\": \"function(doc) { if(doc.type != \"sys_info\" ) return; if(doc.servers) { var srvs = Iterator(doc.servers); for (var srv in srvs) { if (srv[1].monitor.monitor_enabled == true) { emit(doc._id, srv[1].monitor); } } } }\"}}">>}
							     ]}
			       ,{<<"design/LookUpIPAuth">>, [{<<"language">>, <<"javascript">>}
							     ,{<<"views">>, <<"{ \"LookUpIPAuth\": { \"map\": \"function(doc) { if(doc.type != \"sys_info\" ) return; if(doc.servers) { var srvs = Iterator(doc.servers); if(doc.servers) { var srvs = Iterator(doc.servers); for (var srv in srvs)  { if (srv[1].auth) { var IPs = Iterator(srv[1].auth.AuthIP); for (var IP in IPs)  { emit(IP[1], JSON.stringify({auth: srv[1].auth})); } } } } for (var srv in srvs)  { if (srv[1].DIDs) { var DIDs = Iterator(srv[1].DIDs); for (var DID in DIDs)  { emit(DID[0], JSON.stringify({auth: srv.auth, DID_Opts: DID[1]})); } } } }\"} } }">>}
							    ]}
			       ,{<<"design/LookUpDID">>, [{<<"language">>, <<"javascript">>}
							  ,{<<"views">>, <<"{ \"LookUpDID\": { \"map\": \"function(doc) { if(doc.type != \"sys_info\" ) return; if(doc.servers) { var srvs = Iterator(doc.servers); for (var srv in srvs) { if (srv[1].DIDs) { var DIDs = Iterator(srv[1].DIDs); for (var DID in DIDs) { emit(DID[0], { \"account_credit\": doc.account.credits.prepay || 0.0, \"server_credit\": srv[1].credits || 0.0, \"did_credit\": DID[1].credits || 0.0, \"trunks_available\": doc.trunks, \"callerid_server\": srv[1].callerid || \"\", \"callerid_account\": doc.callerid || \"\", \"e911_callerid_server\": srv[1].e911_callerid || \"\", \"e911_callerid_account\": doc.e911_callerid || \"\", \"auth\": srv[1].auth, \"DID_Opts\": DID[1], \"inbound_format\": srv[1].inbound_format || \"NPANXXXXXX\", \"account\": doc.account}); } } } } }\"} }">>}
							 ]}]).
