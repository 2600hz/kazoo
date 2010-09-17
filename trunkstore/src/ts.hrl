-include_lib("couchbeam/include/couchbeam.hrl").
-define(COUCH_PARAMS, #couchdb_params{
	  host = "66.170.17.98"
	  ,port = 5984
	 }).

-define(TS_DB, "ts").

-define(TS_VIEW_IPAUTH, "LookUpIPAuth/LookUpIPAuth").
-define(TS_VIEW_USERAUTH, "LookUpUserAuth/LookUpUserAuth").
