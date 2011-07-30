-module(yadis_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("openid.hrl").

setup() ->
    [ application:start(X) || X <- [ crypto, ssl, sasl, inets, ibrowse ] ].

retrieve_test_() ->
    Cases = [{"https://www.google.com/accounts/o8/id",
	      #openid_xrds{origID = "https://www.google.com/accounts/o8/id",
			   claimedID = none, canonicalID = none, isXRI = false,
			   services = [#openid_xrdservice{types = ["http://specs.openid.net/auth/2.0/server",
								   "http://openid.net/srv/ax/1.0",
								   "http://specs.openid.net/extensions/ui/1.0/mode/popup",
								   "http://specs.openid.net/extensions/ui/1.0/icon",
								   "http://specs.openid.net/extensions/pape/1.0"],
							  uris = ["https://www.google.com/accounts/o8/ud"],
							  localID = none}]}},
	     {"http://exbrend.livejournal.com",
	      #openid_xrds{origID = "http://exbrend.livejournal.com",
			   claimedID = "http://exbrend.livejournal.com/data/yadis",
			   canonicalID = none,isXRI = false,
			   services = [#openid_xrdservice{types = ["http://specs.openid.net/auth/2.0/signon"],
							  uris = ["http://www.livejournal.com/openid/server.bml"],
							  localID = "http://exbrend.livejournal.com/"}]}},
	     {"=brendonh",
	      #openid_xrds{origID = "=brendonh",claimedID = "=brendonh",
			   canonicalID = none,isXRI = true,services = []}}
	    ],
    {setup, fun setup/0, [?_test(?_assertEqual(Result, yadis:retrieve(URL)))
			  || {URL, Result} <- Cases ]}.
