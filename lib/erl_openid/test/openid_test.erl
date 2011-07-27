-module(openid_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("openid.hrl").

setup() ->
    [ application:start(X) || X <- [ crypto, ssl, sasl, inets, ibrowse ] ].

discover2_test_() ->
    Cases = [{"https://www.google.com/accounts/o8/id",
	      #openid_authreq{opURLs = ["https://www.google.com/accounts/o8/ud"],
			      version = {2,0},
			      claimedID = "http://specs.openid.net/auth/2.0/identifier_select",
			      localID = "http://specs.openid.net/auth/2.0/identifier_select",
			      assoc = none}},
	     {"http://flickr.com/exbrend",
	      #openid_authreq{opURLs = ["https://open.login.yahooapis.com/openid/op/auth"],
			      version = {2,0},
			      claimedID = "http://flickr.com/exbrend",
			      localID = "http://flickr.com/exbrend",
			      assoc = none}}
	    ],
    {setup, fun setup/0, [?_assertEqual(Result, openid:discover(URL))
			  || {URL, Result} <- Cases ]}.

discover1_test_() ->
    Cases = [{"blog.paulbonser.com",
	      #openid_authreq{opURLs = ["http://www.livejournal.com/openid/server.bml"],
			      version = {1,1},
			      claimedID = "http://blog.paulbonser.com/",
			      localID = "http://misterpib.livejournal.com/",
			      assoc = none}}
	    ],
    {setup, fun setup/0, [?_assertEqual(Result, openid:discover(URL))
			  || {URL, Result} <- Cases ]}.

