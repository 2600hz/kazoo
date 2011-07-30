-module(openid_srv_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("openid.hrl").

setup() ->
    [ application:start(X) || X <- [ crypto, ssl, sasl, inets, ibrowse ] ].

prepare_test_() ->
    {setup, fun setup/0,
     {timeout, 5000,
      ?_test(
	 begin
	     {ok, Server} = gen_server:start(openid_srv, start_link, [test_server]),
	     Result = (catch gen_server:call(Server, {prepare, "foo", "http://exbrend.livejournal.com", true})),
	     ?_assertEqual(ok, element(1, Result))
	 end)
     }
    }.
