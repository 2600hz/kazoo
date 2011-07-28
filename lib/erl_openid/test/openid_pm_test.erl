-module(openid_pm_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(PL, [{"mode", "error"}, {"error", "This is an example message"}]).
-define(KVF, "mode:error\nerror:This is an example message\n").
-define(URL, "openid.mode=error&openid.error=This%20is%20an%20example%20message").
-define(URI, "openid.mode=error&openid.error=This+is+an+example+message").

url_encode_test_() ->
    {"URL Encode", ?_assertEqual(?URL, openid_pm:url_encode(?PL))}.

uri_encode_test_() ->
    {"URI Encode", ?_assertEqual(?URI, openid_pm:uri_encode(?PL))}.

kvf_encode_test_() ->
    {"Key-Value Form Encode", ?_assertEqual(?KVF, openid_pm:kvf_encode(?PL))}.

kvf_decode_test_() ->
    {"Key-Value Form Decode", ?_assertEqual(?PL, openid_pm:kvf_decode(?KVF))}.

prefix_check_test_() ->
    Input = [ {"openid." ++ K, V} || {K, V} <- ?PL ],
    {"Prefix Check", [?_assertEqual(?KVF, openid_pm:kvf_encode(Input)),
		      ?_assertEqual(?URI, openid_pm:uri_encode(Input))]
    }.

types_test_() ->
    [ {"Encode " ++ Type,
       ?_test(begin
		  Input = [ {Fun(K), Fun(V)} || {K, V} <- ?PL ],
		  ?assertEqual(?KVF, openid_pm:kvf_encode(Input))
	      end)
      } || {Type, Fun} <- [{"Atom", fun erlang:list_to_atom/1},
			   {"Binary", fun erlang:list_to_binary/1}
			  ] ].
     
