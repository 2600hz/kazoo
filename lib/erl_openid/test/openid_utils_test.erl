-module(openid_utils_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

normalization_test_() ->
    [?_assertEqual(Result, openid_utils:normalize_id(Id))
     || {Id, Result} <- [{"example.com", "http://example.com/"},
			 {"http://example.com", "http://example.com/"},
			 {"https://example.com/", "https://example.com/"},
			 {"http://example.com/user", "http://example.com/user"},
			 {"http://example.com/user/", "http://example.com/user/"},
			 {"http://example.com/", "http://example.com/"},
			 {"=example", "=example"},
			 {"xri://=example", "=example"},
			 {"http://ExAmPlE.CoM/foo/../a", "http://example.com/a"},
			 {"http://ExAmPlE.CoM/foo/../a/", "http://example.com/a/"},
			 {"http://eXAMPLE:80//a/./b/../b/%63/%7bfoo%7d", "http://example/a/b/c/%7Bfoo%7D"},
			 {"http://example//a/b/c/%7Bfoo%7D", "http://example/a/b/c/%7Bfoo%7D"}
			]
    ].
