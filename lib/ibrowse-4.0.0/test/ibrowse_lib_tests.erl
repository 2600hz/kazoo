%%% File    : ibrowse_lib.erl
%%% Authors : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>,
%%%           Filipe David Manana <fdmanana@apache.org>
%%% Description : Tests for the module ibrowse_lib.erl
%%% Created : 12 April 2011 by Filipe David Manana <fdmanana@apache.org>

-module(ibrowse_lib_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/ibrowse.hrl").


parse_urls_test_() ->
    {timeout, 60, [fun parse_urls/0]}.


parse_urls() ->
    ?assertMatch(#url{
                    abspath = "http://localhost",
                    host = "localhost",
                    host_type = hostname,
                    port = 80,
                    path = "/",
                    username = undefined,
                    password = undefined,
                    protocol = http
                   },
                 ibrowse_lib:parse_url("http://localhost")),
    ?assertMatch(#url{
                    abspath = "http://localhost:80/",
                    host = "localhost",
                    host_type = hostname,
                    port = 80,
                    path = "/",
                    username = undefined,
                    password = undefined,
                    protocol = http
                   },
                 ibrowse_lib:parse_url("http://localhost:80/")),
    ?assertMatch(#url{
                    abspath = "http://127.0.0.1:8000/",
                    host = "127.0.0.1",
                    host_type = ipv4_address,
                    port = 8000,
                    path = "/",
                    username = undefined,
                    password = undefined,
                    protocol = http
                   },
                 ibrowse_lib:parse_url("http://127.0.0.1:8000/")),
    ?assertMatch(#url{
                    abspath = "https://foo:bar@127.0.0.1:8000/test",
                    host = "127.0.0.1",
                    host_type = ipv4_address,
                    port = 8000,
                    path = "/test",
                    username = "foo",
                    password = "bar",
                    protocol = https
                   },
                 ibrowse_lib:parse_url("https://foo:bar@127.0.0.1:8000/test")),
    ?assertMatch(#url{
                    abspath = "https://[::1]",
                    host = "::1",
                    host_type = ipv6_address,
                    port = 443,
                    path = "/",
                    username = undefined,
                    password = undefined,
                    protocol = https
                   },
                 ibrowse_lib:parse_url("https://[::1]")),
    ?assertMatch(#url{
                    abspath = "http://[::1]:8080",
                    host = "::1",
                    host_type = ipv6_address,
                    port = 8080,
                    path = "/",
                    username = undefined,
                    password = undefined,
                    protocol = http
                   },
                 ibrowse_lib:parse_url("http://[::1]:8080")),
    ?assertMatch(#url{
                    abspath = "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:8081/index.html",
                    host = "FEDC:BA98:7654:3210:FEDC:BA98:7654:3210",
                    host_type = ipv6_address,
                    port = 8081,
                    path = "/index.html",
                    username = undefined,
                    password = undefined,
                    protocol = http
                   },
                 ibrowse_lib:parse_url("http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:8081/index.html")),
    ?assertMatch(#url{
                    abspath = "http://[1080:0:0:0:8:800:200C:417A]/foo/bar",
                    host = "1080:0:0:0:8:800:200C:417A",
                    host_type = ipv6_address,
                    port = 80,
                    path = "/foo/bar",
                    username = undefined,
                    password = undefined,
                    protocol = http
                   },
                 ibrowse_lib:parse_url("http://[1080:0:0:0:8:800:200C:417A]/foo/bar")),
    ?assertMatch(#url{
                    abspath = "http://[1080:0:0:0:8:800:200C:417A]:8080/foo/bar",
                    host = "1080:0:0:0:8:800:200C:417A",
                    host_type = ipv6_address,
                    port = 8080,
                    path = "/foo/bar",
                    username = undefined,
                    password = undefined,
                    protocol = http
                   },
                 ibrowse_lib:parse_url("http://[1080:0:0:0:8:800:200C:417A]:8080/foo/bar")),
    ?assertMatch(#url{
                    abspath = "http://[::192.9.5.5]:6000/foo?q=bar",
                    host = "::192.9.5.5",
                    host_type = ipv6_address,
                    port = 6000,
                    path = "/foo?q=bar",
                    username = undefined,
                    password = undefined,
                    protocol = http
                   },
                 ibrowse_lib:parse_url("http://[::192.9.5.5]:6000/foo?q=bar")),
    ?assertMatch({error, invalid_uri},
                 ibrowse_lib:parse_url("http://[:1080:0:0:0:8:800:200C:417A:]:6000/foo?q=bar")),
    ?assertMatch({error, invalid_uri},
                 ibrowse_lib:parse_url("http://[12::z]")),
    ?assertMatch({error, invalid_uri},
                 ibrowse_lib:parse_url("http://foo[1080:0:0:0:8:800:200C:417A]:6000")),
    ?assertMatch({error, invalid_uri},
                 ibrowse_lib:parse_url("http://foo:[1080:0:0:0:8:800:200C:417A]:6000")),
    ok.
