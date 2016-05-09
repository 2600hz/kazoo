-module(kz_http_util_tests).

-include_lib("eunit/include/eunit.hrl").

urldecode_test()->
    ?assertEqual(<<"abcd">>, kz_http_util:urldecode(<<"abcd">>)),
    ?assertEqual(<<"a b c d">>, kz_http_util:urldecode(<<"a+b+c+d">>)),
    ?assertEqual(<<"a b c d">>, kz_http_util:urldecode(<<"a%20b%20c%20d">>)),
    ?assertEqual(<<"a b c d">>, kz_http_util:urldecode(<<"a%20b+c%20d">>)),
    ?assertEqual(<<"a!b*c'd">>, kz_http_util:urldecode(<<"a%21b%2Ac%27d">>)),
    ?assertEqual(<<"!*'abcd">>, kz_http_util:urldecode(<<"%21%2A%27abcd">>)),
    ?assertEqual(<<"abcd!*'">>, kz_http_util:urldecode(<<"abcd%21%2A%27">>)),
    ?assertEqual(<<"()a;b:c@d&=">>, kz_http_util:urldecode(<<"%28%29a%3Bb%3Ac%40d%26%3D">>)),
    ?assertEqual(<<"+$,/?#[]">>, kz_http_util:urldecode(<<"%2B%24%2C%2F%3F%23%5B%5D">>)),
    ?assertEqual(<<"oetnh98(*&!@)#$*)!&*$TOHEDU)(FKQJTHDOE*UF**)(!@#&)~~()~*TOEADHuaoeua+?+OE{?LER?\"SN_<T>:QNJZVWM">>, kz_http_util:urldecode(<<"oetnh98(*%26!%40)%23%24*)!%26*%24TOHEDU)(FKQJTHDOE*UF**)(!%40%23%26)~~()~*TOEADHuaoeua%2B%3F%2BOE%7B%3FLER%3F%22SN_%3CT%3E%3AQNJZVWM">>)),
    ?assertEqual(<<"abcd">>, kz_http_util:urldecode(<<"%61%62%63%64">>)).

parse_query_string_test() ->
        ?assertEqual(
       [{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"wibble \r\n">>}, {<<"z">>, <<"1">>}],
       kz_http_util:parse_query_string(<<"foo=bar&baz=wibble+%0D%0a&z=1">>)),
    ?assertEqual(
       [{<<"">>, <<"bar">>}, {<<"baz">>, <<"wibble \r\n">>}, {<<"z">>, <<"">>}],
       kz_http_util:parse_query_string(<<"=bar&baz=wibble+%0D%0a&z=">>)),
    ?assertEqual(
       [{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"wibble \r\n">>}, {<<"z">>, <<"1">>}],
       kz_http_util:parse_query_string(<<"foo=bar&baz=wibble+%0D%0a&z=1">>)),
    ?assertEqual(
       [],
       kz_http_util:parse_query_string(<<"">>)),
    ?assertEqual(
       [{<<"foo">>, <<"">>}, {<<"bar">>, <<"">>}, {<<"baz">>, <<"">>}],
       kz_http_util:parse_query_string(<<"foo;bar&baz">>)).

urlsplit_test() ->
    ?assertEqual({<<"">>, <<"">>, <<"/foo">>, <<"">>, <<"bar?baz">>}, kz_http_util:urlsplit(<<"/foo#bar?baz">>)),
    ?assertEqual({<<"http">>, <<"host:port">>, <<"/foo">>, <<"">>, <<"bar?baz">>}, kz_http_util:urlsplit(<<"http://host:port/foo#bar?baz">>)),
    ?assertEqual({<<"http">>, <<"host">>, <<"">>, <<"">>, <<"">>}, kz_http_util:urlsplit(<<"http://host">>)),
    ?assertEqual({<<"">>, <<"">>, <<"/wiki/Category:Fruit">>, <<"">>, <<"">>}, kz_http_util:urlsplit(<<"/wiki/Category:Fruit">>)).

urlunsplit_test() ->
    ?assertEqual(<<"/foo#bar?baz">>, kz_http_util:urlunsplit({<<"">>, <<"">>, <<"/foo">>, <<"">>, <<"bar?baz">>})),
    ?assertEqual(<<"http://host:port/foo#bar?baz">>, kz_http_util:urlunsplit({<<"http">>, <<"host:port">>, <<"/foo">>, <<"">>, <<"bar?baz">>})).

urlencode_test() ->
    ?assertEqual(<<"1">> , kz_http_util:urlencode(1)),
    %?assertEqual(<<"1.1">> , kz_http_util:urlencode(1.1)),
    ?assertEqual(<<"1">> , kz_http_util:urlencode(<<"1">>)),
    ?assertEqual(<<"1.1">> , kz_http_util:urlencode(<<"1.1">>)),
    ?assertEqual(<<"foo">> , kz_http_util:urlencode(<<"foo">>)),
    ?assertEqual(<<"foo">> , kz_http_util:urlencode("foo")),
    ?assertEqual(<<"foo">> , kz_http_util:urlencode(foo)),
    ?assertEqual(<<"foo+bar">> , kz_http_util:urlencode(<<"foo bar">>)),
    ?assertEqual(<<"foo%0A">> , kz_http_util:urlencode(<<"foo\n">>)),
    ?assertEqual(<<"foo%3B%26%3D">> , kz_http_util:urlencode(<<"foo;&=">>)).


resolve_uri_test() ->
    RawPath = <<"http://pivot/script.php">>,
    Relative = <<"script2.php">>,
    RawPathList = [<<"http:">>, <<>>, <<"pivot">>, <<"script2.php">>],

    ?assertEqual(RawPathList, kz_http_util:resolve_uri_path(RawPath, Relative)),
    ?assertEqual(RawPathList, kz_http_util:resolve_uri_path(RawPath, <<"/", Relative/binary>>)).
