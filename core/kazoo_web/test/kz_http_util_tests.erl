-module(kz_http_util_tests).

-include_lib("eunit/include/eunit.hrl").

urldecode_test_()->
    [?_assertEqual(<<"abcd">>, kz_http_util:urldecode(<<"abcd">>))
    ,?_assertEqual(<<"a b c d">>, kz_http_util:urldecode(<<"a+b+c+d">>))
    ,?_assertEqual(<<"a b c d">>, kz_http_util:urldecode(<<"a%20b%20c%20d">>))
    ,?_assertEqual(<<"a b c d">>, kz_http_util:urldecode(<<"a%20b+c%20d">>))
    ,?_assertEqual(<<"a!b*c'd">>, kz_http_util:urldecode(<<"a%21b%2Ac%27d">>))
    ,?_assertEqual(<<"!*'abcd">>, kz_http_util:urldecode(<<"%21%2A%27abcd">>))
    ,?_assertEqual(<<"abcd!*'">>, kz_http_util:urldecode(<<"abcd%21%2A%27">>))
    ,?_assertEqual(<<"()a;b:c@d&=">>, kz_http_util:urldecode(<<"%28%29a%3Bb%3Ac%40d%26%3D">>))
    ,?_assertEqual(<<"+$,/?#[]">>, kz_http_util:urldecode(<<"%2B%24%2C%2F%3F%23%5B%5D">>))
    ,?_assertEqual(<<"oetnh98(*&!@)#$*)!&*$TOHEDU)(FKQJTHDOE*UF**)(!@#&)~~()~*TOEADHuaoeua+?+OE{?LER?\"SN_<T>:QNJZVWM">>, kz_http_util:urldecode(<<"oetnh98(*%26!%40)%23%24*)!%26*%24TOHEDU)(FKQJTHDOE*UF**)(!%40%23%26)~~()~*TOEADHuaoeua%2B%3F%2BOE%7B%3FLER%3F%22SN_%3CT%3E%3AQNJZVWM">>))
    ,?_assertEqual(<<"abcd">>, kz_http_util:urldecode(<<"%61%62%63%64">>))
    ].

parse_query_string_test_() ->
    [?_assertEqual([{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"wibble \r\n">>}, {<<"z">>, <<"1">>}]
                  ,kz_http_util:parse_query_string(<<"foo=bar&baz=wibble+%0D%0a&z=1">>))
    ,?_assertEqual([{<<"">>, <<"bar">>}, {<<"baz">>, <<"wibble \r\n">>}, {<<"z">>, <<"">>}]
                  ,kz_http_util:parse_query_string(<<"=bar&baz=wibble+%0D%0a&z=">>))
    ,?_assertEqual([{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"wibble \r\n">>}, {<<"z">>, <<"1">>}]
                  ,kz_http_util:parse_query_string(<<"foo=bar&baz=wibble+%0D%0a&z=1">>))
    ,?_assertEqual([]
                  ,kz_http_util:parse_query_string(<<"">>))
    ,?_assertEqual([{<<"foo">>, <<"">>}, {<<"bar">>, <<"">>}, {<<"baz">>, <<"">>}]
                  ,kz_http_util:parse_query_string(<<"foo;bar&baz">>))
    ].

urlsplit_test_() ->
    [?_assertEqual({<<"">>, <<"">>, <<"/foo">>, <<"">>, <<"bar?baz">>}, kz_http_util:urlsplit(<<"/foo#bar?baz">>))
    ,?_assertEqual({<<"http">>, <<"host:port">>, <<"/foo">>, <<"">>, <<"bar?baz">>}, kz_http_util:urlsplit(<<"http://host:port/foo#bar?baz">>))
    ,?_assertEqual({<<"http">>, <<"host">>, <<"">>, <<"">>, <<"">>}, kz_http_util:urlsplit(<<"http://host">>))
    ,?_assertEqual({<<"">>, <<"">>, <<"/wiki/Category:Fruit">>, <<"">>, <<"">>}, kz_http_util:urlsplit(<<"/wiki/Category:Fruit">>))
    ].

urlunsplit_test_() ->
    [?_assertEqual(<<"/foo#bar?baz">>, kz_http_util:urlunsplit({<<"">>, <<"">>, <<"/foo">>, <<"">>, <<"bar?baz">>}))
    ,?_assertEqual(<<"http://host:port/foo#bar?baz">>, kz_http_util:urlunsplit({<<"http">>, <<"host:port">>, <<"/foo">>, <<"">>, <<"bar?baz">>}))
    ].

urlencode_test_() ->
    [?_assertEqual(<<"1">> , kz_http_util:urlencode(1))
    %% ,?_assertEqual(<<"1.1">> , kz_http_util:urlencode(1.1))
    ,?_assertEqual(<<"1">> , kz_http_util:urlencode(<<"1">>))
    ,?_assertEqual(<<"1.1">> , kz_http_util:urlencode(<<"1.1">>))
    ,?_assertEqual(<<"foo">> , kz_http_util:urlencode(<<"foo">>))
    ,?_assertEqual(<<"foo">> , kz_http_util:urlencode("foo"))
    ,?_assertEqual(<<"foo">> , kz_http_util:urlencode('foo'))
    ,?_assertEqual(<<"foo+bar">> , kz_http_util:urlencode(<<"foo bar">>))
    ,?_assertEqual(<<"foo%0A">> , kz_http_util:urlencode(<<"foo\n">>))
    ,?_assertEqual(<<"foo%3B%26%3D">> , kz_http_util:urlencode(<<"foo;&=">>))
    ].
