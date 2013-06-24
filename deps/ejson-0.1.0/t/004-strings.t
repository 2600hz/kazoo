#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("t"),
    
    etap:plan(23),
    util:test_good(good()),
    util:test_errors(errors()),
    etap:end_tests().

good() ->
    [
        {<<"\"\"">>, <<"">>},
        {<<"\"0\"">>, <<"0">>},
        {<<"\"foo\"">>, <<"foo">>},
        {<<"\"\\\"foobar\\\"\"">>, <<"\"foobar\"">>},
        {<<"\"\\n\\n\\n\"">>, <<"\n\n\n">>},
        {<<"\"\\\" \\b\\f\\r\\n\\t\\\"\"">>, <<"\" \b\f\r\n\t\"">>},
        {<<"\"foo\\u0005bar\"">>, <<"foo", 5, "bar">>},
        {<<"\"\\uFFFF\"">>, <<239, 191, 191>>, <<"\"", 239, 191, 191, "\"">>},
        {
            <<"\"\\uD834\\uDD1E\"">>,
            <<240, 157, 132, 158>>,
            <<34, 240, 157, 132, 158, 34>>
        },
        % Not sure if this is best but YAJL replaces invalid
        % combining characters with a ?
        {
            <<"\"\\uD834foo\\uDD1E\"">>,
            <<"?oo", 237, 180, 158>>,
            <<34, 63, 111, 111, 237, 180, 158, 34>>
        }
    ].

errors() ->
    [
        <<"\"", 0, "\"">>,
        <<"\"\\g\"">>,
        % CouchDB-345
        <<"\"",78,69,73,77,69,78,32,70,216,82,82,32,70,65,69,78,33,"\"">>
    ].
