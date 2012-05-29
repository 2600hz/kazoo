#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("t"),
    
    etap:plan(15),
    util:test_good(good()),
    util:test_errors(errors()),
    etap:end_tests().

good() ->
    [
        {<<"{}">>, {[]}},
        {<<"{\"foo\": \"bar\"}">>,
            {[{<<"foo">>, <<"bar">>}]},
            <<"{\"foo\":\"bar\"}">>},
        {<<"\n\n{\"foo\":\r \"bar\",\n \"baz\"\t: 123 }">>,
            {[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]},
            <<"{\"foo\":\"bar\",\"baz\":123}">>}
    ].

errors() ->
    [
        <<"{">>,
        <<"{,}">>,
        <<"{123:true}">>,
        <<"{false:123}">>,
        <<"{:\"stuff\"}">>,
        <<"{\"key\":}">>,
        <<"{\"key\": 123">>,
        <<"{\"key\": 123 true">>,
        <<"{\"key\": 123,}">>
    ].
