#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("t"),
    
    etap:plan(12),
    util:test_good(good()),
    util:test_errors(errors()),
    etap:end_tests().

good() ->
    [
        {<<"[{}]">>, [{[]}]},
        {<<"{\"foo\":[123]}">>, {[{<<"foo">>, [123]}]}},
        {<<"{\"foo\":{\"bar\":true}}">>,
            {[{<<"foo">>, {[{<<"bar">>, true}]} }]} },
        {<<"{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}">>,
            {[
                {<<"foo">>, []},
                {<<"bar">>, {[{<<"baz">>, true}]}},
                {<<"alice">>, <<"bob">>}
            ]}
        },
        {<<"[-123,\"foo\",{\"bar\":[]},null]">>,
            [
                -123,
                <<"foo">>,
                {[{<<"bar">>, []}]},
                null
            ]
        }
    ].

errors() ->
    [
        <<"[{}">>,
        <<"}]">>
    ].
