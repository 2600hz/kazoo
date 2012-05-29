#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("t"),
    
    etap:plan(6),
    etap:is(ejson:decode(<<"true">>), true, "DEC: true -> true"),
    etap:is(ejson:encode(true), <<"true">>, "ENC: true -> true"),
    
    etap:is(ejson:decode(<<"false">>), false, "DEC: false -> false"),
    etap:is(ejson:encode(false), <<"false">>, "ENC: false -> false"),
    
    etap:is(ejson:decode(<<"null">>), null, "DEC: null -> null"),
    etap:is(ejson:encode(null), <<"null">>, "ENC: null -> null"),

    etap:end_tests().


