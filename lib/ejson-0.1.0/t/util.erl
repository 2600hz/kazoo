-module(util).
-export([test_good/1, test_errors/1]).

test_good(Cases) ->
    lists:foreach(fun(Case) -> check_good(Case) end, Cases).

test_errors(Cases) ->
    lists:foreach(fun(Case) -> check_error(Case) end, Cases).

ok_dec(J, E) ->
    lists:flatten(io_lib:format("Decoding ~p gives ~p", [J, E])).

ok_enc(E, J) ->
    lists:flatten(io_lib:format("Encoding ~p gives ~p", [E, J])).

error_mesg(J) ->
    lists:flatten(io_lib:format("Decoding ~p returns an error.", [J])).

check_good({J, E}) ->
    etap:is(ejson:decode(J), E, ok_dec(J, E)),
    etap:is(ejson:encode(E), J, ok_enc(E, J));
check_good({J, E, J2}) ->
    etap:is(ejson:decode(J), E, ok_dec(J, E)),
    etap:is(ejson:encode(E), J2, ok_enc(E, J2)).

check_error(J) ->
    etap:fun_is(
        fun({invalid_json, _}) -> true; (_) -> false end,
        catch(ejson:decode(J)),
        error_mesg(J)
    ).
