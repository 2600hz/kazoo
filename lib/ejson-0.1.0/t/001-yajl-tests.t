#! /usr/bin/env escript

main([]) ->
    code:add_pathz("t"),
    code:add_pathz("ebin"),
    
    Cases = read_cases(),

    etap:plan(length(Cases)),
    lists:foreach(fun(Case) -> test(Case) end, Cases),
    etap:end_tests().

test({Name, Json, Erl}) ->
    etap:is(json_decode(Json), Erl, Name).

json_decode(Json) ->
    case catch(ejson:decode(Json)) of
        {invalid_json, {{error, Error}, _}} ->
            {error, Error};
        {invalid_json, Error} ->
            Error;
        Other ->
            Other
    end.

read_cases() ->
    CasesPath = filename:join(["t", "cases", "*.json"]),
    FileNames = lists:sort(filelib:wildcard(CasesPath)),
    lists:map(fun(F) -> make_pair(F) end, FileNames).

make_pair(FileName) ->
    {ok, Json} = file:read_file(FileName),
    {BaseName, _} = lists:splitwith(fun(C) -> C /= $. end, FileName),
    ErlFname = BaseName ++ ".erl",
    {ok, [Term]} = file:consult(ErlFname),
    case Term of
        {error, _} ->
            {BaseName, Json, Term};
        {error, _, _} ->
            {BaseName, Json, Term};
        _ ->
            {BaseName, Json, Term}
    end.
