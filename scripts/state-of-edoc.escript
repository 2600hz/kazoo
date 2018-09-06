#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode(compile).
-compile(nowarn_unused_function).
-compile(nowarn_unused_vars).

-export([main/1]).

-include_lib("xmerl/include/xmerl.hrl").

main(Args) ->
    _ = io:setopts(user, [{encoding, unicode}]),
    io:format("State of EDoc~n"),
    ScriptsDir = filename:dirname(escript:script_name()),
    ok = file:set_cwd(filename:absname(ScriptsDir ++ "/..")),

    Erls = get_erls(Args, []),
    Includes = lists:usort(
                 ["core/"]
                 ++ ["applications/tasks"]
                 ++ [filename:dirname(Path) || Path <- filelib:wildcard("core/*/{src,include}/**/*.hrl")]
                 ++ ["applications"]
                 ++ [filename:dirname(Path) || Path <- filelib:wildcard("applications/*/{src,include}/**/*.hrl")]
                 ++ ["deps"]),
    state_of_edoc(Erls, length(Erls), Includes, {[], []}).

get_erls([], []) ->
    lists:sort(filelib:wildcard("{core,applications}/*/src/**/*.erl"));
get_erls([], Acc) ->
    Acc;
get_erls([File|Files], Acc) ->
    get_erls(Files, [File | Acc]).

state_of_edoc([], ErlsLength, _, {NoModule, NoFunctions}) ->
    print_no_module_summary(lists:reverse(NoModule)),
    print_no_functions(lists:sort(fun sort_no_functions/2, NoFunctions)),

    L1 = length(NoModule),
    L2 = length(NoFunctions),

    NoModulePer = integer_to_binary(L1 * 100 div ErlsLength),
    NoFunPer = integer_to_binary(L2 * 100 div ErlsLength),

    ModuleDocPer = integer_to_binary(100 - L1 * 100 div ErlsLength),
    FunDocPer = integer_to_binary(100 - L2 * 100 div ErlsLength),

    io:put_chars(
      [$\n, $\n, "Processed ", integer_to_binary(ErlsLength), " files", $\n
      ,"Files without documentations in module header: ", integer_to_binary(L1), "/", integer_to_binary(ErlsLength), " (%", NoModulePer, ")", $\n
      ,"Files with undocumented functions: ", integer_to_binary(L2), "/", integer_to_binary(ErlsLength), " (%", NoFunPer, ")", $\n, $\n
      ,"Only %", ModuleDocPer, " has documentations in module header", $\n
      ,"Only %", FunDocPer, " has complete documentations for functions", $\n
      ]
     );
state_of_edoc([Erl|Erls], ErlsLength, Includes, Acc) ->
    state_of_edoc(Erls, ErlsLength, Includes, edoc_state_of_file(Erl, Includes, Acc)).

print_no_module_summary([]) ->
    ok;
print_no_module_summary(NoModule) ->
    io:format("~n~nThese files don't have module header documentation:~n"),
    io:put_chars([["-- ", F, $\n] || F <- NoModule]).

print_no_functions([]) ->
    ok;
print_no_functions(NoFunctions) ->
    io:format("~nThese functions are missing documentation:~n"),
    io:put_chars(
      [["-- ", Erl, " "
       ,"[undocumented functions: ", integer_to_binary(length(NoCommented) + 1), "/", integer_to_binary(FunsCount)
       ," (%", integer_to_binary(Percentage), "), %", integer_to_binary(100 - Percentage), " are documented.]", $\n
        %% ,FNameH, "/", FArityH, [[", ", FName, "/", FArity] || {FName, FArity} <- NoCommented]
       ]
       || {Erl, FunsCount, Percentage, [{FNameH, FArityH}|NoCommented]} <- NoFunctions
      ]
     ).

sort_no_functions({_, _, Percentage1, _}, {_, _, Percentage2, _}) ->
    Percentage1 >= Percentage2.

edoc_state_of_file(Erl, Includes, {NoModule, NoFunctions}=Acc) ->
    io:format("."),
    {_, #xmlElement{name = module, content = Es}} = edoc:get_doc(Erl, [{includes, Includes}, {preprocess, true}]),
    HasModuleComment = get_content(briefDescription, get_content(description, Es)) =/= []
        orelse get_content(deprecated,  Es) =/= []
        orelse get_content(see,  Es) =/= [],
    Functions = get_content(functions, Es),
    case no_function_comment(Functions, []) of
        [] when HasModuleComment ->
            Acc;
        [] ->
            {[Erl|NoModule], NoFunctions};
        NoCommented when HasModuleComment ->
            FunsCount = length(Functions),
            Percentage = length(NoCommented) * 100 div FunsCount,
            {NoModule, [{Erl, length(Functions), Percentage, NoCommented}|NoFunctions]};
        NoCommented ->
            FunsCount = length(Functions),
            Percentage = length(NoCommented) * 100 div FunsCount,
            {[Erl|NoModule], [{Erl, FunsCount, Percentage, NoCommented}|NoFunctions]}
    end.

no_function_comment([], Acc) ->
    Acc;
no_function_comment([#xmlElement{content = Es}=E|Functions], Acc) ->
    case get_content(briefDescription, get_content(description, Es)) =:= []
        andalso get_content(deprecated, Es) =:= []
        andalso get_content(equiv, Es) =:= []
        andalso get_content(see, Es) =:= []
        andalso get_content(throws, Es) =:= []
    of
        false ->
            no_function_comment(Functions, Acc);
        true ->
            FName = get_attrval(name, E),
            FArity = get_attrval(arity, E),
            no_function_comment(Functions, [{FName, FArity} | Acc])
    end.

get_elem(Name, [#xmlElement{name = Name} = E | Es]) ->
    [E | get_elem(Name, Es)];
get_elem(Name, [_ | Es]) ->
    get_elem(Name, Es);
get_elem(_, []) ->
    [].

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
        [#xmlAttribute{value = V}] ->
            V;
        [] -> ""
    end.

get_content(Name, Es) ->
    case get_elem(Name, Es) of
        [#xmlElement{content = Es1}] ->
            Es1;
        [] -> []
    end.
