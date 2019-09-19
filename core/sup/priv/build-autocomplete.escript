#!/usr/bin/env escript
%%! +A0 -sname kazoo_sup_build_autocomplete
%% -*- coding: utf-8 -*-

-mode('compile').
-compile(['debug_info']).

-export([main/1]).

%% This list has to never be empty! (nor contain empty strings)
%% MUST be here only modules that don't end with '_maintenance',
%% as these are automatically added anyway.
-define(REQUIRED_MODULES, ["kapps_account_config"
                          ,"kapps_config"
                          ,"kapps_controller"
                          ]).

%% API

main([]) ->
    usage(),
    halt(255);
main([CompletionFile | Paths]) ->
    _ = [find_group_and_print(File)
         || File <- lists:flatmap(fun find_modules/1, Paths)
        ],
    dump(CompletionFile).

find_group_and_print(File) ->
    Found = find(File),
    group(Found),
    print(Found).

%% Internals

dump(CompFile) ->
    {'ok', Dev} = file:open(CompFile, ['write', 'append']),
    'ok' = file:write(Dev
                     ,"# bash completion for 2600Hz, Inc's sup command\n"
                      "\n"
                      "_sup() {\n"
                      "    local cur prev\n"
                      "    _get_comp_words_by_ref cur prev\n"
                      "\n"
                      "    local path=.$(echo ${COMP_WORDS[*]} | sed 's/ /./g').\n"
                      "    # echo $path\n"
                      "\n"
                      "    args() {\n"
                      "        case $path in\n"
                     ),

    write_module_functions(Dev),

    'ok' = file:write(Dev
                     ,"        esac\n"
                      "    }\n"
                      "\n"
                      "    case $prev in\n"
                      "\n"
                     ),

    'ok' = file:write(Dev, case_sup()),

    write_modules(Dev),

    'ok' = file:write(Dev
                     ,"\n"
                      "        *) args ;;\n"
                      "    esac\n"
                      "}\n"
                      "complete -F _sup sup\n"
                     ),
    'ok' = file:close(Dev).

write_module_functions(Dev) ->
    lists:foreach(fun(MF) ->
                          UChars = unicode:characters_to_binary(case_args(MF)),
                          'ok' = file:write(Dev, UChars)
                  end
                 ,get('mfs')
                 ).

write_modules(Dev) ->
    lists:foreach(fun(M) -> 'ok' = file:write(Dev, case_prev(M)) end
                 ,get('modules')
                 ).

group([]) -> 'ok';
group([{Module,_,_,_}|_]=MFAs) ->
    append('modules', Module),
    Fs = lists:map(fun ({_M, F, _A, As}) ->
                           append({Module,F}, As),
                           F
                   end
                  ,MFAs
                  ),
    [append('mfs', {Module,F}) || F <- lists:usort(Fs)],
    put(Module, lists:usort(Fs)).

append(Key, Value) ->
    case get(Key) of
        L when is_list(L) ->
            put(Key, [Value|L]);
        _ ->
            put(Key, [Value])
    end.

case_sup() ->
    Ms = get('modules'),
    ["        sup) COMPREPLY=( $(compgen -W '", spaces(Ms), "' -- $cur) ) ;;\n\n"].

case_prev(M) ->
    Fs = get(M),
    %% io:format("Fs = ~p\n", [Fs]),
    ["        ", to_list(M), ")\n"
     "            case $path in\n"
     "                .sup.", to_list(M), ".*)"
     " COMPREPLY=( $(compgen -W '", spaces(Fs), "' -- $cur) ) ;;\n"
     "            esac ;;\n"
    ].

case_args({M,F} = MF) ->
    Asz = lists:reverse(get(MF)),
    %% io:format("Asz(~p) ~10000p\n", [MF, Asz]),
    ["            .sup.", to_list(M), $., to_list(F), ".*)"
     " COMPREPLY=( $(compgen -W '--> ", spaces(sep_vars(Asz)), "' -- $cur) ) ;;\n"
    ].

sep_vars(Lists) ->
    [ [$[, join_args($,, As), $]]
      || As <- Lists
    ].

join_args(_, []) -> "";
join_args(_, [A]) -> to_list(A);
join_args(Sep, [H|As]) ->
    [to_list(H), [ [Sep,A] || A <- As ]].

spaces(List) ->
    [[to_list(E), $\s] || E <- List].

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list([$"]++_=Word) -> [Char || Char <- Word, Char =/= $"];
to_list(S) -> S.

print([]) -> 'ok';
print([{M,F,_A,Vs}|Rest]) ->
    Vars = string:join(Vs, " "),
    io:format("sup ~s ~s ~s\n", [M, F, Vars]),
    print(Rest);
print(_R) ->
    io:format("_R = ~p\n", [_R]).

pp(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
pp({'atom',_,Atom}) ->
    "'" ++ pp(Atom) ++ "'";
pp({'var',_,Atom}) ->
    pp(Atom);
pp({'bin',_,[{'bin_element',_,{'var',_,Atom},_,_}]}) ->
    pp(Atom);
pp({'bin',_,[{'bin_element',_,{'string',_,Str},_,_}]}) ->
    "\"" ++ Str ++ "\"";

%% MAY hide those (internals)
pp({'nil',_}) ->
    "[]";
pp({'cons',_,H,T}) ->
    "[" ++ pp(H) ++ "|" ++ pp(T) ++ "]";
pp(_E) ->
    "PLACEHOLDER".

find(File) ->
    {'ok', {Module, [{'exports', FAs0}]}} = beam_lib:chunks(File, ['exports']),
    FAs = [{F,A} || {F,A} <- FAs0, F =/= 'module_info'],
    {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(File, ['abstract_code']),
    %% io:format("AST = ~p\n", [AST]),
    {'raw_abstract_v1', Ts} = AST,
    Clauses = [{F,A,Cs} || {'function',_,F,A,Cs} <- Ts, lists:member({F,A}, FAs)],
    %% io:format("Clauses = ~p\n", [Clauses]),
    Ps = lists:flatmap(fun({F,A,Cs}) ->
                               %% io:format("F ~p A ~p\n", [F,A]),
                               %% Clauses = [Clauses || {function,_,F,A,Clauses} <- Ts],
                               %% io:format("Clauses = ~p\n", [Clauses]),
                               %% io:format("~s ~s\n", [Module, F]),
                               ArgsPerClause = [ [pp(Arg) || Arg <- Args]
                                                 || {'clause',_,Args,_,_} <- Cs
                                               ],
                               [{Module,F,A,Args} || Args <- ArgsPerClause]
                       end
                      ,Clauses
                      ),
    %% Ps.
    lists:usort(Ps).

find_modules(Path) ->
    case filelib:is_dir(Path) of
        'false' -> [];
        'true' ->
            AccFiles = fun (File, Acc) -> [File|Acc] end,
            Required = string:join(?REQUIRED_MODULES, "|"),
            filelib:fold_files(Path, "(.+_maintenance|" ++ Required ++ ")\\.beam", 'true', AccFiles, [])
    end.

usage() ->
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  <completion file destination> <path to dir>+\n", [filename:basename(Arg0)]).

%% End of Module
