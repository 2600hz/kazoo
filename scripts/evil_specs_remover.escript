#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

-define(SAMPLE_OUTPUT,
        list_to_binary(
          lists:flatten(["core/kazoo_apps/src/kapps_util.erl:263:-spec get_all_accounts() -> kz_term:ne_binaries().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:264:-spec get_all_accounts(kz_util:account_format()) -> kz_term:ne_binaries().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:275:-spec get_all_accounts_and_mods() -> kz_term:ne_binaries().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:276:-spec get_all_accounts_and_mods(kz_util:account_format()) -> kz_term:ne_binaries().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:301:-spec get_all_account_mods() -> kz_term:ne_binaries().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:302:-spec get_all_account_mods(kz_util:account_format()) -> kz_term:ne_binaries().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:313:-spec get_account_mods(kz_term:ne_binary()) -> kz_term:ne_binaries().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:314:-spec get_account_mods(kz_term:ne_binary(), kz_util:account_format()) -> kz_term:ne_binaries().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:608:-spec amqp_pool_request(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun()) ->\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:609:                               kz_amqp_worker:request_return().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:610:-spec amqp_pool_request(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), timeout()) ->\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:611:                               kz_amqp_worker:request_return().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:625:-spec amqp_pool_request_custom(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), gen_listener:binding()) ->\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:626:                                      kz_amqp_worker:request_return().\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:627:-spec amqp_pool_request_custom(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), timeout(), gen_listener:binding()) ->\n"
                        ,"core/kazoo_apps/src/kapps_util.erl:628:                                      kz_amqp_worker:request_return().\n"
                        ]))
       ).

-define(SPECSPEC_REGEX, "ag --nogroup '\\-spec[^.]+\\.$(\n\\-spec[^.]+\\.$)+' ").

main([_|_]=Args) ->
    _ = io:setopts('user', [{'encoding', 'unicode'}]),
    check_ag_available(),
    io:format("Searching for evil sepcspecs...~n"),
    {Options, Paths} = parse_args(Args, {[], []}),
    check_result(list_to_binary(search_for_evil_specs(Paths)), Paths, Options);
main(_) ->
    usage().

check_ag_available() ->
    case os:find_executable("ag") of
        'false' ->
            io:format("~nPlease install 'ag' (https://github.com/ggreer/the_silver_searcher):~n~n"),
            io:format("  apt-get install silversearcher-ag~n"),
            io:format("  brew install the_silver_searcher~n"),
            io:format("  yum install the_silver_searcher~n"),
            io:format("  pacman -S the_silver_searcher~n"),
            io:format("~n"),
            halt(1);
        _ ->
            ok
    end.

usage() ->
    io:format("no correct paths to files or directories was given.~n~n"),
    io:format("Usage: \n\t~s [--create-backup|-b] [--use-kazoo-dirs|-k] <paths_to_files_or_dir>+\n", [filename:basename(escript:script_name())]),
    halt(1).

search_for_evil_specs([]) ->
    usage();
search_for_evil_specs(Paths) ->
    Args = lists:flatten([P ++ " " || P <- Paths]),
    try os:cmd(?SPECSPEC_REGEX ++ Args)
    catch
        _E:_T ->
            io:format("ag failed: ~p:~p", [_E, _T]),
            halt(1)
    end.

parse_args([], {Opts, Paths}) ->
    Patss = [Path
             || Path <- Paths,
                filelib:is_file(Path)
                    orelse filelib:is_dir(Path)
            ],
    case Patss of
        [] ->
            case lists:keyfind(use_kazoo_dirs, 1, Opts) of
                {use_kazoo_dirs, true} -> parse_args([], {Opts, filelib:wildcard(kazoo_root("core/*")) ++ filelib:wildcard(kazoo_root("applications/*"))});
                _ -> usage()
            end;
        _ -> {lists:usort(Opts), lists:usort(Patss)}
    end;
parse_args(["--create-backup" | Rest], {Opts, Paths}) ->
    parse_args(Rest, {[{backup, true} | Opts], Paths});
parse_args(["-b" | Rest], {Opts, Paths}) ->
    parse_args(Rest, {[{backup, true} | Opts], Paths});
parse_args(["--use-kazoo-dirs" | Rest], {Opts, Paths}) ->
    parse_args(Rest, {[{use_kazoo_dirs, true} | Opts], Paths});
parse_args(["-k" | Rest], {Opts, Paths}) ->
    parse_args(Rest, {[{use_kazoo_dirs, true} | Opts], Paths});
parse_args(["." | Rest], {Opts, Paths}) ->
    parse_args(Rest, {Opts, [kazoo_root() | Paths]});
parse_args([Path | Rest], {Opts, Paths}) ->
    parse_args(Rest, {Opts, [filename:absname(Path) | Paths]}).

check_result(<<>>, _, _) ->
    io:format("Hooray! no evil specsecs was found 🎉~n");
check_result(<<"ERR:", _/binary>>=Error, _, _) ->
    io:put_chars(Error);
check_result(Result, [Path], Options) ->
    case {filelib:is_dir(Path), filelib:is_file(Path)} of
        {true, _} -> process_ag_result(Result, Options);
        {_, true} ->
            %% since this is a single file ag won't print filename
            %% adding "file_name:" to the result here
            Lines = [<<(list_to_binary(Path))/binary, ":", Line/binary>>
                         || Line <- binary:split(Result, <<"\n">>, [global]),
                            Line =/= <<>>
                    ],
            process_ag_result(Lines, Options)
    end;
check_result(Result, [_|_], Options) ->
    process_ag_result(Result, Options).

%% get list of files and specs from ag result
process_ag_result(Result, Options) ->
    FilesSpecs = parse_ag_result(Options, [Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], []),
    io:format("processing ~b file(s) with evil specspec:~n", [length(FilesSpecs)]),
    _ = lists:map(fun process_file/1, FilesSpecs),
    io:format("~n🍺 finished~n").

%% Get list of specs for each file from ag result by parse
%% the first line, then get all specs for the file.
%% Then extract specs info from AST.
parse_ag_result(_, [], Acc) -> Acc;
parse_ag_result(Options, [H|T], Acc) ->
    FirstLine = {File, _, _, true} = parse_ag_line(H),
    {NewTail, FileSpecs} = get_ag_file_specs(T, File, [FirstLine]),
    FilePath = binary_to_list(File),
    lists:keyfind(backup, 1, Options) =:= {backup, true}
        andalso backup_file(FilePath),
    {ok, Forms} = epp_dodger:quick_parse_file(FilePath, [{no_fail, true}]),
    SpecMaps = get_ag_specs_info(Forms, FileSpecs, []),
    parse_ag_result(Options, NewTail, [{FilePath, SpecMaps}|Acc]).

%% get all specs for this file
get_ag_file_specs([], _, Acc) -> {[], Acc};
get_ag_file_specs([H|T], File, Acc) ->
    case parse_ag_line(H) of
        {File, _, _, _}=Line ->
            get_ag_file_specs(T, File, Acc ++ [Line]);
        _ ->
            {[H|T], Acc}
    end.

%% find spec function name/arity from AST
get_ag_specs_info(_, [], Acc) -> Acc;
get_ag_specs_info(Forms, [{_, Pos, Line, true}|T], Acc) ->
    %% {attribute, line, {{fun_name, arity}, func_type_ast}}
    {_, _, _, {{FunName, Arity}, _}} = lists:keyfind(binary_to_integer(Pos), 2, Forms),
    {NewTail, SpecLines} = get_multi_line_spec(T, [Line]),
    SpecMap = #{spec => SpecLines
               ,spec_pos => binary_to_integer(Pos)
               ,spec_length => length(SpecLines)
               ,fun_name => FunName
               ,arity => Arity
               },
    get_ag_specs_info(Forms, NewTail, Acc ++ [SpecMap]).

%% accumulate lines until next spec definition
get_multi_line_spec([], Acc) -> {[], Acc};
get_multi_line_spec([{_, _, Line, false}|T], Acc) ->
    get_multi_line_spec(T, Acc ++ [Line]);
get_multi_line_spec(Lines, Acc) ->
    {Lines, Acc}.

parse_ag_line(Bin) ->
    [File, PosRest] = binary:split(Bin, <<":">>),
    [Pos, Rest] = binary:split(PosRest, <<":">>),
    Line = iolist_to_binary(Rest),
    {File, Pos, Line, is_spec_line(Line)}.

is_spec_line(<<"-spec", _/binary>>) -> true;
is_spec_line(_) -> false.

%% process files by first removing evil specs, then get function positions from AST
%% and add specs to position right before their function's position
process_file({File, SpecMaps}) ->
    io:format("  file ~s (\e[36;1m~b\e[0m specs): ", [File, length(SpecMaps)]),
    remove_specs(File, SpecMaps),
    {ok, Forms} = epp_dodger:quick_parse_file(File, [{no_fail, true}]),
    move_specs_funs(File, find_functions_position(Forms, SpecMaps, [])).

%% first read files and create tuplelist of {LineNumber, String}
%% Then loop over SpecMaps and get the position of all lines that the spec
%% is occupied.
remove_specs(File, SpecMaps) ->
    Lines = read_lines(File, true),
    Positions = lists:foldl(fun fetch_specs_pos/2, [], SpecMaps),
    io:format("\e[31;1m-~b\e[0m", [length(Positions)]),
    save_lines(File, [L || {LN, L} <- Lines, not lists:member(LN, Positions)]).

%% Extract specs positions (if it's multi line add number of extra lines)
fetch_specs_pos(#{spec_pos := Pos, spec_length := Length}, Acc) ->
    Acc ++ [Pos + I - 1 || I <- lists:seq(1, Length)].

%% find out the position of the function by looking fun/arity in AST (which is newly read after evil specs are removed)
find_functions_position(_, [], Acc) ->
    lists:sort(fun sort_by_fun_position/2, Acc);
find_functions_position(Forms, [#{fun_name := FunName, arity := Arity}=H|T], Acc) ->
    Functions = lists:filter(fun(Form) -> fun_filter(Form, FunName, Arity) end, Forms),
    find_functions_position(Forms, T, Acc ++ maybe_test_or_regular_function(Functions, H, [])).

%% for when there are two implementations for function, one normal and one guarded by ?TEST
maybe_test_or_regular_function([], #{fun_name := FunName, arity := Arity}, []) ->
    io:format("~n can't find function '~s/~b'~n", [FunName, Arity]),
    halt(1);
maybe_test_or_regular_function([], _, Acc) -> Acc;
maybe_test_or_regular_function([{_, FunLine, _, _, _}|T], Map, Acc) ->
    maybe_test_or_regular_function(T, Map, [Map#{fun_pos => FunLine}|Acc]).

%% this important since sometimes the order of function implementation is not guarantied
%% to be in continuance line order. Sometimes other function or other arity of the this function is implemented first
sort_by_fun_position(#{fun_pos := Pos1}, #{fun_pos := Pos2}) ->
    Pos1 < Pos2.

fun_filter({function, _Line, FName, Arity, _Clauses}, FName, Arity) ->
    true;
fun_filter(_, _, _) ->
    false.

%% read lines from the file again, and add specs to the their new position
move_specs_funs(File, SpecMaps) ->
    Lines = read_lines(File, false),
    {Added, NewLines} = move_specs_funs(Lines, 0, SpecMaps),
    io:format(" \e[32;1m+~b\e[0m~n", [Added]),
    save_lines(File, NewLines).

%% the actual hack, split the lines list at the function position
%% in addition to number of the lines that we have added to file so far,
%% then subtract it by one to get the function line in the right hand side list.
%% For adding spec, first add an empty line before spec if it's necessary and adds everything together.
%% At the end sum number of added lines so far with current spec number of the line and possible empty line.
move_specs_funs(Lines, LinesAdded, []) -> {LinesAdded, Lines};
move_specs_funs(Lines, LinesAdded, [#{fun_pos := Pos, spec := Spec, spec_length := Length}|T]) ->
    {Left, [Fun|Right]} = lists:split(Pos + LinesAdded - 1, Lines),
    EmptyLine = maybe_add_new_line(lists:last(Left)),
    move_specs_funs(Left ++ EmptyLine ++ Spec ++ [Fun] ++ Right
                   ,LinesAdded + Length + length(EmptyLine)
                   ,T
                   ).

maybe_add_new_line(<<>>) -> [];
maybe_add_new_line(_) -> [<<>>].

read_lines(File, WithLineNumber) ->
    case file:read_file(File) of
        {ok, Bin} ->
            case WithLineNumber of
                true ->
                    Fun = fun(L, {LN, Ls}) -> {LN+1, [{LN, L}|Ls]} end,
                    Splits = binary:split(Bin, <<"\n">>, [global]),
                    {_, Lines} = lists:foldl(Fun, {1, []}, Splits),
                    lists:reverse(Lines);
                false -> binary:split(Bin, <<"\n">>, [global])
            end;
        {error, Reason} ->
            throw({error, File, Reason})
    end.

save_lines(File, Lines) ->
    case file:write_file(File, lists:droplast([<<L/binary, "\n">> || L <- Lines])) of
        ok -> ok;
        {error, Reason} ->
            throw({error, File, Reason})
    end.

backup_file(File) ->
    SourceFile = File,
    BakFile = File ++ ".bak",
    case filelib:is_file(BakFile) of
        true -> file:copy(BakFile, SourceFile);
        false -> file:copy(SourceFile, BakFile)
    end.

kazoo_root() ->
    ScriptsDir = filename:dirname(escript:script_name()),
    filename:absname(ScriptsDir ++ "/..").

kazoo_root(Dir) -> kazoo_root() ++ "/" ++ Dir.
