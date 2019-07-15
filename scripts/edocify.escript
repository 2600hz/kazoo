#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode(compile).
-compile(nowarn_unused_function).
-compile(nowarn_unused_vars).

-export([main/1]).

-define(SEP(I, C, L), <<(binary:copy(<<$%>>, I))/binary, (binary:copy(C, L))/binary>>).

main(_) ->
    _ = io:setopts(user, [{encoding, unicode}]),
    check_ag_available(),
    ScriptsDir = filename:dirname(escript:script_name()),
    ok = file:set_cwd(filename:absname(ScriptsDir ++ "/..")),

    {Year, _, _} = erlang:date(),

    io:format("Edocify Kazoo...~n~n"),
    FindCmd = regex_find("applications/ core/", "'.*/src/.*.(erl|erl.src)$'", os:type()),
    io:format("find cmd is ~p ~n", [FindCmd]),

    Run = [
           %% regex for evil spec+specs
           {"ag -G '(erl|erl.src|hrl|hrl.src|escript)$' --nogroup '^\\-spec[^.]+\\.$(\\n+\\-spec[^.]+\\.$)+' core/ applications/"
           ,"separate evil sepc+specs"
           ,fun evil_specs/1
           }

           %% ,{"FindCmd
           %%  ,"bump/fix copyright"
           %%  ,fun(R) -> bump_copyright(R, Year) end
           %%  }

           %% regex to find contributors tag.
          ,{"ag -G '(erl|erl.src|hrl|hrl.src|escript)$' -l '%%+ *@?([Cc]ontributors|[Cc]ontributions)' core/ applications/"
           ,"rename contributors tag to author"
           ,fun edocify_headers/1
           }

           %% regex to find `@public' tag.
          ,{"ag -G '(erl|erl.src|hrl|hrl.src|escript)$' '%%* *@(public)' core/ applications/"
           ,"remove public tag"
           ,fun remove_public_tag/1
           }

           %% regex for spec tag in comments: any comments which starts with `@spec' follow by anything (optional one time new line)
           %% until it ends (for single line @spec) any ending with `)' or `}' or any string at the end of the line (should be last regex otherwise
           %% multi line regex won't work). For multi line the first line should end with `|' followed by same regex until exhausted.
          ,{"ag '^%%+\\s*@spec((.*$\\n)?(.*\\)$|.*}$|.*\\|(\\n%%+(.*\\)$|.*}$|.*\\||[^@=-]+$))+)|.*$)' core/ applications/"
           ,"removing spec from comment"
           ,fun remove_comment_specs/1
           }

           %% regex to find functions without comment block before them after a separator comment block
           %% to avoid EDoc to use the separator as the functions comment.
           %% Regex explanation: search for any line starts with at least two `%%' followed by any whitespace, followed by any new line until
           %% a `-spec' attribute or a function head is found.
          ,{"ag -G '(erl)$' '%%%*\\s*==+$(\\n+(^-spec+|[a-z]+))' applications/ core/"
           ,"add missing comments block after separator"
           ,fun missing_comment_blocks_after_sep/1
           }

           %% regex for escaping codes in comment for `resource_exists' function crossbar modules.
          ,{"ag '%%%*\\s*Does the path point to a valid resource$(\\n%%*\\s*.*)*\\n%%%*\\s*@end' applications/"
           ,"escape code block in 'resource_exists' function crossbar modules"
           ,fun cb_resource_exists_comments/1
           }

           %% regex for finding comment block with no @end
          ,{"ag '^%%*[ ]*@doc[^\\n]*$(\\n^(?!(%%* *@end|%%* ?--+$|%%* ?==+$))^%%[^\\n]*$)*(\\n%%* ?(--+|==+)$)' core/ applications/"
           ,"fix comment blocks with no end"
           ,fun comment_blocks_with_no_end/1
           }

           %% regex for separator lines with length lower than 78 (for %%) or 77 (for %%%).
          ,{"ag -G '(applications|core)/.*/src/.*.(erl|erl.src|hrl|hrl.src)$' '^%% *-{50,77}$'"
           ,"increase separator line (starts with %%) length"
           ,fun(R) -> increase_sep_length(R, <<"-">>) end
           }
          ,{"ag -G '(applications|core)/.*/src/.*.(erl|erl.src|hrl|hrl.src)$' '^%%%+ *={50,76}$'"
           ,"increase separator line (starts with %%%) length"
           ,fun(R) -> increase_sep_length(R, <<"=">>) end
           }

           %% regex for finding first comment line after `@doc'
           %% ,{"ag '%%*\\s*@doc$(\\n%%*$)*\\n%%*\\s*[^@\\n]+$' core/ applications/"
           %%  ,"move first comment line to the same line as doc tag"
           %%  ,fun move_to_doc_line/1
           %%  }

           %% regex for empty comment line after @doc to avoid empty paragraph or dot in summary
           %% must be last thing to run
          ,{"ag -G '(erl|erl.src|hrl|hrl.src)$' '%%* *@doc *$(\\n%%* *$)+' core/ applications/"
           ,"remove empty comment line after doc tag"
           ,fun remove_doc_tag_empty_comment/1
           }
          ],
    edocify(Run, 0).

regex_find(Folder, Regex, {'unix', 'darwin'}) ->
    "find -E " ++ Folder ++ " -regex " ++ Regex;
regex_find(Folder, Regex, {'unix', 'linux'}) ->
    "find " ++ Folder ++ " -regextype egrep -regex " ++ Regex;
regex_find(Folder, Regex, _) ->
    io:format("Unplanned OS type, expect Darwin or Linux."),
    error.

check_ag_available() ->
    case os:find_executable("ag") of
        false ->
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

run_ag(Cmd) ->
    try os:cmd(Cmd)
    catch
        _E:_T ->
            io:format("ag command failed: ~p:~p~n", [_E, _T]),
            halt(1)
    end.

edocify([], 0) ->
    io:format("~nAlready EDocified! 🎉~n");
edocify([], Ret) ->
    io:format("~nWe had some EDocification! 🤔~n"),
    halt(Ret);
edocify([{Cmd, Desc, Fun}|Rest], Ret) ->
    io:format("* running command: ~s~n", [Desc]),
    case check_result(list_to_binary(run_ag(Cmd))) of
        ok -> edocify(Rest, Ret);
        AgResult ->
            try Fun(AgResult) of
                ok ->
                    io:format("\e[32;1mdone\e[0m~n"),
                    edocify(Rest, 1);
                NewRet ->
                    io:format("\e[32;1mdone\e[0m~n"),
                    edocify(Rest, NewRet)
            catch
                _T:_R ->
                    io:format("~nexception occurred while running command: ~p:~p~n", [_T, _R]),
                    halt(1)
            end
    end.

check_result(<<>>) ->
    io:format("\e[32;1mdone\e[0m~n");
check_result(<<"ERR:", _/binary>>=Error) ->
    io:format("ag command failed~n"),
    io:put_chars(Error),
    halt(1);
check_result(Result) ->
    Result.

%%------------------------------------------------------------------------------
%% @doc
%% Moving grouped `spec' attributes to their own function header.
%% Ag regex matches the lines starts with `-spec' to end of
%% the it (by a single `.') including multi line spec.
%%
%% For  multi line we accumulate it for that specific `spec' in a map
%% and then first we read source file AST to find spec functions name
%% and arity. Then we remove the evil specs from the file.
%%
%% Then we read AST again to find the function/arity position in the file
%% then adding spec to line before appropriate function header.
%%
%% Ag sample output:
%% ```
%% core/kazoo_apps/src/kapps_util.erl:313:-spec get_account_mods(kz_term:ne_binary()) -> kz_term:ne_binaries().
%% core/kazoo_apps/src/kapps_util.erl:314:-spec get_account_mods(kz_term:ne_binary(), kz_util:account_format()) -> kz_term:ne_binaries().
%% core/kazoo_apps/src/kapps_util.erl:608:-spec amqp_pool_request(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun()) ->
%% core/kazoo_apps/src/kapps_util.erl:609:                               kz_amqp_worker:request_return().
%% core/kazoo_apps/src/kapps_util.erl:610:-spec amqp_pool_request(kz_term:api_terms(), kz_amqp_worker:publish_fun(), kz_amqp_worker:validate_fun(), timeout()) ->
%% core/kazoo_apps/src/kapps_util.erl:611:                               kz_amqp_worker:request_return().
%% '''
%%
%% Expected outcome:
%% move spec to appropriate function header.
%% @end
%%------------------------------------------------------------------------------
evil_specs(Result) ->
    FilesSpecs = map_specs_to_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], []),
    _ = lists:map(fun process_evil_specs/1, FilesSpecs),
    'ok'.

map_specs_to_file([], Acc) -> Acc;
map_specs_to_file([H|T], SpecAcc) ->
    FirstLine = {File, _, _, true} = parse_ag_line(H),
    {NewTail, FileSpecs} = get_specs_for_file(File, T, [FirstLine]),
    FilePath = binary_to_list(File),
    {ok, Forms} = epp_dodger:quick_parse_file(FilePath, [{no_fail, true}]),
    SpecMaps = get_specs_info(File, Forms, FileSpecs, []),
    map_specs_to_file(NewTail, [{FilePath, SpecMaps}|SpecAcc]).

%% Get all specs for this file.
get_specs_for_file(_, [], Acc) -> {[], Acc};
get_specs_for_file(File, [H|T], Acc) ->
    case parse_ag_line(H) of
        {File, _, _, _}=Line ->
            get_specs_for_file(File, T, Acc ++ [Line]);
        {_, _, _, _} ->
            {[H|T], Acc}
    end.

%% Find spec function name/arity from AST.
get_specs_info(_, _, [], Acc) -> Acc;
get_specs_info(File, Forms, [{_, Pos, Line, true}|T], Acc) ->
    %% {attribute, line, {{fun_name, arity}, func_type_ast}}
    {_, _, _, {{FunName, Arity}, _}} = lists:keyfind(Pos, 2, Forms),
    {NewTail, SpecLines} = get_multi_line_spec(T, [Line]),
    SpecMap = #{spec => SpecLines
               ,spec_pos => Pos
               ,spec_length => length(SpecLines)
               ,fun_name => FunName
               ,arity => Arity
               ,file => File
               },
    get_specs_info(File, Forms, NewTail, Acc ++ [SpecMap]).

%% Accumulate lines until next spec definition.
get_multi_line_spec([{_, _, Line, false}|T], Acc) ->
    get_multi_line_spec(T, Acc ++ [Line]);
get_multi_line_spec(Lines, Acc) ->
    {Lines, Acc}.

%% Removing evil specs first then get functions new position from AST
%% and add specs to position right before their function's position.
process_evil_specs({File, SpecMaps}) ->
    io:format("processing ~s~n", [File]),
    remove_evil_specs(File, SpecMaps),
    {ok, Forms} = epp_dodger:quick_parse_file(File, [{no_fail, true}]),
    Lines = read_lines(File, false),
    save_lines(File, move_file_specs(Lines, 0, functions_new_position(Forms, SpecMaps, []))).

%% First read files and create tuple List of {LineNumber, String}
%% Then loop over SpecMaps and get the position of all lines that the spec
%% is occupied.
remove_evil_specs(File, SpecMaps) ->
    Lines = read_lines(File, true),
    Positions = lists:foldl(fun fetch_specs_pos/2, [], SpecMaps),
    save_lines(File, [L || {LN, L} <- Lines, not lists:member(LN, Positions)]).

%% Extract specs positions (if it's multi line spec, add number of all lines it occupied).
fetch_specs_pos(#{spec_pos := Pos, spec_length := Length}, Acc) ->
    Acc ++ [Pos + I - 1 || I <- lists:seq(1, Length)].

%% Find out the new position of the functions by looking fun/arity in AST.
functions_new_position(_, [], Acc) ->
    lists:sort(fun sort_by_fun_position/2, Acc);
functions_new_position(Forms, [#{fun_name := FunName, arity := Arity}=H|T], Acc) ->
    Functions = lists:filter(fun(Form) -> fun_arity_filter(Form, FunName, Arity) end, Forms),
    functions_new_position(Forms, T, Acc ++ is_test_or_regular_function(Functions, H, [])).

fun_arity_filter({function, _Line, FName, Arity, _Clauses}, FName, Arity) ->
    true;
fun_arity_filter(_, _, _) ->
    false.

%% When there are two implementations for function, one normal and one guarded by `?TEST',
%% save position of both functions to move spec to both positions.
is_test_or_regular_function([], #{fun_name := FunName, arity := Arity, file := File}, []) ->
    io:format("~nCan't find function '~s/~b' in file '~s'~nIf this is header file remove the specs line from it and write specs in module itself."
             ,[FunName, Arity, File]
             ),
    halt(1);
is_test_or_regular_function([], _, Acc) -> Acc;
is_test_or_regular_function([{_, FunLine, _, _, _}|T], Map, Acc) ->
    is_test_or_regular_function(T, Map, [Map#{fun_pos => FunLine}|Acc]).

%% This important since we are adding function starting from top line to last line.
sort_by_fun_position(#{fun_pos := Pos1}, #{fun_pos := Pos2}) ->
    Pos1 < Pos2.

%% Split file lines list at the position
%% of the function + number of lines that we have added to file so far,
%% then subtract it by one to get the function line in the right hand side list.
%% For adding spec, first add an empty line before spec if it's necessary and adds everything together.
%% At the end sum the number of added lines so far with current spec number of the line and possible empty line.
move_file_specs(Lines, _, []) -> Lines;
move_file_specs(Lines, LinesAdded, [#{fun_pos := Pos, spec := Spec, spec_length := Length}|T]) ->
    {Left, [Fun|Right]} = lists:split(Pos + LinesAdded - 1, Lines),
    EmptyLine = maybe_add_empty_line_before_spec(lists:last(Left)),
    move_file_specs(Left ++ EmptyLine ++ Spec ++ [Fun] ++ Right
                   ,LinesAdded + Length + length(EmptyLine)
                   ,T
                   ).

%%------------------------------------------------------------------------------
%% @doc Bump copyright year. Also add module header if it is missing (obviously only
%% for files returned by `ag' not all other files).
%%
%% Ag sample output:
%% ```
%% core/kazoo_apps/src/kapps_util.erl
%% core/kazoo_voicemail/src/kvm_message.erl
%% '''
%%
%% Expected outcome:
%% * Bump the year to current year
%% * Have formal format of copyright line
%% * Add blank module header if it is missing
%% @end
%%------------------------------------------------------------------------------

-define(DONT_BUMP, [<<"core/kazoo_stdlib/src/kz_mochinum.erl">>
                   ,<<"core/gcm/src/gcm.erl">>
                   ,<<"core/gcm/src/gcm_api.erl">>
                   ,<<"core/gcm/src/gcm_sup.erl">>
                   ,<<"core/gcm/src/gcm_app.erl">>
                   ,<<"core/amqp_cron/src/amqp_cron_sup.erl">>
                   ,<<"core/amqp_cron/src/amqp_cron_app.erl">>
                   ,<<"core/amqp_cron/src/amqp_cron_task.erl">>
                   ,<<"core/amqp_cron/src/amqp_cron.erl">>
                   ,<<"core/kazoo_ast/src/kz_edoc_layout.erl">>
                   ]).

bump_copyright(Result, Y) ->
    Year = integer_to_binary(Y),
    Files = [F
             || F <- binary:split(Result, <<"\n">>, [global]),
                F =/= <<>>,
                not lists:member(F, ?DONT_BUMP)
            ],
    Bumped = [bump_copyright_file(F, Year) || F <- Files],
    case [OkBump || OkBump <- Bumped, OkBump =:= ok] of
        [] -> 0;
        _ -> 1
    end.

bump_copyright_file(File, Year) ->
    Lines = read_lines(File, false),
    {Module, Header, OtherLines} = get_module_header_comments(Lines, [], []),
    case re:run(iolist_to_binary(Header), "2600[Hh]z", [global]) of
        {match, _} ->
            bump_copyright_file(File, Module, Header, OtherLines, Year);
        _ ->
            case re:run(iolist_to_binary(Header), "@copyright", [global]) of
                {match, _} -> ignore;
                _ ->
                    bump_copyright_file(File, Module, Header, OtherLines, Year)
            end
    end.

bump_copyright_file(File, Module, Header, OtherLines, Year) ->
    MaybeBumped = bump_copyright(Module, Header, [], [], Year),
    case is_bumped(Header, MaybeBumped, Module) of
        true -> ignore;
        false ->
            io:format("processing ~s~n", [File]),
            save_lines(File,  MaybeBumped ++ OtherLines)
    end.

is_bumped(OldHeader, NewHeader, Module) ->
    OldHeader ++ Module =:= [strip_comment(H) || H <- NewHeader].

bump_copyright(Module, [], Copyright, [], Year) ->
    bump_copyright(Module, [], Copyright, [<<"%%% @doc">>], Year);

bump_copyright(Module, [], [], Header, Year) ->
    bump_copyright(Module, [], generate_copyright_line(Year, <<>>), Header, Year);

bump_copyright(Module, [], Copyright, Header, _) ->
    [?SEP(3, <<$->>, 77)] ++ Copyright ++ Header ++ [<<"%%% @end">>, ?SEP(3, <<$->>, 77)] ++ Module;

bump_copyright(Module, [<<"@copyright", Rest/binary>>|T], _, Header, Year) ->
    bump_copyright(Module, T, do_bump_copyright(Rest, Year), Header, Year);

bump_copyright(Module, [H|T], Copyright, Header, Year) ->
    Striped = strip_right_spaces(strip_left_spaces(H)),
    case Striped =/= <<"@end">>
        andalso is_separator_chars(Striped, [<<$=>>, <<$->>])
    of
        false ->
            %% removing end tag to add it later
            bump_copyright(Module, T, Copyright, Header, Year);
        {true, _} ->
            %% removing separator to replace later
            bump_copyright(Module, T, Copyright, Header, Year);
        {false, _} when H =:= <<>> ->
            %% to avoid add whitespace if it is an empty comment line.
            bump_copyright(Module, T, Copyright, Header ++ [<<"%%%">>], Year);
        {false, _} ->
            bump_copyright(Module, T, Copyright, Header ++ [<<"%%% ", H/binary>>], Year)
    end.

do_bump_copyright(C, Year) ->
    Nums = re:replace(C, "([^0-9\\-]*|2600)", <<>>, [global, {return, binary}]),
    case lists:usort([B || B <- binary:split(Nums, <<"-">>, [global]), B =/= <<>>]) of
        [Year] -> generate_copyright_line(Year, <<>>);
        [<<"20", _:2/binary>> = Y] -> generate_copyright_line(Y, Year);
        [<<"20", _:2/binary>> = Y, Year] -> generate_copyright_line(Y, Year);
        [<<"20", _:2/binary>> = Y, _] -> generate_copyright_line(Y, Year);
        _ -> generate_copyright_line(Year, <<>>)
    end.

generate_copyright_line(StartY, EndY) ->
    [<<"%%% @copyright (C) ", StartY/binary, "-", EndY/binary, ", 2600Hz">>].

%%------------------------------------------------------------------------------
%% @doc
%% Edocify Header by rename @contributors to @author.
%% Ag will return a list of files and then we open each file and fix their
%% header comment.
%% * `-module' line should be immediately after header comment
%%
%% Ag sample output:
%% ```
%% core/kazoo_apps/src/kapps_util.erl
%% core/kazoo_voicemail/src/kvm_message.erl
%% '''
%%
%% Expected outcome:
%% * create an `@author' tag for all names after `@contributors'
%% or `@contributors'. (if any name at all)
%% * all header comment lines (all comment lines before `-module' line or
%% non comment line (non empty)) will start with `%%%'
%% * header separator character will be `='
%% * will add `@end' tag if it's not there
%% @end
%%------------------------------------------------------------------------------
edocify_headers(Result) ->
    Files = [F || F <- binary:split(Result, <<"\n">>, [global]), F =/= <<>>],
    _ = [edocify_header(F) || F <- Files],
    'ok'.

edocify_header(File) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, false),
    {Module, Header, OtherLines} = get_module_header_comments(Lines, [], []),
    save_lines(File, edocify_header(Module, Header, []) ++ OtherLines).

edocify_header(Module, [], Header) ->
    [?SEP(3, <<$->>, 77)] ++ Header ++ [<<"%%% @end">>, ?SEP(3, <<$->>, 77)] ++ Module;
edocify_header(Module, [<<"@contributors", _/binary>>|T], Header) ->
    Authors = [<<"%%% @author ", Author/binary>>
                   || A <- T,
                      Author <- [strip_right_spaces(strip_left_spaces(A))],
                      Author =/= <<>>,
                      <<"@end">> =/= Author,
                      not is_separator_char(Author, <<$->>),
                      not is_separator_char(Author, <<$=>>)
              ],
    edocify_header(Module, [], Header ++ [<<"%%%">>] ++ Authors);
edocify_header(Module, [<<"Contributors", Rest/binary>>|T], Header) ->
    %% it's capital `C'
    edocify_header(Module, [<<"@contributors", " ", Rest/binary>>|T], Header);
edocify_header(Module, [<<"@contributions", Rest/binary>>|T], Header) ->
    %% mind you it is `contributions' not `contributors'
    edocify_header(Module, [<<"@contributors", " ", Rest/binary>>|T], Header);
edocify_header(Module, [<<"@Contributions", Rest/binary>>|T], Header) ->
    %% mind you it is `Contributions' not `Contributors'
    edocify_header(Module, [<<"@contributors", " ", Rest/binary>>|T], Header);
edocify_header(Module, [H|T], Header) ->
    Striped = strip_right_spaces(strip_left_spaces(H)),
    case Striped =/= <<"@end">>
        andalso is_separator_chars(Striped, [<<$=>>, <<$->>])
    of
        false ->
            %% removing end tag to add it later
            edocify_header(Module, T, Header);
        {true, _} ->
            %% removing separator to replace later
            edocify_header(Module, T, Header);
        {false, _} when H =:= <<>> ->
            edocify_header(Module, T, Header ++ [<<"%%%">>]);
        {false, _} ->
            edocify_header(Module, T, Header ++ [<<"%%% ", H/binary>>])
    end.

get_module_header_comments([], Module, Header) ->
    {Module, Header};

get_module_header_comments([<<"%", _/binary>>=H | Lines], Module, Header) ->
    get_module_header_comments(Lines, Module, Header ++ [strip_right_spaces(strip_comment(H))]);

get_module_header_comments([<<"-module", _/binary>>=Mod | Lines], _, Header) ->
    get_module_header_comments(Lines, [Mod], Header);

get_module_header_comments([<<>>,  <<"-module", _/binary>>=Mod| Lines], _, Header) ->
    get_module_header_comments(Lines, [Mod], Header);

get_module_header_comments(Lines, Module, Header) ->
    {Module, Header, Lines}.

%%------------------------------------------------------------------------------
%% @doc
%% Removing `@public' from comments.
%% Ad regex will match line with `@public'.
%% So we can simply get file and positions for each file and remove the lines.
%%
%% Ag sample output:
%% ```
%% applications/skel/src/skel_listener.erl:111:%% @public
%% '''
%%
%% Expected outcome:
%% * all found lines should be removed.
%% @end
%%------------------------------------------------------------------------------
remove_public_tag(Result) ->
    CommentSpecs = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun do_remove_public_tag/2, CommentSpecs),
    'ok'.

do_remove_public_tag(File, Positions) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, true),
    save_lines(File, [L || {LN, L} <- Lines, not lists:member(LN, Positions)]).

%%------------------------------------------------------------------------------
%% @doc
%% Removing @spec from comments.
%% Ad regex will match line starts with `@spec' and the line it ends. So
%% we can simply get file and positions for each file and remove the lines.
%%
%% Ag sample output:
%% ```
%% applications/skel/src/skel_listener.erl:111:%% @spec handle_info(Info, State) -> {noreply, State} |
%% applications/skel/src/skel_listener.erl:112:%%                                   {noreply, State, Timeout} |
%% applications/skel/src/skel_listener.erl:113:%%                                   {stop, Reason, State}
%% applications/skel/src/skel_listener.erl:122:%% @spec handle_event(JObj, State) -> {reply, Options}
%% applications/skel/src/skel_listener.erl:136:%% @spec terminate(Reason, State) -> void()
%% '''
%%
%% Expected outcome:
%% * all found lines should be removed.
%% @end
%%------------------------------------------------------------------------------
remove_comment_specs(Result) ->
    CommentSpecs = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun do_remove_comment_specs/2, CommentSpecs),
    'ok'.

do_remove_comment_specs(File, Positions) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, true),
    save_lines(File, [L || {LN, L} <- Lines, not lists:member(LN, Positions)]).

%%------------------------------------------------------------------------------
%% @doc
%% Missing comment block after separator.
%% Ad regex will match lines which starts with comment separator and ends with
%% the line which has a `-spec' attribute or function head. We then go to those
%% positions and format those lines.
%%
%% Ag sample output:
%% ```
%% core/kazoo_media/src/kz_media_file_cache.erl:243:%%%=============================================================================
%% core/kazoo_media/src/kz_media_file_cache.erl:244:-spec start_timer() -> reference().
%% core/kazoo_media/src/kz_media_map.erl:311:%%%=============================================================================
%% core/kazoo_media/src/kz_media_map.erl:312:
%% core/kazoo_media/src/kz_media_map.erl:313:-spec init_map() -> 'ok'.
%% '''
%%
%% Expected outcome:
%% * an empty comment block before the spec line or function header (func without spec line).
%% * maybe an empty comment line after the separator line if it's not exists.
%% * any extra line between separator and `spec' line will be removed.
%% @end
%%------------------------------------------------------------------------------
missing_comment_blocks_after_sep(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun add_missing_comment_blocks/2, Positions),
    'ok'.

add_missing_comment_blocks(File, Positions) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, true),
    save_lines(File, do_add_missing_comment_blocks(Lines, Positions, [])).

do_add_missing_comment_blocks([], _, Formatted) ->
    Formatted;
do_add_missing_comment_blocks([{LN, Line}|Lines], Positions, Formatted) ->
    case lists:member(LN, Positions)
        andalso Line
    of
        false -> do_add_missing_comment_blocks(Lines, Positions, Formatted ++ [Line]);
        <<>> ->
            %% remove extra new lines
            do_add_missing_comment_blocks(Lines, Positions, Formatted);
        <<"-spec", _/binary>> ->
            %% add empty comment block
            do_add_missing_comment_blocks(Lines, Positions, Formatted ++ maybe_add_empty_line(lists:last(Formatted)) ++ empty_block() ++ [Line]);
        <<"%", _/binary>> ->
            %% maybe add empty line after separator
            case Lines of
                [] -> do_add_missing_comment_blocks(Lines, Positions, Formatted ++ [Line]);
                [<<>>|_] -> do_add_missing_comment_blocks(Lines, Positions, Formatted ++ [Line]);
                _ -> do_add_missing_comment_blocks(Lines, Positions, Formatted ++ [Line, <<>>])
            end;
        <<"-", _/binary>> ->
            %% the idea was the regex should match any attribute line before `spec' but it change to match only spec
            %% so this is just left over from previous version and should not be run anyway.
            do_add_missing_comment_blocks(Lines, Positions, Formatted ++ [Line]);
        _ ->
            %% add empty comment block
            do_add_missing_comment_blocks(Lines, Positions, Formatted ++ maybe_add_empty_line(lists:last(Formatted)) ++ empty_block() ++ [Line])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Escape codes in comment block for `resource_exists' function crossbar modules.
%% Ag regex will match the comments before resource_exists function and last line
%% is `%% @end'. We then formats those lines accordingly.
%%
%% Ag sample output:
%% ```
%% applications/acdc/src/cb_queues.erl:143:%% Does the path point to a valid resource
%% applications/acdc/src/cb_queues.erl:144:%% So /queues => []
%% applications/acdc/src/cb_queues.erl:145:%%    /queues/foo => [<<"foo">>]
%% applications/acdc/src/cb_queues.erl:146:%%    /queues/foo/bar => [<<"foo">>, <<"bar">>]
%% applications/acdc/src/cb_queues.erl:147:%% @end
%% '''
%%
%% Expected outcome:
%% %% Does the path point to a valid resource.
%% %%
%% %% For example:
%% %%
%% %% ```
%% %%    /queues => []
%% %%    /queues/foo => [<<"foo">>]
%% %%    /queues/foo/bar => [<<"foo">>, <<"bar">>]
%% %% '''
%% %% @end
%% @end
%%------------------------------------------------------------------------------
cb_resource_exists_comments(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun fix_cb_resource_exists_comment/2, Positions),
    'ok'.

fix_cb_resource_exists_comment(File, Positions) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, true),
    save_lines(File, do_cb_resource_exists_comment(Lines, Positions, [])).

do_cb_resource_exists_comment([], _, Formatted) ->
    Formatted;
do_cb_resource_exists_comment([{LN, Line}|Lines], Positions, Formatted) ->
    case lists:member(LN, Positions)
        andalso Line
    of
        false ->
            do_cb_resource_exists_comment(Lines, Positions, Formatted ++ [Line]);
        <<"%%">> ->
            %% remove empty comment line
            do_cb_resource_exists_comment(Lines, Positions, Formatted);
        <<"%% @end">> ->
            Formatted ++ [Line] ++ [L || {_, L} <- Lines];
        <<"%% So ", Rest/binary>> ->
            do_cb_resource_exists_comment(Lines
                                         ,Positions
                                         ,Formatted ++ [<<"%%">>, <<"%% For example:">>, <<"%%">>, <<"%% ```">>, <<"%%    ", Rest/binary, ".">>]
                                         );
        _ ->
            case Lines of
                [{_, <<"%% @end">>}|_] ->
                    do_cb_resource_exists_comment(Lines, Positions, Formatted ++ [Line, <<"%% '''">>]);
                _ ->
                    do_cb_resource_exists_comment(Lines, Positions, Formatted ++ [Line])
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Fix comment block which doesn't end properly with `@end' tag.
%%
%% Ag sample output:
%% ```
%% applications/ecallmgr/src/ecallmgr_originate.erl:67:%% @doc Starts the server
%% applications/ecallmgr/src/ecallmgr_originate.erl:68:%%--------------------------------------------------------------------
%% applications/ecallmgr/src/ecallmgr_originate.erl:128:%% @doc
%% applications/ecallmgr/src/ecallmgr_originate.erl:129:%% Initializes the server
%% applications/ecallmgr/src/ecallmgr_originate.erl:130:%%--------------------------------------------------------------------
%% '''
%%
%% Expected outcome:
%% * an `@end' tag should be added before the separator line.
%% @end
%%------------------------------------------------------------------------------
comment_blocks_with_no_end(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun comment_blocks_with_no_end/2, Positions),
    'ok'.

comment_blocks_with_no_end(File, Positions) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, true),
    save_lines(File, do_comment_blocks_with_no_end(Lines, Positions, [])).

do_comment_blocks_with_no_end([], _, Formatted) ->
    Formatted;
do_comment_blocks_with_no_end([{LN, Line}|Lines], Positions, Formatted) ->
    {PerCount, {IsSeprator, _}} = analyze_separator(Line, [<<$=>>, <<$->>]),
    case lists:member(LN, Positions)
        andalso IsSeprator
    of
        false ->
            do_comment_blocks_with_no_end(Lines, Positions, Formatted ++ [Line]);
        true ->
            do_comment_blocks_with_no_end(Lines, Positions, Formatted ++ [<<(binary:copy(<<$%>>, PerCount))/binary, " @end">>, Line])
    end.

%%------------------------------------------------------------------------------
%% @doc Increase separator line length.
%%
%% Ag sample output:
%% ``
%% applications/konami/src/konami_listener.erl:3:%%-------_more_---
%% applications/konami/src/konami_listener.erl:5:%%-------_more_---
%% '''
%%
%% Expected outcome:
%% * For `%%' separator, there should be 78 `-'
%% * For `%%%' separator, there should be 77 `='
%% * for both cases any spaces between comment character and separator should be remove.
%% @end
%%------------------------------------------------------------------------------
increase_sep_length(Result, SepChar) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    Separator = make_me_sep(SepChar),
    _ = maps:map(fun(F, P) -> increase_sep_length(F, P, Separator) end, Positions),
    'ok'.

make_me_sep(S = <<"-">>) ->
    ?SEP(2, S, 78);
make_me_sep(S = <<"=">>) ->
    ?SEP(3, S, 77).

increase_sep_length(File, Positions, Separator) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, true),
    save_lines(File, do_increase_sep_length(Lines, Positions, Separator, [])).

do_increase_sep_length([], _, _, Formatted) ->
    Formatted;
do_increase_sep_length([{LN, Line}|Lines], Positions, Separator, Formatted) ->
    case lists:member(LN, Positions) of
        true ->
            do_increase_sep_length(Lines, Positions, Separator, Formatted ++ [Separator]);
        false ->
            do_increase_sep_length(Lines, Positions, Separator, Formatted ++ [Line])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Move first comment line to the same line as `@doc'. So EDoc is not
%% adding extra new line character and spaces to beginning of the
%% paragraph. (Not particularly necessary but it makes better
%% looking HTML code at the end).
%%
%% CAUTION: This code also makes sure there is no empty comment line
%% between `@doc' line and separator to avoid inclusion of separator line
%% in the documentation.
%%
%% Ag sample output:
%% ``
%% applications/konami/src/konami_listener.erl:3:%%% @doc
%% applications/konami/src/konami_listener.erl:4:%%%
%% applications/konami/src/konami_listener.erl:149:%% @doc
%% applications/konami/src/konami_listener.erl:150:%% Initializes the server
%% applications/konami/src/konami_listener.erl:164:%% @doc
%% applications/konami/src/konami_listener.erl:165:%% Handling call messages
%% '''
%%
%% Expected outcome:
%% * for example,
%% %% @doc Initializes the server.
%% * also removes empty comment lines after `@doc' and before first non empty comment line
%% @end
%%------------------------------------------------------------------------------
move_to_doc_line(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun move_to_doc_line/2, Positions),
    'ok'.

move_to_doc_line(File, Positions) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, true),
    save_lines(File, do_move_to_doc_line(Lines, Positions, [])).

do_move_to_doc_line([], _, Formatted) ->
    Formatted;
do_move_to_doc_line([{LN, Line}|Lines], Positions, Formatted) ->
    case lists:member(LN, Positions)
        andalso strip_right_spaces(strip_left_spaces(strip_comment(Line)))
    of
        false ->
            do_move_to_doc_line(Lines, Positions, Formatted ++ [Line]);
        <<>> ->
            %% remove empty comment line
            do_move_to_doc_line(Lines, Positions, Formatted);
        <<"@doc">> ->
            PerCount = count_percent(Line),
            %% add doc tag in case there is no non-empty comment lines so we don't loose the doc tag
            do_move_to_doc_line(Lines, Positions, Formatted ++ [<<(binary:copy(<<$%>>, PerCount))/binary, " @doc">>]);
        Rest ->
            %% this clause should only match once for the first non empty comment line
            PerCount = count_percent(Line),
            DocTagLine = <<(binary:copy(<<$%>>, PerCount))/binary, " @doc">>,
            NewForm = case lists:last(Formatted) of
                          DocTagLine -> lists:droplast(Formatted);
                          _ -> Formatted
                      end,
            do_move_to_doc_line(Lines, Positions, NewForm ++ [<<(DocTagLine)/binary, " ", Rest/binary>>])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Remove empty comment lines after `@doc' to avoid empty paragraph
%% or dot in summary. Regex only returns the line with `@doc' and
%% empty comment line.
%%
%% Ag sample output:
%% ```
%% applications/stepswitch/src/stepswitch_util.erl:3:%%% @doc
%% applications/stepswitch/src/stepswitch_util.erl:4:%%%
%% applications/stepswitch/src/stepswitch_util.erl:25:%% @doc
%% applications/stepswitch/src/stepswitch_util.erl:26:%%
%% '''
%%
%% Expected outcome:
%% * removes all those empty comment line after `@doc'.
%% @end
%%------------------------------------------------------------------------------
remove_doc_tag_empty_comment(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun remove_doc_tag_empty_comment/2, Positions),
    'ok'.

remove_doc_tag_empty_comment(File, Positions) ->
    io:format("processing ~s~n", [File]),
    Lines = read_lines(File, true),
    save_lines(File, do_remove_doc_tag_empty_comment(Lines, Positions, [])).

do_remove_doc_tag_empty_comment([], _, Formatted) ->
    Formatted;
do_remove_doc_tag_empty_comment([{LN, Line}|Lines], Positions, Formatted) ->
    case lists:member(LN, Positions)
        andalso strip_right_spaces(strip_left_spaces(strip_comment(Line)))
    of
        false ->
            do_remove_doc_tag_empty_comment(Lines, Positions, Formatted ++ [Line]);
        <<>> ->
            %% remove empty comment line
            do_remove_doc_tag_empty_comment(Lines, Positions, Formatted);
        _ ->
            do_remove_doc_tag_empty_comment(Lines, Positions, Formatted ++ [Line])
    end.

%%%=============================================================================
%%% Utilities
%%%=============================================================================

maybe_add_empty_line(<<>>) -> [];
maybe_add_empty_line(_) -> [<<>>].

maybe_add_empty_line_before_spec(<<>>) -> [];
%% don't add empty line if it's a comment block, maybe it has some corner case,
%% e.g. the separator belongs to function comment block or not. But most of the
%% time we're good to go this way.
maybe_add_empty_line_before_spec(<<"%%--", _/binary>>) -> [];
maybe_add_empty_line_before_spec(<<"%% --", _/binary>>) -> [];
maybe_add_empty_line_before_spec(_) -> [<<>>].

analyze_separator(<<"%", _/binary>>=Line, SepChars) ->
    analyze_separator(Line, SepChars, 0);
analyze_separator(_, _) ->
    {0, {false, []}}.

analyze_separator(<<>>, _, _) ->
    {0, {false, []}};
analyze_separator(<<"%", Rest/binary>>, Chars, PerCount) ->
    analyze_separator(Rest, Chars, PerCount + 1);
analyze_separator(Rest, Chars, PerCount) ->
    {PerCount, is_separator_chars(strip_right_spaces(strip_left_spaces(Rest)), Chars)}.

is_separator_chars(_, []) -> {false, []};
is_separator_chars(Line, [H|T]) ->
    case is_separator_char(Line, H) of
        true -> {true, H};
        false -> is_separator_chars(Line, T)
    end.

is_separator_char(C, C) ->
    true;
is_separator_char(<<C:1/binary, B/binary>>, C) ->
    is_separator_char(B, C);
is_separator_char(_, _) ->
    false.

collect_positions_per_file([], Map) -> Map;
collect_positions_per_file([Line | Lines], Map) ->
    {File, Pos, _, _} = parse_ag_line(Line),
    collect_positions_per_file(Lines, Map#{File => maps:get(File, Map, []) ++ [Pos]}).

parse_ag_line(<<"ERR:", _/binary>>=Error) ->
    io:format("~nag command failed,~n"),
    io:put_chars(Error),
    halt(1);
parse_ag_line(Bin) ->
    try explode_line(Bin)
    catch _:_ ->
            io:format("~nfailed to explode line in ag output, failed output line:~n~s~n", [Bin]),
            halt(1)
    end.

explode_line(Bin) ->
    [File, PosRest] = binary:split(Bin, <<":">>),
    [Pos, Rest] = binary:split(PosRest, <<":">>),
    Line = iolist_to_binary(Rest),
    {File, binary_to_integer(Pos), Line, is_spec_line(Line)}.

is_spec_line(<<"-spec", _/binary>>) -> true;
is_spec_line(_) -> false.

strip_comment(<<$%, B/binary>>) -> strip_comment(B);
strip_comment(<<$\s, B/binary>>) -> B;
strip_comment(A) -> A.

count_percent(A) ->
    count_percent(A, 0).

count_percent(<<$%, B/binary>>, Count) -> count_percent(B, Count + 1);
count_percent(_, Count) -> Count.

strip_left_spaces(<<$\s, B/binary>>) -> strip_left_spaces(B);
strip_left_spaces(A) -> A.

strip_right_spaces(<<$\s>>) -> <<>>;
strip_right_spaces(<<$\s, B/binary>>) ->
    case strip_right_spaces(B) of
        <<>> -> <<>>;
        T -> <<$\s, T/binary>>
    end;
strip_right_spaces(<<A, B/binary>>) ->
    <<A, (strip_right_spaces(B))/binary>>;
strip_right_spaces(<<>>) -> <<>>.

empty_block() ->
    [<<"%%--------------------------------------------------------------------">>
    ,<<"%% @doc">>
    ,<<"%% @end">>
    ,<<"%%--------------------------------------------------------------------">>
    ].

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
            io:format("failed to read lines from ~s: ~p~n", [File, Reason]),
            throw({error, Reason})
    end.

save_lines(File, Lines) ->
    Data = check_final_newline(lists:reverse([<<L/binary, "\n">> || L <- Lines])),
    case file:write_file(File, Data) of
        ok -> ok;
        {error, Reason} ->
            io:format("failed to write lines to ~s: ~p~n", [File, Reason]),
            throw({error, Reason})
    end.

check_final_newline([<<"\n">>|Tser]) ->
    check_final_newline(Tser);
check_final_newline(Senil) ->
    lists:reverse(Senil).
