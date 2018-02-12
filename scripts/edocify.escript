#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode(compile).

-export([main/1]).

-define(SEP, <<"%%%-------------------------------------------------------------------">>).
-define(SEP_2, <<"%%%===================================================================">>).

%% regex for evil spec+specs
-define(REGEX_SPECSPEC, "ag -G '(erl|erl.src|hrl|hrl.src|escript)$' --nogroup '^\\-spec[^.]+\\.$(\\n+\\-spec[^.]+\\.$)+' core/ applications/").

%% regex to find contributors tag.
-define(REGEX_HAS_CONTRIBUTORS, "ag -G '(erl)$' -l '%%%+\\s*@?([Cc]ontributors|[Cc]ontributions)'").

%% regex to spec tag in comments: any comments which starts with `@spec' follow by anything (optional one time new line)
%% until it ends (for single line @spec) any ending with `)' or `}' or any string at the end of the line (should be last regex otherwise
%% multi line regex won't work). For multi line the first line should end with `|' followed by same regex until exhausted.
-define(REGEX_COMMENT_SPEC, "ag '^%%+\\s*@spec((.*$\\n)?(.*\\)$|.*}$|.*\\|(\\n%%+(.*\\)$|.*}$|.*\\||[^@=-]+$))+)|.*$)' core/ applications/").

%% regex to find functions without comment block before them after a separator comment block
%% to avoid EDoc to use the separator as the functions comment.
%% Regex explanation: search for any line starts with at least two `%%' followed by any whitespace, followed by any new line until
%% a `-spec' attribute or a function head is found.
-define(REGEX_SEP_SPEC, "ag -G '(erl)$' '%%%*\\s*==+$(\\n+(^-spec+|[a-z]+))' applications/ core/").

%% regex for escaping codes in comment for `resource_exists' function crossbar modules.
-define(REGEX_CB_RESOURCE_EXISTS_COMMENT, "ag '%%%*\\s*Does the path point to a valid resource$(\\n%%*\\s*.*)*\\n%%%*\\s*@end' applications/crossbar/").

%% regex for finding comment block with no @end
-define(REGEX_COMMENT_BLOCK_WITH_NO_END, "ag '^%%*[ ]*@doc[^\\n]*$(\\n^(?!(%%* *@end|%%* ?--+$|%%* ?==+$))^%%[^\\n]*$)*(\\n%%* ?(--+|==+)$)' core/ applications/").

%% regex for finding comments written in the same line as `@doc'
-define(REGEX_IN_DOC_LINE, "ag '%%*\\s*@doc$\\n%%*\\s*[^@\\n]+$'  core/ applications/").

%% regex for empty comment line after @doc to avoid empty paragraph or dot in summary
-define(REGEX_DOC_TAG_EMPTY_COMMENT, "ag -G '(erl|erl.src|hrl|hrl.src)$' '%%*\\s*@doc.*(\\n%%*$)+' core/ applications/").

main(_) ->
    _ = io:setopts(user, [{encoding, unicode}]),
    check_ag_available(),
    ScriptsDir = filename:dirname(escript:script_name()),
    ok = file:set_cwd(filename:absname(ScriptsDir ++ "/..")),

    io:format("Edocify Kazoo...~n~n"),

    Run = [{?REGEX_SPECSPEC, "removing evil sepc+specs", fun evil_specs/1}
          ,{?REGEX_HAS_CONTRIBUTORS, "rename and fix `@contributors' tags to '@author'", fun edocify_headers/1}
          ,{?REGEX_COMMENT_SPEC, "removing @spec from comments", fun remove_comment_specs/1}
          ,{?REGEX_SEP_SPEC, "adding missing comments block after separator", fun missing_comment_blocks_after_sep/1}
          ,{?REGEX_CB_RESOURCE_EXISTS_COMMENT, "escape code block for 'resource_exists' function crossbar modules", fun cb_resource_exists_comments/1}
          ,{?REGEX_COMMENT_BLOCK_WITH_NO_END, "fix comment blocks with no @end", fun comment_blocks_with_no_end/1}
          ,{?REGEX_IN_DOC_LINE, "move first comment line to the same line as @doc", fun move_in_doc_line/1}
           %% must be last thing to run
          ,{?REGEX_DOC_TAG_EMPTY_COMMENT, "remove empty comment line after @doc", fun remove_doc_tag_empty_comment/1}
          ],
    edocify(Run, 0).

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
            io:format("ag failed: ~p:~p~n", [_E, _T]),
            halt(1)
    end.

edocify([], 0) ->
    io:format("~nAlready EDocified! 🎉~n");
edocify([], Ret) ->
    io:format("~nWe had some EDocification! 🤔~n"),
    halt(Ret);
edocify([{Cmd, Desc, Fun}|Rest], Ret) ->
    io:format("* ~s: ", [Desc]),
    case check_result(list_to_binary(run_ag(Cmd))) of
        ok -> edocify(Rest, Ret);
        AgResult ->
            _ = Fun(AgResult),
            io:format(" done~n"),
            edocify(Rest, 1)
    end.

check_result(<<>>) ->
    io:format(" done~n");
check_result(<<"ERR:", _/binary>>=Error) ->
    io:put_chars(Error),
    halt(1);
check_result(Result) ->
    Result.

%%--------------------------------------------------------------------
%% @doc
%% Moving grouped `spec' attributes to line before their own function
%% header. Ag regex matches the lines starts with `-spec' to end of
%% the it (by a single `.') including multi line spec.
%%
%% For  multi line we accumulate it for that specific `spec' in a map
%% and then first we read source file AST to find spec functions name
%% and arity. Then we remove the evil specs from the file.
%%
%% Then we read AST again to find the function/arity position in the file
%% then adding spec to line before appropriate function header.
%% @end
%%--------------------------------------------------------------------
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
    SpecMaps = get_specs_info(Forms, FileSpecs, []),
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
get_specs_info(_, [], Acc) -> Acc;
get_specs_info(Forms, [{_, Pos, Line, true}|T], Acc) ->
    %% {attribute, line, {{fun_name, arity}, func_type_ast}}
    {_, _, _, {{FunName, Arity}, _}} = lists:keyfind(Pos, 2, Forms),
    {NewTail, SpecLines} = get_multi_line_spec(T, [Line]),
    SpecMap = #{spec => SpecLines
               ,spec_pos => Pos
               ,spec_length => length(SpecLines)
               ,fun_name => FunName
               ,arity => Arity
               },
    get_specs_info(Forms, NewTail, Acc ++ [SpecMap]).

%% Accumulate lines until next spec definition.
get_multi_line_spec([{_, _, Line, false}|T], Acc) ->
    get_multi_line_spec(T, Acc ++ [Line]);
get_multi_line_spec(Lines, Acc) ->
    {Lines, Acc}.

%% Removing evil specs first then get functions new position from AST
%% and add specs to position right before their function's position.
process_evil_specs({File, SpecMaps}) ->
    io:format("."),
    remove_evil_specs(File, SpecMaps),
    {ok, Forms} = epp_dodger:quick_parse_file(File, [{no_fail, true}]),
    Lines = read_lines(File, false),
    save_lines(File, move_file_specs(Lines, 0, functions_new_position(Forms, SpecMaps, []))).

%% First read files and create tupleList of {LineNumber, String}
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
is_test_or_regular_function([], #{fun_name := FunName, arity := Arity}, []) ->
    io:format("~ncan't find function '~s/~b'~n", [FunName, Arity]),
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

%%--------------------------------------------------------------------
%% @doc
%% Edocify Header by rename @contributors to @author.
%% Ag will return a list of files and then we open each file and fix their
%% header comment.
%% @end
%%--------------------------------------------------------------------
edocify_headers(Result) ->
    Files = [F || F <- binary:split(Result, <<"\n">>, [global]), F =/= <<>>],
    _ = [edocify_header(F) || F <- Files],
    'ok'.

edocify_header(File) ->
    io:format("."),
    Lines = read_lines(File, false),
    {Header, OtherLines} = find_header(Lines, []),
    save_lines(File, edocify_header(Header, []) ++ OtherLines).

edocify_header([], Header) ->
    [?SEP] ++ Header ++ [<<"%%% @end">>, ?SEP];
edocify_header([<<"@contributors", _/binary>>], Header) ->
    edocify_header([], Header);
edocify_header([<<"@contributors", _/binary>>|T], Header) ->
    Authors = [<<"%%% @author ", Author/binary>>
                   || A <- T,
                      Author <- [strip_left_space(A)],
                      Author =/= <<>>
              ],
    edocify_header([], Header ++ [<<"%%%">>] ++ Authors);
edocify_header([<<>>|T], Header) ->
    edocify_header(T, Header ++ [<<"%%%">>]);
edocify_header([<<"@contributions", Rest/binary>>|T], Header) ->
    %% mind you it is `contributions' not `contributors'
    edocify_header([<<"@contributors", " ", Rest/binary>>|T], Header);
edocify_header([<<"@Contributions", Rest/binary>>|T], Header) ->
    %% mind you it is `Contributions' not `Contributors'
    edocify_header([<<"@contributors", " ", Rest/binary>>|T], Header);
edocify_header([<<"Contributors", Rest/binary>>|T], Header) ->
    edocify_header([<<"@contributors", " ", Rest/binary>>|T], Header);
edocify_header([H|T], Header) ->
    edocify_header(T, Header ++ [<<"%%% ", H/binary>>]).

find_header([], Header) ->
    {Header, []};
find_header([<<"-module", _/binary>>=ModLine | Lines], Header) ->
    {Header, [ModLine | Lines]};
find_header([<<"%%% --", _/binary>> | Lines], Header) ->
    %% remove separator
    find_header(Lines, Header);
find_header([<<"%%% ==", _/binary>> | Lines], Header) ->
    %% remove separator
    find_header(Lines, Header);
find_header([<<"%%%% --", _/binary>> | Lines], Header) ->
    %% remove separator
    find_header(Lines, Header);
find_header([<<"%%%% ==", _/binary>> | Lines], Header) ->
    %% remove separator
    find_header(Lines, Header);
find_header([<<"%%%--", _/binary>> | Lines], Header) ->
    %% remove separator
    find_header(Lines, Header);
find_header([<<"%%%==", _/binary>> | Lines], Header) ->
    %% remove separator
    find_header(Lines, Header);
find_header([<<"%%%%--", _/binary>> | Lines], Header) ->
    %% remove separator
    find_header(Lines, Header);
find_header([<<"%%%%==", _/binary>> | Lines], Header) ->
    %% remove separator
    find_header(Lines, Header);
find_header([<<"%%% @end", _/binary>> | Lines], Header) ->
    %% remove @end
    find_header(Lines, Header);
find_header([<<"%%%", _/binary>>=Comment | Lines], Header) ->
    find_header(Lines, Header ++ [strip_comment(Comment)]);
find_header([<<"%%", _/binary>>=Line | Lines], Header) ->
    case is_seprator(Line)
        andalso look_after(Lines)
    of
        true -> find_header(Lines, Header ++ [strip_comment(Line)]);
        false ->  find_header(Lines ++ [Line], Header)
    end;
find_header([Line | Lines], Header) ->
    find_header(Lines ++ [Line], Header).

look_after([<<"%%%", _/binary>>|_]) -> true;
look_after([<<"%%%%", _/binary>>|_]) -> true;
look_after(_) -> false.

%%--------------------------------------------------------------------
%% @doc
%% Removing @spec from comments.
%% Ad regex will match line starts with `@spec' and the line it ends. So
%% we can simply get file and positions for each file and remove the lines.
%% @end
%%--------------------------------------------------------------------
remove_comment_specs(Result) ->
    CommentSpecs = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun do_remove_comment_specs/2, CommentSpecs),
    'ok'.

do_remove_comment_specs(File, Positions) ->
    io:format("."),
    Lines = read_lines(File, true),
    save_lines(File, [L || {LN, L} <- Lines, not lists:member(LN, Positions)]).

%%--------------------------------------------------------------------
%% @doc
%% Missing comment block after separator.
%% Ad regex will match lines which starts with comment separator and ends with
%% the line which has a `-spec' attribute or function head. We then go to those
%% positions and format those lines.
%% @end
%%--------------------------------------------------------------------
missing_comment_blocks_after_sep(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun add_missing_comment_blocks/2, Positions),
    'ok'.

add_missing_comment_blocks(File, Positions) ->
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
            do_add_missing_comment_blocks(Lines, Positions, Formatted ++ [Line]);
        _ ->
            %% add empty comment block
            do_add_missing_comment_blocks(Lines, Positions, Formatted ++ maybe_add_empty_line(lists:last(Formatted)) ++ empty_block() ++ [Line])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Escape codes in comment block for `resource_exists' function crossbar modules.
%% Ag regex will match the comments before resource_exists function with last line
%% is `%% @end' line. We then formats those lines accordingly.
%% @end
%%--------------------------------------------------------------------
cb_resource_exists_comments(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun fix_cb_resource_exists_comment/2, Positions),
    'ok'.

fix_cb_resource_exists_comment(File, Positions) ->
    io:format("."),
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
            do_cb_resource_exists_comment(Lines, Positions, Formatted ++ [<<"%%">>, <<"%% For example:">>, <<"%%">>, <<"%% ```">>, <<"%%    ", Rest/binary, ".">>]);
        _ ->
            case Lines of
                [{_, <<"%% @end">>}|_] ->
                    do_cb_resource_exists_comment(Lines, Positions, Formatted ++ [Line, <<"%% '''">>]);
                _ ->
                    do_cb_resource_exists_comment(Lines, Positions, Formatted ++ [Line])
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Fix comment block before `start_link` which doesn't end properly with
%% with `@end' tag.
%% @end
%%--------------------------------------------------------------------
comment_blocks_with_no_end(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun comment_blocks_with_no_end/2, Positions),
    'ok'.

comment_blocks_with_no_end(File, Positions) ->
    io:format("."),
    Lines = read_lines(File, true),
    save_lines(File, do_comment_blocks_with_no_end(Lines, Positions, [])).

do_comment_blocks_with_no_end([], _, Formatted) ->
    Formatted;
do_comment_blocks_with_no_end([{LN, Line}|Lines], Positions, Formatted) ->
    case lists:member(LN, Positions)
        andalso is_seprator(Line)
    of
        false ->
            do_comment_blocks_with_no_end(Lines, Positions, Formatted ++ [Line]);
        true ->
            do_comment_blocks_with_no_end(Lines, Positions, Formatted ++ [<<"%% @end">>, Line])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Move first comment line to the same line as @doc.
%% @end
%%--------------------------------------------------------------------
move_in_doc_line(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun move_in_doc_line/2, Positions),
    'ok'.

move_in_doc_line(File, Positions) ->
    io:format("."),
    Lines = read_lines(File, true),
    save_lines(File, do_move_in_doc_line(Lines, Positions, [])).

do_move_in_doc_line([], _, Formatted) ->
    Formatted;
do_move_in_doc_line([{LN, Line}|Lines], Positions, Formatted) ->
    case lists:member(LN, Positions)
        andalso Line
    of
        false ->
            do_move_in_doc_line(Lines, Positions, Formatted ++ [Line]);
        <<"%% @doc">> ->
            case Lines of
                [{_, Comment}|T] ->
                    do_move_in_doc_line(T, Positions, Formatted ++ [<<Line/binary, " ", (strip_left_space(strip_comment(Comment)))/binary>>]);
                _ ->
                    do_move_in_doc_line(Lines, Positions, Formatted ++ [Line])
            end;
        <<"%% @doc", _/binary>> ->
            do_move_in_doc_line(Lines, Positions, Formatted ++ [Line]);
        <<"%%% @doc">> ->
            case Lines of
                [{_, Comment}|T] ->
                    do_move_in_doc_line(T, Positions, Formatted ++ [<<Line/binary, " ", (strip_left_space(strip_comment(Comment)))/binary>>]);
                _ ->
                    do_move_in_doc_line(Lines, Positions, Formatted ++ [Line])
            end;
        <<"%%% @doc", _/binary>> ->
            do_move_in_doc_line(Lines, Positions, Formatted ++ [Line])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Remove empty comment lines after `@doc' to avoid empty paragraph
%% or dot in summary. Regex only returns the line with `@doc' and
%% empty comment line.
%% @end
%%--------------------------------------------------------------------
remove_doc_tag_empty_comment(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun remove_doc_tag_empty_comment/2, Positions),
    'ok'.

remove_doc_tag_empty_comment(File, Positions) ->
    io:format("."),
    Lines = read_lines(File, true),
    save_lines(File, do_remove_doc_tag_empty_comment(Lines, Positions, [])).

do_remove_doc_tag_empty_comment([], _, Formatted) ->
    Formatted;
do_remove_doc_tag_empty_comment([{LN, Line}|Lines], Positions, Formatted) ->
    case lists:member(LN, Positions)
        andalso Line
    of
        false ->
            do_remove_doc_tag_empty_comment(Lines, Positions, Formatted ++ [Line]);
        <<"%%">> ->
            %% remove empty comment line
            do_remove_doc_tag_empty_comment(Lines, Positions, Formatted);
        <<"%%%">> ->
            %% remove empty comment line
            do_remove_doc_tag_empty_comment(Lines, Positions, Formatted);
        _ ->
            do_remove_doc_tag_empty_comment(Lines, Positions, Formatted ++ [Line])
    end.

%%%===================================================================
%%% Utilities
%%%===================================================================

maybe_add_empty_line(<<>>) -> [];
maybe_add_empty_line(_) -> [<<>>].

maybe_add_empty_line_before_spec(<<>>) -> [];
%% don't add empty line if it's a comment block, maybe it has some corner case,
%% e.g. the separator belongs to function comment block or not. But most of the
%% time we're good to go this way.
maybe_add_empty_line_before_spec(<<"%%--", _/binary>>) -> [];
maybe_add_empty_line_before_spec(<<"%% --", _/binary>>) -> [];
maybe_add_empty_line_before_spec(_) -> [<<>>].

is_seprator(<<"%%--", _/binary>>) -> true;
is_seprator(<<"%%==", _/binary>>) -> true;
is_seprator(<<"%% --", _/binary>>) -> true;
is_seprator(<<"%% ==", _/binary>>) -> true;
is_seprator(_) -> false.

collect_positions_per_file([], Map) -> Map;
collect_positions_per_file([Line | Lines], Map) ->
    {File, Pos, _, _} = parse_ag_line(Line),
    collect_positions_per_file(Lines, Map#{File => maps:get(File, Map, []) ++ [Pos]}).

parse_ag_line(<<"ERR:", _/binary>>=Error) ->
    io:put_chars(Error),
    halt(1);
parse_ag_line(Bin) ->
    try explode_line(Bin)
    catch _E:_T ->
            io:format("~nfailed to parse file, position in ag response, ~p:~p, line:~n~s~n", [_E, _T, Bin]),
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

strip_left_space(<<$\s, B/binary>>) -> strip_left_space(B);
strip_left_space(A) -> A.

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
            throw({error, File, Reason})
    end.

save_lines(File, Lines) ->
    Data = check_final_newline(lists:reverse([<<L/binary, "\n">> || L <- Lines])),
    case file:write_file(File, Data) of
        ok -> ok;
        {error, Reason} ->
            throw({error, File, Reason})
    end.

check_final_newline([<<"\n">>|Tser]) ->
    check_final_newline(Tser);
check_final_newline(Senil) ->
    lists:reverse(Senil).
