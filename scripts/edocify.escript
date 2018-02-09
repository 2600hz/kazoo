#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode(compile).

-export([main/1]).

-define(SEP, <<"%%%-------------------------------------------------------------------">>).
-define(SEP_2, <<"%%%===================================================================">>).
-define(HAS_CONTRIBUTORS_REGEX, "ag -G '(erl)$' -l '%%%+\\s*@?[Cc]ontributors'").
-define(COMMENT_SPEC, "ag '^%%+\\s*@spec((.*$\\n)?(.*\\)$|.*}$|.*\\|(\\n%%+(.*\\)$|.*}$|.*\\||[^@=-]+$))+)|.*$)'").
-define(SEP_SPEC, "ag -G '(erl)$' '%%%*\s*==+\n+[a-z-]+' applications/ core/").

main(_) ->
    _ = io:setopts(user, [{encoding, unicode}]),
    check_ag_available(),
    ScriptsDir = filename:dirname(escript:script_name()),
    ok = file:set_cwd(filename:absname(ScriptsDir ++ "/..")),

    io:format("Edocify Kazoo...~n~n"),

    Run = [{?HAS_CONTRIBUTORS_REGEX, "rename and fix '@contributors' tags to '@author'", fun edocify_headers/1}
          ,{?COMMENT_SPEC, "removing @spec from comments", fun remove_comment_specs/1}
          ,{?SEP_SPEC, "adding missing comments block after separator", fun missing_comment_blocks_after_sep/1}
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
    io:format("~nEDocified! 🎉~n");
edocify([], Ret) ->
    halt(Ret);
edocify([{Cmd, Desc, Fun}|Rest], Ret) ->
    io:format(":: ~s ", [Desc]),
    case check_result(list_to_binary(run_ag(Cmd))) of
        ok -> edocify(Rest, Ret);
        AgResult ->
            _ = Fun(AgResult),
            edocify(Rest, 1)
    end.

check_result(<<>>) ->
    io:format("done.~n");
check_result(<<"ERR:", _/binary>>=Error) ->
    io:put_chars(Error),
    halt(1);
check_result(Result) ->
    Result.

%%%===================================================================
%%% Edocify Header
%%%===================================================================

edocify_headers(Result) ->
    Files = [F || F <- binary:split(Result, <<"\n">>, [global]), F =/= <<>>],
    io:format("~b file(s)~n", [length(Files)]),
    _ = [edocify_header(F) || F <- Files],
    io:format("done.~n").

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
edocify_header([<<"Contributors", _/binary>>], Header) ->
    edocify_header([<<"@contributors">>], Header);
edocify_header([<<"Contributors", _/binary>>|T], Header) ->
    edocify_header([<<"@contributors">>|T], Header);
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

is_seprator(<<"%%--", _/binary>>) -> true;
is_seprator(<<"%%==", _/binary>>) -> true;
is_seprator(<<"%% --", _/binary>>) -> true;
is_seprator(<<"%% ==", _/binary>>) -> true;
is_seprator(_) -> false.

look_after([<<"%%%", _/binary>>|_]) -> true;
look_after([<<"%%%%", _/binary>>|_]) -> true;
look_after(_) -> false.

%%%===================================================================
%%% Edocify Header
%%%===================================================================

remove_comment_specs(Result) ->
    CommentSpecs = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun do_remove_comment_specs/2, CommentSpecs),
    io:format(" done.~n").

do_remove_comment_specs(File, Positions) ->
    Lines = read_lines(File, true),
    save_lines(File, [L || {LN, L} <- Lines, not lists:member(LN, Positions)]).

%%%===================================================================
%%% Missing comment block after separator
%%%===================================================================

missing_comment_blocks_after_sep(Result) ->
    Positions = collect_positions_per_file([Line || Line <- binary:split(Result, <<"\n">>, [global]), Line =/= <<>>], #{}),
    _ = maps:map(fun add_missing_comment_blocks/2, Positions),
    io:format("done.~n").

add_missing_comment_blocks(File, Positions) ->
    Lines = read_lines(File, true),
    save_lines(File, do_add_missing_comment_blocks(Lines, Positions, [])).

do_add_missing_comment_blocks([], _, Formatted) ->
    Formatted;
do_add_missing_comment_blocks([{LN, Line}|Lines], Positions, Formatted) ->
    lists:member(LN, Positions)
        andalso io:format("~nline ~p~n", [Line]),
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

%%%===================================================================
%%% Utilities
%%%===================================================================

maybe_add_empty_line(<<>>) -> [];
maybe_add_empty_line(_) -> [<<>>].

collect_positions_per_file([], Map) -> Map;
collect_positions_per_file([Line | Lines], Map) ->
    io:format("."),
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
