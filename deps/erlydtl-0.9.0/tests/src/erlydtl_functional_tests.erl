%%%-------------------------------------------------------------------
%%% File:      erlydtl_tests.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc       ErlyDTL test suite
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2008-02-11 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlydtl_functional_tests).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').


%% API
-export([run_tests/0, run_test/1]).

test_list() ->
    %% order is important.
    ["autoescape", "comment", "extends", "filters", "for", "for_list",
     "for_tuple", "for_list_preset", "for_preset", "for_records",
     "for_records_preset", "include", "if", "if_preset", "ifequal",
     "ifequal_preset", "ifnotequal", "ifnotequal_preset", "now",
     "var", "var_preset", "cycle", "custom_tag", "custom_tag1",
     "custom_tag2", "custom_tag3", "custom_tag4", "custom_call",
     "include_template", "include_path", "ssi", "extends_path",
     "extends_path2", "trans", "extends2", "extends3",
     "recursive_block", "extend_recursive_block"
    ].

setup_compile("for_list_preset") ->
    CompileVars = [{fruit_list, [["apple", "apples"], ["banana", "bananas"], ["coconut", "coconuts"]]}],
    {ok, CompileVars};
setup_compile("for_preset") ->
    CompileVars = [{fruit_list, ["preset-apple", "preset-banana", "preset-coconut"]}],
    {ok, CompileVars};
setup_compile("for_records_preset") ->
    Link1a = [{name, "Amazon (preset)"}, {url, "http://amazon.com"}],
    Link2a = [{name, "Google (preset)"}, {url, "http://google.com"}],
    Link3a = [{name, "Microsoft (preset)"}, {url, "http://microsoft.com"}],
    CompileVars = [{software_links, [Link1a, Link2a, Link3a]}],
    {ok, CompileVars};
setup_compile("if_preset") ->
    CompileVars = [{var1, "something"}],
    {ok, CompileVars};
setup_compile("ifequal_preset") ->
    CompileVars = [{var1, "foo"}, {var2, "foo"}],
    {ok, CompileVars};
setup_compile("ifnotequal_preset") ->
    CompileVars = [{var1, "foo"}, {var2, "foo"}],
    {ok, CompileVars};
setup_compile("var_preset") ->
    CompileVars = [{preset_var1, "preset-var1"}, {preset_var2, "preset-var2"}],
    {ok, CompileVars};
setup_compile("extends2") ->
    File = templates_dir("input/extends2"),
    Error = {none, erlydtl_compiler, unexpected_extends_tag},
    {{error, [{File, [Error]}], []}, []};
setup_compile("extends3") ->
    File = templates_dir("input/extends3"),
    Include = templates_dir("input/imaginary"),
    Error = {none, erlydtl_compiler, {read_file, Include, enoent}},
    {{error, [{File, [Error]}], []}, []};
setup_compile(_) ->
    {ok, []}.

%% @spec (Name::string()) -> {CompileStatus::atom(), PresetVars::list(),
%%     RenderStatus::atom(), RenderVars::list()} | skip
%% @doc
%% @end
%%--------------------------------------------------------------------
setup("autoescape") ->
    RenderVars = [{var1, "<b>bold</b>"}],
    {ok, RenderVars};
setup("extends") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("filters") ->
    RenderVars = [
                  {date_var1, {1975,7,24}},
                  {datetime_var1, {{1975,7,24}, {7,13,1}}},
                  {'list', ["eins", "zwei", "drei"]}
                 ],
    {ok, RenderVars};
setup("for") ->
    RenderVars = [{fruit_list, ["apple", "banana", "coconut"]}],
    {ok, RenderVars};
setup("for_list") ->
    RenderVars = [{fruit_list, [["apple", "apples", "$1"], ["banana", "bananas", "$2"], ["coconut", "coconuts", "$500"]]}],
    {ok, RenderVars};
setup("for_tuple") ->
    RenderVars = [{fruit_list, [{"apple", "apples"}, {"banana", "bananas"}, {"coconut", "coconuts"}]}],
    {ok, RenderVars};
setup("for_records") ->
    Link1 = [{name, "Amazon"}, {url, "http://amazon.com"}],
    Link2 = [{name, "Google"}, {url, "http://google.com"}],
    Link3 = [{name, "Microsoft"}, {url, "http://microsoft.com"}],
    RenderVars = [{link_list, [Link1, Link2, Link3]}],
    {ok, RenderVars};
setup("for_records_preset") ->
    Link1b = [{name, "Canon"}, {url, "http://canon.com"}],
    Link2b = [{name, "Leica"}, {url, "http://leica.com"}],
    Link3b = [{name, "Nikon"}, {url, "http://nikon.com"}],
    RenderVars = [{photo_links, [Link1b, Link2b, Link3b]}],
    {ok, RenderVars};
setup("include") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, RenderVars};
setup("if") ->
    RenderVars = [{var1, "something"}],
    {ok, RenderVars};
setup("ifequal") ->
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, RenderVars};
setup("ifequal_preset") ->
    RenderVars = [{var3, "bar"}],
    {ok, RenderVars};
setup("ifnotequal") ->
    RenderVars = [{var1, "foo"}, {var2, "foo"}, {var3, "bar"}],
    {ok, RenderVars};
setup("now") ->
    {ok, [], [], skip_check};
setup("var") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}, {var_not_used, "foostring3"}],
    {ok, RenderVars};
setup("var_preset") ->
    RenderVars = [{var1, "foostring1"}, {var2, "foostring2"}],
    {ok, RenderVars};
setup("cycle") ->
    RenderVars = [{test, [integer_to_list(X) || X <- lists:seq(1, 20)]},
                  {a, "Apple"}, {b, "Banana"}, {c, "Cherry"}],
    {ok, RenderVars};
setup("include_template") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("include_path") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("extends_path") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("extends_path2") ->
    RenderVars = [{base_var, "base-barstring"}, {test_var, "test-barstring"}],
    {ok, RenderVars};
setup("trans") ->
    RenderVars = [{locale, "reverse"}],
    {ok, RenderVars};
setup("locale") ->
    {ok, _RenderVars = [{locale, "ru"}]};
setup("custom_tag1") ->
    {ok, [{a, <<"a1">>}], [{locale, ru}], <<"b1\n">>};
setup("custom_tag2") ->
    {ok, [{a, <<"a1">>}], [{locale, ru}, {foo, bar}], <<"b2\n">>};
setup("custom_tag3") ->
    {ok, [{a, <<"a1">>}], [{locale, ru}], <<"b3\n">>};
setup("custom_tag4") ->
    {ok, [], [], <<"a\n">>};
setup("ssi") ->
    RenderVars = [{path, filename:absname(filename:join(["tests", "input", "ssi_include.html"]))}],
    {ok, RenderVars};


%%--------------------------------------------------------------------
%% Custom tags
%%--------------------------------------------------------------------
setup("custom_call") ->
    RenderVars = [{var1, "something"}],
    {ok, RenderVars};

setup(_) ->
    {ok, []}.


run_tests() ->
    io:format("Running functional tests...~n"),
    file:set_cwd(erlydtl_deps:get_base_dir()),
    case [filelib:ensure_dir(
            filename:join([templates_dir(Dir), "foo"]))

          || Dir <- ["output", "expect"]] -- [ok,ok]
    of
        [] ->
            case fold_tests() of
                {N, []}->
                    Msg = lists:concat(["All ", N, " functional tests passed~n~n"]),
                    io:format(Msg),
                    {ok, Msg};
                {N, Errs} ->
                    io:format(
                      "~b / ~b functional tests failed.~nErrors: ~n",
                      [length(Errs), N]),
                    [io:format("  ~s [~s] ~s~n", [Name, Error, Reason])
                     || {Name, Error, Reason} <- Errs],
                    throw(failed)
            end;
        Err ->
            [io:format("Ensure dir failed: ~p~n~n", [Reason]) || {error, Reason} <- Err],
            throw(failed)
    end.


run_test(Name) ->
    test_compile_render(Name).


%%====================================================================
%% Internal functions
%%====================================================================

fold_tests() ->
    lists:foldl(fun(Name, {AccCount, AccErrs}) ->
                        Res = case catch test_compile_render(Name) of
                                  ok -> {AccCount + 1, AccErrs};
                                  {'EXIT', Reason} ->
                                      {AccCount + 1, [{Name, crash,
                                                       io_lib:format("~p", [Reason])}
                                                      | AccErrs]};
                                  {Error, Reason} ->
                                      {AccCount + 1, [{Name, Error, Reason}
                                                      | AccErrs]}
                              end,
                        io:format("~n"), Res
                end, {0, []}, test_list()).

test_compile_render(Name) ->
    File = filename:join([templates_docroot(), Name]),
    Module = "functional_test_" ++ Name,
    io:format(" Template: ~p, ... ", [Name]),
    case setup_compile(Name) of
        {CompileStatus, CompileVars} ->
            Options = [
                       {vars, CompileVars},
                       force_recompile,
                       return_errors,
                       return_warnings,
                       %% debug_compiler,
                       {custom_tags_modules, [erlydtl_custom_tags]}],
            io:format("compiling ... "),
            case erlydtl:compile(File, Module, Options) of
                {ok, Mod, [{File, [{none,erlydtl_compiler,no_out_dir}]}]} ->
                    if CompileStatus =:= ok -> test_render(Name, Mod);
                       true ->
                            io:format("missing error"),
                            {error, "compiling should have failed :" ++ File}
                    end;
                {error, _, _}=CompileStatus ->
                    io:format("ok");
                Err ->
                    io:format("failed"),
                    {compile_error, io_lib:format("Actual: ~p, Expected: ~p", [Err, CompileStatus])}
            end;
        skip -> io:format("skipped")
    end.

test_render(Name, Module) ->
    File = filename:join([templates_docroot(), Name]),
    {RenderStatus, Vars, Opts, RenderResult} =
        case setup(Name) of
            {RS, V}       -> {RS, V, [], get_expected_result(Name)};
            {RS, V, O}    -> {RS, V, O, get_expected_result(Name)};
            {RS, V, O, R} -> {RS, V, O, R}
        end,
    io:format("rendering ... "),
    case catch Module:render(Vars, Opts) of
        {ok, Output} ->
            Data = iolist_to_binary(Output),
            if RenderStatus =:= ok ->
                    if RenderResult =:= undefined ->
                            [with_template_filename(
                               Dir, Name,
                               fun(F) -> file:write_file(F, Data) end)
                             || Dir <- ["output", "expect"]],
                            io:format("~n    #### NOTE: created new expected output file: \"tests/expect/~s\"."
                                      "~n    Please verify contents.", [Name]);
                       RenderResult =:= Data ->
                            io:format("ok");
                       RenderResult =:= skip_check ->
                            io:format("ok (not checked for regression)");
                       true ->
                            io:format("failed"),
                            with_template_filename(
                              "output", Name,
                              fun(F) -> file:write_file(F, Data) end),
                            {error, io_lib:format(
                                      "Expected output does not match rendered output~n"
                                      "==Expected==~n~p~n--Actual--~n~p~n==End==~n",
                                      [RenderResult, Data])}
                    end;
               true ->
                    io:format("missing error"),
                    {missing_error, "rendering should have failed :" ++ File}
            end;
        {'EXIT', Reason} ->
            io:format("failed"),
            {render_error, io_lib:format("failed invoking render method of ~p ~p", [Module, Reason])};
        Err ->
            if RenderStatus =:= error -> io:format("ok");
               true -> io:format("failed"),
                       {render_error, io_lib:format("~p", [Err])}
            end
    end.

get_expected_result(Name) ->
    with_template_filename(
      "expect", Name,
      fun(F) ->
              case filelib:is_regular(F) of
                  true -> {ok, Data} = file:read_file(F), Data;
                  false -> undefined
              end
      end).

with_template_filename(Dir, Name, Fun) ->
    FileName = filename:join([templates_dir(Dir), Name]),
    Fun(FileName).

templates_docroot() -> templates_dir("input").
templates_dir(Name) -> filename:join(["tests", Name]).
