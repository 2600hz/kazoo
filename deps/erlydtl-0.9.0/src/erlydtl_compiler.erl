%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @copyright 2014 Andreas Stenius
%%% @doc
%%% ErlyDTL template compiler
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%% Copyright (c) 2014 Andreas Stenius
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
%%% @since 2007-12-16 by Roberto Saccon, Evan Miller
%%% @since 2014 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(erlydtl_compiler).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('Andreas Stenius <kaos@astekk.se>').

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

-export([compile_file/3, compile_template/3, compile_dir/3,
         format_error/1, default_options/0]).

%% internal use
-export([
         parse/1,
         merge_info/2,
         format/3,
         value_ast/5,
         resolve_scoped_variable_ast/2,
         resolve_scoped_variable_ast/3,
         interpret_args/3,
         unescape_string_literal/1
        ]).

-include("erlydtl_ext.hrl").

default_options() -> [verbose, report].

compile_template(Template, Module, Options) ->
    Context = process_opts(undefined, Module, Options),
    compile(Context#dtl_context{ bin = Template }).

compile_file(File, Module, Options) ->
    Context = process_opts(File, Module, Options),
    print("Compile template: ~s~n", [File], Context),
    compile(Context).

compile_dir(Dir, Module, Options) ->
    Context0 = process_opts({dir, Dir}, Module, Options),
    %% Find all files in Dir (recursively), matching the regex (no
    %% files ending in "~").
    Files = filelib:fold_files(Dir, ".+[^~]$", true, fun(F1,Acc1) -> [F1 | Acc1] end, []),
    {ParserResults,
     #dtl_context{ errors=#error_info{ list=ParserErrors } }=Context1}
        = lists:foldl(
            fun (File, {ResultAcc, Ctx}) ->
                    case filename:basename(File) of
                        "."++_ ->
                            {ResultAcc, Ctx};
                        _ ->
                            FilePath = filename:absname(File),
                            case filelib:is_dir(FilePath) of
                                true ->
                                    {ResultAcc, Ctx};
                                false ->
                                    case parse_file(FilePath, Ctx) of
                                        up_to_date -> {ResultAcc, Ctx};
                                        {ok, DjangoParseTree, CheckSum} ->
                                            {[{File, DjangoParseTree, CheckSum}|ResultAcc], Ctx};
                                        {error, Reason} -> {ResultAcc, add_error(Reason, Ctx)}
                                    end
                            end
                    end
            end,
            {[], Context0},
            Files),
    Context2 = if length(ParserErrors) == 0 ->
                       compile_multiple_to_binary(Dir, ParserResults, Context1);
                  true -> Context1
               end,
    collect_result(Context2).

parse(Data) ->
    parse_template(Data, #dtl_context{}).

format_error(no_out_dir) ->
    "Compiled template not saved (need out_dir option)";
format_error(unexpected_extends_tag) ->
    "The extends tag must be at the very top of the template";
format_error(circular_include) ->
    "Circular file inclusion!";
format_error({read_file, Error}) ->
    io_lib:format(
      "Failed to read file: ~s",
      [file:format_error(Error)]);
format_error({read_file, File, Error}) ->
    io_lib:format(
      "Failed to include file ~s: ~s",
      [File, file:format_error(Error)]);
format_error({write_file, Error}) ->
    io_lib:format(
      "Failed to write file: ~s",
      [file:format_error(Error)]);
format_error(compile_beam) ->
    "Failed to compile template to .beam file";
format_error(Other) ->
    io_lib:format("## Error description for ~p not implemented.", [Other]).


%%====================================================================
%% Internal functions
%%====================================================================

process_opts(File, Module, Options0) ->
    Options1 = proplists:normalize(
                 update_defaults(Options0),
                 [{aliases, [{outdir, out_dir}]}
                 ]),
    Source0 = filename:absname(
                case File of
                    undefined ->
                        filename:join(
                          [case proplists:get_value(out_dir, Options1, false) of
                               false -> ".";
                               OutDir -> OutDir
                           end,
                           Module]);
                    {dir, Dir} ->
                        Dir;
                    _ ->
                        File
                end),
    Source = shorten_filename(Source0),
    Options = [{compiler_options, [{source, Source}]}
               |compiler_opts(Options1, [])],
    case File of
        {dir, _} ->
            init_context([], Source, Module, Options);
        _ ->
            init_context([Source], filename:dirname(Source), Module, Options)
    end.

compiler_opts([CompilerOption|Os], Acc)
  when
      CompilerOption =:= return;
      CompilerOption =:= return_warnings;
      CompilerOption =:= return_errors;
      CompilerOption =:= report;
      CompilerOption =:= report_warnings;
      CompilerOption =:= report_errors;
      CompilerOption =:= warnings_as_errors;
      CompilerOption =:= verbose;
      CompilerOption =:= debug_info ->
    compiler_opts(Os, [CompilerOption, {compiler_options, [CompilerOption]}|Acc]);
compiler_opts([O|Os], Acc) ->
    compiler_opts(Os, [O|Acc]);
compiler_opts([], Acc) ->
    lists:reverse(Acc).

update_defaults(Options) ->
    maybe_add_env_default_opts(Options).

maybe_add_env_default_opts(Options) ->
    case proplists:get_bool(no_env, Options) of
        true -> Options;
        _ -> Options ++ env_default_opts()
    end.

%% shamelessly borrowed from:
%% https://github.com/erlang/otp/blob/21095e6830f37676dd29c33a590851ba2c76499b/\
%% lib/compiler/src/compile.erl#L128
env_default_opts() ->
    Key = "ERLYDTL_COMPILER_OPTIONS",
    case os:getenv(Key) of
        false -> [];
        Str when is_list(Str) ->
            case erl_scan:string(Str) of
                {ok,Tokens,_} ->
                    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
                        {ok,List} when is_list(List) -> List;
                        {ok,Term} -> [Term];
                        {error,_Reason} ->
                            io:format("Ignoring bad term in ~s\n", [Key]),
                            []
                    end;
                {error, {_,_,_Reason}, _} ->
                    io:format("Ignoring bad term in ~s\n", [Key]),
                    []
            end
    end.

%% shorten_filename/1 copied from Erlang/OTP lib/compiler/src/compile.erl
shorten_filename(Name0) ->
    {ok,Cwd} = file:get_cwd(),
    case lists:prefix(Cwd, Name0) of
	false -> Name0;
	true ->
	    case lists:nthtail(length(Cwd), Name0) of
		"/"++N -> N;
		N -> N
	    end
    end.

compile(Context) ->
    Context1 = do_compile(Context),
    collect_result(Context1).

collect_result(#dtl_context{
                  module=Module, 
                  errors=#error_info{ list=[] },
                  warnings=Ws }=Context) ->
    Info = case Ws of
               #error_info{ return=true, list=Warnings } ->
                   [pack_error_list(Warnings)];
               _ ->
                   []
           end,
    Res = case proplists:get_bool(binary, Context#dtl_context.all_options) of
              true ->
                  [ok, Module, Context#dtl_context.bin | Info];
              false ->
                  [ok, Module | Info]
          end,
    list_to_tuple(Res);
collect_result(#dtl_context{ errors=Es, warnings=Ws }) ->
    if Es#error_info.return ->
            {error,
             pack_error_list(Es#error_info.list),
             case Ws of
                 #error_info{ list=L } ->
                     pack_error_list(L);
                 _ ->
                     []
             end};
       true -> error
    end.

do_compile(#dtl_context{ bin=undefined, parse_trail=[File|_] }=Context) ->
    {M, F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} when is_binary(Data) ->
            do_compile(Context#dtl_context{ bin=Data });
        {error, Reason} ->
            add_error({read_file, Reason}, Context)
    end;
do_compile(#dtl_context{ bin=Template }=Context) ->
    case parse_template(Template, Context) of
        up_to_date -> Context;
        {ok, DjangoParseTree, CheckSum} ->
            compile_to_binary(DjangoParseTree, CheckSum, Context);
        {error, Reason} -> add_error(Reason, Context)
    end.

compile_multiple_to_binary(Dir, ParserResults, Context0) ->
    MatchAst = options_match_ast(Context0),
    {Functions,
     {AstInfo, _,
      #dtl_context{ errors=#error_info{ list=Errors } }=Context1}}
        = lists:mapfoldl(
            fun({File, DjangoParseTree, CheckSum}, {AstInfo, TreeWalker, Ctx}) ->
                    try
                        FilePath = full_path(File, Ctx#dtl_context.doc_root),
                        {{BodyAst, BodyInfo}, TreeWalker1} = with_dependency(
                                                               {FilePath, CheckSum},
                                                               body_ast(DjangoParseTree, Ctx, TreeWalker)),
                        FunctionName = filename:rootname(filename:basename(File)),
                        Function1 = erl_syntax:function(
                                      erl_syntax:atom(FunctionName),
                                      [erl_syntax:clause(
                                         [erl_syntax:variable("_Variables")],
                                         none,
                                         [erl_syntax:application(
                                            none, erl_syntax:atom(FunctionName),
                                            [erl_syntax:variable("_Variables"), erl_syntax:list([])])
                                         ])
                                      ]),
                        Function2 = erl_syntax:function(
                                      erl_syntax:atom(FunctionName),
                                      [erl_syntax:clause(
                                         [erl_syntax:variable("_Variables"),
                                          erl_syntax:variable("RenderOptions")],
                                         none,
                                         MatchAst ++ stringify(BodyAst, Ctx))
                                      ]),
                        {{FunctionName, Function1, Function2}, {merge_info(AstInfo, BodyInfo), TreeWalker1, Ctx}}
                    catch
                        throw:Error ->
                            {error, {AstInfo, TreeWalker, add_error(Error, Ctx)}}
                    end
            end,
            {#ast_info{},
             init_treewalker(Context0),
             Context0},
            ParserResults),
    if length(Errors) == 0 ->
            Forms = custom_forms(Dir, Context1#dtl_context.module, Functions, AstInfo),
            compile_forms(Forms, Context1);
       true ->
            Context1
    end.

compile_to_binary(DjangoParseTree, CheckSum, Context) ->
    try body_ast(DjangoParseTree, Context, init_treewalker(Context)) of
        {{BodyAst, BodyInfo}, BodyTreeWalker} ->
            try custom_tags_ast(BodyInfo#ast_info.custom_tags, Context, BodyTreeWalker) of
                {{CustomTagsAst, CustomTagsInfo}, _} ->
                    Forms = forms(
                              Context#dtl_context.module,
                              {BodyAst, BodyInfo},
                              {CustomTagsAst, CustomTagsInfo},
                              CheckSum,
                              BodyTreeWalker,
                              Context),
                    compile_forms(Forms, Context)
            catch
                throw:Error -> add_error(Error, Context)
            end
    catch
        throw:Error -> add_error(Error, Context)
    end.

compile_forms(Forms, Context) ->
    maybe_debug_template(Forms, Context),
    Options = Context#dtl_context.compiler_options,
    case compile:forms(Forms, Options) of
        Compiled when element(1, Compiled) =:= ok ->
            [ok, Module, Bin|Info] = tuple_to_list(Compiled),
            lists:foldl(
              fun (F, C) -> F(Module, Bin, C) end,
              Context#dtl_context{ bin=Bin },
              [fun maybe_write/3,
               fun maybe_load/3,
               fun (_, _, C) ->
                       case Info of
                           [Ws] when length(Ws) > 0 ->
                               add_warnings(Ws, C);
                           _ -> C
                       end
               end
              ]);
        error ->
            add_error(compile_beam, Context);
        {error, Es, Ws} ->
            add_warnings(Ws, add_errors(Es, Context))
    end.

maybe_write(Module, Bin, Context) ->
    case proplists:get_value(out_dir, Context#dtl_context.all_options) of
        false -> Context;
        undefined ->
            add_warning(no_out_dir, Context);
        OutDir ->
            BeamFile = filename:join([OutDir, [Module, ".beam"]]),
            print("Template module: ~w -> ~s\n", [Module, BeamFile], Context),
            case file:write_file(BeamFile, Bin) of
                ok -> Context;
                {error, Reason} ->
                    add_error({write_file, Reason}, Context)
            end
    end.

maybe_load(Module, Bin, Context) ->
    case proplists:get_bool(no_load, Context#dtl_context.all_options) of
        true -> Context;
        false -> load_code(Module, Bin, Context)
    end.

load_code(Module, Bin, Context) ->
    code:purge(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
        {module, Module} -> Context;
        Error -> add_warning({load, Error}, Context)
    end.

maybe_debug_template(Forms, Context) ->
    %% undocumented option to debug the compiled template
    case proplists:get_bool(debug_info, Context#dtl_context.all_options) of
        false -> nop;
        true ->
            Options = Context#dtl_context.compiler_options,
            print("Compiler options: ~p~n", [Options], Context),
            try
                Source = erl_prettypr:format(erl_syntax:form_list(Forms)),
                File = lists:concat([proplists:get_value(source, Options), ".erl"]),
                io:format("Saving template source to: ~s.. ~p~n",
                          [File, file:write_file(File, Source)])
            catch
                error:Err ->
                    io:format("Pretty printing failed: ~p~n"
                              "Context: ~n~p~n"
                              "Forms: ~n~p~n",
                              [Err, Context, Forms])
            end
    end.

init_context(ParseTrail, DefDir, Module, Options) when is_list(Module) ->
    init_context(ParseTrail, DefDir, list_to_atom(Module), Options);
init_context(ParseTrail, DefDir, Module, Options) ->
    Ctx = #dtl_context{},
    Locale = proplists:get_value(locale, Options),
    BlocktransLocales = proplists:get_value(blocktrans_locales, Options),
    TransLocales = case {Locale, BlocktransLocales} of
                       {undefined, undefined} -> Ctx#dtl_context.trans_locales;
                       {undefined, Val} -> Val;
                       {Val, undefined} -> [Val];
                       _ -> lists:usort([Locale | BlocktransLocales])
                   end,
    Context = #dtl_context{
                 all_options = Options,
                 auto_escape = case proplists:get_value(auto_escape, Options, true) of
                                   true -> on;
                                   _ -> off
                               end,
                 parse_trail = ParseTrail,
                 module = Module,
                 doc_root = proplists:get_value(doc_root, Options, DefDir),
                 filter_modules = proplists:get_value(
                                    custom_filters_modules, Options,
                                    Ctx#dtl_context.filter_modules) ++ [erlydtl_filters],
                 custom_tags_dir = proplists:get_value(
                                     custom_tags_dir, Options,
                                     filename:join([erlydtl_deps:get_base_dir(), "priv", "custom_tags"])),
                 custom_tags_modules = proplists:get_value(custom_tags_modules, Options, Ctx#dtl_context.custom_tags_modules),
                 trans_fun = proplists:get_value(blocktrans_fun, Options, Ctx#dtl_context.trans_fun),
                 trans_locales = TransLocales,
                 vars = proplists:get_value(vars, Options, Ctx#dtl_context.vars),
                 reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
                 compiler_options = proplists:append_values(compiler_options, Options),
                 binary_strings = proplists:get_value(binary_strings, Options, Ctx#dtl_context.binary_strings),
                 force_recompile = proplists:get_bool(force_recompile, Options),
                 verbose = proplists:get_value(verbose, Options, Ctx#dtl_context.verbose),
                 is_compiling_dir = ParseTrail == [],
                 extension_module = proplists:get_value(extension_module, Options, Ctx#dtl_context.extension_module),
                 scanner_module = proplists:get_value(scanner_module, Options, Ctx#dtl_context.scanner_module),
                 record_info = [{R, lists:zip(I, lists:seq(2, length(I) + 1))}
                                || {R, I} <- proplists:get_value(record_info, Options, Ctx#dtl_context.record_info)],
                 errors = init_error_info(errors, Ctx#dtl_context.errors, Options),
                 warnings = init_error_info(warnings, Ctx#dtl_context.warnings, Options)
                },
    case call_extension(Context, init_context, [Context]) of
        {ok, C} when is_record(C, dtl_context) -> C;
        undefined -> Context
    end.

init_error_info(warnings, Ei, Options) ->
    case proplists:get_bool(warnings_as_errors, Options) of
        true -> warnings_as_errors;
        false ->
            init_error_info(get_error_info_opts(warnings, Options), Ei)
    end;
init_error_info(Class, Ei, Options) ->
    init_error_info(get_error_info_opts(Class, Options), Ei).

init_error_info([{return, true}|Flags], #error_info{ return = false }=Ei) ->
    init_error_info(Flags, Ei#error_info{ return = true });
init_error_info([{report, true}|Flags], #error_info{ report = false }=Ei) ->
    init_error_info(Flags, Ei#error_info{ report = true });
init_error_info([_|Flags], Ei) ->
    init_error_info(Flags, Ei);
init_error_info([], Ei) -> Ei.

get_error_info_opts(Class, Options) ->
    Flags = case Class of
                errors ->
                    [return, report, {return_errors, return}, {report_errors, report}];
                warnings ->
                    [return, report, {return_warnings, return}, {report_warnings, report}]
            end,
    [begin
         {Key, Value} = if is_atom(Flag) -> {Flag, Flag};
                           true -> Flag
                        end,
         {Value, proplists:get_bool(Key, Options)}
     end || Flag <- Flags].
    
init_treewalker(Context) ->
    TreeWalker = #treewalker{},
    case call_extension(Context, init_treewalker, [TreeWalker]) of
        {ok, TW} when is_record(TW, treewalker) -> TW;
        undefined -> TreeWalker
    end.

is_up_to_date(_, #dtl_context{force_recompile = true}) ->
    false;
is_up_to_date(CheckSum, Context) ->
    Module = Context#dtl_context.module,
    {M, F} = Context#dtl_context.reader,
    case catch Module:source() of
        {_, CheckSum} ->
            case catch Module:dependencies() of
                L when is_list(L) ->
                    RecompileList = lists:foldl(
                                      fun ({XFile, XCheckSum}, Acc) ->
                                              case catch M:F(XFile) of
                                                  {ok, Data} ->
                                                      case binary_to_list(erlang:md5(Data)) of
                                                          XCheckSum ->
                                                              Acc;
                                                          _ ->
                                                              [recompile | Acc]
                                                      end;
                                                  _ ->
                                                      [recompile | Acc]
                                              end
                                      end, [], L),
                    case RecompileList of
                        [] -> true;
                        _ -> false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.

parse_file(File, Context) ->
    {M, F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            parse_template(Data, Context);
        {error, Reason} ->
            {read_file, File, Reason}
    end.

parse_template(Data, Context) ->
    CheckSum = binary_to_list(erlang:md5(Data)),
    case is_up_to_date(CheckSum, Context) of
        true -> up_to_date;
        false ->
            case do_parse(Data, Context) of
                {ok, Val} -> {ok, Val, CheckSum};
                Err -> Err
            end
    end.

do_parse(Data, #dtl_context{ scanner_module=Scanner }=Context) ->
    check_scan(
      apply(Scanner, scan, [Data]),
      Context).

call_extension(#dtl_context{ extension_module=undefined }, _Fun, _Args) ->
    undefined;
call_extension(#dtl_context{ extension_module=Mod }, Fun, Args)
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    M = case code:is_loaded(Mod) of
            false ->
                case code:load_file(Mod) of
                    {module, Mod} ->
                        Mod;
                    _ ->
                        undefined
                end;
            _ -> Mod
        end,
    if M /= undefined ->
            case erlang:function_exported(M, Fun, length(Args)) of
                true ->
                    apply(M, Fun, Args);
                false ->
                    undefined
            end;
       true ->
            undefined
    end.

check_scan({ok, Tokens}, Context) ->
    Tokens1 = case call_extension(Context, post_scan, [Tokens]) of
                  undefined -> Tokens;
                  {ok, T} -> T
              end,
    check_parse(erlydtl_parser:parse(Tokens1), [], Context#dtl_context{ scanned_tokens=Tokens1 });
check_scan({error, Err, State}, Context) ->
    case call_extension(Context, scan, [State]) of
        undefined ->
            {error, Err};
        {ok, NewState} ->
            check_scan(apply(Context#dtl_context.scanner_module, resume, [NewState]), Context);
        ExtRes ->
            ExtRes
    end;
check_scan({error, _}=Error, _Context) ->
    Error.

check_parse({ok, _}=Ok, [], _Context) -> Ok;
check_parse({ok, Parsed}, Acc, _Context) -> {ok, Acc ++ Parsed};
check_parse({error, _}=Err, _, _Context) -> Err;
check_parse({error, Err, State}, Acc, Context) ->
    {State1, Parsed} = reset_parse_state(State, Context),
    case call_extension(Context, parse, [State1]) of
        undefined ->
            {error, Err};
        {ok, ExtParsed} ->
            {ok, Acc ++ Parsed ++ ExtParsed};
        {error, ExtErr, ExtState} ->
            case reset_parse_state(ExtState, Context) of
                {_, []} ->
                    %% todo: see if this is indeed a sensible ext error,
                    %% or if we should rather present the original Err message
                    {error, ExtErr};
                {State2, ExtParsed} ->
                    check_parse(erlydtl_parser:resume(State2), Acc ++ Parsed ++ ExtParsed, Context)
            end;
        ExtRes ->
            ExtRes
    end.

%% backtrack up to the nearest opening tag, and keep the value stack parsed ok so far
reset_parse_state([[{Tag, _, _}|_]=Ts, Tzr, _, _, Stack], Context)
  when Tag==open_tag; Tag==open_var ->
    %% reached opening tag, so the stack should be sensible here
    {[reset_token_stream(Ts, Context#dtl_context.scanned_tokens),
      Tzr, 0, [], []], lists:flatten(Stack)};
reset_parse_state([_, _, 0, [], []]=State, _Context) ->
    %% top of (empty) stack
    {State, []};
reset_parse_state([Ts, Tzr, _, [0 | []], [Parsed | []]], Context)
  when is_list(Parsed) ->
    %% top of good stack
    {[reset_token_stream(Ts, Context#dtl_context.scanned_tokens),
      Tzr, 0, [], []], Parsed};
reset_parse_state([Ts, Tzr, _, [S | Ss], [T | Stack]], Context) ->
    %% backtrack...
    reset_parse_state([[T|Ts], Tzr, S, Ss, Stack], Context).

reset_token_stream([T|_], [T|Ts]) -> [T|Ts];
reset_token_stream(Ts, [_|S]) ->
    reset_token_stream(Ts, S).
%% we should find the next token in the list of scanned tokens, or something is real fishy


custom_tags_ast(CustomTags, Context, TreeWalker) ->
    %% avoid adding the render_tag/3 fun if it isn't used,
    %% since we can't add a -compile({nowarn_unused_function, render_tag/3}).
    %% attribute due to a bug in syntax_tools.
    case custom_tags_clauses_ast(CustomTags, Context, TreeWalker) of
        skip ->
            {{erl_syntax:comment(
                ["% render_tag/3 is not used in this template."]),
              #ast_info{}},
             TreeWalker};
        {{CustomTagsClauses, CustomTagsInfo}, TreeWalker1} ->
            {{erl_syntax:function(
                erl_syntax:atom(render_tag),
                CustomTagsClauses),
              CustomTagsInfo},
             TreeWalker1}
    end.

custom_tags_clauses_ast([], _Context, _TreeWalker) -> skip;
custom_tags_clauses_ast(CustomTags, Context, TreeWalker) ->
    custom_tags_clauses_ast1(CustomTags, [], [], #ast_info{}, Context, TreeWalker).

custom_tags_clauses_ast1([], _ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker) ->
    {{DefaultAst, DefaultInfo}, TreeWalker1} =
        case call_extension(Context, custom_tag_ast, [Context, TreeWalker]) of
            undefined ->
                {{erl_syntax:clause(
                    [erl_syntax:variable("_TagName"), erl_syntax:underscore(), erl_syntax:underscore()],
                    none,
                    [erl_syntax:list([])]),
                  InfoAcc},
                 TreeWalker};
            {{ExtAst, ExtInfo}, ExtTreeWalker} ->
                Clause = erl_syntax:clause(
                           [erl_syntax:variable("TagName"), erl_syntax:variable("_Variables"), erl_syntax:variable("RenderOptions")],
                           none, options_match_ast(Context, ExtTreeWalker) ++ [ExtAst]),
                {{Clause, merge_info(ExtInfo, InfoAcc)}, ExtTreeWalker}
        end,
    {{lists:reverse([DefaultAst|ClauseAcc]), DefaultInfo}, TreeWalker1};
custom_tags_clauses_ast1([Tag|CustomTags], ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker) ->
    case lists:member(Tag, ExcludeTags) of
        true ->
            custom_tags_clauses_ast1(CustomTags, ExcludeTags, ClauseAcc, InfoAcc, Context, TreeWalker);
        false ->
            CustomTagFile = full_path(Tag, Context#dtl_context.custom_tags_dir),
            case filelib:is_file(CustomTagFile) of
                true ->
                    case parse_file(CustomTagFile, Context) of
                        {ok, DjangoParseTree, CheckSum} ->
                            {{BodyAst, BodyAstInfo}, TreeWalker1} = with_dependency(
                                                                      {CustomTagFile, CheckSum},
                                                                      body_ast(DjangoParseTree, Context, TreeWalker)),
                            MatchAst = options_match_ast(Context, TreeWalker),
                            Clause = erl_syntax:clause(
                                       [erl_syntax:atom(Tag),
                                        erl_syntax:variable("_Variables"),
                                        erl_syntax:variable("RenderOptions")],
                                       none,
                                       MatchAst ++ [BodyAst]),
                            custom_tags_clauses_ast1(
                              CustomTags, [Tag|ExcludeTags],
                              [Clause|ClauseAcc], merge_info(BodyAstInfo, InfoAcc),
                              Context, TreeWalker1);
                        Error ->
                            throw(Error)
                    end;
                false ->
                    case call_extension(Context, custom_tag_ast, [Tag, Context, TreeWalker]) of
                        undefined ->
                            custom_tags_clauses_ast1(
                              CustomTags, [Tag | ExcludeTags],
                              ClauseAcc, InfoAcc, Context, TreeWalker);
                        {{Ast, Info}, TW} ->
                            Clause = erl_syntax:clause(
                                       [erl_syntax:atom(Tag),
                                        erl_syntax:variable("_Variables"),
                                        erl_syntax:variable("RenderOptions")],
                                       none,
                                       options_match_ast(Context, TW) ++ [Ast]),
                            custom_tags_clauses_ast1(
                              CustomTags, [Tag | ExcludeTags],
                              [Clause|ClauseAcc], merge_info(Info, InfoAcc),
                              Context, TW)
                    end
            end
    end.

dependencies_function(Dependencies) ->
    erl_syntax:function(
      erl_syntax:atom(dependencies),
      [erl_syntax:clause(
         [], none,
         [erl_syntax:list(
            lists:map(
              fun ({XFile, XCheckSum}) ->
                      erl_syntax:tuple([erl_syntax:string(XFile), erl_syntax:string(XCheckSum)])
              end,
              Dependencies))
         ])
      ]).

translatable_strings_function(TranslatableStrings) ->
    erl_syntax:function(
      erl_syntax:atom(translatable_strings),
      [erl_syntax:clause(
         [], none,
         [erl_syntax:list(
            lists:map(
              fun(String) ->
                      erl_syntax:string(String)
              end,
              TranslatableStrings))
         ])
      ]).

translated_blocks_function(TranslatedBlocks) ->
    erl_syntax:function(
      erl_syntax:atom(translated_blocks),
      [erl_syntax:clause(
         [], none,
         [erl_syntax:list(
            lists:map(
              fun(String) ->
                      erl_syntax:string(String)
              end,
              TranslatedBlocks))
         ])
      ]).

variables_function(Variables) ->
    erl_syntax:function(
      erl_syntax:atom(variables),
      [erl_syntax:clause(
         [], none,
         [erl_syntax:list(
            [erl_syntax:atom(S) || S <- lists:usort(Variables)])
         ])
      ]).

custom_forms(Dir, Module, Functions, AstInfo) ->
    ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    ExportAst = erl_syntax:attribute(
                  erl_syntax:atom(export),
                  [erl_syntax:list(
                     [erl_syntax:arity_qualifier(erl_syntax:atom(source_dir), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(translatable_strings), erl_syntax:integer(0))
                      | lists:foldl(
                          fun({FunctionName, _, _}, Acc) ->
                                  [erl_syntax:arity_qualifier(erl_syntax:atom(FunctionName), erl_syntax:integer(1)),
                                   erl_syntax:arity_qualifier(erl_syntax:atom(FunctionName), erl_syntax:integer(2))
                                   |Acc]
                          end, [], Functions)
                     ])
                  ]),
    SourceFunctionAst = erl_syntax:function(
                          erl_syntax:atom(source_dir),
                          [erl_syntax:clause([], none, [erl_syntax:string(Dir)])]),
    DependenciesFunctionAst = dependencies_function(AstInfo#ast_info.dependencies),
    TranslatableStringsFunctionAst = translatable_strings_function(AstInfo#ast_info.translatable_strings),
    FunctionAsts = lists:foldl(fun({_, Function1, Function2}, Acc) -> [Function1, Function2 | Acc] end, [], Functions),

    [erl_syntax:revert(X)
     || X <- [ModuleAst, ExportAst, SourceFunctionAst, DependenciesFunctionAst, TranslatableStringsFunctionAst
              | FunctionAsts] ++ AstInfo#ast_info.pre_render_asts
    ].

stringify(BodyAst, #dtl_context{ binary_strings=BinaryStrings }) ->
    [erl_syntax:application(
       erl_syntax:atom(erlydtl_runtime),
       erl_syntax:atom(stringify_final),
       [BodyAst, erl_syntax:atom(BinaryStrings)])
    ].

forms(Module, {BodyAst, BodyInfo}, {CustomTagsFunctionAst, CustomTagsInfo}, CheckSum, TreeWalker,
      #dtl_context{ parse_trail=[File|_] }=Context) ->
    MergedInfo = merge_info(BodyInfo, CustomTagsInfo),
    Render0FunctionAst = erl_syntax:function(
                           erl_syntax:atom(render),
                           [erl_syntax:clause(
                              [],
                              none,
                              [erl_syntax:application(
                                 none, erl_syntax:atom(render),
                                 [erl_syntax:list([])])
                              ])
                           ]),
    Render1FunctionAst = erl_syntax:function(
                           erl_syntax:atom(render),
                           [erl_syntax:clause(
                              [erl_syntax:variable("Variables")],
                              none,
                              [erl_syntax:application(
                                 none, erl_syntax:atom(render),
                                 [erl_syntax:variable("Variables"),
                                  erl_syntax:list([])])
                              ])
                           ]),
    Function2 = erl_syntax:application(none, erl_syntax:atom(render_internal),
                                       [erl_syntax:variable("Variables"), erl_syntax:variable("RenderOptions")]),
    ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")],
                                 none,
                                 [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),
    ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")],
                                    none,
                                    [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),
    Render2FunctionAst = erl_syntax:function(
                           erl_syntax:atom(render),
                           [erl_syntax:clause(
                              [erl_syntax:variable("Variables"),
                               erl_syntax:variable("RenderOptions")],
                              none,
                              [erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])
                           ]),

    SourceFunctionTuple = erl_syntax:tuple(
                            [erl_syntax:string(File), erl_syntax:string(CheckSum)]),
    SourceFunctionAst = erl_syntax:function(
                          erl_syntax:atom(source),
                          [erl_syntax:clause([], none, [SourceFunctionTuple])]),

    DependenciesFunctionAst = dependencies_function(MergedInfo#ast_info.dependencies),

    TranslatableStringsAst = translatable_strings_function(MergedInfo#ast_info.translatable_strings),

    TranslatedBlocksAst = translated_blocks_function(MergedInfo#ast_info.translated_blocks),

    VariablesAst = variables_function(MergedInfo#ast_info.var_names),

    MatchAst = options_match_ast(Context, TreeWalker),

    BodyAstTmp = MatchAst ++ stringify(BodyAst, Context),

    RenderInternalFunctionAst = erl_syntax:function(
                                  erl_syntax:atom(render_internal),
                                  [erl_syntax:clause(
                                     [erl_syntax:variable("_Variables"),
                                      erl_syntax:variable("RenderOptions")],
                                     none,
                                     BodyAstTmp)
                                  ]),

    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),

    ExportAst = erl_syntax:attribute(
                  erl_syntax:atom(export),
                  [erl_syntax:list(
                     [erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(1)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(2)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(source), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(translatable_strings), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(translated_blocks), erl_syntax:integer(0)),
                      erl_syntax:arity_qualifier(erl_syntax:atom(variables), erl_syntax:integer(0))
                     ])
                  ]),

    erl_syntax:revert_forms(
      erl_syntax:form_list(
        [ModuleAst, ExportAst, Render0FunctionAst, Render1FunctionAst, Render2FunctionAst,
         SourceFunctionAst, DependenciesFunctionAst, TranslatableStringsAst,
         TranslatedBlocksAst, VariablesAst, RenderInternalFunctionAst,
         CustomTagsFunctionAst
         |BodyInfo#ast_info.pre_render_asts
        ])).

options_match_ast(Context) -> options_match_ast(Context, undefined).
options_match_ast(Context, TreeWalker) ->
    [
     erl_syntax:match_expr(
       erl_syntax:variable("_TranslationFun"),
       erl_syntax:application(
         erl_syntax:atom(proplists),
         erl_syntax:atom(get_value),
         [erl_syntax:atom(translation_fun), erl_syntax:variable("RenderOptions"), erl_syntax:atom(none)])),
     erl_syntax:match_expr(
       erl_syntax:variable("_CurrentLocale"),
       erl_syntax:application(
         erl_syntax:atom(proplists),
         erl_syntax:atom(get_value),
         [erl_syntax:atom(locale), erl_syntax:variable("RenderOptions"), erl_syntax:atom(none)])),
     erl_syntax:match_expr(
       erl_syntax:variable("_RecordInfo"),
       erl_syntax:abstract(Context#dtl_context.record_info))
     | case call_extension(Context, setup_render_ast, [Context, TreeWalker]) of
           undefined -> [];
           Ast when is_list(Ast) -> Ast
       end].

                                                % child templates should only consist of blocks at the top level
body_ast([{'extends', {string_literal, _Pos, String}} | ThisParseTree], Context, TreeWalker) ->
    File = full_path(unescape_string_literal(String), Context#dtl_context.doc_root),
    case lists:member(File, Context#dtl_context.parse_trail) of
        true ->
            throw(circular_include);
        _ ->
            case parse_file(File, Context) of
                {ok, ParentParseTree, CheckSum} ->
                    BlockDict = lists:foldl(
                                  fun
                                      ({block, {identifier, _, Name}, Contents}, Dict) ->
                                                   dict:store(Name, Contents, Dict);
                                      (_, Dict) ->
                                                   Dict
                                           end, dict:new(), ThisParseTree),
                    with_dependency({File, CheckSum}, body_ast(ParentParseTree, Context#dtl_context{
                                                                                  block_dict = dict:merge(fun(_Key, _ParentVal, ChildVal) -> ChildVal end,
                                                                                                          BlockDict, Context#dtl_context.block_dict),
                                                                                  parse_trail = [File | Context#dtl_context.parse_trail]}, TreeWalker));
                Err ->
                    throw(Err)
            end
    end;


body_ast(DjangoParseTree, Context, TreeWalker) ->
    {AstInfoList, TreeWalker2} = lists:mapfoldl(
                                   fun
                                       ({'autoescape', {identifier, _, OnOrOff}, Contents}, TreeWalkerAcc) ->
                                                       body_ast(Contents, Context#dtl_context{auto_escape = OnOrOff},
                                                                TreeWalkerAcc);
                                       ({'block', {identifier, _, Name}, Contents}, TreeWalkerAcc) ->
                                                       Block = case dict:find(Name, Context#dtl_context.block_dict) of
                                                                   {ok, ChildBlock} -> ChildBlock;
                                                                   _ -> Contents
                                                               end,
                                                       body_ast(Block, Context, TreeWalkerAcc);
                                       ({'blocktrans', Args, Contents}, TreeWalkerAcc) ->
                                                       blocktrans_ast(Args, Contents, Context, TreeWalkerAcc);
                                       ({'call', {identifier, _, Name}}, TreeWalkerAcc) ->
                                                       call_ast(Name, TreeWalkerAcc);
                                       ({'call', {identifier, _, Name}, With}, TreeWalkerAcc) ->
                                                       call_with_ast(Name, With, Context, TreeWalkerAcc);
                                       ({'comment', _Contents}, TreeWalkerAcc) ->
                                                       empty_ast(TreeWalkerAcc);
                                       ({'cycle', Names}, TreeWalkerAcc) ->
                                                       cycle_ast(Names, Context, TreeWalkerAcc);
                                       ({'cycle_compat', Names}, TreeWalkerAcc) ->
                                                       cycle_compat_ast(Names, Context, TreeWalkerAcc);
                                       ({'date', 'now', {string_literal, _Pos, FormatString}}, TreeWalkerAcc) ->
                                                       now_ast(FormatString, Context, TreeWalkerAcc);
                                       ({'filter', FilterList, Contents}, TreeWalkerAcc) ->
                                                       filter_tag_ast(FilterList, Contents, Context, TreeWalkerAcc);
                                       ({'firstof', Vars}, TreeWalkerAcc) ->
                                                       firstof_ast(Vars, Context, TreeWalkerAcc);
                                       ({'for', {'in', IteratorList, Variable, Reversed}, Contents}, TreeWalkerAcc) ->
                                                       {EmptyAstInfo, TreeWalker1} = empty_ast(TreeWalkerAcc),
                                                       for_loop_ast(IteratorList, Variable, Reversed, Contents, EmptyAstInfo, Context, TreeWalker1);
                                       ({'for', {'in', IteratorList, Variable, Reversed}, Contents, EmptyPartContents}, TreeWalkerAcc) ->
                                                       {EmptyAstInfo, TreeWalker1} = body_ast(EmptyPartContents, Context, TreeWalkerAcc),
                                                       for_loop_ast(IteratorList, Variable, Reversed, Contents, EmptyAstInfo, Context, TreeWalker1);
                                       ({'if', Expression, Contents, Elif}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElifAstInfo, TreeWalker2} = body_ast(Elif, Context, TreeWalker1),
                                                       ifelse_ast(Expression, IfAstInfo, ElifAstInfo, Context, TreeWalker2);
                                       ({'if', Expression, Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifelse_ast(Expression, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifchanged', '$undefined', Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifchanged_contents_ast(Contents, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifchanged', Values, Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifchanged_values_ast(Values, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifchangedelse', '$undefined', IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                                                       ifchanged_contents_ast(IfContents, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifchangedelse', Values, IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                                                       ifchanged_values_ast(Values, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifelse', Expression, IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                                                       ifelse_ast(Expression, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifequal', [Arg1, Arg2], Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifelse_ast({'expr', "eq", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifequalelse', [Arg1, Arg2], IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context,TreeWalker1),
                                                       ifelse_ast({'expr', "eq", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifnotequal', [Arg1, Arg2], Contents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                                                       ifelse_ast({'expr', "ne", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'ifnotequalelse', [Arg1, Arg2], IfContents, ElseContents}, TreeWalkerAcc) ->
                                                       {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                                                       {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                                                       ifelse_ast({'expr', "ne", Arg1, Arg2}, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
                                       ({'include', {string_literal, _, File}, Args}, TreeWalkerAcc) ->
                                                       include_ast(unescape_string_literal(File), Args, Context#dtl_context.local_scopes, Context, TreeWalkerAcc);
                                       ({'include_only', {string_literal, _, File}, Args}, TreeWalkerAcc) ->
                                                       include_ast(unescape_string_literal(File), Args, [], Context, TreeWalkerAcc);
                                       ({'regroup', {ListVariable, Grouper, {identifier, _, NewVariable}}, Contents}, TreeWalkerAcc) ->
                                                       regroup_ast(ListVariable, Grouper, NewVariable, Contents, Context, TreeWalkerAcc);
                                       ({'spaceless', Contents}, TreeWalkerAcc) ->
                                                       spaceless_ast(Contents, Context, TreeWalkerAcc);
                                       ({'ssi', Arg}, TreeWalkerAcc) ->
                                                       ssi_ast(Arg, Context, TreeWalkerAcc);
                                       ({'ssi_parsed', {string_literal, _, FileName}}, TreeWalkerAcc) ->
                                                       include_ast(unescape_string_literal(FileName), [], Context#dtl_context.local_scopes, Context, TreeWalkerAcc);
                                       ({'string', _Pos, String}, TreeWalkerAcc) ->
                                                       string_ast(String, Context, TreeWalkerAcc);
                                       ({'tag', {identifier, _, Name}, Args}, TreeWalkerAcc) ->
                                                       tag_ast(Name, Args, Context, TreeWalkerAcc);
                                       ({'templatetag', {_, _, TagName}}, TreeWalkerAcc) ->
                                                       templatetag_ast(TagName, Context, TreeWalkerAcc);
                                       ({'trans', Value}, TreeWalkerAcc) ->
                                                       translated_ast(Value, Context, TreeWalkerAcc);
                                       ({'widthratio', Numerator, Denominator, Scale}, TreeWalkerAcc) ->
                                                       widthratio_ast(Numerator, Denominator, Scale, Context, TreeWalkerAcc);
                                       ({'with', Args, Contents}, TreeWalkerAcc) ->
                                                       with_ast(Args, Contents, Context, TreeWalkerAcc);
                                       ({'extension', Tag}, TreeWalkerAcc) ->
                                                       extension_ast(Tag, Context, TreeWalkerAcc);
                                       ({'extends', _}, _TreeWalkerAcc) ->
                                                       throw(unexpected_extends_tag);
                                       (ValueToken, TreeWalkerAcc) ->
                                                       {{ValueAst,ValueInfo},ValueTreeWalker} = value_ast(ValueToken, true, true, Context, TreeWalkerAcc),
                                                       {{format(ValueAst, Context, ValueTreeWalker),ValueInfo},ValueTreeWalker}
                                               end, TreeWalker, DjangoParseTree),
    {AstList, {Info, TreeWalker3}} = lists:mapfoldl(
                                       fun({Ast, Info}, {InfoAcc, TreeWalkerAcc}) ->
                                               PresetVars = lists:foldl(fun
                                                                            (X, Acc) ->
                                                                               case proplists:lookup(X, Context#dtl_context.vars) of
                                                                                   none ->
                                                                                       Acc;
                                                                                   Val ->
                                                                                       [erl_syntax:abstract(Val) | Acc]
                                                                               end
                                                                       end, [], Info#ast_info.var_names),
                                               case PresetVars of
                                                   [] ->
                                                       {Ast, {merge_info(Info, InfoAcc), TreeWalkerAcc}};
                                                   _ ->
                                                       Counter = TreeWalkerAcc#treewalker.counter,
                                                       Name = lists:concat([pre_render, Counter]),
                                                       Ast1 = erl_syntax:application(none, erl_syntax:atom(Name),
                                                                                     [erl_syntax:list(PresetVars),
                                                                                      erl_syntax:variable("RenderOptions")]),
                                                       PreRenderAst = erl_syntax:function(erl_syntax:atom(Name),
                                                                                          [erl_syntax:clause([erl_syntax:variable("_Variables"),
                                                                                                              erl_syntax:variable("RenderOptions")],
                                                                                                             none,
                                                                                                             options_match_ast(Context, TreeWalkerAcc)
                                                                                                             ++ [Ast])]),
                                                       PreRenderAsts = Info#ast_info.pre_render_asts,
                                                       Info1 = Info#ast_info{pre_render_asts = [PreRenderAst | PreRenderAsts]},
                                                       {Ast1, {merge_info(Info1, InfoAcc), TreeWalkerAcc#treewalker{counter = Counter + 1}}}
                                               end
                                       end, {#ast_info{}, TreeWalker2}, AstInfoList),
    {{erl_syntax:list(AstList), Info}, TreeWalker3}.


value_ast(ValueToken, AsString, EmptyIfUndefined, Context, TreeWalker) ->
    case ValueToken of
        {'expr', Operator, Value} ->
            {{ValueAst,InfoValue}, TreeWalker1} = value_ast(Value, false, EmptyIfUndefined, Context, TreeWalker),
            Ast = erl_syntax:application(erl_syntax:atom(erlydtl_runtime),
                                         erl_syntax:atom(Operator),
                                         [ValueAst]),
            {{Ast, InfoValue}, TreeWalker1};
        {'expr', Operator, Value1, Value2} ->
            {{Value1Ast,InfoValue1}, TreeWalker1} = value_ast(Value1, false, EmptyIfUndefined, Context, TreeWalker),
            {{Value2Ast,InfoValue2}, TreeWalker2} = value_ast(Value2, false, EmptyIfUndefined, Context, TreeWalker1),
            Ast = erl_syntax:application(erl_syntax:atom(erlydtl_runtime),
                                         erl_syntax:atom(Operator),
                                         [Value1Ast, Value2Ast]),
            {{Ast, merge_info(InfoValue1,InfoValue2)}, TreeWalker2};
        {'string_literal', _Pos, String} ->
            string_ast(unescape_string_literal(String), Context, TreeWalker);
        {'number_literal', _Pos, Number} ->
            case AsString of
                true  -> string_ast(Number, Context, TreeWalker);
                false -> {{erl_syntax:integer(list_to_integer(Number)), #ast_info{}}, TreeWalker}
            end;
        {'apply_filter', Variable, Filter} ->
            filter_ast(Variable, Filter, Context, TreeWalker);
        {'attribute', _} = Variable ->
            resolve_variable_ast(Variable, Context, TreeWalker, EmptyIfUndefined);
        {'variable', _} = Variable ->
            resolve_variable_ast(Variable, Context, TreeWalker, EmptyIfUndefined);
        {extension, Tag} ->
            extension_ast(Tag, Context, TreeWalker)
    end.

extension_ast(Tag, Context, TreeWalker) ->
    case call_extension(Context, compile_ast, [Tag, Context, TreeWalker]) of
        undefined ->
            throw({unknown_extension, Tag});
        Result ->
            Result
    end.

merge_info(Info1, Info2) ->
    #ast_info{
       dependencies =
           lists:merge(
             lists:sort(Info1#ast_info.dependencies),
             lists:sort(Info2#ast_info.dependencies)),
       var_names =
           lists:merge(
             lists:sort(Info1#ast_info.var_names),
             lists:sort(Info2#ast_info.var_names)),
       translatable_strings =
           lists:merge(
             lists:sort(Info1#ast_info.translatable_strings),
             lists:sort(Info2#ast_info.translatable_strings)),
       translated_blocks =
           lists:merge(
             lists:sort(Info1#ast_info.translated_blocks),
             lists:sort(Info2#ast_info.translated_blocks)),
       custom_tags =
           lists:merge(
             lists:sort(Info1#ast_info.custom_tags),
             lists:sort(Info2#ast_info.custom_tags)),
       pre_render_asts =
           lists:merge(
             Info1#ast_info.pre_render_asts,
             Info2#ast_info.pre_render_asts)}.


with_dependencies([], Args) ->
    Args;
with_dependencies([Dependency | Rest], Args) ->
    with_dependencies(Rest, with_dependency(Dependency, Args)).

with_dependency(FilePath, {{Ast, Info}, TreeWalker}) ->
    {{Ast, Info#ast_info{dependencies = [FilePath | Info#ast_info.dependencies]}}, TreeWalker}.


empty_ast(TreeWalker) ->
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker}.

blocktrans_ast(ArgList, Contents, Context, TreeWalker) ->
    %% add new scope using 'with' values
    {NewScope, {ArgInfo, TreeWalker1}} = lists:mapfoldl(fun
                                                            ({{identifier, _, LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
                                                               {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
                                                               {{LocalVarName, Ast}, {merge_info(AstInfo1, Info), TreeWalker2}}
                                                       end, {#ast_info{}, TreeWalker}, ArgList),
    NewContext = Context#dtl_context{ local_scopes = [NewScope|Context#dtl_context.local_scopes] },
    %% key for translation lookup
    SourceText = lists:flatten(erlydtl_unparser:unparse(Contents)),
    {{DefaultAst, AstInfo}, TreeWalker2} = body_ast(Contents, NewContext, TreeWalker1),
    MergedInfo = merge_info(AstInfo, ArgInfo),
    case Context#dtl_context.trans_fun of
        none ->
            %% translate in runtime
            blocktrans_runtime_ast({DefaultAst, MergedInfo}, TreeWalker2, SourceText, Contents, NewContext);
        BlockTransFun when is_function(BlockTransFun) ->
            %% translate in compile-time
            {FinalAstInfo, FinalTreeWalker, Clauses} = lists:foldr(fun(Locale, {AstInfoAcc, ThisTreeWalker, ClauseAcc}) ->
                                                                           case BlockTransFun(SourceText, Locale) of
                                                                               default ->
                                                                                   {AstInfoAcc, ThisTreeWalker, ClauseAcc};
                                                                               Body ->
                                                                                   {ok, DjangoParseTree} = do_parse(Body, Context),
                                                                                   {{ThisAst, ThisAstInfo}, TreeWalker3} = body_ast(DjangoParseTree, NewContext, ThisTreeWalker),
                                                                                   {merge_info(ThisAstInfo, AstInfoAcc), TreeWalker3,
                                                                                    [erl_syntax:clause([erl_syntax:string(Locale)], none, [ThisAst])|ClauseAcc]}
                                                                           end
                                                                   end, {MergedInfo, TreeWalker2, []}, Context#dtl_context.trans_locales),
            Ast = erl_syntax:case_expr(erl_syntax:variable("_CurrentLocale"),
                                       Clauses ++ [erl_syntax:clause([erl_syntax:underscore()], none, [DefaultAst])]),
            {{Ast, FinalAstInfo#ast_info{ translated_blocks = [SourceText] }}, FinalTreeWalker}
    end.

blocktrans_runtime_ast({DefaultAst, Info}, Walker, SourceText, Contents, Context) ->
    %% Contents is flat - only strings and '{{var}}' allowed.
    %% build sorted list (orddict) of pre-resolved variables to pass to runtime translation function
    USortedVariables = lists:usort(fun({variable, {identifier, _, A}},
                                       {variable, {identifier, _, B}}) ->
                                           A =< B
                                   end, [Var || {variable, _}=Var <- Contents]),
    VarBuilder = fun({variable, {identifier, _, Name}}=Var, Walker1) ->
                         {{Ast2, _InfoIgn}, Walker2}  = resolve_variable_ast(Var, Context, Walker1, false),
                         KVAst = erl_syntax:tuple([erl_syntax:string(atom_to_list(Name)), Ast2]),
                         {KVAst, Walker2}
                 end,
    {VarAsts, Walker2} = lists:mapfoldl(VarBuilder, Walker, USortedVariables),
    VarListAst = erl_syntax:list(VarAsts),
    RuntimeTransAst =  [erl_syntax:application(
                          erl_syntax:atom(erlydtl_runtime),
                          erl_syntax:atom(translate_block),
                          [erl_syntax:string(SourceText),
                           erl_syntax:variable("_TranslationFun"),
                           VarListAst])],
    Ast1 = erl_syntax:case_expr(erl_syntax:variable("_TranslationFun"),
                                [erl_syntax:clause([erl_syntax:atom(none)], none, [DefaultAst]),
                                 erl_syntax:clause([erl_syntax:underscore()], none,
                                                   RuntimeTransAst)]),
    {{Ast1, Info}, Walker2}.

translated_ast({string_literal, _, String}, Context, TreeWalker) ->
    UnescapedStr = unescape_string_literal(String),
    case call_extension(Context, translate_ast, [UnescapedStr, Context, TreeWalker]) of
        undefined ->
            AstInfo = #ast_info{translatable_strings = [UnescapedStr]},
            case Context#dtl_context.trans_fun of
                none -> runtime_trans_ast(erl_syntax:string(UnescapedStr), AstInfo, TreeWalker);
                _ -> compiletime_trans_ast(UnescapedStr, AstInfo, Context, TreeWalker)
            end;
        Translated ->
            Translated
    end;
translated_ast(ValueToken, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = value_ast(ValueToken, true, false, Context, TreeWalker),
    runtime_trans_ast(Ast, Info, TreeWalker1).

runtime_trans_ast(ValueAst, AstInfo, TreeWalker) ->
    StringLookupAst = erl_syntax:application(
                        erl_syntax:atom(erlydtl_runtime),
                        erl_syntax:atom(translate),
                        [ValueAst, erl_syntax:variable("_TranslationFun")]),
    {{StringLookupAst, AstInfo}, TreeWalker}.

compiletime_trans_ast(String, AstInfo,
                      #dtl_context{trans_fun=TFun,
                                   trans_locales=TLocales}=Context,
                      TreeWalker) ->
    {{DefaultAst, Info1}, TWalker1} = Default =  string_ast(String, Context, TreeWalker),
    DefaultClauseAst = erl_syntax:clause([erl_syntax:underscore()], none, [DefaultAst]), %or runtime trans?
    FoldFun = fun(Locale, {ClausesAcc, Info2, TWalker2}) ->
                      {{TranslatedAst, Info3}, TWalker3} =
                          case TFun(String, Locale) of
                              default -> Default; %or runtime trans?
                              Translated ->
                                  string_ast(binary_to_list(Translated), Context, TWalker2)
                          end,
                      ClauseAst = erl_syntax:clause(
                                    [erl_syntax:string(Locale)],
                                    none,
                                    [TranslatedAst]),
                      {[ClauseAst | ClausesAcc], merge_info(Info2, Info3), TWalker3}
              end,
    {ClAst, ClInfo, ClTreeWalker} = lists:foldl(
                                      FoldFun,
                                      {[DefaultClauseAst], merge_info(AstInfo, Info1), TWalker1},
                                      TLocales),
    CaseAst = erl_syntax:case_expr(erl_syntax:variable("_CurrentLocale"), ClAst),
    {{CaseAst, ClInfo}, ClTreeWalker}.

                                                % Completely unnecessary in ErlyDTL (use {{ "{%" }} etc), but implemented for compatibility.
templatetag_ast("openblock", Context, TreeWalker) ->
    string_ast("{%", Context, TreeWalker);
templatetag_ast("closeblock", Context, TreeWalker) ->
    string_ast("%}", Context, TreeWalker);
templatetag_ast("openvariable", Context, TreeWalker) ->
    string_ast("{{", Context, TreeWalker);
templatetag_ast("closevariable", Context, TreeWalker) ->
    string_ast("}}", Context, TreeWalker);
templatetag_ast("openbrace", Context, TreeWalker) ->
    string_ast("{", Context, TreeWalker);
templatetag_ast("closebrace", Context, TreeWalker) ->
    string_ast("}", Context, TreeWalker);
templatetag_ast("opencomment", Context, TreeWalker) ->
    string_ast("{#", Context, TreeWalker);
templatetag_ast("closecomment", Context, TreeWalker) ->
    string_ast("#}", Context, TreeWalker).


widthratio_ast(Numerator, Denominator, Scale, Context, TreeWalker) ->
    {{NumAst, NumInfo}, TreeWalker1} = value_ast(Numerator, false, true, Context, TreeWalker),
    {{DenAst, DenInfo}, TreeWalker2} = value_ast(Denominator, false, true, Context, TreeWalker1),
    {{ScaleAst, ScaleInfo}, TreeWalker3} = value_ast(Scale, false, true, Context, TreeWalker2),
    {{format_number_ast(erl_syntax:application(
                          erl_syntax:atom(erlydtl_runtime),
                          erl_syntax:atom(widthratio),
                          [NumAst, DenAst, ScaleAst])), merge_info(ScaleInfo, merge_info(NumInfo, DenInfo))},
     TreeWalker3}.

binary_string(String) ->
    erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]).

string_ast(String, #dtl_context{ binary_strings = true }, TreeWalker) when is_list(String) ->
    {{binary_string(String), #ast_info{}}, TreeWalker};
string_ast(String, #dtl_context{ binary_strings = false }, TreeWalker) when is_list(String) ->
    {{erl_syntax:string(String), #ast_info{}}, TreeWalker}; %% less verbose AST, better for development and debugging
string_ast(S, Context, TreeWalker) when is_atom(S) ->
    string_ast(atom_to_list(S), Context, TreeWalker).


include_ast(File, ArgList, Scopes, Context, TreeWalker) ->
    FilePath = full_path(File, Context#dtl_context.doc_root),
    case parse_file(FilePath, Context) of
        {ok, InclusionParseTree, CheckSum} ->
            {NewScope, {ArgInfo, TreeWalker1}}
                = lists:mapfoldl(
                    fun ({{identifier, _, LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
                            {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
                            {{LocalVarName, Ast}, {merge_info(AstInfo1, Info), TreeWalker2}}
                    end, {#ast_info{}, TreeWalker}, ArgList),

            {{BodyAst, BodyInfo}, TreeWalker2} = with_dependency(
                                                   {FilePath, CheckSum},
                                                   body_ast(
                                                     InclusionParseTree,
                                                     Context#dtl_context{
                                                       parse_trail = [FilePath | Context#dtl_context.parse_trail],
                                                       local_scopes = [NewScope|Scopes]
                                                      }, TreeWalker1)),

            {{BodyAst, merge_info(BodyInfo, ArgInfo)}, TreeWalker2};
        Err -> throw(Err)
    end.

                                                % include at run-time
ssi_ast(FileName, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = value_ast(FileName, true, true, Context, TreeWalker),
    {Mod, Fun} = Context#dtl_context.reader,
    {{erl_syntax:application(
        erl_syntax:atom(erlydtl_runtime),
        erl_syntax:atom(read_file),
        [erl_syntax:atom(Mod), erl_syntax:atom(Fun), erl_syntax:string(Context#dtl_context.doc_root), Ast]), Info}, TreeWalker1}.

filter_tag_ast(FilterList, Contents, Context, TreeWalker) ->
    {{InnerAst, Info}, TreeWalker1} = body_ast(Contents, Context#dtl_context{auto_escape = did}, TreeWalker),
    {{FilteredAst, FilteredInfo}, TreeWalker2} =
        lists:foldl(
          fun ({{identifier, _, Name}, []}, {{AstAcc, InfoAcc}, TreeWalkerAcc})
                when Name =:= 'escape'; Name =:= 'safe'; Name =:= 'safeseq' ->
                  {{AstAcc, InfoAcc}, TreeWalkerAcc#treewalker{safe = true}};
              (Filter, {{AstAcc, InfoAcc}, TreeWalkerAcc}) ->
                  {{Ast, AstInfo}, TW} = filter_ast1(Filter, AstAcc, Context, TreeWalkerAcc),
                  {{Ast, merge_info(InfoAcc, AstInfo)}, TW}
          end,
          {{erl_syntax:application(
              erl_syntax:atom(erlang),
              erl_syntax:atom(iolist_to_binary),
              [InnerAst]),
            Info},
           TreeWalker1},
          FilterList),

    EscapedAst = case search_for_escape_filter(lists:reverse(FilterList), Context) of
                     on ->
                         erl_syntax:application(
                           erl_syntax:atom(erlydtl_filters),
                           erl_syntax:atom(force_escape),
                           [FilteredAst]);
                     _ ->
                         FilteredAst
                 end,
    {{EscapedAst, FilteredInfo}, TreeWalker2}.

search_for_escape_filter(FilterList, #dtl_context{auto_escape = on}) ->
    search_for_safe_filter(FilterList);
search_for_escape_filter(_, #dtl_context{auto_escape = did}) -> off;
search_for_escape_filter([{{identifier, _, 'escape'}, []}|Rest], _Context) ->
    search_for_safe_filter(Rest);
search_for_escape_filter([_|Rest], Context) ->
    search_for_escape_filter(Rest, Context);
search_for_escape_filter([], _Context) -> off.

search_for_safe_filter([{{identifier, _, Name}, []}|_])
  when Name =:= 'safe'; Name =:= 'safeseq' -> off;
search_for_safe_filter([_|Rest]) -> search_for_safe_filter(Rest);
search_for_safe_filter([]) -> on.

filter_ast(Variable, Filter, Context, TreeWalker) ->
    %% the escape filter is special; it is always applied last, so we have to go digging for it

    %% AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    %% so don't do any more escaping
    {{UnescapedAst, Info}, TreeWalker2} = filter_ast_noescape(
                                            Variable, Filter,
                                            Context#dtl_context{auto_escape = did},
                                            TreeWalker),

    EscapedAst = case search_for_escape_filter(Variable, Filter, Context) of
                     on ->
                         erl_syntax:application(
                           erl_syntax:atom(erlydtl_filters),
                           erl_syntax:atom(force_escape),
                           [UnescapedAst]);
                     _ ->
                         UnescapedAst
                 end,
    {{EscapedAst, Info}, TreeWalker2}.

filter_ast_noescape(Variable, {{identifier, _, Name}, []}, Context, TreeWalker)
  when Name =:= 'escape'; Name =:= 'safe'; Name =:= 'safeseq' ->
    value_ast(Variable, true, false, Context, TreeWalker#treewalker{safe = true});
filter_ast_noescape(Variable, Filter, Context, TreeWalker) ->
    {{ValueAst, Info1}, TreeWalker2} = value_ast(Variable, true, false, Context, TreeWalker),
    {{VarValue, Info2}, TreeWalker3} = filter_ast1(Filter, ValueAst, Context, TreeWalker2),
    {{VarValue, merge_info(Info1, Info2)}, TreeWalker3}.

filter_ast1({{identifier, _, Name}, Args}, ValueAst, Context, TreeWalker) ->
    {{ArgsAst, ArgsInfo}, TreeWalker2} =
        lists:foldr(
          fun (Arg, {{AccAst, AccInfo}, AccTreeWalker}) ->
                  {{ArgAst, ArgInfo}, ArgTreeWalker} = value_ast(Arg, false, false, Context, AccTreeWalker),
                  {{[ArgAst|AccAst], merge_info(ArgInfo, AccInfo)}, ArgTreeWalker}
          end,
          {{[], #ast_info{}}, TreeWalker},
          Args),
    FilterAst = filter_ast2(Name, [ValueAst|ArgsAst], Context),
    {{FilterAst, ArgsInfo}, TreeWalker2}.

filter_ast2(Name, Args, #dtl_context{ filter_modules = [Module|Rest] } = Context) ->
    case lists:member({Name, length(Args)}, Module:module_info(exports)) of
        true ->
            erl_syntax:application(
              erl_syntax:atom(Module),
              erl_syntax:atom(Name),
              Args);
        false ->
            filter_ast2(Name, Args, Context#dtl_context{ filter_modules = Rest })
    end;
filter_ast2(Name, Args, _) ->
    throw({unknown_filter, Name, length(Args)}).

search_for_escape_filter(Variable, Filter, #dtl_context{auto_escape = on}) ->
    search_for_safe_filter(Variable, Filter);
search_for_escape_filter(_, _, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter(Variable, {{identifier, _, 'escape'}, []} = Filter, _Context) ->
    search_for_safe_filter(Variable, Filter);
search_for_escape_filter({apply_filter, Variable, Filter}, _, Context) ->
    search_for_escape_filter(Variable, Filter, Context);
search_for_escape_filter(_Variable, _Filter, _Context) ->
    off.

search_for_safe_filter(_, {{identifier, _, 'safe'}, []}) ->
    off;
search_for_safe_filter(_, {{identifier, _, 'safeseq'}, []}) ->
    off;
search_for_safe_filter({apply_filter, Variable, Filter}, _) ->
    search_for_safe_filter(Variable, Filter);
search_for_safe_filter(_Variable, _Filter) ->
    on.

finder_function(true) -> {erlydtl_runtime, fetch_value};
finder_function(false) -> {erlydtl_runtime, find_value}.

finder_function(EmptyIfUndefined, Context) ->
    case call_extension(Context, finder_function, [EmptyIfUndefined]) of
        undefined -> finder_function(EmptyIfUndefined);
        Result -> Result
    end.

resolve_variable_ast({extension, Tag}, Context, TreeWalker, _) ->
    extension_ast(Tag, Context, TreeWalker);
resolve_variable_ast(VarTuple, Context, TreeWalker, EmptyIfUndefined)
  when is_boolean(EmptyIfUndefined) ->
    resolve_variable_ast(VarTuple, Context, TreeWalker, finder_function(EmptyIfUndefined, Context));
resolve_variable_ast(VarTuple, Context, TreeWalker, FinderFunction) ->
    resolve_variable_ast1(VarTuple, Context, TreeWalker, FinderFunction).

resolve_variable_ast1({attribute, {{AttrKind, Pos, Attr}, Variable}}, Context, TreeWalker, FinderFunction) ->
    {{VarAst, VarInfo}, TreeWalker1} = resolve_variable_ast(Variable, Context, TreeWalker, FinderFunction),
    FileNameAst = erl_syntax:tuple(
                    [erl_syntax:atom(filename),
                     case Context#dtl_context.parse_trail of
                         [] -> erl_syntax:atom(undefined);
                         [H|_] -> erl_syntax:string(H)
                     end]),
    AttrAst = erl_syntax:abstract(
                case AttrKind of
                    number_literal -> erlang:list_to_integer(Attr);
                    _ -> Attr
                end),
    {Runtime, Finder} = FinderFunction,
    {{erl_syntax:application(
        erl_syntax:atom(Runtime),
        erl_syntax:atom(Finder),
        [AttrAst, VarAst,
         erl_syntax:list(
           [FileNameAst,
            erl_syntax:abstract({pos, Pos}),
            erl_syntax:tuple([erl_syntax:atom(record_info),
                              erl_syntax:variable("_RecordInfo")]),
            erl_syntax:tuple([erl_syntax:atom(render_options),
                              erl_syntax:variable("RenderOptions")])
           ])
        ]),
      VarInfo},

     TreeWalker1};

resolve_variable_ast1({variable, {identifier, Pos, VarName}}, Context, TreeWalker, FinderFunction) ->
    VarValue = case resolve_scoped_variable_ast(VarName, Context) of
                   undefined ->
                       FileNameAst = erl_syntax:tuple(
                                       [erl_syntax:atom(filename),
                                        case Context#dtl_context.parse_trail of
                                            [] -> erl_syntax:atom(undefined);
                                            [H|_] -> erl_syntax:string(H)
                                        end]),
                       {Runtime, Finder} = FinderFunction,
                       erl_syntax:application(
                         erl_syntax:atom(Runtime), erl_syntax:atom(Finder),
                         [erl_syntax:atom(VarName), erl_syntax:variable("_Variables"),
                          erl_syntax:list(
                            [FileNameAst,
                             erl_syntax:abstract({pos, Pos}),
                             erl_syntax:tuple([erl_syntax:atom(record_info),
                                               erl_syntax:variable("_RecordInfo")]),
                             erl_syntax:tuple([erl_syntax:atom(render_options),
                                               erl_syntax:variable("RenderOptions")])
                            ])
                         ]);
                   Val ->
                       Val
               end,
    {{VarValue, #ast_info{ var_names=[VarName] }}, TreeWalker}.

resolve_scoped_variable_ast(VarName, Context) ->
    resolve_scoped_variable_ast(VarName, Context, undefined).

resolve_scoped_variable_ast(VarName, Context, Default) ->
    lists:foldl(
      fun (Scope, Res) ->
              if Res =:= Default ->
                      proplists:get_value(VarName, Scope, Default);
                 true -> Res
              end
      end,
      Default,
      Context#dtl_context.local_scopes).

format(Ast, Context, TreeWalker) ->
    auto_escape(format_number_ast(Ast), Context, TreeWalker).

format_number_ast(Ast) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(format_number),
                           [Ast]).


auto_escape(Value, _, #treewalker{safe = true}) ->
    Value;
auto_escape(Value, #dtl_context{auto_escape = on}, _) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape), [Value]);
auto_escape(Value, _, _) ->
    Value.

firstof_ast(Vars, Context, TreeWalker) ->
    body_ast([lists:foldr(fun
                              ({L, _, _}=Var, []) when L=:=string_literal;L=:=number_literal ->
                                 Var;
                              ({L, _, _}, _) when L=:=string_literal;L=:=number_literal ->
                                 erlang:error(errbadliteral);
                              (Var, []) ->
                                 {'ifelse', Var, [Var], []};
                              (Var, Acc) ->
                                 {'ifelse', Var, [Var], [Acc]} end,
                          [], Vars)], Context, TreeWalker).

ifelse_ast(Expression, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    {{Ast, ExpressionInfo}, TreeWalker1} = value_ast(Expression, false, false, Context, TreeWalker),
    {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(is_true), [Ast]),
                           [erl_syntax:clause([erl_syntax:atom(true)], none,
                                              [IfContentsAst]),
                            erl_syntax:clause([erl_syntax:underscore()], none,
                                              [ElseContentsAst])
                           ]), merge_info(ExpressionInfo, Info)}, TreeWalker1}.

with_ast(ArgList, Contents, Context, TreeWalker) ->
    {ArgAstList, {ArgInfo, TreeWalker1}} =
        lists:mapfoldl(
          fun ({{identifier, _, _LocalVarName}, Value}, {AstInfo1, TreeWalker1}) ->
                  {{Ast, Info}, TreeWalker2} = value_ast(Value, false, false, Context, TreeWalker1),
                  {Ast, {merge_info(AstInfo1, Info), TreeWalker2}}
          end, {#ast_info{}, TreeWalker}, ArgList),

    NewScope = lists:map(
                 fun({{identifier, _, LocalVarName}, _Value}) ->
                         {LocalVarName, erl_syntax:variable(lists:concat(["Var_", LocalVarName]))}
                 end, ArgList),

    {{InnerAst, InnerInfo}, TreeWalker2} =
        body_ast(
          Contents,
          Context#dtl_context{local_scopes = [NewScope|Context#dtl_context.local_scopes]},
          TreeWalker1),

    {{erl_syntax:application(
        erl_syntax:fun_expr(
          [erl_syntax:clause(
             lists:map(fun({_, Var}) -> Var end, NewScope),
             none,
             [InnerAst])]),
        ArgAstList),
      merge_info(ArgInfo, InnerInfo)},
     TreeWalker2}.

regroup_ast(ListVariable, GrouperVariable, LocalVarName, Contents, Context, TreeWalker) ->
    {{ListAst, ListInfo}, TreeWalker1} = value_ast(ListVariable, false, true, Context, TreeWalker),
    NewScope = [{LocalVarName, erl_syntax:variable(lists:concat(["Var_", LocalVarName]))}],

    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(Contents,
                                                    Context#dtl_context{ local_scopes = [NewScope|Context#dtl_context.local_scopes] }, TreeWalker1),

    Ast = {erl_syntax:application(
             erl_syntax:fun_expr([
                                  erl_syntax:clause([erl_syntax:variable(lists:concat(["Var_", LocalVarName]))], none,
                                                    [InnerAst])]),
             [erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(regroup),
                                     [ListAst, regroup_filter(GrouperVariable,[])])]), merge_info(ListInfo, InnerInfo)},
    {Ast,TreeWalker2}.

regroup_filter({attribute,{{identifier,_,Ident},Next}},Acc) ->
    regroup_filter(Next,[erl_syntax:atom(Ident)|Acc]);
regroup_filter({variable,{identifier,_,Var}},Acc) ->
    erl_syntax:list([erl_syntax:atom(Var)|Acc]).

to_list_ast(Value, IsReversed) ->
    erl_syntax:application(
      erl_syntax:atom(erlydtl_runtime),
      erl_syntax:atom(to_list),
      [Value, IsReversed]).

to_list_ast(Value, IsReversed, Context, TreeWalker) ->
    case call_extension(Context, to_list_ast, [Value, IsReversed, Context, TreeWalker]) of
        undefined -> to_list_ast(Value, IsReversed);
        Result -> Result
    end.

for_loop_ast(IteratorList, LoopValue, IsReversed, Contents, {EmptyContentsAst, EmptyContentsInfo}, Context, TreeWalker) ->
    %% create unique namespace for this instance
    Level = length(Context#dtl_context.local_scopes),
    {Row, Col} = element(2, hd(IteratorList)),
    ForId = lists:concat(["/", Level, "_", Row, ":", Col]),

    Counters = lists:concat(["Counters", ForId]),
    Vars = lists:concat(["Vars", ForId]),

    %% setup
    VarScope = lists:map(
                 fun({identifier, {R, C}, Iterator}) ->
                         {Iterator, erl_syntax:variable(
                                      lists:concat(["Var_", Iterator,
                                                    "/", Level, "_", R, ":", C
                                                   ]))}
                 end, IteratorList),
    {Iterators, IteratorVars} = lists:unzip(VarScope),
    IteratorCount = length(IteratorVars),

    {{LoopBodyAst, Info}, TreeWalker1} =
        body_ast(
          Contents,
          Context#dtl_context{
            local_scopes =
                [[{'forloop', erl_syntax:variable(Counters)} | VarScope]
                 | Context#dtl_context.local_scopes]
           },
          TreeWalker),

    CounterAst = erl_syntax:application(
                   erl_syntax:atom(erlydtl_runtime),
                   erl_syntax:atom(increment_counter_stats),
                   [erl_syntax:variable(Counters)]),

    {{LoopValueAst, LoopValueInfo}, TreeWalker2} = value_ast(LoopValue, false, true, Context, TreeWalker1),

    LoopValueAst0 = to_list_ast(LoopValueAst, erl_syntax:atom(IsReversed), Context, TreeWalker2),

    ParentLoop = resolve_scoped_variable_ast('forloop', Context, erl_syntax:atom(undefined)),

    %% call for loop
    {{erl_syntax:case_expr(
        erl_syntax:application(
          erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('forloop'),
          [erl_syntax:fun_expr(
             [erl_syntax:clause(
                [erl_syntax:variable(Vars),
                 erl_syntax:variable(Counters)],
                none,
                [erl_syntax:match_expr(
                   erl_syntax:tuple(IteratorVars),
                   erl_syntax:if_expr(
                     [erl_syntax:clause(
                        [], [erl_syntax:application(none, erl_syntax:atom(is_tuple), [erl_syntax:variable(Vars)]),
                             erl_syntax:infix_expr(
                               erl_syntax:application(none, erl_syntax:atom(size), [erl_syntax:variable(Vars)]),
                               erl_syntax:operator('=='),
                               erl_syntax:integer(IteratorCount))
                            ],
                        [erl_syntax:variable(Vars)])
                      | if IteratorCount > 1 ->
                                [erl_syntax:clause(
                                   [], [erl_syntax:application(none, erl_syntax:atom(is_list), [erl_syntax:variable(Vars)]),
                                        erl_syntax:infix_expr(
                                          erl_syntax:application(none, erl_syntax:atom(length), [erl_syntax:variable(Vars)]),
                                          erl_syntax:operator('=='),
                                          erl_syntax:integer(IteratorCount))
                                       ],
                                   [erl_syntax:application(none, erl_syntax:atom(list_to_tuple), [erl_syntax:variable(Vars)])]),
                                 erl_syntax:clause(
                                   [], [erl_syntax:atom(true)],
                                   [erl_syntax:application(
                                      none, erl_syntax:atom(throw),
                                      [erl_syntax:tuple(
                                         [erl_syntax:atom(for_loop),
                                          erl_syntax:abstract(Iterators),
                                          erl_syntax:variable(Vars)])
                                      ])
                                   ])
                                ];
                           true ->
                                [erl_syntax:clause(
                                   [], [erl_syntax:atom(true)],
                                   [erl_syntax:tuple([erl_syntax:variable(Vars)])])
                                ]
                        end
                     ])),
                 erl_syntax:tuple([LoopBodyAst, CounterAst])
                ])
             ]),
           LoopValueAst0, ParentLoop
          ]),
        %% of
        [erl_syntax:clause(
           [erl_syntax:atom(empty)],
           none,
           [EmptyContentsAst]),
         erl_syntax:clause(
           [erl_syntax:tuple([erl_syntax:variable("L"), erl_syntax:underscore()])],
           none, [erl_syntax:variable("L")])
        ]),
      merge_info(merge_info(Info, EmptyContentsInfo), LoopValueInfo)},
     TreeWalker2}.

ifchanged_values_ast(Values, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    ValueAstFun = fun(Expr, {LTreeWalker, LInfo, Acc}) ->
                          {{EAst, EInfo}, ETw} = value_ast(Expr, false, true, Context, LTreeWalker),
                          {ETw, merge_info(LInfo, EInfo), [erl_syntax:tuple([erl_syntax:integer(erlang:phash2(Expr)), EAst])|Acc]} end,
    {TreeWalker1, MergedInfo, Changed} = lists:foldl(ValueAstFun, {TreeWalker, Info,  []}, Values),
    {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(ifchanged), [erl_syntax:list(Changed)]),
                           [erl_syntax:clause([erl_syntax:atom(true)], none,
                                              [IfContentsAst]),
                            erl_syntax:clause([erl_syntax:underscore()], none,
                                              [ElseContentsAst])
                           ]), MergedInfo}, TreeWalker1}.

ifchanged_contents_ast(Contents, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, _Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    Key = erl_syntax:integer(erlang:phash2(Contents)),
    {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(ifchanged), [erl_syntax:list([erl_syntax:tuple([Key, IfContentsAst])])]),
                           [erl_syntax:clause([erl_syntax:atom(true)], none,
                                              [IfContentsAst]),
                            erl_syntax:clause([erl_syntax:underscore()], none,
                                              [ElseContentsAst])
                           ]), Info}, TreeWalker}.


cycle_ast(Names, Context, TreeWalker) ->
    {NamesTuple, VarNames}
        = lists:mapfoldl(
            fun ({string_literal, _, Str}, VarNamesAcc) ->
                    {{S, _}, _} = string_ast(unescape_string_literal(Str), Context, TreeWalker),
                    {S, VarNamesAcc};
                ({variable, _}=Var, VarNamesAcc) ->
                    {{V, #ast_info{ var_names=[VarName] }}, _} = resolve_variable_ast(Var, Context, TreeWalker, true),
                    {V, [VarName|VarNamesAcc]};
                ({number_literal, _, Num}, VarNamesAcc) ->
                    {format(erl_syntax:integer(Num), Context, TreeWalker), VarNamesAcc};
                (_, VarNamesAcc) ->
                    {[], VarNamesAcc}
            end, [], Names),
    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), resolve_scoped_variable_ast('forloop', Context)]),
      #ast_info{ var_names = VarNames }},
     TreeWalker}.

%% Older Django templates treat cycle with comma-delimited elements as strings
cycle_compat_ast(Names, Context, TreeWalker) ->
    NamesTuple = lists:map(
                   fun ({identifier, _, X}) ->
                           {{S, _}, _} = string_ast(X, Context, TreeWalker),
                           S
                   end, Names),
    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), resolve_scoped_variable_ast('forloop', Context)]),
      #ast_info{}},
     TreeWalker}.

now_ast(FormatString, Context, TreeWalker) ->
    %% Note: we can't use unescape_string_literal here
    %% because we want to allow escaping in the format string.
    %% We only want to remove the surrounding escapes,
    %% i.e. \"foo\" becomes "foo"
    UnescapeOuter = string:strip(FormatString, both, 34),
    {{StringAst, Info}, TreeWalker1} = string_ast(UnescapeOuter, Context, TreeWalker),
    {{erl_syntax:application(
        erl_syntax:atom(erlydtl_dateformat),
        erl_syntax:atom(format),
        [StringAst]), Info}, TreeWalker1}.

spaceless_ast(Contents, Context, TreeWalker) ->
    {{Ast, Info}, TreeWalker1} = body_ast(Contents, Context, TreeWalker),
    {{erl_syntax:application(
        erl_syntax:atom(erlydtl_runtime),
        erl_syntax:atom(spaceless),
        [Ast]), Info}, TreeWalker1}.

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\n | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\r | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\t | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).


full_path(File, DocRoot) ->
    case filename:absname(File) of
        File -> File;
        _ -> filename:join([DocRoot, File])
    end.

%%-------------------------------------------------------------------
%% Custom tags
%%-------------------------------------------------------------------

interpret_value({trans, StringLiteral}, Context, TreeWalker) ->
    translated_ast(StringLiteral, Context, TreeWalker);
interpret_value(Value, Context, TreeWalker) ->
    value_ast(Value, false, false, Context, TreeWalker).

interpret_args(Args, Context, TreeWalker) ->
    lists:foldr(
      fun ({{identifier, _, Key}, Value}, {{ArgsAcc, AstInfoAcc}, TreeWalkerAcc}) ->
              {{Ast0, AstInfo0}, TreeWalker0} = interpret_value(Value, Context, TreeWalkerAcc),
              {{[erl_syntax:tuple([erl_syntax:atom(Key), Ast0])|ArgsAcc], merge_info(AstInfo0, AstInfoAcc)}, TreeWalker0};
          (Value, {{ArgsAcc, AstInfoAcc}, TreeWalkerAcc}) ->
              {{Ast0, AstInfo0}, TreeWalker0} = value_ast(Value, false, false, Context, TreeWalkerAcc),
              {{[Ast0|ArgsAcc], merge_info(AstInfo0, AstInfoAcc)}, TreeWalker0}
      end, {{[], #ast_info{}}, TreeWalker}, Args).

tag_ast(Name, Args, Context, TreeWalker) ->
    {{InterpretedArgs, AstInfo1}, TreeWalker1} = interpret_args(Args, Context, TreeWalker),
    {RenderAst, RenderInfo} = custom_tags_modules_ast(Name, InterpretedArgs, Context),
    {{RenderAst, merge_info(AstInfo1, RenderInfo)}, TreeWalker1}.

custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [], is_compiling_dir = false }) ->
    {erl_syntax:application(none, erl_syntax:atom(render_tag),
                            [erl_syntax:atom(Name), erl_syntax:list(InterpretedArgs),
                             erl_syntax:variable("RenderOptions")]),
     #ast_info{custom_tags = [Name]}};
custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [], is_compiling_dir = true, module = Module }) ->
    {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name),
                            [erl_syntax:list(InterpretedArgs), erl_syntax:variable("RenderOptions")]),
     #ast_info{ custom_tags = [Name] }};
custom_tags_modules_ast(Name, InterpretedArgs, #dtl_context{ custom_tags_modules = [Module|Rest] } = Context) ->
    try lists:max([I || {N,I} <- Module:module_info(exports), N =:= Name]) of
        2 ->
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name),
                                    [erl_syntax:list(InterpretedArgs),
                                     erl_syntax:variable("RenderOptions")]), #ast_info{}};
        1 ->
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Name),
                                    [erl_syntax:list(InterpretedArgs)]), #ast_info{}};
        I ->
            throw({unsupported_custom_tag_fun, {Module, Name, I}})
    catch _:function_clause ->
            custom_tags_modules_ast(Name, InterpretedArgs,
                                    Context#dtl_context{ custom_tags_modules = Rest })
    end.

call_ast(Module, TreeWalkerAcc) ->
    call_ast(Module, erl_syntax:variable("_Variables"), #ast_info{}, TreeWalkerAcc).

call_with_ast(Module, Variable, Context, TreeWalker) ->
    {{VarAst, VarInfo}, TreeWalker2} = resolve_variable_ast(Variable, Context, TreeWalker, false),
    call_ast(Module, VarAst, VarInfo, TreeWalker2).

call_ast(Module, Variable, AstInfo, TreeWalker) ->
    AppAst = erl_syntax:application(
               erl_syntax:atom(Module),
               erl_syntax:atom(render),
               [Variable, erl_syntax:variable("RenderOptions")]),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
              [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])],
              none,
              [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
                  erl_syntax:atom(io_lib),
                  erl_syntax:atom(format),
                  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
                 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])],
                 none,
                 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),
    with_dependencies(Module:dependencies(), {{CallAst, AstInfo}, TreeWalker}).


print(Fmt, Args, #dtl_context{ verbose = true }) -> io:format(Fmt, Args);
print(_Fmt, _Args, _Context) -> ok.

get_current_file(#dtl_context{ parse_trail=[File|_] }) -> File;
get_current_file(#dtl_context{ doc_root=Root }) -> Root.

add_error(Error, #dtl_context{
                    errors=#error_info{ report=Report, list=Es }=Ei
                   }=Context) ->
    Item = get_error_item(
             Report, "",
             get_current_file(Context),
             Error),
    Context#dtl_context{
      errors=Ei#error_info{ list=[Item|Es] }
     }.

add_errors(Errors, Context) ->
    lists:foldl(
      fun (E, C) -> add_error(E, C) end,
      Context, Errors).

add_warning(Warning, #dtl_context{ warnings=warnings_as_errors }=Context) ->
    add_error(Warning, Context);
add_warning(Warning, #dtl_context{
                        warnings=#error_info{ report=Report, list=Ws }=Wi
                       }=Context) ->
    Item = get_error_item(
             Report, "Warning: ",
             get_current_file(Context),
             Warning),
    Context#dtl_context{
      warnings=Wi#error_info{ list=[Item|Ws] }
     }.

add_warnings(Warnings, Context) ->
    lists:foldl(
      fun (W, C) -> add_warning(W, C) end,
      Context, Warnings).

get_error_item(Report, Prefix, File, Error) ->
    case compose_error_desc(Error) of
        {Pos, Module, ErrorDesc} ->
            new_error_item(Report, Prefix, File, Pos, Module, ErrorDesc);
        ErrorItem ->
            ErrorItem
    end.

compose_error_desc({Line, ErrorDesc})
  when is_integer(Line) ->
    {Line, ?MODULE, ErrorDesc};
compose_error_desc({{Line, Col}, Module, _}=ErrorDesc)
  when is_integer(Line), is_integer(Col), is_atom(Module) ->
    ErrorDesc;
compose_error_desc({Line, Module, _}=ErrorDesc)
  when is_integer(Line), is_atom(Module) ->
    ErrorDesc;
compose_error_desc({_, InfoList}=ErrorDesc)
  when is_list(InfoList) -> ErrorDesc;
compose_error_desc(ErrorDesc) ->
    {none, ?MODULE, ErrorDesc}.

new_error_item(Report, Prefix, File, Pos, Module, ErrorDesc) ->
    if Report  ->
            io:format("~s:~s~s~s~n",
                      [File, pos_info(Pos), Prefix,
                       Module:format_error(ErrorDesc)]);
       true -> nop
    end,
    {File, [{Pos, Module, ErrorDesc}]}.

pos_info(none) -> " ";
pos_info(Line) when is_integer(Line) ->
    io_lib:format("~b: ", [Line]);
pos_info({Line, Col}) when is_integer(Line), is_integer(Col) ->
    io_lib:format("~b:~b ", [Line, Col]).

pack_error_list(Es) ->
    collect_error_info([], Es, []).

collect_error_info([], [], Acc) ->
    lists:reverse(Acc);
collect_error_info([{File, ErrorInfo}|Es], Rest, [{File, FEs}|Acc]) ->
    collect_error_info(Es, Rest, [{File, ErrorInfo ++ FEs}|Acc]);
collect_error_info([E|Es], Rest, Acc) ->
    collect_error_info(Es, [E|Rest], Acc);
collect_error_info([], Rest, Acc) ->
    case lists:reverse(Rest) of
        [E|Es] ->
            collect_error_info(Es, [], [E|Acc])
    end.
