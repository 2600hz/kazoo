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
-export([parse_file/2, parse_template/2, do_parse_template/2]).

-import(erlydtl_compiler_utils,
         [add_filters/2, add_tags/2, call_extension/3,
         load_library/2, shorten_filename/1, get_current_file/1]).

-include("erlydtl_ext.hrl").


%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------

default_options() -> [verbose, report].

compile_template(Template, Module, Options) ->
    Context = process_opts(undefined, Module, Options),
    Bin = iolist_to_binary(Template),
    ?LOG_INFO("Compile template: ~32s~s~n",
              [Bin, if size(Bin) > 32 -> "...";
                       true -> ""
                    end],
              Context),
    compile(Context#dtl_context{ bin = Bin }).

compile_file(File, Module, Options) ->
    Context = process_opts(File, Module, Options),
    ?LOG_INFO("Compile file: ~s~n", [File], Context),
    compile(Context).

compile_dir(Dir, Module, Options) ->
    Context = process_opts({dir, Dir}, Module, Options),
    ?LOG_INFO("Compile directory: ~s~n", [Dir], Context),
    compile(Context).


format_error({read_file, Error}) ->
    io_lib:format(
      "Failed to read file: ~s",
      [file:format_error(Error)]);
format_error({read_file, File, Error}) ->
    io_lib:format(
      "Failed to include file ~s: ~s",
      [File, file:format_error(Error)]);
format_error({deprecated_option, Opt, NewOpt}) ->
    io_lib:format(
      "Compile option '~s' has been deprecated. Use '~s' instead.",
      [Opt, NewOpt]);
format_error(Error) ->
    erlydtl_compiler_utils:format_error(Error).


%%====================================================================
%% Internal functions
%%====================================================================

process_opts(File, Module, Options0) ->
    Options1 = proplists:normalize(
                 update_defaults(Options0),
                 [{aliases, deprecated_opts()}
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
    Context =
        case File of
            {dir, _} ->
                init_context([], Source, Module, Options);
            _ ->
                init_context([Source], filename:dirname(Source), Module, Options)
        end,

    %% check original options here now that we have a context to
    %% process any warnings/errors generated.
    check_opts(Options0, Context).

deprecated_opts() ->
    [{outdir, out_dir},
     {vars, default_vars},
     {blocktrans_fun, translation_fun},
     {blocktrans_locales, locales}].

check_opts(Options, Context) ->
    lists:foldl(
      fun ({Opt, NewOpt}, Ctx) ->
              case proplists:get_value(Opt, Options) of
                  undefined -> Ctx;
                  _ -> ?WARN({deprecated_option, Opt, NewOpt}, Ctx)
              end
      end, Context, deprecated_opts()).

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


init_context(ParseTrail, DefDir, Module, Options) when is_list(Module) ->
    init_context(ParseTrail, DefDir, list_to_atom(Module), Options);
init_context(ParseTrail, DefDir, Module, Options) ->
    Ctx = #dtl_context{},
    Locales = lists:usort(
                lists:concat(
                  [proplists:get_all_values(locale, Options),
                   proplists:get_value(locales, Options, Ctx#dtl_context.trans_locales)]
                 )),
    Context0 =
        #dtl_context{
           all_options = Options,
           auto_escape = case proplists:get_value(auto_escape, Options, true) of
                             true -> [on];
                             _ -> [off]
                         end,
           parse_trail = ParseTrail,
           module = Module,
           doc_root = proplists:get_value(doc_root, Options, DefDir),
           libraries = proplists:get_value(libraries, Options, Ctx#dtl_context.libraries),
           custom_tags_dir = proplists:get_value(
                               custom_tags_dir, Options,
                               filename:join([erlydtl_deps:get_base_dir(), "priv", "custom_tags"])),
           trans_fun = erlydtl_runtime:init_translation(
                         proplists:get_value(translation_fun, Options, Ctx#dtl_context.trans_fun)),
           trans_locales = Locales,
           vars = proplists:get_value(default_vars, Options, Ctx#dtl_context.vars),
           const = proplists:get_value(constants, Options, Ctx#dtl_context.const),
           reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
           compiler_options = proplists:append_values(compiler_options, Options),
           binary_strings = proplists:get_value(binary_strings, Options, Ctx#dtl_context.binary_strings),
           force_recompile = proplists:get_bool(force_recompile, Options),
           verbose = length(proplists:get_all_values(verbose, Options)),
           is_compiling_dir = if ParseTrail == [] -> DefDir; true -> false end,
           extension_module = proplists:get_value(extension_module, Options, Ctx#dtl_context.extension_module),
           scanner_module = proplists:get_value(scanner_module, Options, Ctx#dtl_context.scanner_module),
           record_info = [{R, lists:zip(I, lists:seq(2, length(I) + 1))}
                          || {R, I} <- proplists:get_value(record_info, Options, Ctx#dtl_context.record_info)],
           errors = init_error_info(errors, Ctx#dtl_context.errors, Options),
           warnings = init_error_info(warnings, Ctx#dtl_context.warnings, Options),
           lists_0_based = proplists:get_value(lists_0_based, Options, Ctx#dtl_context.lists_0_based),
           tuples_0_based = proplists:get_value(tuples_0_based, Options, Ctx#dtl_context.tuples_0_based)
          },
    Context = load_libraries(proplists:get_value(default_libraries, Options, []), Context0),
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

load_libraries([], #dtl_context{ all_options=Options }=Context) ->
    %% Load filters and tags passed using the old options
    Filters = proplists:get_value(custom_filters_modules, Options, []) ++ [erlydtl_filters],
    Tags = proplists:get_value(custom_tags_modules, Options, []),
    load_legacy_filters(Filters, load_legacy_tags(Tags, Context));
load_libraries([Lib|Libs], Context) ->
    load_libraries(Libs, load_library(Lib, Context)).

load_legacy_filters([], Context) -> Context;
load_legacy_filters([Mod|Mods], Context) ->
    {Filters, Context1} = read_legacy_library(Mod, Context),
    load_legacy_filters(Mods, add_filters(Filters, Context1)).

load_legacy_tags([], Context) -> Context;
load_legacy_tags([Mod|Mods], Context) ->
    {Tags, Context1} = read_legacy_library(Mod, Context),
    load_legacy_tags(Mods, add_tags(Tags, Context1)).

read_legacy_library(Mod, Context) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            {[{Name, {Mod, Name}}
              || {Name, _} <- lists:ukeysort(1, Mod:module_info(exports)),
                 Name =/= module_info
             ], Context};
        {error, Reason} ->
            {[], ?WARN({load_library, '(custom-legacy)', Mod, Reason}, Context)}
    end.

is_up_to_date(_, #dtl_context{force_recompile = true}) ->
    false;
is_up_to_date(CheckSum, Context) ->
    erlydtl_beam_compiler:is_up_to_date(CheckSum, Context).

parse_file(File, Context) ->
    {M, F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            parse_template(Data, Context);
        {error, Reason} ->
            {error, {read_file, File, Reason}}
    end.

parse_template(Data, Context) ->
    CheckSum = binary_to_list(erlang:md5(Data)),
    case is_up_to_date(CheckSum, Context) of
        true -> up_to_date;
        false ->
            case do_parse_template(Data, Context) of
                {ok, Val} -> {ok, Val, CheckSum};
                Err -> Err
            end
    end.

do_parse_template(Data, #dtl_context{ scanner_module=Scanner }=Context) ->
    check_scan(
      apply(Scanner, scan, [Data]),
      Context).

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


do_compile(#dtl_context{ is_compiling_dir=false, bin=undefined }=Context) ->
    compile_output(parse_file(get_current_file(Context), Context), Context);
do_compile(#dtl_context{ is_compiling_dir=false, bin=Template }=Context) ->
    compile_output(parse_template(Template, Context), Context);
do_compile(#dtl_context{ is_compiling_dir=Dir }=Context) ->
    erlydtl_beam_compiler:compile_dir(Dir, Context).


compile_output(up_to_date, Context) -> Context;
compile_output({ok, DjangoParseTree, CheckSum}, Context) ->
    erlydtl_beam_compiler:compile(DjangoParseTree, CheckSum, Context#dtl_context{ bin=undefined });
compile_output({error, Reason}, Context) -> ?ERR(Reason, Context).
