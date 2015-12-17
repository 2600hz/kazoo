%%%-------------------------------------------------------------------
%%% File:      erlydtl.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @copyright 2014 Andreas Stenius
%%% @doc
%%% Public interface for ErlyDTL
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2008 Roberto Saccon, Evan Miller
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
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%% @since 2014 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(erlydtl).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('Andreas Stenius <kaos@astekk.se>').


%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------

-export([compile_file/2, compile_file/3]).
-export([compile_template/2, compile_template/3]).
-export([compile/2, compile/3]).
-export([compile_dir/2, compile_dir/3]).

-type position() :: non_neg_integer().
-type location() :: none | position() | {Line::position(), Column::position()}.
-type error_info() :: {File::list(),
                       [{location(),
                         Module::atom(),
                         ErrorDesc::term()}]}.
-type errors() :: list(error_info()).
-type warnings() :: list(error_info()).
-type ok_ret() :: {ok, Module::atom()} | {ok, Module::atom(), warnings()}.
-type err_ret() :: error | {error, errors(), warnings()}.

-type filename() :: file:name_all().

-type compiler_options() :: compiler_option() | compile:option().
-type compiler_option() :: return | return_warnings | return_errors
                        | report | report_warnings | report_errors
                        | warnings_as_errors | debug_info | verbose.

-type compile_options() :: [compile_option() | {atom(), term()}].
-type compile_option() :: compiler_option()
                        | auto_escape | binary | binary_strings
                        | force_recompile | no_env | no_load
                        | {blocktrans_fun, Trans::fun((Block::iodata(), Locale::string()) ->
                                                             iodata() | default)}
                        | {blocktrans_locales, [string()]}
                        | {compiler_options, [compiler_options()]}
                        | {custom_filters_modules, [Module::atom]}
                        | {custom_tags_dirs, [filename()]}
                        | {custom_tags_modules, [Module::atom]}
                        | {default_libraries, [Name::atom()]}
                        | {doc_root, filename()}
                        | {extension_module, Module::atom()}
                        | {libraries, [{Name::atom(), Module::atom()}]}
                        | {locale, string()}
                        | {out_dir, false | filename()}
                        | {reader, {Module::atom(), Function::atom}}
                        | {record_info, [{Name::atom(), [Field::atom()]}]}
                        | {scanner_module, Module::atom()}
                        | {vars, [{atom(), iodata()}]}.


%% --------------------------------------------------------------------
%% Compile file
%% --------------------------------------------------------------------

-spec compile_file(filename(), atom()) -> {ok, Module::atom()} | error.
compile_file(File, Module) ->
    erlydtl_compiler:compile_file(File, Module, erlydtl_compiler:default_options()).

-spec compile_file(filename(), atom(), compile_options()) -> ok_ret() | err_ret().
compile_file(File, Module, Options) ->
    erlydtl_compiler:compile_file(File, Module, Options).


%% --------------------------------------------------------------------
%% Compile template
%% --------------------------------------------------------------------

-spec compile_template(iodata(), atom()) -> {ok, Module::atom()} | error.
compile_template(Template, Module) ->
    erlydtl_compiler:compile_template(Template, Module, erlydtl_compiler:default_options()).

-spec compile_template(iodata(), atom(), compile_options()) -> ok_ret() | err_ret().
compile_template(Template, Module, Options) ->
    erlydtl_compiler:compile_template(Template, Module, Options).


%% --------------------------------------------------------------------
%% Compile directory
%% --------------------------------------------------------------------

-spec compile_dir(filename(), atom()) -> {ok, Module::atom()} | error.
compile_dir(DirectoryPath, Module) ->
    erlydtl_compiler:compile_dir(DirectoryPath, Module, erlydtl_compiler:default_options()).

-spec compile_dir(filename(), atom(), compile_options()) -> ok_ret() | err_ret().
compile_dir(DirectoryPath, Module, Options) ->
    erlydtl_compiler:compile_dir(DirectoryPath, Module, Options).


%% --------------------------------------------------------------------
%% Legacy API
%% --------------------------------------------------------------------

%% keep for backwards compatibility, with a tuple-twist to ease migration / offer alternative path..
-spec compile(FileOrTemplate, atom()) -> {ok, Module::atom()} | error
                                           when FileOrTemplate :: string() | binary()
                                                              | {file, filename()}
                                                              | {template, iodata()}
                                                              | {dir, filename()}.
compile({file, File}, Module) ->
    compile_file(File, Module);
compile({template, Template}, Module) ->
    compile_template(Template, Module);
compile({dir, Directory}, Module) ->
    compile_dir(Directory, Module);
compile(FileOrTemplate, Module) when is_binary(FileOrTemplate) ->
    compile_template(FileOrTemplate, Module);
compile(FileOrTemplate, Module) ->
    compile_file(FileOrTemplate, Module).

-spec compile(FileOrTemplate, atom(), compile_options() ) -> ok_ret() | err_ret()
                                                               when FileOrTemplate :: string() | binary()
                                                                                  | {file, filename()}
                                                                                  | {template, iodata()}
                                                                                  | {dir, filename()}.
compile({file, File}, Module, Options) ->
    compile_file(File, Module, Options);
compile({template, Template}, Module, Options) ->
    compile_template(Template, Module, Options);
compile({dir, Directory}, Module, Options) ->
    compile_dir(Directory, Module, Options);
compile(FileOrTemplate, Module, Options) when is_binary(FileOrTemplate) ->
    compile_template(FileOrTemplate, Module, Options);
compile(FileOrTemplate, Module, Options) ->
    compile_file(FileOrTemplate, Module, Options).
