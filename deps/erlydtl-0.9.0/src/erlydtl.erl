%%%-------------------------------------------------------------------
%%% File:      erlydtl.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc
%%% Public interface for ErlyDTL
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2008 Roberto Saccon, Evan Miller
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
%%%-------------------------------------------------------------------
-module(erlydtl).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

%% API
-export([compile_file/2, compile_file/3]).
-export([compile_template/2, compile_template/3]).
-export([compile/2, compile/3]).
-export([compile_dir/2, compile_dir/3]).

-type error_info() :: {File::list(),
                       [{Line::integer() | none,
                         Module::atom(),
                         ErrorDesc::term()}]}.
-type errors() :: list(error_info()).
-type warnings() :: list(error_info()).
-type ok_ret() :: {ok, Module::atom()} | {ok, Module::atom(), warnings()}.
-type err_ret() :: error | {error, errors(), warnings()}.

-spec compile_file( list() | binary(), atom() ) -> {ok, Module::atom()} | error.
compile_file(File, Module) ->
    erlydtl_compiler:compile_file(File, Module, erlydtl_compiler:default_options()).

-spec compile_file( list() | binary(), atom(), list() ) -> ok_ret() | err_ret().
compile_file(File, Module, Options) ->
    erlydtl_compiler:compile_file(File, Module, Options).

-spec compile_template( list() | binary(), atom() ) -> {ok, Module::atom()} | error.
compile_template(Template, Module) ->
    erlydtl_compiler:compile_template(Template, Module, erlydtl_compiler:default_options()).

-spec compile_template( list() | binary(), atom(), list() ) -> ok_ret() | err_ret().
compile_template(Template, Module, Options) ->
    erlydtl_compiler:compile_template(Template, Module, Options).

-spec compile_dir(list() | binary(), atom()) -> {ok, Module::atom()} | error.
compile_dir(DirectoryPath, Module) ->
    erlydtl_compiler:compile_dir(DirectoryPath, Module, erlydtl_compiler:default_options()).

-spec compile_dir(list() | binary(), atom(), list()) -> ok_ret() | err_ret().
compile_dir(DirectoryPath, Module, Options) ->
    erlydtl_compiler:compile_dir(DirectoryPath, Module, Options).


%% keep for backwards compatibility, with a tuple-twist to ease migration / offer alternative path..
-spec compile(FileOrBinary, atom() ) -> {ok, Module::atom()} | error
                                            when FileOrBinary :: list() | binary() 
                                                               | {file, list() | binary()}
                                                               | {template, list() | binary()}.
compile({file, File}, Module) ->
    compile_file(File, Module);
compile({template, Template}, Module) ->
    compile_template(Template, Module);
compile(FileOrBinary, Module) when is_binary(FileOrBinary) ->
    compile_template(FileOrBinary, Module);
compile(FileOrBinary, Module) ->
    compile_file(FileOrBinary, Module).

-spec compile( FileOrBinary, atom(), list() ) -> ok_ret() | err_ret()
                                                     when FileOrBinary :: list() | binary() 
                                                                        | {file, list() | binary()}
                                                                        | {template, list() | binary()}.
compile({file, File}, Module, Options) ->
    compile_file(File, Module, Options);
compile({template, Template}, Module, Options) ->
    compile_template(Template, Module, Options);
compile(FileOrBinary, Module, Options) when is_binary(FileOrBinary) ->
    compile_template(FileOrBinary, Module, Options);
compile(FileOrBinary, Module, Options) ->
    compile_file(FileOrBinary, Module, Options).
