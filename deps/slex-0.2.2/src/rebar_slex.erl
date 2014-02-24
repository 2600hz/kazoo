%%%-------------------------------------------------------------------
%%% File:      rebar_tsd.erl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% slex compiler plugin for rebar.
%%% @end
%%%
%%% Copyright 2013 Andreas Stenius
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @since 2013-11-05 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(rebar_slex).
-export([pre_compile/2]).

%% for internal use only
-export([info/2]).

%% rebar.hrl
-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
%% end rebar.hrl


%% ===================================================================
%% Public API
%% ===================================================================

pre_compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config,
                            [],
                            "src", ".slex", "src", ".erl",
                            fun compile_slex/3).


%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    ?CONSOLE(
       "Compile slex (*.slex) sources to Erlang sources (*.erl).~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n",
       [
        undefined
       ]).

compile_slex(Source, Target, Config) ->
    try slex_compiler:compile(Source, [{target, erl}, {out_dir, src}]) of
        {ok, Target} -> ok;
        {error, Error} ->
            ?DEBUG("compile ~p -> ~p ~n  fail: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Config, Source, [{Source, [Error]}], [], [])
    catch
        throw:Error ->
            ?DEBUG("compile ~p -> ~p ~n  throw: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Config, Source, [{Source, [Error]}], [], [])
    end.
