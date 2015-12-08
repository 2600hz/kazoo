%% -----------------------------------------------------------------------------
%%
%% Hamcrest Erlang.
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2010 Tim Watson.
%%
%% Generates hamcrest.hrl header file during build process. Loaded by rebar.
%% -----------------------------------------------------------------------------
-module(eqc_resolver).

-export([preprocess/2]).

preprocess(Config, _) ->
    case rebar_utils:processing_base_dir(Config) of
        false -> {ok, []};
        true  -> case code:lib_dir(eqc, include) of
                     {error, bad_name} ->
                         write_include("proper"),
                         {ok, []};
                     _ ->
                         write_include("eqc"),
                         Deps = rebar_config:get_local(Config, deps, []),
                         DepsNoPropEr = proplists:delete(proper, Deps),
                         Config2 = rebar_config:set(Config, deps, DepsNoPropEr),
                         {ok, Config2, []}
                 end
    end.

write_include(Lib) ->
    file:write_file(filename:join(test_dir(), "qc.hrl"),
                    include(Lib)).

test_dir() ->
    filename:join(rebar_utils:get_cwd(), "test").

include(Lib) ->
    [<<"-include_lib(\"">>,
     Lib, <<"/include/">>,
     Lib, <<".hrl\").\n">>,
     <<"-define(">>, Lib, <<", true).">>].
