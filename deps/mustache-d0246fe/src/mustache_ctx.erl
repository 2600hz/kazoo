%% The MIT License
%%
%% Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
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

%% See the README at http://github.com/mojombo/mustache.erl for additional
%% documentation and usage examples.

-module(mustache_ctx).

-define(MODULE_KEY, '__mod__').
-define(NEW_EXIT(Data), exit({improper_ctx, Data})).

-export([ new/0, new/1, to_list/1 ]).
-export([ merge/2 ]).
-export([ module/1, module/2 ]).
-export([ get/2 ]).

-ifdef(EUNIT).
-compile(export_all).
-endif.

%% ===================================================================
%% Create new context
%% ===================================================================

new() -> new([]).

new(List) when is_list(List) ->
    try dict:from_list(List)
    catch
        _:_ -> ?NEW_EXIT(List)
    end;
new(Data) when is_tuple(Data) ->
    case erlang:element(1, Data) of
        dict -> Data;
        _ -> ?NEW_EXIT(Data)
    end;
new(Data) ->
    ?NEW_EXIT(Data).

to_list(Ctx) ->
    List = dict:to_list(Ctx),
    lists:keydelete(?MODULE_KEY, 1, List).

%% ===================================================================
%% Merge
%% ===================================================================

merge(Ctx1, Ctx2) ->
    dict:merge(fun(_, Value1, _) -> Value1 end, Ctx1, Ctx2).


%% ===================================================================
%% Dynamic data module
%% ===================================================================

module(Ctx) ->
    case dict:find(?MODULE_KEY, Ctx) of
        {ok, Module} -> {ok, Module};
        error -> {error, module_not_set}
    end.

module(Module, Ctx) ->
    dict:store(?MODULE_KEY, Module, Ctx).

%% ===================================================================
%% Module
%% ===================================================================

get(Key, Ctx) ->
    case dict:find(Key, Ctx) of
        {ok, Value} -> {ok, Value};
        error ->
            get_from_module(Key, Ctx)
    end.

get_from_module(Key, Ctx) ->
    FunList = case module(Ctx) of
        {error, _} -> [];
        {ok, Module} -> [
                fun() -> Module:Key(Ctx) end,
                fun() -> Module:Key() end
            ]
    end,
    get_from_module(FunList).

get_from_module([]) -> {error, not_found};
get_from_module([ Fun | Rest ]) ->
    try Value = Fun(),
        {ok, Value}
    catch
        _:_ ->
        get_from_module(Rest)
    end.

