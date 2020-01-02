%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_api_endpoint_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("crossbar/src/crossbar.hrl").

sort_methods_test_() ->
    Tests =
        [{[?HTTP_GET, ?HTTP_DELETE], [?HTTP_GET, ?HTTP_DELETE]}
        ,{[?HTTP_GET, ?HTTP_PUT], [?HTTP_GET, ?HTTP_PUT]}
        ,{[?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE], [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE]}
        ],
    lists:foldl(fun create_test_generator/2, [], Tests).

create_test_generator({Input, Output}, Tests) ->
    lists:foldl(fun(Perm, Acc) -> add_generator(Perm, Output, Acc) end
               ,Tests
               ,perms(Input)
               ).

add_generator(Input, Output, Tests) ->
    [?_assertEqual(Output, cb_api_endpoints:sort_methods(Input)) | Tests].

%% from http://erlang.org/doc/programming_examples/list_comprehensions.html
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].
