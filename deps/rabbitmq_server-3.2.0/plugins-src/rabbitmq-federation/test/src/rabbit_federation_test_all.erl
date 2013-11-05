%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ
%%
%%   The Initial Developer of the Original Code is GoPivotal, Inc.
%%   Copyright (c) 2010-2013 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_federation_test_all).

-export([all_tests/0]).

all_tests() ->
    ok = eunit:test(tests(rabbit_federation_unit_test, 60), [verbose]),
    ok = eunit:test(tests(rabbit_federation_exchange_test, 60), [verbose]),
    ok = eunit:test(tests(rabbit_federation_queue_test, 60), [verbose]).

tests(Module, Timeout) ->
    {foreach, fun() -> ok end,
     [{timeout, Timeout, fun Module:F/0} ||
         {F, _Arity} <- proplists:get_value(exports, Module:module_info()),
         string:right(atom_to_list(F), 5) =:= "_test"]}.
