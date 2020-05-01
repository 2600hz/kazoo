%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_callflow_tests).

-include_lib("eunit/include/eunit.hrl").

action_routing_key_test_() ->
    Test = fun kapi_callflow:action_routing_key/1,
    Prefix = <<"callflow.action.">>,
    Action = kz_binary:rand_hex(4),

    [{"Action must be binary"
     ,?_assertEqual(<<Prefix/binary, Action/binary>>, Test(Action))
     }
    ,{"Action must be binary"
     ,?_assertException('error'
                       ,'badarg'
                       ,Test(binary_to_list(Action))
                       )
     }
    ].
