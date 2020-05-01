%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_metaflow_tests).

-include_lib("eunit/include/eunit.hrl").

flow_routing_key_test_() ->
    Test = fun kapi_metaflow:flow_routing_key/1,
    Prefix = <<"metaflow.flow.">>,
    CallId = kz_binary:rand_hex(4),

    [{"CallId must be binary"
     ,?_assertEqual(<<Prefix/binary, CallId/binary>>, Test(CallId))
     }
    ,{"CallId must be binary"
     ,?_assertException('error', 'function_clause', Test(binary_to_list(CallId)))
     }
    ].

action_routing_key_test_() ->
    Test = fun kapi_metaflow:action_routing_key/2,
    Prefix = <<"metaflow.action.">>,
    CallId = kz_binary:rand_hex(4),
    Action = kz_binary:rand_hex(4),

    [{"CallId and Action must be binary"
     ,?_assertEqual(<<Prefix/binary, CallId/binary, ".", Action/binary>>, Test(CallId, Action))
     }
    ,{"CallId must be binary"
     ,?_assertException('error', 'function_clause', Test(binary_to_list(CallId), Action))
     }
    ,{"Action must be binary"
     ,?_assertException('error', 'badarg', Test(CallId, binary_to_list(Action)))
     }
    ].

bind_req_routing_key_test_() ->
    Test = fun kapi_metaflow:bind_req_routing_key/1,
    Prefix = <<"metaflow.bind_req.">>,
    AccountId = kz_binary:rand_hex(4),

    [{"AccountId must be binary"
     ,?_assertEqual(<<Prefix/binary, AccountId/binary>>, Test(AccountId))
     }
    ,{"AccountId must be binary"
     ,?_assertException('error', 'function_clause', Test(binary_to_list(AccountId)))
     }
    ].

binding_routing_key_test_() ->
    Test = fun kapi_metaflow:binding_routing_key/2,
    Prefix = <<"metaflow.bind.">>,
    AccountId = kz_binary:rand_hex(4),
    CallId = kz_binary:rand_hex(4),

    [{"AccountId and CallId must be binary"
     ,?_assertEqual(<<Prefix/binary, AccountId/binary, ".", CallId/binary>>
                   ,Test(AccountId, CallId)
                   )
     }
    ,{"AccountId must be binary"
     ,?_assertException('error', 'function_clause', Test(binary_to_list(AccountId), CallId))
     }
    ,{"CallId must be binary"
     ,?_assertException('error', 'function_clause', Test(AccountId, binary_to_list(CallId)))
     }
    ].
