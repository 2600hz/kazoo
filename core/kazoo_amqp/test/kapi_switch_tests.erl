%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_switch_tests).

-include_lib("eunit/include/eunit.hrl").

notify_routing_key_test_() ->
    Test = fun kapi_switch:notify_routing_key/2,
    Prefix = <<"switch.notify.">>,
    Realm = kz_binary:rand_hex(4),
    Username = kz_binary:rand_hex(4),

    [{"Realm and Username must be binary"
     ,?_assertEqual(<<Prefix/binary, Realm/binary, ".", Username/binary>>, Test(Realm, Username))
     }
    ,{"Realm must be binary"
     ,?_assertException('error', 'function_clause', Test(binary_to_list(Realm), Username))
     }
    ,{"Username must be binary"
     ,?_assertException('error', 'function_clause', Test(Realm, binary_to_list(Username)))
     }
    ].

fs_command_routing_key_test_() ->
    Test = fun kapi_switch:fs_command_routing_key/1,
    Prefix = <<"switch.command.">>,
    Node = kz_binary:rand_hex(4),

    [{"Node must be binary"
     ,?_assertEqual(<<Prefix/binary, Node/binary>>, Test(Node))
     }
    ,{"Node must be binary"
     ,?_assertException('error', 'function_clause', Test(binary_to_list(Node)))
     }
    ].
