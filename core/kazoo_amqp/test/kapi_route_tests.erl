%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_route_tests).

-include_lib("eunit/include/eunit.hrl").
-include("src/api/kapi_route.hrl"). %% ?KEY_ROUTE_REQ

get_route_req_account_routing_test_() ->
    Test = fun kapi_route:get_route_req_account_routing/2,
    Prefix = ?KEY_ROUTE_REQ,
    Type = kz_binary:rand_hex(4),
    AccountId = kz_binary:rand_hex(4),

    [{"Type and AccountId must be binary"
     ,?_assertEqual(<<Prefix/binary, ".", Type/binary, ".", AccountId/binary>>
                   ,Test(Type, AccountId)
                   )
     }
    ,{"Type must be binary"
     ,?_assertException('error'
                       ,'function_clause'
                       ,Test(binary_to_list(Type), AccountId)
                       )
     }
    ,{"AccountId must be binary"
     ,?_assertException('error'
                       ,'function_clause'
                       ,Test(Type, binary_to_list(AccountId))
                       )
     }
    ].

get_route_req_realm_routing_test_() ->
    Test = fun kapi_route:get_route_req_realm_routing/3,
    Prefix = ?KEY_ROUTE_REQ,
    Type = kz_binary:rand_hex(4),
    Realm = kz_binary:rand_hex(4),
    User = kz_binary:rand_hex(4),

    [{"Type, Realm and User must be binary"
     ,?_assertEqual(<<Prefix/binary, ".", Type/binary, ".", Realm/binary, ".", User/binary>>
                   ,Test(Type, Realm, User)
                   )
     }
    ,{"Type must be binary"
     ,?_assertException('error'
                       ,'function_clause'
                       ,Test(binary_to_list(Type), Realm, User)
                       )
     }
    ,{"Realm must be binary"
     ,?_assertException('error'
                       ,'function_clause'
                       ,Test(Type, binary_to_list(Realm), User)
                       )
     }
    ,{"User must be binary"
     ,?_assertException('error'
                       ,'function_clause'
                       ,Test(Type, Realm, binary_to_list(User))
                       )
     }
    ].
