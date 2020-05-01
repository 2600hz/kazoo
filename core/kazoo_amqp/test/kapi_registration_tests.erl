%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_registration_tests).

-include_lib("eunit/include/eunit.hrl").

get_success_routing_test_() ->
    Test = fun kapi_registration:get_success_routing/2,
    Prefix = <<"registration.success.">>,
    Realm = kz_binary:rand_hex(4),
    User = kz_binary:rand_hex(4),

    [{"Realm and User must be binary"
     ,?_assertEqual(<<Prefix/binary, Realm/binary, ".", User/binary>>, Test(Realm, User))
     }
    ,{"Realm must be binary"
     ,?_assertException('error', 'function_clause', Test(binary_to_list(Realm), User))
     }
    ,{"User must be binary"
     ,?_assertException('error', 'function_clause', Test(Realm, binary_to_list(User)))
     }
    ].

get_query_routing_test_() ->
    Test = fun kapi_registration:get_query_routing/2,
    Prefix = <<"registration.query.">>,
    Realm = kz_binary:rand_hex(4),
    User = kz_binary:rand_hex(4),

    [{"Realm and User must be binary"
     ,?_assertEqual(<<Prefix/binary, Realm/binary, ".", User/binary>>, Test(Realm, User))
     }
    ,{"Realm must be binary"
     ,?_assertException('error', 'function_clause', Test(binary_to_list(Realm), User))
     }
    ,{"User must be binary or 'undefined'"
     ,?_assertEqual(<<Prefix/binary, Realm/binary, ".*">>, Test(Realm, 'undefined'))
     }
    ,{"User must be binary or 'undefined'"
     ,?_assertException('error', 'function_clause', Test(Realm, binary_to_list(User)))
     }
    ].

get_flush_routing_test_() ->
    Test = fun kapi_registration:get_flush_routing/1,
    Prefix = <<"registration.flush.">>,
    Realm = kz_binary:rand_hex(4),
    BinProp = [{<<"Realm">>, Realm}],

    [{"Realm must be binary"
     ,?_assertEqual(<<Prefix/binary, Realm/binary>>, Test(Realm))
     }
    ,{"Realm must be binary"
     ,?_assertEqual(<<Prefix/binary, Realm/binary>>, Test(BinProp))
     }
    ,{"Realm must be binary"
     ,?_assertEqual(<<Prefix/binary, Realm/binary>>, Test(kz_json:from_list(BinProp)))
     }
    ].
