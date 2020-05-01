%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_fax_tests).

-include_lib("eunit/include/eunit.hrl").

fax_start_key_test_() ->
    Test = fun kapi_fax:fax_start_key/2,
    Prefix = <<"fax.start.">>,
    Type = kz_binary:rand_hex(4),
    Id = kz_binary:rand_hex(4),

    [{"Must work with '*' Type"
     ,?_assertEqual(<<Prefix/binary, "*.", Id/binary>>, Test(<<"*">>, Id))
     }
    ,{"Must work with binary Type and Id"
     ,?_assertEqual(<<Prefix/binary, Type/binary, ".", Id/binary>>, Test(Type, Id))
     }
    ,{"Must work with string Type"
     ,?_assertEqual(<<Prefix/binary, Type/binary, ".", Id/binary>>
                   ,Test(binary_to_list(Type), Id)
                   )
     }
    ,{"Id must be binary"
     ,?_assertException('error', 'function_clause', Test(Type, binary_to_list(Id)))
     }
    ].

status_routing_key_test_() ->
    Test = fun kapi_fax:status_routing_key/2,
    Prefix = <<"fax.status.">>,
    AccountId = kz_binary:rand_hex(4),
    FaxId = kz_binary:rand_hex(4),

    [{"Must work with binary AccountId and FaxId"
     ,?_assertEqual(<<Prefix/binary, AccountId/binary, ".", FaxId/binary>>
                   ,Test(AccountId, FaxId)
                   )
     }
    ,{"Must work with string AccountId"
     ,?_assertEqual(<<Prefix/binary, AccountId/binary, ".", FaxId/binary>>
                   ,Test(binary_to_list(AccountId), FaxId)
                   )
     }
    ,{"FaxId must be binary"
     ,?_assertException('error', 'function_clause', Test(AccountId, binary_to_list(FaxId)))
     }
    ].
