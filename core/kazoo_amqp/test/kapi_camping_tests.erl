%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_camping_tests).

-include_lib("eunit/include/eunit.hrl").

build_binding_test_() ->
    Prefix = <<"camping.request">>,
    AccountId = kz_binary:rand_hex(4),

    [{"AccountId must be a binary"
     ,?_assertEqual(<<Prefix/binary, ".", AccountId/binary>>
                   ,kapi_camping:build_binding(AccountId)
                   )
     }
    ,{"AccountId must be a binary"
     ,?_assertException('error', 'badarg', kapi_camping:build_binding('undefined'))
     }
    ].
