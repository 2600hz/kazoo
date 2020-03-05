%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_im_tests).

-include_lib("eunit/include/eunit.hrl").

build_binding_test_() ->
    Test = fun kapi_im:routing_key/4,
    Category = kz_binary:rand_hex(4),
    Direction = kz_binary:rand_hex(4),
    RouteType = kz_binary:rand_hex(4),
    MsgId = kz_binary:rand_hex(4),

    [{"Category, Direction, RouteType, and MsgId must be binary"
     ,?_assertEqual(<<Category/binary, ".", Direction/binary, ".", RouteType/binary, ".", MsgId/binary>>
                   ,Test(Category, Direction, RouteType, MsgId)
                   )
     }
    ,{"RouteType may be 'undefined'"
     ,?_assertEqual(<<Category/binary, ".", Direction/binary, ".", MsgId/binary>>
                   ,Test(Category, Direction, 'undefined', MsgId)
                   )
     }
    ,{"MsgId must be binary"
     ,?_assertException('error'
                       ,'function_clause'
                       ,Test(Category, Direction, RouteType, binary_to_list(MsgId))
                       )
     }
    ].
