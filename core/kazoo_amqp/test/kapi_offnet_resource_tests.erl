%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_offnet_resource_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl"). %% ?KEY_OFFNET_RESOURCE_REQ
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl"). %% ?KEY_RESOURCE_TYPE

routing_key_test_() ->
    Test = fun kapi_offnet_resource:routing_key/1,
    Prefix = ?KEY_OFFNET_RESOURCE_REQ,
    Type = kz_binary:rand_hex(4),
    PList = fun(T) -> [{?KEY_RESOURCE_TYPE, T}] end,
    BinList = PList(Type),
    StrList = PList(binary_to_list(Type)),
    AtomList = PList(kz_term:to_atom(Type, 'true')),
    Expected = <<Prefix/binary, ".", Type/binary>>,

    [{"Type may be binary"
     ,[?_assertEqual(Expected, Test(BinList))
      ,?_assertEqual(Expected, Test(kz_json:from_list(BinList)))
      ]
     }
    ,{"Type may be string"
     ,[?_assertEqual(Expected, Test(StrList))
      ,?_assertEqual(Expected, Test(kz_json:from_list(StrList)))
      ]
     }
    ,{"Type may be atom"
     ,[?_assertEqual(Expected, Test(AtomList))
      ,?_assertEqual(Expected, Test(kz_json:from_list(AtomList)))
      ]
     }
    ].
