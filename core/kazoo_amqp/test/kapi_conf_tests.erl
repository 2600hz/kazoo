%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_conf_tests).

-include_lib("eunit/include/eunit.hrl").

doc_type_update_routing_key_test_() ->
    Test = fun kapi_conf:doc_type_update_routing_key/1,
    Prefix = <<"configuration.doc_type_update.">>,
    BinType = kz_binary:rand_hex(4),
    API = [{<<"Type">>, BinType}],

    [{"Type must be binary"
     ,[?_assertEqual(<<Prefix/binary, BinType/binary>>, Test(BinType))
      ,?_assertEqual(<<Prefix/binary, BinType/binary>>, Test(API))
      ,?_assertEqual(<<Prefix/binary, BinType/binary>>, Test(kz_json:from_list(API)))
      ]
      %% Cannot test with values different than binary because Test's second clause will loop forever.
     }
    ].
