%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_delegate_tests).

-include_lib("eunit/include/eunit.hrl").

build_binding_test_() ->
    Cat = <<"delegate">>,
    App = kz_binary:rand_hex(4),
    Key = kz_binary:rand_hex(4),

    [{"Must work with 'undefined' Key"
     ,?_assertEqual(<<Cat/binary, ".", App/binary>>
                   ,kapi_delegate:build_binding(App, 'undefined')
                   )
     }
    ,{"Must work with binary Key"
     ,?_assertEqual(<<Cat/binary, ".", App/binary, ".", Key/binary>>
                   ,kapi_delegate:build_binding(App, Key)
                   )
     }
    ,{"App must be a binary"
     ,?_assertException('error'
                       ,'function_clause'
                       ,kapi_delegate:build_binding('undefined', Key)
                       )
     }
    ].
