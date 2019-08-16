%%%-----------------------------------------------------------------------------
%%% @Copyright (C) 2010-2016, 2600Hz
%%% @doc Test utilities for manipulating maps
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(kz_os_tests).

-include_lib("eunit/include/eunit.hrl").

cmd_test_() ->
    [?_assertMatch({'ok', <<"hello\n">>}, kz_os:cmd(<<"echo hello">>))].
cmd_vars_test_() ->
    [?_assertMatch({'ok', <<"hello 1\n">>}
                  ,kz_os:cmd(<<"echo $var $number">>
                            ,[{<<"var">>, <<"hello">>}
                             ,{<<"number">>, 1}
                             ]
                            )
                  )
    ].
cmd_stream_test_() ->
    [?_assertMatch({'ok', <<"hello">>}, kz_os:cmd(<<"echo -n hello">>, [], [{<<"read_mode">>, stream}]))].
cmd_stream_newline_test_() ->
    [?_assertMatch({'ok', <<"hello\n">>}, kz_os:cmd(<<"echo hello">>, [], [{<<"read_mode">>, stream}]))].
cmd_timeout_test_() ->
    [?_assertMatch({'error', 'timeout', _}, kz_os:cmd(<<"tail -f /dev/null">>
                                                     ,[{<<"var">>, <<"hello">>}]
                                                     ,[{<<"timeout">>, 100}
                                                      ,{<<"max_size">>, 10000000}
                                                      ]
                                                     )
                  )
    ].
cmd_absolute_timeout_test_() ->
    [?_assertMatch({'error', 'absolute_timeout', _ }, kz_os:cmd(<<"tail -f /dev/urandom">>
                                                               ,[]
                                                               ,[{<<"max_size">>, 10000000}
                                                                ,{<<"timeout">>, 1000}
                                                                ,{<<"absolute_timeout">>, 50}
                                                                ]
                                                               )
                  )
    ].
cmd_absolute_timeout_evil_cmd_test_() ->
    [?_assertMatch({'error', 'absolute_timeout', <<>> }, kz_os:cmd(<<"yes 'stop hitting yourself'">>
                                                                  ,[]
                                                                  ,[{<<"max_size">>, 10000000}
                                                                   ,{<<"timeout">>, 1000}
                                                                   ,{<<"absolute_timeout">>, 50}
                                                                   ]
                                                                  )
                  )
    ].
cmd_max_size_default_test_() ->
    [?_assertMatch({'error', 'max_size', _}, kz_os:cmd(<<"cat /dev/urandom">>
                                                      ,[]
                                                      ,[{<<"max_size">>, 100}]
                                                      )
                  )
    ].
cmd_max_size_stream_test_() ->
    [?_assertMatch({'error', 'max_size', _}, kz_os:cmd(<<"cat /dev/urandom">>
                                                      ,[]
                                                      ,[{<<"max_size">>, 100}
                                                       ,{<<"read_mode">>, 'stream'}
                                                       ]
                                                      )
                  )
    ].
