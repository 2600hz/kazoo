%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_attributes_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").

-define(NEW_CF_FLAGS, [fun(C) -> kapps_call:set_account_id(?FIXTURE_RESELLER_ACCOUNT_ID, C) end
                      ,fun(C) -> kapps_call:set_authorizing_id(<<"device00000000000000000000000002">>, C) end
                      ,fun(C) -> kapps_call:set_owner_id(<<"user0000000000000000000000000002">>, C) end
                      ]).

-define(MIXED_CF_FLAGS, [fun(C) -> kapps_call:set_account_id(?FIXTURE_PARENT_ACCOUNT_ID, C) end
                        ,fun(C) -> kapps_call:set_authorizing_id(<<"device00000000000000000000000003">>, C) end
                        ,fun(C) -> kapps_call:set_owner_id(<<"user0000000000000000000000000003">>, C) end
                        ]).

-define(NEW_TS_FLAGS, [fun(C) -> kapps_call:set_account_id(?FIXTURE_RESELLER_ACCOUNT_ID, C) end
                      ,fun(C) -> kapps_call:set_authorizing_id(<<"trunkstore0000000000000000000002">>, C) end
                      ]).

kz_attributes_test_() ->
    {'setup'
    ,fun setup_db/0
    ,fun terminate_db/1
    ,fun(_ReturnOfSetup) ->
             [test_get_flags_callflow()
             ,test_get_flags_trunkstore()
             ,test_process_dynamic_flags()
             ]
     end
    }.

setup_db() ->
    ?LOG_DEBUG(":: Starting Kazoo FixtureDB"),
    {'ok', _} = application:ensure_all_started('kazoo_config'),
    kazoo_fixturedb:start().

terminate_db(Pid) ->
    _DataLink = erlang:exit(Pid, 'normal'),
    Ref = monitor('process', Pid),
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            _KConfig = application:stop('kazoo_config'),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: ~p kazoo_config: ~p", [_DataLink, _KConfig])
    after 1000 ->
            _KConfig = application:stop('kazoo_config'),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: timeout kazoo_config: ~p", [_KConfig])
    end.


test_get_flags_callflow() ->
    Call = kapps_call_tests:create_callflow_call(),
    ExpectedOld = [<<"user_old_static_flag">>
                  ,<<"device_old_static_flag">>
                  ,<<"account_old_static_flag">>
                  ],
    ExpectedNew = [<<"user0000000000000000000000000002">>
                  ,<<"user_new_static_flag">>
                  ,<<"local">>
                  ,<<"device_new_static_flag">>
                  ,<<"4a6863.sip.2600hz.local">>
                  ,<<"account_new_static_flag">>
                  ],
    ExpectedMixed = [<<"user_new_static_flag">>
                    ,<<"local">>
                    ,<<"device_old_static_flag">>
                    ,<<"user0000000000000000000000000003">>
                    ,<<"account_new_static_flag">>
                    ],
    [{"verify that get flags will pull the static and dynamic flags from all sources with old formats"
     ,?_assertEqual(ExpectedOld, kz_attributes:get_flags(<<"callflows">>, Call))
     }
    ,{"verify that get flags will pull the static and dynamic flags from all sources with new formats"
     ,?_assertEqual(ExpectedNew, kz_attributes:get_flags(<<"callflows">>, kapps_call:exec(?NEW_CF_FLAGS, Call)))
     }
    ,{"verify that get flags will pull the static and dynamic flags from all sources with mixed formats"
     ,?_assertEqual(ExpectedMixed, kz_attributes:get_flags(<<"callflows">>, kapps_call:exec(?MIXED_CF_FLAGS, Call)))
     }
    ].

test_get_flags_trunkstore() ->
    Call = kapps_call_tests:create_trunkstore_call(),
    ExpectedOld = [<<"account_old_static_flag">>],
    ExpectedNew = [<<"local">>
                  ,<<"account_new_static_flag">>
                  ],
    [{"verify that get flags will pull the static and dynamic flags from all sources with old formats"
     ,?_assertEqual(ExpectedOld, kz_attributes:get_flags(<<"trunkstore">>, Call))
     }
    ,{"verify that get flags will pull the static and dynamic flags from all sources with new formats"
     ,?_assertEqual(ExpectedNew, kz_attributes:get_flags(<<"trunkstore">>, kapps_call:exec(?NEW_TS_FLAGS, Call)))
     }
    ].

test_process_dynamic_flags() ->
    Call = kapps_call_tests:create_callflow_call(),
    [{"verify that dynamic CCVs can be fetched and are converted to binary"
     ,?_assertEqual([<<"device">>], kz_attributes:process_dynamic_flags([<<"custom_channel_vars.authorizing_type">>], Call))
     }
    ,{"verify that exported kapps_call functions can be used"
     ,?_assertEqual([<<"20255520140">>], kz_attributes:process_dynamic_flags([<<"to_user">>], Call))
     }
    ,{"verify that non-exported kapps_call functions dont crash"
     ,?_assertEqual([], kz_attributes:process_dynamic_flags([<<"not_exported">>], Call))
     }
    ,{"verify that the zone name can be resolved"
     ,?_assertEqual([<<"local">>], kz_attributes:process_dynamic_flags([<<"zone">>], Call))
     }
    ,{"verify that dynamic flags are added to a provided list of static flags"
     ,?_assertEqual([<<"local">>, <<"static">>], kz_attributes:process_dynamic_flags([<<"zone">>], [<<"static">>], Call))
     }
    ].
