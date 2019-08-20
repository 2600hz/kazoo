%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(provisioner_v5_tests).

-include_lib("eunit/include/eunit.hrl").

is_device_enabled_test_() ->
    AccountDoc = kzd_accounts:new(),
    DisabledAccountDoc = kzd_accounts:disable(AccountDoc),

    UserDoc = kzd_users:new(),
    DisabledUserDoc = kzd_users:disable(UserDoc),

    DeviceDoc = kzd_devices:new(),
    DisabledDeviceDoc = kzd_devices:set_enabled(DeviceDoc, 'false'),

    Tests = [{'true', [DeviceDoc, UserDoc, AccountDoc]}
            ,{'false', [DeviceDoc, UserDoc, DisabledAccountDoc]}
            ,{'false', [DeviceDoc, DisabledUserDoc, AccountDoc]}
            ,{'false', [DeviceDoc, DisabledUserDoc, DisabledAccountDoc]}

            ,{'false', [DisabledDeviceDoc, UserDoc, AccountDoc]}
            ,{'false', [DisabledDeviceDoc, UserDoc, DisabledAccountDoc]}
            ,{'false', [DisabledDeviceDoc, DisabledUserDoc, AccountDoc]}
            ,{'false', [DisabledDeviceDoc, DisabledUserDoc, DisabledAccountDoc]}
            ],
    [?_assertEqual(Result, provisioner_v5:is_device_enabled(D, U, A)) ||
        {Result, [D, U, A]} <- Tests
    ].

device_display_name_test_() ->
    UserDoc = kzd_users:new(),
    FirstName = <<"User">>,
    LastName = <<"Name">>,
    UserFullName = <<FirstName/binary, " ", LastName/binary>>,
    EmptyNameUserDoc = kzd_users:set_first_name(kzd_users:set_last_name(UserDoc, <<>>)
                                               ,<<>>
                                               ),
    NonEmptyNameUserDoc = kzd_users:set_first_name(kzd_users:set_last_name(UserDoc
                                                                          ,LastName
                                                                          )
                                                  ,FirstName
                                                  ),

    DeviceDoc = kzd_devices:new(),
    DeviceName = <<"Device Name">>,
    EmptyNameDeviceDoc = kzd_devices:set_name(DeviceDoc, 'undefined'),
    NonEmptyNameDeviceDoc = kzd_devices:set_name(DeviceDoc, DeviceName),

    AccountDoc = kzd_accounts:new(),
    AccountName = <<"Account Name">>,
    EmptyNameAccountDoc = kzd_accounts:set_name(AccountDoc, 'undefined'),
    NonEmptyNameAccountDoc = kzd_accounts:set_name(AccountDoc, AccountName),

    Tests = [%% Device's name has preference
             {DeviceName
             ,[NonEmptyNameDeviceDoc, NonEmptyNameUserDoc, NonEmptyNameAccountDoc]
             }
            ,{DeviceName
             ,[NonEmptyNameDeviceDoc, EmptyNameUserDoc, NonEmptyNameAccountDoc]
             }
            ,{DeviceName
             ,[NonEmptyNameDeviceDoc, NonEmptyNameUserDoc, EmptyNameAccountDoc]
             }

             %% If Device's name is not set then use User's name if set
            ,{UserFullName
             ,[EmptyNameDeviceDoc, NonEmptyNameUserDoc, NonEmptyNameAccountDoc]
             }
            ,{UserFullName
             ,[EmptyNameDeviceDoc, NonEmptyNameUserDoc, EmptyNameAccountDoc]
             }

             %% If not User's name nor Device's name set then use Account's name if set.
            ,{AccountName, [EmptyNameDeviceDoc, EmptyNameUserDoc, NonEmptyNameAccountDoc]}

             %% If not User's name nor Device's name nor Account's name set then return 'undefined'
            ,{'undefined', [EmptyNameDeviceDoc, EmptyNameUserDoc, EmptyNameAccountDoc]}
            ],
    [?_assertEqual(Result, provisioner_v5:device_display_name(Device, User, Account)) ||
        {Result, [Device, User, Account]} <- Tests
    ].
