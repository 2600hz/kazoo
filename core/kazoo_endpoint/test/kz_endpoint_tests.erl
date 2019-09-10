%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_endpoint_tests).

-include_lib("eunit/include/eunit.hrl").

attributes_keys_unique_test_() ->
    Keys = kz_endpoint:attributes_keys(),
    [?_assertEqual(length(Keys), length(lists:usort(Keys)))].

call_recording_test_() ->
    [all_off()
    ,all_on()
    ,device_on()
    ,device_off_user_on()
    ,device_undefined_user_on()
    ,device_off_user_off_account_on()
    ,device_undefined_user_off_account_on()
    ,device_undefined_user_undefined_account_on()
    ].

device_on() ->
    Expectations = [{<<"account">>, <<"inbound">>, <<"offnet">>,   'false'}
                   ,{<<"account">>, <<"inbound">>, <<"onnet">>,    'false'}
                   ,{<<"account">>, <<"outbound">>, <<"offnet">>,  'false'}
                   ,{<<"account">>, <<"outbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"offnet">>,  'true'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"onnet">>,   'true'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"offnet">>, 'true'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"onnet">>,  'true'}
                   ],

    AccountDoc = kzd_accounts:set_call_recording(kzd_accounts:new(), kz_json:from_list([{<<"account">>, all_off_object()}])),
    UserDoc = kzd_users:set_call_recording(kzd_users:new(), all_off_object()),
    DeviceDoc = kzd_devices:set_call_recording(kzd_devices:new(), all_on_object()),

    Merged = kz_endpoint:merge_attribute(<<"call_recording">>, AccountDoc, DeviceDoc, UserDoc),

    check_expectations(?FUNCTION_NAME, Merged, Expectations).

device_off_user_on() ->
    Expectations = [{<<"account">>, <<"inbound">>, <<"offnet">>,   'false'}
                   ,{<<"account">>, <<"inbound">>, <<"onnet">>,    'false'}
                   ,{<<"account">>, <<"outbound">>, <<"offnet">>,  'false'}
                   ,{<<"account">>, <<"outbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"offnet">>,  'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"offnet">>, 'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"onnet">>,  'false'}
                   ],

    AccountDoc = kzd_accounts:set_call_recording(kzd_accounts:new(), kz_json:from_list([{<<"account">>, all_off_object()}])),
    UserDoc = kzd_users:set_call_recording(kzd_users:new(), all_on_object()),
    DeviceDoc = kzd_devices:set_call_recording(kzd_devices:new(), all_off_object()),

    Merged = kz_endpoint:merge_attribute(<<"call_recording">>, AccountDoc, DeviceDoc, UserDoc),

    check_expectations(?FUNCTION_NAME, Merged, Expectations).

device_undefined_user_on() ->
    Expectations = [{<<"account">>, <<"inbound">>, <<"offnet">>,   'false'}
                   ,{<<"account">>, <<"inbound">>, <<"onnet">>,    'false'}
                   ,{<<"account">>, <<"outbound">>, <<"offnet">>,  'false'}
                   ,{<<"account">>, <<"outbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"offnet">>,  'true'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"onnet">>,   'true'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"offnet">>, 'true'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"onnet">>,  'true'}
                   ],

    AccountDoc = kzd_accounts:set_call_recording(kzd_accounts:new(), kz_json:from_list([{<<"account">>, all_off_object()}])),
    UserDoc = kzd_users:set_call_recording(kzd_users:new(), all_on_object()),
    DeviceDoc = kzd_devices:new(),

    Merged = kz_endpoint:merge_attribute(<<"call_recording">>, AccountDoc, DeviceDoc, UserDoc),

    check_expectations(?FUNCTION_NAME, Merged, Expectations).

device_off_user_off_account_on() ->
    Expectations = [{<<"account">>, <<"inbound">>, <<"offnet">>,   'true'}
                   ,{<<"account">>, <<"inbound">>, <<"onnet">>,    'true'}
                   ,{<<"account">>, <<"outbound">>, <<"offnet">>,  'true'}
                   ,{<<"account">>, <<"outbound">>, <<"onnet">>,   'true'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"offnet">>,  'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"offnet">>, 'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"onnet">>,  'false'}
                   ],

    AccountDoc = kzd_accounts:set_call_recording(kzd_accounts:new(), kz_json:from_list([{<<"account">>, all_on_object()}])),
    UserDoc = kzd_users:set_call_recording(kzd_users:new(), all_off_object()),
    DeviceDoc = kzd_devices:set_call_recording(kzd_devices:new(), all_off_object()),

    Merged = kz_endpoint:merge_attribute(<<"call_recording">>, AccountDoc, DeviceDoc, UserDoc),

    check_expectations(?FUNCTION_NAME, Merged, Expectations).

device_undefined_user_off_account_on() ->
    Expectations = [{<<"account">>, <<"inbound">>, <<"offnet">>,   'true'}
                   ,{<<"account">>, <<"inbound">>, <<"onnet">>,    'true'}
                   ,{<<"account">>, <<"outbound">>, <<"offnet">>,  'true'}
                   ,{<<"account">>, <<"outbound">>, <<"onnet">>,   'true'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"offnet">>,  'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"offnet">>, 'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"onnet">>,  'false'}
                   ],

    AccountDoc = kzd_accounts:set_call_recording(kzd_accounts:new(), kz_json:from_list([{<<"account">>, all_on_object()}])),
    UserDoc = kzd_users:set_call_recording(kzd_users:new(), all_off_object()),
    DeviceDoc = kzd_devices:new(),

    Merged = kz_endpoint:merge_attribute(<<"call_recording">>, AccountDoc, DeviceDoc, UserDoc),

    check_expectations(?FUNCTION_NAME, Merged, Expectations).

device_undefined_user_undefined_account_on() ->
    Expectations = [{<<"account">>, <<"inbound">>, <<"offnet">>,   'true'}
                   ,{<<"account">>, <<"inbound">>, <<"onnet">>,    'true'}
                   ,{<<"account">>, <<"outbound">>, <<"offnet">>,  'true'}
                   ,{<<"account">>, <<"outbound">>, <<"onnet">>,   'true'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"offnet">>,  'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"offnet">>, 'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"onnet">>,  'false'}
                   ],

    AccountDoc = kzd_accounts:set_call_recording(kzd_accounts:new(), kz_json:from_list([{<<"account">>, all_on_object()}])),
    UserDoc = kzd_users:new(),
    DeviceDoc = kzd_devices:new(),

    Merged = kz_endpoint:merge_attribute(<<"call_recording">>, AccountDoc, DeviceDoc, UserDoc),
    ?debugFmt("merged: ~p~n", [Merged]),

    check_expectations(?FUNCTION_NAME, Merged, Expectations).

all_off() ->
    Expectations = [{<<"account">>, <<"inbound">>, <<"offnet">>,   'false'}
                   ,{<<"account">>, <<"inbound">>, <<"onnet">>,    'false'}
                   ,{<<"account">>, <<"outbound">>, <<"offnet">>,  'false'}
                   ,{<<"account">>, <<"outbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"offnet">>,  'false'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"onnet">>,   'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"offnet">>, 'false'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"onnet">>,  'false'}
                   ],

    AllOff = all_off_object(),
    AccountDoc = kzd_accounts:set_call_recording(kzd_accounts:new(), kz_json:from_list([{<<"account">>, AllOff}])),
    UserDoc = kzd_users:set_call_recording(kzd_users:new(), AllOff),
    DeviceDoc = kzd_devices:set_call_recording(kzd_devices:new(), AllOff),

    Merged = kz_endpoint:merge_attribute(<<"call_recording">>, AccountDoc, DeviceDoc, UserDoc),

    check_expectations(?FUNCTION_NAME, Merged, Expectations).

all_on() ->
    Expectations = [{<<"account">>, <<"inbound">>, <<"offnet">>,   'true'}
                   ,{<<"account">>, <<"inbound">>, <<"onnet">>,    'true'}
                   ,{<<"account">>, <<"outbound">>, <<"offnet">>,  'true'}
                   ,{<<"account">>, <<"outbound">>, <<"onnet">>,   'true'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"offnet">>,  'true'}
                   ,{<<"endpoint">>, <<"inbound">>, <<"onnet">>,   'true'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"offnet">>, 'true'}
                   ,{<<"endpoint">>, <<"outbound">>, <<"onnet">>,  'true'}
                   ],

    AllOn = all_on_object(),
    AccountDoc = kzd_accounts:set_call_recording(kzd_accounts:new(), kz_json:from_list([{<<"account">>, AllOn}])),
    UserDoc = kzd_users:set_call_recording(kzd_users:new(), AllOn),
    DeviceDoc = kzd_devices:set_call_recording(kzd_devices:new(), AllOn),

    Merged = kz_endpoint:merge_attribute(<<"call_recording">>, AccountDoc, DeviceDoc, UserDoc),

    check_expectations(?FUNCTION_NAME, Merged, Expectations).

check_expectations(Test, Merged, Expectations) ->
    [{kz_term:to_list(kz_binary:join([Test, Type, Direction, Network]))
     ,?_assertEqual(Result, kz_json:is_true([<<"call_recording">>, Type, Direction, Network, <<"enabled">>], Merged))
     } ||
        {Type, Direction, Network, Result} <- Expectations
    ].

all_off_object() ->
    kz_json:set_values([{[Direction, Network, <<"enabled">>], 'false'} ||
                           Direction <- [<<"inbound">>, <<"outbound">>],
                           Network <- [<<"onnet">>, <<"offnet">>]
                       ]
                      ,kz_json:new()
                      ).

all_on_object() ->
    kz_json:set_values([{[Direction, Network, <<"enabled">>], 'true'} ||
                           Direction <- [<<"inbound">>, <<"outbound">>],
                           Network <- [<<"onnet">>, <<"offnet">>]
                       ]
                      ,kz_json:new()
                      ).
