%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_config_tests).

-export([]).

-include_lib("eunit/include/eunit.hrl").
-include("ecallmgr.hrl").

-spec render_test_() -> any().
render_test_() ->
    {'setup'
    ,fun kz_fixturedb_util:start_me/0
    ,fun kz_fixturedb_util:stop_me/1
    ,fun(_ReturnOfSetup) ->
             [{"HELP-44944: no filtering of callfwd endpoints", no_filter_when_callfwd()}
             ,{"HELP-44944: filtering of unregistered SIP endpoints", filter_when_unreged()}
             ,{"HELP-44944: filtering of unregistered SIP endpoints with dedup", filter_when_unreged_dedup()}
             ]
     end
    }.

no_filter_when_callfwd() ->
    AccountId = <<"account0000000000000000000044944">>,
    UserId = <<"user0000000000000000000000000004">>,

    Needles = [_DeviceId = <<"Authorizing-ID='device00000000000000000000000004'">>
              ,_CallFwdDeviceId = <<"Authorizing-ID='device00000000000000000000000005'">>
              ,_CallFwdDestination = <<"loopback/+12223334455">>
              ],

    Endpoints = kz_endpoints:by_owner_id(UserId, kz_json:new(), new_call(AccountId)),

    BridgeEndpoints = ecallmgr_util:endpoint_jobjs_to_records(Endpoints),
    Filtered = ecallmgr_util:maybe_filter_failover_channels(BridgeEndpoints, 'true'),
    Channels = ecallmgr_util:build_bridge_channels(Filtered, []),

    %% SIP device4 and call-fwd device5
    [?_assertEqual(2, length(Filtered))
     | [?_assert(found(Needle, Channels)) || Needle <- Needles]
    ].

found(_Needle, []) -> ?debugFmt("needle ~s not found~n", [_Needle]), 'false';
found(Needle, [Haystack|Haystacks]) ->
    case binary:match(Haystack, Needle) of
        'nomatch' -> found(Needle, Haystacks);
        {_, _} -> 'true'
    end.

filter_when_unreged() ->
    AccountId = <<"account0000000000000000000044944">>,
    UserId = <<"user0000000000000000000000000005">>,

    Needles = [_FailoverRoute = <<"loopback/+13334445555">>],

    Endpoints = kz_endpoints:by_owner_id(UserId, kz_json:new(), new_call(AccountId)),

    BridgeEndpoints = ecallmgr_util:endpoint_jobjs_to_records(Endpoints),
    Filtered = ecallmgr_util:maybe_filter_failover_channels(BridgeEndpoints, 'true'),
    Channels = ecallmgr_util:build_bridge_channels(Filtered, []),

    [%% The SIP device and the failover device
     ?_assertEqual(2, length(BridgeEndpoints))
     %% Just the failover device
    ,?_assertEqual(1, length(Filtered))
     | [?_assert(found(Needle, Channels)) || Needle <- Needles]
    ].

filter_when_unreged_dedup() ->
    AccountId = <<"account0000000000000000000044944">>,
    UserId = <<"user0000000000000000000000000005">>,

    Needles = [_FailoverRoute = <<"loopback/+13334445555">>],

    Endpoints = kz_endpoints:by_owner_id(UserId, kz_json:new(), new_call(AccountId)),

    BridgeEndpoints = ecallmgr_util:endpoint_jobjs_to_records(Endpoints),
    Filtered = ecallmgr_util:maybe_filter_failover_channels(BridgeEndpoints ++ BridgeEndpoints, 'true'),
    Channels = ecallmgr_util:build_bridge_channels(Filtered, []),

    [%% The SIP device and the failover device, twice
     ?_assertEqual(2, length(BridgeEndpoints))
     %% Just the failover device, deduped
    ,?_assertEqual(1, length(Filtered))
     | [?_assert(found(Needle, Channels)) || Needle <- Needles]
    ].

new_call(AccountId) ->
    Exec = [{fun kapps_call:set_account_id/2, AccountId}
           ,{fun kapps_call:set_account_db/2, kz_util:format_account_db(AccountId)}
           ,{fun kapps_call:set_request/2, <<"+12223334444@2600hz.com">>}
           ,{fun kapps_call:set_to/2, <<"+12223334444@2600hz.com">>}
           ,{fun kapps_call:set_from/2, <<"+19998887777@atari.com">>}
           ,{fun kapps_call:set_call_id/2, kz_binary:rand_hex(6)}
           ,{fun kapps_call:set_resource_type/2, <<"audio">>}
           ,{fun kapps_call:set_caller_id_name/2, <<"Caller ID Name">>}
           ,{fun kapps_call:set_caller_id_number/2, <<"+19998887777">>}
           ],
    kapps_call:exec(Exec, kapps_call:new()).
