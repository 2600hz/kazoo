%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_bridge_tests).

-include_lib("eunit/include/eunit.hrl").

avoid_privacy_if_emergency_call_test_() ->
    EmergencyPrivacy = emergency_endpoint_with_privacy(),
    EmergencyNotPrivacy = emergency_endpoint_without_privacy(),
    NotEmergencyPrivacy = not_emergency_endpoint_with_privacy(),
    NotEmergencyNotPrivacy = not_emergency_endpoint_without_privacy(),

    [{"If emergency endpoint and privacy enabled, don't enforce privacy"
     ,check_expected(EmergencyNotPrivacy, [EmergencyPrivacy])
     }
    ,{"If emergency endpoint and privacy disabled, let it be"
     ,check_expected(EmergencyNotPrivacy, [EmergencyNotPrivacy])
     }
    ,{"If NOT emergency endpoint and privacy enabled, let it be"
     ,check_expected(NotEmergencyPrivacy, [NotEmergencyPrivacy])
     }
    ,{"If NOT emergency endpoint and privacy disabled, let it be"
     ,check_expected(NotEmergencyNotPrivacy, [NotEmergencyNotPrivacy])
     }
    ].

check_expected(Expected, Endpoints) ->
    Resp = stepswitch_bridge:avoid_privacy_if_emergency_call(
             stepswitch_bridge:contains_emergency_endpoints(Endpoints)
            ,Endpoints
            ),
    ?_assert(kz_json:are_equal(Expected, hd(Resp))).

emergency_endpoint_with_privacy() ->
    endpoint(<<"true">>, <<"kazoo">>, 'true', 'true').

emergency_endpoint_without_privacy() ->
    endpoint(<<"true">>, <<"none">>, 'false', 'false').

not_emergency_endpoint_with_privacy() ->
    endpoint(<<"false">>, <<"kazoo">>, 'true', 'true').

not_emergency_endpoint_without_privacy() ->
    endpoint(<<"false">>, <<"none">>, 'false', 'false').

endpoint(IsEmergency, PrivacyMethod, PrivacyHideName, PrivacyHideNumber) ->
    kz_json:from_list_recursive(
      [{<<"Route">>, <<"sip:911@testing.zswitch.net">>}
      ,{<<"Outbound-Callee-ID-Name">>, <<"911">>}
      ,{<<"Outbound-Callee-ID-Number">>, <<"911">>}
      ,{<<"To-DID">>, <<"911">>}
      ,{<<"Invite-Format">>, <<"route">>}
      ,{<<"Caller-ID-Type">>, <<"external">>}
      ,{<<"Bypass-Media">>, false}
      ,{<<"Auth-User">>, <<"user_endpoint">>}
      ,{<<"Auth-Password">>, <<"password_endpoint">>}
      ,{<<"Endpoint-Type">>, <<"sip">>}
      ,{<<"Endpoint-Progress-Timeout">>, <<"8">>}
      ,{<<"Custom-Channel-Vars">>
       ,[{<<"Emergency-Resource">>, IsEmergency}
        ,{<<"Matched-Number">>, <<"911">>}
        ,{<<"Resource-Type">>, <<"offnet-termination">>}
        ,{<<"Format-From-URI">>, 'false'}
        ,{<<"Original-Number">>, <<"911">>}
        ,{<<"DID-Classifier">>, <<"emergency">>}
        ,{<<"E164-Destination">>, <<"911">>}
        ,{<<"Resource-ID">>, <<"eb79c3e4a17518e83307482bf3f2e40b-emergency">>}
        ,{<<"Global-Resource">>, 'true'}
        ]
       }
      ,{<<"Outbound-Caller-ID-Number">>, <<"+14151234567">>}
      ,{<<"Outbound-Caller-ID-Name">>, <<"Emergency Caller ID Test">>}
      ,{<<"Privacy-Method">>, PrivacyMethod}
      ,{<<"Privacy-Hide-Name">>, PrivacyHideName}
      ,{<<"Privacy-Hide-Number">>, PrivacyHideNumber}
      ,{<<"Weight">>, 50}
      ,{<<"Name">>, <<"E911 Testing Outbound Carrier - emergency">>}
      ]
     ).
