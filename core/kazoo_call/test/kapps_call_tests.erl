%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_call_tests).

-export([create_callflow_call/0
        ,create_trunkstore_call/0
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(APP, 'kazoo_call').

-define(UPDATERS, [fun(C) -> kapps_call:set_custom_channel_var(<<"key1">>, <<"value1">>, C) end
                  ,fun(C) -> kapps_call:set_custom_channel_var(<<"key2">>, 2600, C) end
                  ,fun(C) -> kapps_call:set_custom_channel_var([<<"key3">>, <<"key4">>], 'true', C) end
                  ,fun(C) -> kapps_call:kvs_store(<<"kvs_key_1">>, <<"kvs_value_1">>, C) end
                  ,fun(C) -> kapps_call:kvs_store(<<"kvs_key_2">>, <<"kvs_value_2">>, C) end
                  ,fun(C) -> kapps_call:kvs_store(<<"kvs_key_2">>, kz_json:from_list([{<<"sub_key_1">>, <<"sub_value_1">>}]), C) end
                  ]).

route_test_() ->
    {'setup'
    ,fun kazoo_call_test_util:setup_db/0
    ,fun kazoo_call_test_util:terminate_db/1
    ,fun(_ReturnOfSetup) ->
             [{"Testing Callflow route request"
              ,test_callflow_route_request()
              }
             ,{"Testing Callflow route win"
              ,test_callflow_route_win()
              }
             ,{"Testing Trunkstore route request"
              ,test_trunkstore_route_request()
              }
             ,{"Testing Trunkstore route win"
              ,test_trunkstore_route_win()
              }
             ,test_encode_decode()
             ]
     end
    }.

ensure_call_id_binary_test_() ->
    RouteReq = kz_json:set_value(<<"Call-ID">>
                                ,'atom'
                                ,inbound_onnet_callflow_req()
                                ),
    Call = kapps_call:from_route_req(RouteReq),
    [{"verify that the call id is always returned as a binary"
     ,?_assertEqual(<<"atom">>, kapps_call:call_id(Call))
     }
    ].

ensure_prepend_cid_name_kvs_test_() ->
    RouteReq = kz_json:set_value(<<"Prepend-CID-Name">>
                                ,<<"prepend">>
                                ,inbound_onnet_callflow_req()
                                ),
    Call = kapps_call:from_route_req(RouteReq),
    [{"verify that the CCV prepend CID name is moved to the KVS"
     ,?_assertEqual({'ok', <<"prepend">>}, kapps_call:kvs_find('prepend_cid_name', Call))
     }
    ].

updateable_test_() ->
    %% {CCVsToSend, ExistingCCVs, NewCCVs}
    Tests = [{[], [], []}

            ,{[], [{k, v}], []}
            ,{[{k, v}], [], [{k, v}]}
            ,{[], [{k, v}], [{k, v}]}
            ,{[{k, v1}], [{k, v}], [{k, v1}]}

            ,{[], [{k0, v0}, {k1, v1}], []}
            ,{[], [{k0, v0}, {k1, v1}], [{k0, v0}, {k1, v1}]}

            ,{[{k0, v0}, {k1, v1}], [], [{k0, v0}, {k1, v1}]}
            ,{[{k0, v9}], [{k0, v0}, {k1, v1}], [{k0, v9}]}
            ,{[{k1, v9}], [{k0, v0}, {k1, v1}], [{k1, v9}]}
            ,{[{k0, v9}, {k1, v9}], [{k0, v0}, {k1, v1}], [{k0, v9}, {k1, v9}]}
            ],
    [?_assertEqual(ToSend, kapps_call:updateable_ccvs(New, Existing))
     || {ToSend, Existing, New} <- Tests
    ].

test_callflow_route_request() ->
    {'ok', RouteReq} = kz_json:fixture(?APP, "fixtures/route_req/inbound-onnet-callflow.json"),
    'true' = kapi_route:req_v(RouteReq),
    Call = kapps_call:from_route_req(RouteReq),
    validate_kapps_call_basic(Call) ++ validate_kapps_call_callflow_req(Call).

test_trunkstore_route_request() ->
    {'ok', RouteReq} = kz_json:fixture(?APP, "fixtures/route_req/inbound-onnet-trunkstore.json"),
    'true' = kapi_route:req_v(RouteReq),
    Call = kapps_call:from_route_req(RouteReq),
    validate_kapps_call_basic(Call) ++ validate_kapps_call_trunkstore_req(Call).

test_callflow_route_win() ->
    Call = create_callflow_call(),
    validate_kapps_call_basic(Call) ++ validate_kapps_call_callflow_win(Call).

test_trunkstore_route_win() ->
    Call = create_trunkstore_call(),
    validate_kapps_call_basic(Call) ++ validate_kapps_call_trunkstore_win(Call).

test_encode_decode() ->
    Call = kapps_call:exec(?UPDATERS, create_callflow_call()),
    Call1 = kapps_call:from_json(kapps_call:to_json(Call)),
    [{"Testing kapps_call encode decode"
     ,?_assert(kapps_call:eq(Call, Call1))
     }
    ].

validate_kapps_call_basic(Call) ->
    [{"verify the caller id name is the expected value"
     ,?_assertEqual(<<"caller-id-name">>, kapps_call:caller_id_name(Call))
     }
    ,{"verify the caller id number is the expected value"
     ,?_assertEqual(<<"caller-id-number">>, kapps_call:caller_id_number(Call))
     }
    ,{"verify the request is the expected value"
     ,?_assertEqual(<<"20255520140@4a6863.sip.2600hz.local">>, kapps_call:request(Call))
     }
    ,{"verify the request user is the expected value"
     ,?_assertEqual(<<"20255520140">>, kapps_call:request_user(Call))
     }
    ,{"verify the request realm is the expected value"
     ,?_assertEqual(<<"4a6863.sip.2600hz.local">>, kapps_call:request_realm(Call))
     }
    ,{"verify the request realm is the expected value"
     ,?_assertEqual(<<"4a6863.sip.2600hz.local">>, kapps_call:request_realm(Call))
     }
    ,{"verify the to is the expected value"
     ,?_assertEqual(<<"20255520140@4a6863.sip.2600hz.local">>, kapps_call:to(Call))
     }
    ,{"verify the to user is the expected value"
     ,?_assertEqual(<<"20255520140">>, kapps_call:to_user(Call))
     }
    ,{"verify the to realm is the expected value"
     ,?_assertEqual(<<"4a6863.sip.2600hz.local">>, kapps_call:to_realm(Call))
     }
    ,{"verify the account db is the expected value"
     ,?_assertEqual(<<"account%2Fac%2Fco%2Funt0000000000000000000000001">>, kapps_call:account_db(Call))
     }
    ,{"verify the account id is the expected value"
     ,?_assertEqual(<<"account0000000000000000000000001">>, kapps_call:account_id(Call))
     }
    ,{"verify the account realm is the expected value"
     ,?_assertEqual(<<"4a6863.sip.2600hz.local">>, kapps_call:account_realm(Call))
     }
    ,{"verify the switch nodename is the expected value"
     ,?_assertEqual(<<"freeswitch@apps001.2600hz.local">>, kapps_call:switch_nodename(Call))
     }
    ,{"verify the switch hostname is the expected value"
     ,?_assertEqual(<<"apps001.2600hz.local">>, kapps_call:switch_hostname(Call))
     }
    ,{"verify the switch url is the expected value"
     ,?_assertEqual(<<"sip:mod_sofia@10.26.10.10:11000">>, kapps_call:switch_url(Call))
     }
    ,{"verify the switch uri is the expected value"
     ,?_assertEqual(<<"sip:10.26.10.10:11000">>, kapps_call:switch_uri(Call))
     }
    ,{"verify the resource type is the expected value"
     ,?_assertEqual(<<"audio">>, kapps_call:resource_type(Call))
     }
    ,{"verify the direction is the expected value"
     ,?_assertEqual(<<"inbound">>, kapps_call:direction(Call))
     }
    ,{"verify the CCV caller privacy hide name is the expected value"
     ,?_assertEqual('false', kapps_call:custom_channel_var(<<"Caller-Privacy-Hide-Name">>, Call))
     }
    ,{"verify the CCV caller privacy hide number is the expected value"
     ,?_assertEqual('false', kapps_call:custom_channel_var(<<"Caller-Privacy-Hide-Number">>, Call))
     }
    ,{"verify the CCV caller screen bit is the expected value"
     ,?_assertEqual('true', kapps_call:custom_channel_var(<<"Caller-Screen-Bit">>, Call))
     }
    ].

validate_kapps_call_callflow_req(Call) ->
    [{"verify the call id is the expected value"
     ,?_assertEqual(<<"82159NjBmNTI0YjQ5MTE2NmFkNmQ3NmRmYzFiYWRhMGQ0NGI">>, kapps_call:call_id(Call))
     }
    ,{"verify the authorizing id is the expected value"
     ,?_assertEqual(<<"device00000000000000000000000001">>, kapps_call:authorizing_id(Call))
     }
    ,{"verify the authorizing type is the expected value"
     ,?_assertEqual(<<"device">>, kapps_call:authorizing_type(Call))
     }
    ,{"verify the owner id is the expected value"
     ,?_assertEqual(<<"user0000000000000000000000000001">>, kapps_call:owner_id(Call))
     }
    ,{"verify the fetch id is the expected value"
     ,?_assertEqual(<<"f1a51368-844d-11e7-9441-03f51cdc7211">>, kapps_call:fetch_id(Call))
     }
    ,{"verify the from tag is the expected value"
     ,?_assertEqual(<<"5a7d0f43">>, kapps_call:from_tag(Call))
     }
    ,{"verify the from is the expected value"
     ,?_assertEqual(<<"7900@4a6863.sip.2600hz.local">>, kapps_call:from(Call))
     }
    ,{"verify the from user is the expected value"
     ,?_assertEqual(<<"7900">>, kapps_call:from_user(Call))
     }
    ,{"verify the from realm is the expected value"
     ,?_assertEqual(<<"4a6863.sip.2600hz.local">>, kapps_call:from_realm(Call))
     }
    ,{"verify control queue is 'undefined'"
     ,?_assertEqual('undefined', kapps_call:control_queue(Call))
     }
    ,{"verify inception is 'undefined'"
     ,?_assertEqual('undefined', kapps_call:inception(Call))
     }
    ,{"verify it is not flagged as inter-account"
     ,?_assertEqual('false', kapps_call:is_inter_account(Call))
     }
    ,{"verify the CCV call interaction id is the expected value"
     ,?_assertEqual(<<"63670304795-386ddd58">>, kapps_call:custom_channel_var(<<"Call-Interaction-ID">>, Call))
     }
    ].

validate_kapps_call_callflow_win(Call) ->
    [{"verify control queue is 'undefined'"
     ,?_assertEqual(<<"ecallmgr@apps001.2600hz.local-<0.29590.28>-521b30b6">>, kapps_call:control_queue(Call))
     }
    ,{"verify the CCV channel authorized is the expected value"
     ,?_assertEqual(<<"true">>, kapps_call:custom_channel_var(<<"Channel-Authorized">>, Call))
     }
    ,{"verify the CCV global resource is the expected value"
     ,?_assertEqual('false', kapps_call:custom_channel_var(<<"Global-Resource">>, Call))
     }
    ,{"verify the CCV application name is the expected value"
     ,?_assertEqual(<<"callflow">>, kapps_call:custom_channel_var(<<"Application-Name">>, Call))
     }
    ,{"verify the CCV application node is the expected value"
     ,?_assertEqual(<<"kazoo_apps@apps001.2600hz.local">>, kapps_call:custom_channel_var(<<"Application-Node">>, Call))
     }
    ,{"verify the CCV callflow id is the expected value"
     ,?_assertEqual(<<"callflow000000000000000000000001">>, kapps_call:custom_channel_var(<<"CallFlow-ID">>, Call))
     }
    ,{"verify the CCV call interaction id is the expected value"
     ,?_assertEqual(<<"63670304795-386ddd58">>, kapps_call:custom_channel_var(<<"Call-Interaction-ID">>, Call))
     }
    ].

validate_kapps_call_trunkstore_req(Call) ->
    [{"verify the call id is the expected value"
     ,?_assertEqual(<<"82159ZWEwNTQ2NGY4Yzc2YzRiNDE4YzY1YzdmZmVjNWQ3ZjQ">>, kapps_call:call_id(Call))
     }
    ,{"verify the authorizing id is the expected value"
     ,?_assertEqual(<<"trunkstore0000000000000000000001">>, kapps_call:authorizing_id(Call))
     }
    ,{"verify the authorizing type is the expected value"
     ,?_assertEqual(<<"sys_info">>, kapps_call:authorizing_type(Call))
     }
    ,{"verify owner id is 'undefined'"
     ,?_assertEqual('undefined', kapps_call:owner_id(Call))
     }
    ,{"verify the fetch id is the expected value"
     ,?_assertEqual(<<"2f12140e-844d-11e7-9426-03f51cdc7211">>, kapps_call:fetch_id(Call))
     }
    ,{"verify the from tag is the expected value"
     ,?_assertEqual(<<"55969f2e">>, kapps_call:from_tag(Call))
     }
    ,{"verify the from is the expected value"
     ,?_assertEqual(<<"+14158867900@4a6863.sip.2600hz.local">>, kapps_call:from(Call))
     }
    ,{"verify the from user is the expected value"
     ,?_assertEqual(<<"+14158867900">>, kapps_call:from_user(Call))
     }
    ,{"verify the from realm is the expected value"
     ,?_assertEqual(<<"4a6863.sip.2600hz.local">>, kapps_call:from_realm(Call))
     }
    ,{"verify control queue is 'undefined'"
     ,?_assertEqual('undefined', kapps_call:control_queue(Call))
     }
    ,{"verify inception is 'undefined'"
     ,?_assertEqual('undefined', kapps_call:inception(Call))
     }
    ,{"verify it is not flagged as inter-account"
     ,?_assertEqual('false', kapps_call:is_inter_account(Call))
     }
    ].

validate_kapps_call_trunkstore_win(Call) ->
    [{"verify control queue is 'undefined'"
     ,?_assertEqual(<<"ecallmgr@apps001.2600hz.local-<0.28561.28>-a4a36185">>, kapps_call:control_queue(Call))
     }
    ,{"verify the CCV channel authorized is the expected value"
     ,?_assertEqual(<<"true">>, kapps_call:custom_channel_var(<<"Channel-Authorized">>, Call))
     }
    ,{"verify the CCV global resource is the expected value"
     ,?_assertEqual('false', kapps_call:custom_channel_var(<<"Global-Resource">>, Call))
     }
    ,{"verify the CCV application name is the expected value"
     ,?_assertEqual(<<"trunkstore">>, kapps_call:custom_channel_var(<<"Application-Name">>, Call))
     }
    ,{"verify the CCV application node is the expected value"
     ,?_assertEqual(<<"kazoo_apps@apps001.2600hz.local">>, kapps_call:custom_channel_var(<<"Application-Node">>, Call))
     }
    ,{"verify the CCV call interaction id is the expected value"
     ,?_assertEqual(<<"63670304469-eac90b9a">>, kapps_call:custom_channel_var(<<"Call-Interaction-ID">>, Call))
     }
    ].

create_callflow_call() ->
    RouteReq = inbound_onnet_callflow_req(),
    RouteWin = inbound_onnet_callflow_win(),
    kapps_call:from_route_win(RouteWin, kapps_call:from_route_req(RouteReq)).

inbound_onnet_callflow_req() ->
    {ok,RouteReq} = kz_json:fixture(?APP, "fixtures/route_req/inbound-onnet-callflow.json"),
    'true' = kapi_route:req_v(RouteReq),
    RouteReq.

inbound_onnet_callflow_win() ->
    {ok,RouteWin} = kz_json:fixture(?APP, "fixtures/route_win/inbound-onnet-callflow.json"),
    'true' = kapi_route:win_v(RouteWin),
    RouteWin.

create_trunkstore_call() ->
    RouteReq = inbound_onnet_trunkstore_req(),
    RouteWin = inbound_onnet_trunkstore_win(),
    kapps_call:from_route_win(RouteWin, kapps_call:from_route_req(RouteReq)).

inbound_onnet_trunkstore_req() ->
    {ok,RouteReq} = kz_json:fixture(?APP, "fixtures/route_req/inbound-onnet-trunkstore.json"),
    'true' = kapi_route:req_v(RouteReq),
    RouteReq.

inbound_onnet_trunkstore_win() ->
    {ok,RouteWin} = kz_json:fixture(?APP, "fixtures/route_win/inbound-onnet-trunkstore.json"),
    'true' = kapi_route:win_v(RouteWin),
    RouteWin.
