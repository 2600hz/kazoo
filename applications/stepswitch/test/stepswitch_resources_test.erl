%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(stepswitch_resources_test).

-include_lib("eunit/include/eunit.hrl").
-include("src/stepswitch.hrl").

-define(RESOURCE_1_ID, <<"resource000000000000000000000001">>).

check_fixtures_test_() ->
    {'ok', Schema} = kz_json_schema:fload(<<"resources">>),
    {'ok', Resources} = kz_json:fixture(?APP, <<"fixtures/resources/global.json">>),
    [{"validate resource fixture", ?_assertMatch({'ok', _}, validate(Schema, Resource))}
     || Resource <- Resources
    ].

invite_parameters_test_() ->
    {'ok', Offnet} = kz_json:fixture(?APP, <<"fixtures/offnet_req/global.json">>),
    {'ok', [ResourceJObj|_]} = kz_json:fixture(?APP, <<"fixtures/resources/global.json">>),
    [GatewayJObj|_]  = kz_json:get_value(<<"gateways">>, ResourceJObj),
    Resource = stepswitch_resources:resource_from_jobj(ResourceJObj),

    DynamicParameters1 = kz_json:from_list([{<<"key">>, <<"custom_sip_headers.x_auth_ip">>}, {<<"tag">>, <<"somethingelse">>}]),
    DynamicParameters2 = kz_json:from_list([{<<"key">>, <<"custom_sip_headers.x_auth_ip">>}, {<<"tag">>, <<"somethingelse">>}, {<<"seperator">>, <<"&">>}]),

    GatewayJObjs = [kz_json:set_value([<<"invite_parameters">>, <<"static">>], [<<"npid">>], GatewayJObj)
                   ,kz_json:set_value([<<"invite_parameters">>, <<"static">>], [<<"npid">>], GatewayJObj)
                   ,kz_json:set_value([<<"invite_parameters">>, <<"static">>], [<<"test">>], GatewayJObj)
                   ,kz_json:set_value([<<"invite_parameters">>, <<"dynamic">>], [DynamicParameters1], GatewayJObj)
                   ,kz_json:set_value([<<"invite_parameters">>, <<"dynamic">>], [DynamicParameters2], GatewayJObj)
                   ],

    Gateways = [stepswitch_resources:gateway_from_jobj(GatewayJObj2, Resource) || GatewayJObj2 <- GatewayJObjs],

    Offnets = [kz_json:set_value([<<"Requestor-Custom-Channel-Vars">>, <<"TNS-CIC">>], <<"cic=2002">>, Offnet)
              ,kz_json:set_value([<<"Requestor-Custom-Channel-Vars">>, <<"Account-ID">>], <<"12345">>, Offnet)
              ,kz_json:set_value([<<"Requestor-Custom-SIP-Headers">>, <<"X-Auth-IP">>], <<"127.0.0.1">>, Offnet)
              ,kz_json:set_value([<<"Requestor-Custom-SIP-Headers">>, <<"X-Auth-IP">>], <<"127.0.0.1">>, Offnet)
              ,kz_json:set_value([<<"Requestor-Custom-SIP-Headers">>, <<"X-Auth-IP">>], <<"127.0.0.1">>, Offnet)
              ],

    ExpectedValues = [[<<"npid">>, <<"zone=local">>, <<"cic=2002">>]
                     ,[<<"npid">>, <<"zone=local">>, <<"account_id=12345">>]
                     ,[<<"test">>, <<"zone=local">>, <<"source_ip=127.0.0.1">>]
                     ,[<<"npid">>, <<"somethingelse=127.0.0.1">>]
                     ,[<<"npid">>, <<"somethingelse&127.0.0.1">>]
                     ],

    Zipped = lists:zip3(ExpectedValues, Gateways, Offnets),

    [?_assertEqual(ExpectedValue, stepswitch_resources:sip_invite_parameters(Gateway, ModifiedOffnet))
     || {ExpectedValue, Gateway, ModifiedOffnet} <- Zipped].

validate(Schema, Object) ->
    kz_json_schema:validate(Schema
                           ,Object
                           ,[{'schema_loader_fun', fun kz_json_schema:fload/1}]
                           ).

