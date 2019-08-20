%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Module for interacting with JSON schema docs
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_json_schema_tests).

-include_lib("eunit/include/eunit.hrl").
-include("schemas.hrl").

add_defaults_test_() ->
    JObj = kz_json:decode(<<"{\"key1\":\"value1\"}">>),
    SchemaJObj = kz_json:decode(<<"{\"properties\":{\"key1\":{\"type\":\"string\",\"default\":\"default1\"},\"key2\":{\"type\":\"string\",\"default\":\"default2\"}}}">>),

    WithDefaults = kz_json_schema:add_defaults(JObj, SchemaJObj),
    [?_assertEqual(kz_json:get_value(<<"key1">>, WithDefaults), <<"value1">>)
    ,?_assertEqual(kz_json:get_value(<<"key2">>, WithDefaults), <<"default2">>)
    ].

add_sub_defaults_test_() ->
    JObj = kz_json:decode(<<"{\"caller_id\":{\"internal\":{\"name\":\"internal_cid\"},\"external\":{\"name\":\"external_cid\"},\"emergency\":{}}}">>),

    SchemaJObj = kz_json:decode(<<"{\"properties\":{\"caller_id\":{\"description\":\"The default caller ID parameters\",\"type\":\"object\",\"properties\":{ \"internal\":{\"description\":\"The default caller ID used when dialing internal extensions\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Called ID Internal Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":30}}},\"external\":{\"description\":\"The default caller ID used when dialing external numbers\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID External Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}},\"emergency\":{\"description\":\"The caller ID used when external, internal, or emergency is not defined\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID Emergency Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15,\"default\":\"emer_default\"}}}},\"default\":{}}}}">>),

    WithDefaults = kz_json_schema:add_defaults(JObj, SchemaJObj),
    [?_assert(kz_json:is_json_object(kz_json:get_value(<<"caller_id">>, WithDefaults)))
    ,?_assertEqual(<<"external_cid">>, kz_json:get_value([<<"caller_id">>, <<"external">>, <<"name">>], WithDefaults))
    ,?_assertEqual(<<"emer_default">>, kz_json:get_value([<<"caller_id">>, <<"emergency">>, <<"name">>], WithDefaults))
    ].

add_sub_defaults_to_empty_test_() ->
    EmptyJObj = kz_json:new(),

    SchemaJObj = kz_json:decode(<<"{\"properties\":{\"caller_id\":{\"description\":\"The default caller ID parameters\",\"type\":\"object\",\"properties\":{ \"internal\":{\"description\":\"The default caller ID used when dialing internal extensions\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Called ID Internal Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":30}}},\"external\":{\"description\":\"The default caller ID used when dialing external numbers\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID External Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}},\"emergency\":{\"description\":\"The caller ID used when external, internal, or emergency is not defined\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID Emergency Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}}},\"default\":{}}}}">>),

    WithDefaults = kz_json_schema:add_defaults(EmptyJObj, SchemaJObj),
    [?_assert(kz_json:is_json_object(kz_json:get_value(<<"caller_id">>, WithDefaults)))
    ,?_assertEqual(EmptyJObj, kz_json:get_value(<<"caller_id">>, WithDefaults))
    ].

add_defaults_array_test_() ->
    SchemaJObj = kz_json:decode(<<"{\"properties\":{\"bypass_media\":{\"name\":\"Bypass Media\",\"description\":\"The device bypass media mode\",\"type\":\"string\",\"enum\":[\"true\",\"false\",\"auto\"],\"default\":\"auto\"},\"ignore_early_media\":{\"name\":\"Ignore Early Media\",\"description\":\"The option to determine if early media from the device should always be ignored\",\"type\":\"boolean\"},\"progress_timeout\":{\"name\":\"Progress Timeout\",\"description\":\"The progress timeout to apply to the device\",\"type\":\"integer\"},\"audio\":{\"description\":\"The device audio media parameters\",\"type\":\"object\",\"properties\":{\"codecs\":{\"name\":\"Audio Codecs\",\"description\":\"A list of audio codecs to use\",\"type\":\"array\",\"uniqueItems\":true,\"items\":{\"type\":\"string\",\"enum\":[\"G729\",\"GSM\",\"PCMU\",\"PCMA\",\"G722_16\",\"G722_32\",\"CELT_48\",\"CELT_64\",\"Speex\",\"GSM\",\"OPUS\"]},\"default\":[\"PCMU\"]}}}}}">>),
    JObj = kz_json:decode(<<"{\"audio\":{}}">>),

    WithDefaults = kz_json_schema:add_defaults(JObj, SchemaJObj),

    [?_assertEqual(<<"auto">>, kz_json:get_value(<<"bypass_media">>, WithDefaults))
    ,?_assertEqual([<<"PCMU">>], kz_json:get_value([<<"audio">>, <<"codecs">>], WithDefaults))
    ].

add_sub_defaults_array_test_() ->
    {ok, SchemaJObj} = kz_json:fixture(?APP, "fixtures/schemav3_sub_defaults_array.json"),

    JObj = kz_json:decode(<<"{\"gateways\":[{\"name\":\"gateway1\",\"media\":{}}]}">>),

    WithDefaults = kz_json_schema:add_defaults(JObj, SchemaJObj),

    Gateway = kz_json:get_value([<<"gateways">>, 1], WithDefaults),

    [?_assertEqual('true', kz_json:get_value(<<"enabled">>, WithDefaults))
    ,?_assertEqual('false', kz_json:get_value(<<"emergency">>, WithDefaults))
    ,?_assertEqual(5, kz_json:get_value(<<"grace_period">>, WithDefaults))
    ,?_assertEqual(50, kz_json:get_value(<<"weight_cost">>, WithDefaults))
    ,?_assertEqual([], kz_json:get_value(<<"flags">>, WithDefaults))

    ,?_assert(kz_json:is_json_object(Gateway))

    ,?_assertEqual('true', kz_json:get_value(<<"enabled">>, Gateway))
    ,?_assertEqual(<<"+1">>, kz_json:get_value(<<"prefix">>, Gateway))
    ,?_assertEqual([<<"PCMU">>, <<"PCMA">>], kz_json:get_value(<<"codecs">>, Gateway))
    ,?_assertEqual(<<"auto">>, kz_json:get_value([<<"media">>, <<"fax_option">>], Gateway))
    ,?_assertEqual([<<"PCMU">>, <<"PCMA">>], kz_json:get_value([<<"media">>, <<"codecs">>], Gateway))
    ,?_assertEqual(kz_json:new(), kz_json:get_value(<<"custom_sip_headers">>, Gateway))
    ,?_assertEqual(<<"auto">>, kz_json:get_value(<<"bypass_media">>, Gateway))
    ,?_assertEqual(<<"sip">>, kz_json:get_value(<<"endpoint_type">>, Gateway))
    ].

validate_sub_array_test_() ->
    {ok, SchemaJObj} = kz_json:fixture(?APP, "fixtures/schemav3_sub_defaults_array.json"),

    JObj = kz_json:decode(<<"{\"gateways\":[{\"name\":\"gateway1\",\"media\":{}, \"codecs\": [\"G999\"]}, {\"name\":\"gateway2\", \"codecs\": [\"G999\"]}, {\"name\":\"gateway3\", \"codecs\": [\"PCMU\", \"G999\"]}]}">>),

    [?_assertMatch({error,[{data_invalid,_, not_in_enum,<<"G999">>,[<<"gateways">>,0,<<"codecs">>,0]}
                          ,{data_invalid,_, not_in_enum,<<"G999">>,[<<"gateways">>,1,<<"codecs">>,0]}
                          ,{data_invalid,_, not_in_enum,<<"G999">>,[<<"gateways">>,2,<<"codecs">>,1]}
                          ]}
                  ,kz_json_schema:validate(SchemaJObj, JObj)
                  )
    ].

valid_task_data() ->
    kz_json:from_list([{<<"do_it_now">>, true}]).

invalid_task_data1() ->
    kz_json:from_list([{<<"records">>, []}]).

invalid_task_data2() ->
    kz_json:from_list([{<<"records">>, []}
                      ,{<<"do_it_now">>, true}
                      ]).

invalid_task_data3() ->
    kz_json:from_list([{<<"file_name">>, <<>>}
                      ,{<<"do_it_now">>, true}
                      ]).

invalid_task_data4() ->
    kz_json:from_list([{<<"file_name">>, 42}
                      ,{<<"do_it_now">>, true}
                      ]).

validate_v3_test_() ->
    {ok,V3SchemaJObj} = kz_json:fixture(?APP, "fixtures/schemav3_tasks.json"),
    [?_assertMatch({ok,_}, kz_json_schema:validate(V3SchemaJObj, valid_task_data()))
    ,?_assertMatch({error, [{data_invalid,_,{missing_required_property,<<"do_it_now">>},_,_}
                           ,{data_invalid,_,wrong_size,_,[<<"records">>]}
                           ]}
                  ,kz_json_schema:validate(V3SchemaJObj, invalid_task_data1())
                  )
    ,?_assertMatch({error, [{data_invalid,_,wrong_size,_,[<<"records">>]}
                           ]}
                  ,kz_json_schema:validate(V3SchemaJObj, invalid_task_data2())
                  )
    ,?_assertMatch({error, [{data_invalid,_,wrong_length,_,[<<"file_name">>]}
                           ]}
                  ,kz_json_schema:validate(V3SchemaJObj, invalid_task_data3())
                  )
    ,?_assertMatch({error, [{data_invalid,_,wrong_type,_,[<<"file_name">>]}
                           ]}
                  ,kz_json_schema:validate(V3SchemaJObj, invalid_task_data4())
                  )
    ].

validate_v4_test_() ->
    {ok,V4SchemaJObj} = kz_json:fixture(?APP, "fixtures/schemav4_tasks.json"),
    [?_assertMatch({ok,_}, kz_json_schema:validate(V4SchemaJObj, valid_task_data()))
    ,?_assertMatch({error, [{data_invalid,_,wrong_size,_,[<<"records">>]}
                           ,{data_invalid,_,missing_required_property,_,_}
                           ]}
                  ,kz_json_schema:validate(V4SchemaJObj, invalid_task_data1())
                  )
    ,?_assertMatch({error, [{data_invalid,_,wrong_size,_,[<<"records">>]}
                           ]}
                  ,kz_json_schema:validate(V4SchemaJObj, invalid_task_data2())
                  )
    ,?_assertMatch({error, [{data_invalid,_,wrong_length,_,[<<"file_name">>]}
                           ]}
                  ,kz_json_schema:validate(V4SchemaJObj, invalid_task_data3())
                  )
    ,?_assertMatch({error, [{data_invalid,_,wrong_type,_,[<<"file_name">>]}
                           ]}
                  ,kz_json_schema:validate(V4SchemaJObj, invalid_task_data4())
                  )
    ].

get_schema() ->
    kz_json:decode(<<"{\"properties\":{\"caller_id\":{\"description\":\"The default caller ID parameters\",\"type\":\"object\",\"properties\":{ \"internal\":{\"description\":\"The default caller ID used when dialing internal extensions\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Called ID Internal Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":30}}},\"external\":{\"description\":\"The default caller ID used when dialing external numbers\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID External Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}},\"emergency\":{\"description\":\"The caller ID used when external, internal, or emergency is not defined\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID Emergency Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15,\"default\":\"emer_default\"}}}},\"default\":{}}}}">>).

get_schema_sms() ->
    kz_json:decode(<<"{\"_id\":\"system_config.sms\",\"_rev\":\"3-0861f3d8db3f26883e3a69274aeb94bd\",\"$schema\":\"http://json-schema.org/draft-04/schema#\",\"description\":\"Schema for sms system_config\",\"properties\":{\"outbound\":{\"properties\":{\"options\":{\"properties\":{\"default\":{\"delivery_mode\":2,\"mandatory\":true},\"description\":\"sms options\",\"type\":\"object\"}}}}},\"required\":[\"outbound\"],\"type\":\"object\",\"id\":\"system_config.sms\"}">>).

default_object_test() ->
    Schema = get_schema(),
    Default = kz_json_schema:default_object(Schema),
    [?_assertEqual(kz_json:from_list_recursive([{<<"caller_id">>, [{<<"emergency">>,[{<<"name">>,<<"emer_default">>}]}]}])
                  ,Default
                  )
    ].

flatten_sms_schema_test() ->
    SMSSchema = get_schema_sms(),
    Flat = kz_json_schema:flatten(SMSSchema),

    JObj = kz_json:from_list_recursive([{<<"outbound">>, [{<<"options">>, [{<<"default">>, [{<<"delivery_mode">>,2},{<<"mandatory">>,true}]}]}]}
                                       ,{[<<"outbound">>,<<"options">>,<<"description">>], <<"sms options">>}
                                       ,{[<<"outbound">>,<<"options">>,<<"type">>],<<"object">>}
                                       ]),
    [?_assertEqual(Flat, JObj)].

did_duplication_test() ->
    SrvA = kz_json:from_list([{<<"DIDs">>,kz_json:new()}
                             ,{<<"auth">>, kz_json:from_list([{<<"auth_method">>, <<"password">>}])}
                             ,{<<"server_name">>,<<"AAA1">>}
                             ]
                            ),
    SrvB = kz_json:from_list([{<<"DIDs">>,kz_json:new()}
                             ,{<<"auth">>, kz_json:from_list([{<<"auth_method">>, <<"password">>}])}
                             ,{<<"server_name">>,<<"BBB1">>}
                             ]
                            ),
    SrvC = kz_json:from_list([{<<"DIDs">>,kz_json:from_list([{<<"+78121111111">>,kz_json:new()}])}
                             ,{<<"auth">>, kz_json:from_list([{<<"auth_method">>, <<"password">>}])}
                             ,{<<"server_name">>,<<"CCC1">>}
                             ]
                            ),

    JObj = kz_json:from_list([{<<"servers">>, [SrvA, SrvB, SrvC]}]),

    {'ok', Schema} = kz_json_schema:fload(<<"connectivity">>),

    {'ok', Valid} = kz_json_schema:validate(Schema, JObj),

    ?assertEqual(name_and_did(JObj), name_and_did(Valid)).

name_and_did(JObj) ->
    [{kz_json:get_value(<<"server_name">>, Server)
     ,kz_json:is_empty(kz_json:get_json_value(<<"DIDs">>, Server))
     }
     || Server <- kz_json:get_value(<<"servers">>, JObj)
    ].

validate_regexp_test_() ->
    {ok, SchemaJObj} = kz_json:fixture(?APP, "fixtures/schema_regexp.json"),

    JObj1 = kz_json:decode(<<"{\"list\":[\"should@work.com\", \"this_one@too.com\", \"*@fail.com\"]}">>),
    JObj2 = kz_json:decode(<<"{\"list\":[\"should@work.com\", \"this_one@too.com\", \"@wont-fail.com\"]}">>),
    JObj3 = kz_json:decode(<<"{\"item\":\"should@work.com\"}">>),
    JObj4 = kz_json:decode(<<"{\"item\":\"*@fail.com\"}">>),
    Err = <<"invalid regular expression: '*@fail.com'">>,

    [?_assertMatch({error,[{data_invalid,_, {external_error, Err}, _,[<<"list">>,2]}]} ,kz_json_schema:validate(SchemaJObj, JObj1))
    ,?_assertMatch({ok, _}, kz_json_schema:validate(SchemaJObj, JObj2))
    ,?_assertMatch({ok, _}, kz_json_schema:validate(SchemaJObj, JObj3))
    ,?_assertMatch({error,[{data_invalid,_, {external_error, Err}, _,[<<"item">>]}]} ,kz_json_schema:validate(SchemaJObj, JObj4))
    ].
