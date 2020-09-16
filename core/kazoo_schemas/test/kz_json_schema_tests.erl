%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc Module for interacting with JSON schema docs
%%% @author James Aimonetti
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

default_object_test() ->
    Schema = get_schema(),
    Default = kz_json_schema:default_object(Schema),
    [?_assertEqual(kz_json:from_list_recursive([{<<"caller_id">>, [{<<"emergency">>,[{<<"name">>,<<"emer_default">>}]}]}])
                  ,Default
                  )
    ].

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

filter_jobj_by_schema_test() ->
    SchemaWithoutAdditionalPropertiesFalse =
        kz_json:decode(
          "{
                \"$schema\": \"http://json-schema.org/draft-04/schema#\",
                \"description\": \"Filter JObj test schema\",
                \"properties\": {
                    \"property1\": {
                        \"description\": \"Property 1\",
                        \"type\": \"string\"
                    },\"property2\": {
                        \"additionalProperties\": {
                            \"properties\": {
                                \"additionalProperty1\": {
                                    \"description\": \"Additional Property 1\",
                                    \"type\": \"string\"
                                }
                            },
                            \"required\": [
                                \"additionalProperty1\"
                            ],
                            \"type\": \"object\"
                        },
                        \"description\": \"Property 2\",
                        \"type\": \"object\"
                    },
                    \"property3\": {
                        \"description\": \"Property 3\",
                        \"type\": \"array\"
                    }
                },
                \"type\": \"object\"
            }"
        ),

    SchemaWithAdditionalPropertiesFalse =
        kz_json:decode(
            "{
                \"$schema\": \"http://json-schema.org/draft-04/schema#\",
                \"description\": \"Filter JObj test schema\",
                \"additionalProperties\": false,
                \"properties\": {
                    \"property1\": {
                        \"description\": \"Property 1\",
                        \"type\": \"string\"
                    },
                    \"property2\": {
                        \"additionalProperties\": {
                            \"additionalProperties\": false,
                            \"properties\": {
                                \"additionalProperty1\": {
                                    \"description\": \"Additional Property 1\",
                                    \"type\": \"string\"
                                }
                            },
                            \"required\": [
                                \"additionalProperty1\"
                            ],
                            \"type\": \"object\"
                        },
                        \"description\": \"Property 2\",
                        \"type\": \"object\"
                    },
                    \"property3\": {
                        \"description\": \"Property 3\",
                        \"type\": \"array\"
                    }
                },
                \"type\": \"object\"
            }"
        ),

    JObj = kz_json:from_list_recursive([{<<"property1">>, <<"property1Value">>}
                                       ,{<<"property2">>, [{<<"id12345678">>, [{<<"additionalProperty1">>, <<"additionalProperty1Value">>}
                                                                              ,{<<"additionalProperty2">>, <<"additionalProperty2Value">>}
                                                                              ]}
                                                          ,{<<"id87654321">>, [{<<"additionalProperty1">>, <<"additionalProperty1Value">>}]}
                                                          ,{<<"id_abcdefg">>, [{<<"additionalProperty2">>, <<"additionalProperty2Value">>}]}
                                                          ]}
                                       ,{<<"property3">>, <<"property3Value">>}
                                       ,{<<"property4">>, <<"property4Value">>}
                                       ]),

    OutputJObjWithoutAdditionalPropertiesFalse =
        kz_json:from_list_recursive([{<<"property1">>, <<"property1Value">>}
                                    ,{<<"property2">>, [{<<"id12345678">>, [{<<"additionalProperty1">>, <<"additionalProperty1Value">>}
                                                                           ,{<<"additionalProperty2">>, <<"additionalProperty2Value">>}
                                                                           ]}
                                                       ,{<<"id87654321">>, [{<<"additionalProperty1">>, <<"additionalProperty1Value">>}]}
                                                       ]}
                                    ,{<<"property4">>, <<"property4Value">>}
                                    ]),

    OutputJObjWithAdditionalPropertiesFalse =
        kz_json:from_list_recursive([{<<"property1">>, <<"property1Value">>}
                                    ,{<<"property2">>, [{<<"id12345678">>, [{<<"additionalProperty1">>, <<"additionalProperty1Value">>}]}
                                                       ,{<<"id87654321">>, [{<<"additionalProperty1">>, <<"additionalProperty1Value">>}]}
                                                       ]}
                                    ]),

    RespJObjWithoutAdditionalPropertiesFalse = kz_json_schema:filter(JObj, SchemaWithoutAdditionalPropertiesFalse),
    RespJObjWithAdditionalPropertiesFalse = kz_json_schema:filter(JObj, SchemaWithAdditionalPropertiesFalse),

    [?_assert(kz_json:are_equal(OutputJObjWithoutAdditionalPropertiesFalse, RespJObjWithoutAdditionalPropertiesFalse))
    ,?_assert(kz_json:are_equal(OutputJObjWithAdditionalPropertiesFalse, RespJObjWithAdditionalPropertiesFalse))
    ].
