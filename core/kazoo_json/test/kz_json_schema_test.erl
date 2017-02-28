%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz INC
%%% @doc
%%% Module for interacting with JSON schema docs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_json_schema_test).

-include_lib("eunit/include/eunit.hrl").

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
    SchemaJObj = kz_json:decode(<<"{\"properties\":{\"enabled\":{\"name\":\"Enabled\",\"description\":\"Determines if the resource is currently enabled\",\"type\":\"boolean\",\"default\":true},\"emergency\":{\"name\":\"Emergency\",\"description\":\"Determines if the resource represents emergency services\",\"type\":\"boolean\",\"default\":false},\"grace_period\":{\"name\":\"Grace Period\",\"description\":\"The amount of time, in seconds, to wait before starting another resource\",\"type\":\"integer\",\"default\":5,\"minimum\":3,\"maximum\":20},\"weight_cost\":{\"name\":\"Weight Cost\",\"description\":\"A value between 0 and 100 that determines the order of resources when multiple can be used\",\"type\":\"integer\",\"default\":50,\"minimum\":0,\"maximum\":100},\"flags\":{\"name\":\"Flags\",\"description\":\"A list of flags that can be provided on the request and must match for the resource to be eligible\",\"type\":\"array\",\"default\":[]},\"gateways\":{\"description\":\"A list of gateways avaliable for this resource\",\"type\":\"array\",\"items\":{\"properties\":{\"enabled\":{\"name\":\"Enabled (gateways)\",\"description\":\"Determines if the resource gateway is currently enabled\",\"type\":\"boolean\",\"default\":true},\"prefix\":{\"name\":\"Prefix (gateways)\",\"description\":\"A string to prepend to the dialed number or capture group of the matching rule\",\"type\":\"string\",\"default\":\"+1\",\"minLength\":1,\"maxLength\":64},\"suffix\":{\"name\":\"Suffix (gateways)\",\"description\":\"A string to append to the dialed number or capture group of the matching rule\",\"type\":\"string\",\"minLength\":1,\"maxLength\":64},\"codecs\":{\"name\":\"Codecs (gateways)\",\"description\":\"A list of audio codecs supported by this gateway\",\"type\":\"array\",\"enum\":[\"G729\",\"PCMU\",\"PCMA\",\"G722_16\",\"G722_32\",\"CELT_48\",\"CELT_64\",\"Speex\",\"GSM\",\"OPUS\",\"H261\",\"H263\",\"H264\"],\"default\":[\"PCMU\",\"PCMA\" ]},\"media\":{\"description\":\"The media parameters for the device\",\"type\":\"object\",\"properties\":{\"fax_option\":{\"description\":\"The fax mode to option\",\"type\":\"string\",\"enum\":[\"true\",\"false\",\"auto\"],\"default\":\"auto\"},\"codecs\":{\"name\":\"Codecs (gateways)\",\"description\":\"A list of audio codecs supported by this gateway\",\"type\":\"array\",\"enum\":[\"G729\",\"PCMU\",\"PCMA\",\"G722_16\",\"G722_32\",\"CELT_48\",\"CELT_64\",\"Speex\",\"GSM\",\"OPUS\",\"H261\",\"H263\",\"H264\",\"VP8\"],\"default\":[\"PCMU\",\"PCMA\" ]}}},\"custom_sip_headers\":{\"name\":\"Custom SIP Header (gateways)\",\"type\":\"object\",\"default\":{}},\"bypass_media\":{\"name\":\"Bypass Media (gateways)\",\"description\":\"The device bypass media mode\",\"type\":\"string\",\"enum\":[\"true\",\"false\",\"auto\"],\"default\":\"auto\"},\"progress_timeout\":{\"name\":\"Progress Timeout (gateways)\",\"description\":\"The progress timeout to apply to the device\",\"type\":\"integer\"},\"endpoint_type\":{\"name\":\"Endpoint Type (gateways)\",\"description\":\"What type of endpoint is this gateway.\",\"type\":\"string\",\"enum\":[\"sip\",\"freetdm\",\"skype\"],\"default\":\"sip\"},\"invite_format\":{\"name\":\"Invite Format (gateways)\",\"description\":\"The format of the DID needed by the underlying hardware/gateway\",\"type\":\"string\",\"enum\":[\"route\",\"username\",\"e164\",\"npan\",\"1npan\"],\"default\":\"route\"}}}}}}">>),

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

-spec from_file(nonempty_string()) -> kz_json:object().
from_file(File) ->
    kz_json:load_fixture_from_file('kazoo', "fixtures", File).

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
    V3SchemaJObj = from_file("schemav3_tasks.json"),
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
    V4SchemaJObj = from_file("schemav4_tasks.json"),
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
    [?_assertEqual({[{<<"caller_id">>,{[{<<"emergency">>,{[{<<"name">>,<<"emer_default">>}]}}]}}]}, Default)].

flatten_sms_schema_test() ->
    Flat = kz_json_schema:flatten(get_schema_sms()),
    [?_assertEqual(Flat, {[
                           {[<<"outbound">>,<<"options">>,<<"default">>], {[{<<"delivery_mode">>,2},{<<"mandatory">>,true}]}},
                           {[<<"outbound">>,<<"options">>,<<"description">>], <<"sms options">>},
                           {[<<"outbound">>,<<"options">>,<<"type">>],<<"object">>}]
                         })].
