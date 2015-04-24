%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%% Module for interacting with JSON schema docs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_json_schema_test).

-include_lib("eunit/include/eunit.hrl").

add_defaults_test() ->
    JObj = wh_json:decode(<<"{\"key1\":\"value1\"}">>),
    SchemaJObj = wh_json:decode(<<"{\"properties\":{\"key1\":{\"type\":\"string\",\"default\":\"default1\"},\"key2\":{\"type\":\"string\",\"default\":\"default2\"}}}">>),

    WithDefaults = wh_json_schema:add_defaults(JObj, SchemaJObj),
    ?assertEqual(wh_json:get_value(<<"key1">>, WithDefaults), <<"value1">>),
    ?assertEqual(wh_json:get_value(<<"key2">>, WithDefaults), <<"default2">>).

add_sub_defaults_test() ->
    JObj = wh_json:decode(<<"{\"caller_id\":{\"internal\":{\"name\":\"internal_cid\"},\"external\":{\"name\":\"external_cid\"},\"emergency\":{}}}">>),

    SchemaJObj = wh_json:decode(<<"{\"properties\":{\"caller_id\":{\"description\":\"The default caller ID parameters\",\"type\":\"object\",\"properties\":{ \"internal\":{\"description\":\"The default caller ID used when dialing internal extensions\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Called ID Internal Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":30}}},\"external\":{\"description\":\"The default caller ID used when dialing external numbers\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID External Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}},\"emergency\":{\"description\":\"The caller ID used when external, internal, or emergency is not defined\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID Emergency Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15,\"default\":\"emer_default\"}}}},\"default\":{}}}}">>),

    WithDefaults = wh_json_schema:add_defaults(JObj, SchemaJObj),
    ?assert(wh_json:is_json_object(wh_json:get_value(<<"caller_id">>, WithDefaults))),
    ?assertEqual(<<"external_cid">>, wh_json:get_value([<<"caller_id">>, <<"external">>, <<"name">>], WithDefaults)),
    ?assertEqual(<<"emer_default">>, wh_json:get_value([<<"caller_id">>, <<"emergency">>, <<"name">>], WithDefaults)).

add_sub_defaults_to_empty_test() ->
    EmptyJObj = wh_json:new(),

    SchemaJObj = wh_json:decode(<<"{\"properties\":{\"caller_id\":{\"description\":\"The default caller ID parameters\",\"type\":\"object\",\"properties\":{ \"internal\":{\"description\":\"The default caller ID used when dialing internal extensions\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Called ID Internal Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":30}}},\"external\":{\"description\":\"The default caller ID used when dialing external numbers\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID External Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}},\"emergency\":{\"description\":\"The caller ID used when external, internal, or emergency is not defined\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID Emergency Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}}},\"default\":{}}}}">>),

    WithDefaults = wh_json_schema:add_defaults(EmptyJObj, SchemaJObj),
    ?assert(wh_json:is_json_object(wh_json:get_value(<<"caller_id">>, WithDefaults))),
    ?assertEqual(EmptyJObj, wh_json:get_value(<<"caller_id">>, WithDefaults)).

add_defaults_array_test() ->
    SchemaJObj = wh_json:decode(<<"{\"properties\":{\"bypass_media\":{\"name\":\"Bypass Media\",\"description\":\"The device bypass media mode\",\"type\":\"string\",\"enum\":[\"true\",\"false\",\"auto\"],\"default\":\"auto\"},\"ignore_early_media\":{\"name\":\"Ignore Early Media\",\"description\":\"The option to determine if early media from the device should always be ignored\",\"type\":\"boolean\"},\"progress_timeout\":{\"name\":\"Progress Timeout\",\"description\":\"The progress timeout to apply to the device\",\"type\":\"integer\"},\"audio\":{\"description\":\"The device audio media parameters\",\"type\":\"object\",\"properties\":{\"codecs\":{\"name\":\"Audio Codecs\",\"description\":\"A list of audio codecs to use\",\"type\":\"array\",\"uniqueItems\":true,\"items\":{\"type\":\"string\",\"enum\":[\"G729\",\"GSM\",\"PCMU\",\"PCMA\",\"G722_16\",\"G722_32\",\"CELT_48\",\"CELT_64\",\"Speex\",\"GSM\",\"OPUS\"]},\"default\":[\"PCMU\"]}}}}}">>),
    JObj = wh_json:decode(<<"{\"audio\":{}}">>),

    WithDefaults = wh_json_schema:add_defaults(JObj, SchemaJObj),

    ?assertEqual(<<"auto">>, wh_json:get_value(<<"bypass_media">>, WithDefaults)),
    ?assertEqual([<<"PCMU">>], wh_json:get_value([<<"audio">>, <<"codecs">>], WithDefaults)).

add_sub_defaults_array_test() ->
    SchemaJObj = wh_json:decode(<<"{\"properties\":{\"enabled\":{\"name\":\"Enabled\",\"description\":\"Determines if the resource is currently enabled\",\"type\":\"boolean\",\"default\":true},\"emergency\":{\"name\":\"Emergency\",\"description\":\"Determines if the resource represents emergency services\",\"type\":\"boolean\",\"default\":false},\"grace_period\":{\"name\":\"Grace Period\",\"description\":\"The amount of time, in seconds, to wait before starting another resource\",\"type\":\"integer\",\"default\":5,\"minimum\":3,\"maximum\":20},\"weight_cost\":{\"name\":\"Weight Cost\",\"description\":\"A value between 0 and 100 that determines the order of resources when multiple can be used\",\"type\":\"integer\",\"default\":50,\"minimum\":0,\"maximum\":100},\"flags\":{\"name\":\"Flags\",\"description\":\"A list of flags that can be provided on the request and must match for the resource to be eligible\",\"type\":\"array\",\"default\":[]},\"gateways\":{\"description\":\"A list of gateways avaliable for this resource\",\"type\":\"array\",\"items\":{\"properties\":{\"enabled\":{\"name\":\"Enabled (gateways)\",\"description\":\"Determines if the resource gateway is currently enabled\",\"type\":\"boolean\",\"default\":true},\"prefix\":{\"name\":\"Prefix (gateways)\",\"description\":\"A string to prepend to the dialed number or capture group of the matching rule\",\"type\":\"string\",\"default\":\"+1\",\"minLength\":1,\"maxLength\":64},\"suffix\":{\"name\":\"Suffix (gateways)\",\"description\":\"A string to append to the dialed number or capture group of the matching rule\",\"type\":\"string\",\"minLength\":1,\"maxLength\":64},\"codecs\":{\"name\":\"Codecs (gateways)\",\"description\":\"A list of audio codecs supported by this gateway\",\"type\":\"array\",\"enum\":[\"G729\",\"PCMU\",\"PCMA\",\"G722_16\",\"G722_32\",\"CELT_48\",\"CELT_64\",\"Speex\",\"GSM\",\"OPUS\",\"H261\",\"H263\",\"H264\"],\"default\":[\"PCMU\",\"PCMA\" ]},\"media\":{\"description\":\"The media parameters for the device\",\"type\":\"object\",\"properties\":{\"fax_option\":{\"description\":\"The fax mode to option\",\"type\":\"string\",\"enum\":[\"true\",\"false\",\"auto\"],\"default\":\"auto\"},\"codecs\":{\"name\":\"Codecs (gateways)\",\"description\":\"A list of audio codecs supported by this gateway\",\"type\":\"array\",\"enum\":[\"G729\",\"PCMU\",\"PCMA\",\"G722_16\",\"G722_32\",\"CELT_48\",\"CELT_64\",\"Speex\",\"GSM\",\"OPUS\",\"H261\",\"H263\",\"H264\",\"VP8\"],\"default\":[\"PCMU\",\"PCMA\" ]}}},\"custom_sip_headers\":{\"name\":\"Custom SIP Header (gateways)\",\"type\":\"object\",\"default\":{}},\"bypass_media\":{\"name\":\"Bypass Media (gateways)\",\"description\":\"The device bypass media mode\",\"type\":\"string\",\"enum\":[\"true\",\"false\",\"auto\"],\"default\":\"auto\"},\"progress_timeout\":{\"name\":\"Progress Timeout (gateways)\",\"description\":\"The progress timeout to apply to the device\",\"type\":\"integer\"},\"endpoint_type\":{\"name\":\"Endpoint Type (gateways)\",\"description\":\"What type of endpoint is this gateway.\",\"type\":\"string\",\"enum\":[\"sip\",\"freetdm\",\"skype\"],\"default\":\"sip\"},\"invite_format\":{\"name\":\"Invite Format (gateways)\",\"description\":\"The format of the DID needed by the underlying hardware/gateway\",\"type\":\"string\",\"enum\":[\"route\",\"username\",\"e164\",\"npan\",\"1npan\"],\"default\":\"route\"}}}}}}">>),

    JObj = wh_json:decode(<<"{\"gateways\":[{\"name\":\"gateway1\",\"media\":{}}]}">>),

    WithDefaults = wh_json_schema:add_defaults(JObj, SchemaJObj),

    ?assertEqual('true', wh_json:get_value(<<"enabled">>, WithDefaults)),
    ?assertEqual('false', wh_json:get_value(<<"emergency">>, WithDefaults)),
    ?assertEqual(5, wh_json:get_value(<<"grace_period">>, WithDefaults)),
    ?assertEqual(50, wh_json:get_value(<<"weight_cost">>, WithDefaults)),
    ?assertEqual([], wh_json:get_value(<<"flags">>, WithDefaults)),

    Gateway = wh_json:get_value([<<"gateways">>, 1], WithDefaults),

    ?assert(wh_json:is_json_object(Gateway)),

    ?assertEqual('true', wh_json:get_value(<<"enabled">>, Gateway)),
    ?assertEqual(<<"+1">>, wh_json:get_value(<<"prefix">>, Gateway)),
    ?assertEqual([<<"PCMU">>, <<"PCMA">>], wh_json:get_value(<<"codecs">>, Gateway)),
    ?assertEqual(<<"auto">>, wh_json:get_value([<<"media">>, <<"fax_option">>], Gateway)),
    ?assertEqual([<<"PCMU">>, <<"PCMA">>], wh_json:get_value([<<"media">>, <<"codecs">>], Gateway)),
    ?assertEqual(wh_json:new(), wh_json:get_value(<<"custom_sip_headers">>, Gateway)),
    ?assertEqual(<<"auto">>, wh_json:get_value(<<"bypass_media">>, Gateway)),
    ?assertEqual(<<"sip">>, wh_json:get_value(<<"endpoint_type">>, Gateway)).
