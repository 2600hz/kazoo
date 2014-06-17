%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% Module for interacting with JSON schema docs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_json_schema).

-export([add_defaults/2
         ,load/1
        ]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-spec load(ne_binary()) -> {'ok', wh_json:object()} |
                                  {'error', term()}.
load(<<_/binary>> = Schema) ->
    couch_mgr:open_cache_doc(?WH_SCHEMA_DB, Schema).

-spec add_defaults(api_object(), ne_binary() | wh_json:object()) -> api_object().
add_defaults(JObj, <<_/binary>> = Schema) ->
    {'ok', SchemaJObj} = load(Schema),
    add_defaults(JObj, SchemaJObj);
add_defaults(JObj, SchemaJObj) ->
    wh_json:foldl(fun add_defaults_foldl/3, JObj, wh_json:get_value(<<"properties">>, SchemaJObj)).

-spec add_defaults_foldl(wh_json:key(), wh_json:object(), api_object()) -> api_object().
add_defaults_foldl(SchemaKey, SchemaValue, JObj) ->
    case wh_json:get_value(<<"default">>, SchemaValue) of
        'undefined' ->
            maybe_add_sub_properties(SchemaKey, SchemaValue, JObj);
        Default ->
            maybe_add_sub_properties(SchemaKey
                                     ,SchemaValue
                                     ,maybe_add_default(SchemaKey, Default, JObj)
                                    )
    end.

-spec maybe_add_sub_properties(wh_json:key(), wh_json:object(), api_object()) -> api_object().
maybe_add_sub_properties(SchemaKey, SchemaValue, JObj) ->
    case wh_json:get_value(<<"type">>, SchemaValue) of
        <<"object">> ->
            case add_defaults(wh_json:get_value(SchemaKey, JObj), SchemaValue) of
                'undefined' -> JObj;
                SubJObj ->  wh_json:set_value(SchemaKey, SubJObj, JObj)
            end;
        _Type -> JObj
    end.

-spec maybe_add_default(wh_json:key(), wh_json:json_term(), api_object()) -> api_object().
maybe_add_default(Key, Default, JObj) ->
    case wh_json:get_value(Key, JObj) of
        'undefined' when JObj =/= 'undefined' -> wh_json:set_value(Key, Default, JObj);
        'undefined' ->  wh_json:set_value(Key, Default, wh_json:new());
        _Value -> JObj
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_defaults_test() ->
    JObj = wh_json:decode(<<"{\"key1\":\"value1\"}">>),
    SchemaJObj = wh_json:decode(<<"{\"properties\":{\"key1\":{\"type\":\"string\",\"default\":\"default1\"},\"key2\":{\"type\":\"string\",\"default\":\"default2\"}}}">>),

    WithDefaults = add_defaults(JObj, SchemaJObj),
    ?assertEqual(wh_json:get_value(<<"key1">>, WithDefaults), <<"value1">>),
    ?assertEqual(wh_json:get_value(<<"key2">>, WithDefaults), <<"default2">>).

add_sub_defaults_test() ->
    JObj = wh_json:decode(<<"{\"caller_id\":{\"internal\":{\"name\":\"internal_cid\"},\"external\":{\"name\":\"external_cid\"}}}">>),

    SchemaJObj = wh_json:decode(<<"{\"properties\":{\"caller_id\":{\"description\":\"The default caller ID parameters\",\"type\":\"object\",\"properties\":{ \"internal\":{\"description\":\"The default caller ID used when dialing internal extensions\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Called ID Internal Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":30}}},\"external\":{\"description\":\"The default caller ID used when dialing external numbers\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID External Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}},\"emergency\":{\"description\":\"The caller ID used when external, internal, or emergency is not defined\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID Emergency Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15,\"default\":\"emer_default\"}}}},\"default\":{}}}}">>),

    WithDefaults = add_defaults(JObj, SchemaJObj),
    ?assert(wh_json:is_json_object(wh_json:get_value(<<"caller_id">>, WithDefaults))),
    ?assertEqual(<<"external_cid">>, wh_json:get_value([<<"caller_id">>, <<"external">>, <<"name">>], WithDefaults)),
    ?assertEqual(<<"emer_default">>, wh_json:get_value([<<"caller_id">>, <<"emergency">>, <<"name">>], WithDefaults)).

add_sub_defaults_to_empty_test() ->
    EmptyJObj = wh_json:new(),

    SchemaJObj = wh_json:decode(<<"{\"properties\":{\"caller_id\":{\"description\":\"The default caller ID parameters\",\"type\":\"object\",\"properties\":{ \"internal\":{\"description\":\"The default caller ID used when dialing internal extensions\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Called ID Internal Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":30}}},\"external\":{\"description\":\"The default caller ID used when dialing external numbers\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID External Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}},\"emergency\":{\"description\":\"The caller ID used when external, internal, or emergency is not defined\",\"type\":\"object\",\"properties\":{\"name\":{\"name\":\"Caller ID Emergency Name\",\"description\":\"The caller id name for the object type\",\"type\":\"string\",\"maxLength\":15}}}},\"default\":{}}}}">>),

    WithDefaults = add_defaults(EmptyJObj, SchemaJObj),
    ?assert(wh_json:is_json_object(wh_json:get_value(<<"caller_id">>, WithDefaults))),
    ?assertEqual(EmptyJObj, wh_json:get_value(<<"caller_id">>, WithDefaults)).

-endif.
