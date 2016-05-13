%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%% Module for interacting with JSON schema docs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_json_schema).

-export([add_defaults/2
         ,load/1
         ,flush/0, flush/1
         ,validate/2, validate/3
        ]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-spec load(ne_binary() | string()) -> {'ok', kz_json:object()} |
                                      {'error', any()}.
load(<<"./", Schema/binary>>) -> load(Schema);
load(<<_/binary>> = Schema) ->
    case kz_datamgr:open_cache_doc(?KZ_SCHEMA_DB, Schema, [{'cache_failures', ['not_found']}]) of
        {'error', _E}=E -> E;
        {'ok', JObj} -> {'ok', kz_json:insert_value(<<"id">>, Schema, JObj)}
    end;
load(Schema) -> load(kz_util:to_binary(Schema)).

-spec flush() -> 'ok'.
-spec flush(ne_binary()) -> 'ok'.
flush() ->
    kz_datamgr:flush_cache_docs(?KZ_SCHEMA_DB).
flush(Schema) ->
    kz_datamgr:flush_cache_doc(?KZ_SCHEMA_DB, Schema).

-spec add_defaults(api_object() | ne_binary(), ne_binary() | kz_json:object()) -> api_object().
add_defaults(JObj, <<_/binary>> = Schema) ->
    {'ok', SchemaJObj} = load(Schema),
    add_defaults(JObj, SchemaJObj);
add_defaults(JObj, SchemaJObj) ->
    kz_json:foldl(fun add_defaults_foldl/3
                  ,JObj
                  ,kz_json:get_value(<<"properties">>, SchemaJObj, kz_json:new())
                 ).

-spec add_defaults_foldl(kz_json:key(), kz_json:object(), api_object()) -> api_object().
add_defaults_foldl(SchemaKey, SchemaValue, JObj) ->
    case kz_json:get_value(<<"default">>, SchemaValue) of
        'undefined' ->
            maybe_add_sub_properties(SchemaKey, SchemaValue, JObj);
        Default ->
            maybe_add_sub_properties(SchemaKey
                                     ,SchemaValue
                                     ,maybe_add_default(SchemaKey, Default, JObj)
                                    )
    end.

-spec maybe_add_sub_properties(kz_json:key(), kz_json:object(), api_object()) -> api_object().
maybe_add_sub_properties(SchemaKey, SchemaValue, JObj) ->
    case kz_json:get_value(<<"type">>, SchemaValue) of
        <<"object">> ->
            maybe_update_data_with_sub(SchemaKey, SchemaValue, JObj);
        <<"array">> ->
            {_, JObj1} = lists:foldl(fun(SubJObj, Acc) ->
                                             maybe_add_sub_properties_foldl(SchemaKey, SchemaValue, SubJObj, Acc)
                                     end
                                     ,{1, JObj}
                                     ,kz_json:get_value(SchemaKey, JObj, [])
                                    ),
            JObj1;
        _Type -> JObj
    end.

-spec maybe_add_sub_properties_foldl(kz_json:key(), kz_json:object(), kz_json:json_term(), {pos_integer(), kz_json:object()}) ->
                                            {pos_integer(), kz_json:object()}.
maybe_add_sub_properties_foldl(SchemaKey, SchemaValue, SubJObj, {Idx, JObj}) ->
    case add_defaults(SubJObj, kz_json:get_value(<<"items">>, SchemaValue, kz_json:new())) of
        'undefined' -> {Idx+1, JObj};
        NewSubJObj -> {Idx+1, kz_json:set_value([SchemaKey, Idx], NewSubJObj, JObj)}
    end.

-spec maybe_update_data_with_sub(kz_json:key(), kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_update_data_with_sub(SchemaKey, SchemaValue, JObj) ->
    case add_defaults(kz_json:get_value(SchemaKey, JObj), SchemaValue) of
        'undefined' -> JObj;
        SubJObj ->  kz_json:set_value(SchemaKey, SubJObj, JObj)
    end.

-spec maybe_add_default(kz_json:key(), kz_json:json_term(), api_object()) -> api_object().
maybe_add_default(Key, Default, JObj) ->
    case kz_json:is_json_object(JObj)
        andalso kz_json:get_value(Key, JObj)
    of
        'undefined' when JObj =/= 'undefined' -> kz_json:set_value(Key, Default, JObj);
        'undefined' ->  kz_json:set_value(Key, Default, kz_json:new());
        _Value -> JObj
    end.

-type jesse_option() :: {'parser_fun', fun((_) -> _)} |
{'error_handler', fun((jesse_error:error_reason(), jesse_error:error_reasons(), non_neg_integer()) -> jesse_error:error_reasons())} |
{'allowed_errors', non_neg_integer() | 'infinity'} |
{'default_schema_ver', binary()} |
{'schema_loader_fun', fun((binary()) -> {'ok', jesse:json_term()} | jesse:json_term() | 'not_found')}.

-type jesse_options() :: [jesse_option()].

-spec validate(ne_binary() | kz_json:object(), kz_json:object()) ->
                      {'ok', kz_json:object()} |
                      {'error', jesse_error:error()}.
-spec validate(ne_binary() | kz_json:object(), kz_json:object(), jesse_options()) ->
                      {'ok', kz_json:object()} |
                      {'error', jesse_error:error()}.
validate(SchemaJObj, DataJObj) ->
    validate(SchemaJObj, DataJObj, [{'schema_loader_fun', fun load/1}
                                    ,{'allowed_errors', 'infinity'}
                                   ]).

validate(<<_/binary>> = Schema, DataJObj, Options) ->
    {'ok', SchemaJObj} = load(Schema),
    validate(SchemaJObj, DataJObj, Options);
validate(SchemaJObj, DataJObj, Options) ->
    jesse:validate_with_schema(SchemaJObj, DataJObj, Options).
