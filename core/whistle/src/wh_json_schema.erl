%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%% Module for interacting with JSON schema docs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_json_schema).

-export([add_defaults/2
         ,load/1
         ,flush/0, flush/1
         ,validate/2, validate/3
        ]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-spec load(ne_binary()) -> {'ok', wh_json:object()} |
                           {'error', any()}.
load(<<_/binary>> = Schema) ->
    case couch_mgr:open_cache_doc(?WH_SCHEMA_DB, Schema, [{'cache_failures', ['not_found']}]) of
        {'error', _E}=E -> E;
        {'ok', JObj} -> {'ok', wh_json:insert_value(<<"id">>, Schema, JObj)}
    end.

-spec flush() -> 'ok'.
-spec flush(ne_binary()) -> 'ok'.
flush() ->
    couch_mgr:flush_cache_docs(?WH_SCHEMA_DB).
flush(Schema) ->
    couch_mgr:flush_cache_doc(?WH_SCHEMA_DB, Schema).

-spec add_defaults(api_object() | ne_binary(), ne_binary() | wh_json:object()) -> api_object().
add_defaults(JObj, <<_/binary>> = Schema) ->
    {'ok', SchemaJObj} = load(Schema),
    add_defaults(JObj, SchemaJObj);
add_defaults(JObj, SchemaJObj) ->
    wh_json:foldl(fun add_defaults_foldl/3
                  ,JObj
                  ,wh_json:get_value(<<"properties">>, SchemaJObj, wh_json:new())
                 ).

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
            maybe_update_data_with_sub(SchemaKey, SchemaValue, JObj);
        <<"array">> ->
            {_, JObj1} = lists:foldl(fun(SubJObj, Acc) ->
                                             maybe_add_sub_properties_foldl(SchemaKey, SchemaValue, SubJObj, Acc)
                                     end
                                     ,{1, JObj}
                                     ,wh_json:get_value(SchemaKey, JObj, [])
                                    ),
            JObj1;
        _Type -> JObj
    end.

-spec maybe_add_sub_properties_foldl(wh_json:key(), wh_json:object(), wh_json:json_term(), {pos_integer(), wh_json:object()}) ->
                                            {pos_integer(), wh_json:object()}.
maybe_add_sub_properties_foldl(SchemaKey, SchemaValue, SubJObj, {Idx, JObj}) ->
    case add_defaults(SubJObj, wh_json:get_value(<<"items">>, SchemaValue, wh_json:new())) of
        'undefined' -> {Idx+1, JObj};
        NewSubJObj -> {Idx+1, wh_json:set_value([SchemaKey, Idx], NewSubJObj, JObj)}
    end.

-spec maybe_update_data_with_sub(wh_json:key(), wh_json:object(), wh_json:object()) -> wh_json:object().
maybe_update_data_with_sub(SchemaKey, SchemaValue, JObj) ->
    case add_defaults(wh_json:get_value(SchemaKey, JObj), SchemaValue) of
        'undefined' -> JObj;
        SubJObj ->  wh_json:set_value(SchemaKey, SubJObj, JObj)
    end.

-spec maybe_add_default(wh_json:key(), wh_json:json_term(), api_object()) -> api_object().
maybe_add_default(Key, Default, JObj) ->
    case wh_json:is_json_object(JObj)
        andalso wh_json:get_value(Key, JObj)
    of
        'undefined' when JObj =/= 'undefined' -> wh_json:set_value(Key, Default, JObj);
        'undefined' ->  wh_json:set_value(Key, Default, wh_json:new());
        _Value -> JObj
    end.

-type jesse_option() :: {'parser_fun', fun((_) -> _)} |
{'error_handler', fun((jesse_error:error_reason(), jesse_error:error_reasons(), non_neg_integer()) -> jesse_error:error_reasons())} |
{'allowed_errors', non_neg_integer()} |
{'default_schema_ver', binary()} |
{'schema_loader_fun', fun((binary()) -> {'ok', jesse:json_term()} | jesse:json_term() | 'not_found')}.

-type jesse_options() :: [jesse_option()].

-spec validate(ne_binary() | wh_json:object(), wh_json:object()) ->
                      {'ok', wh_json:object()} |
                      {'error', jesse_error:error()}.
-spec validate(ne_binary() | wh_json:object(), wh_json:object(), jesse_options()) ->
                      {'ok', wh_json:object()} |
                      {'error', jesse_error:error()}.
validate(SchemaJObj, DataJObj) ->
    validate(SchemaJObj, DataJObj, [{'schema_loader_fun', fun load/1}]).
validate(<<_/binary>> = Schema, DataJObj, Options) ->
    {'ok', SchemaJObj} = load(Schema),
    validate(SchemaJObj, DataJObj, Options);
validate(SchemaJObj, DataJObj, Options) ->
    jesse:validate_with_schema(SchemaJObj, DataJObj, Options).
