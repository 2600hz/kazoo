%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2018, 2600Hz
%%% @doc Module for interacting with JSON schema docs
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_json_schema).

-export([add_defaults/2
        ,load/1, fload/1
        ,flush/0, flush/1
        ,validate/2, validate/3
        ,errors_to_jobj/1, errors_to_jobj/2
        ,error_to_jobj/1, error_to_jobj/2
        ,validation_error/4
        ,build_error_message/2
        ,default_object/1, filter/2
        ]).

-ifdef(TEST).
-export([flatten/1]).
-endif.

-export_type([validation_error/0, validation_errors/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").


-type extra_validator() :: fun((jesse:json_term(), jesse_state:state()) -> jesse_state:state()).
-type validator_option() :: 'use_defaults' | 'apply_defaults_to_empty_objects'.
-type validator_options() :: [validator_option()].

-type jesse_option() :: {'parser_fun', fun((_) -> _)} |
{'error_handler', fun((jesse_error:error_reason(), [jesse_error:error_reason()], non_neg_integer()) ->
 [jesse_error:error_reason()])} |
{'allowed_errors', non_neg_integer() | 'infinity'} |
{'default_schema_ver', binary()} |
{'schema_loader_fun', fun((kz_term:ne_binary()) -> {'ok', kz_json:object()} | kz_json:object() | 'not_found')} |
{'extra_validator', extra_validator()} |
{'setter_fun', fun((kz_json:path(), kz_json:json_term(), kz_json:object()) -> kz_json:object())} |
{'validator_options', validator_options()}.

-type jesse_options() :: [jesse_option()].

-define(DEFAULT_OPTIONS, [{'schema_loader_fun', fun load/1}
                         ,{'allowed_errors', 'infinity'}
                         ,{'extra_validator', fun kz_json_schema_extensions:extra_validator/2}
                         ,{'setter_fun', fun set_value/3}
                         ,{'validator_options', ['use_defaults'
                                                ,'apply_defaults_to_empty_objects'
                                                ]}
                         ]).

-ifdef(TEST).
load(Schema) -> fload(Schema).
-else.
-spec load(kz_term:ne_binary() | string()) -> {'ok', kz_json:object()} |
                                      {'error', any()}.
load(<<"./", Schema/binary>>) -> load(Schema);
load(<<"file://", Schema/binary>>) -> load(Schema);
load(<<_/binary>> = Schema) ->
    case kz_datamgr:open_cache_doc(?KZ_SCHEMA_DB, Schema, [{'cache_failures', ['not_found']}]) of
        {'error', _E}=E -> E;
        {'ok', JObj} -> {'ok', kz_json:insert_value(<<"id">>, Schema, JObj)}
    end;
load(Schema) -> load(kz_term:to_binary(Schema)).
-endif.

-spec fload(kz_term:ne_binary() | string()) -> {'ok', kz_json:object()} |
                                       {'error', 'not_found'}.
fload(<<"./", Schema/binary>>) -> fload(Schema);
fload(<<"file://", Schema/binary>>) -> fload(Schema);
fload(<<_/binary>> = Schema) ->
    case filelib:is_regular(Schema) of
        'true' -> fload_file(Schema);
        'false' -> find_and_fload(Schema)
    end;
fload(Schema) -> fload(kz_term:to_binary(Schema)).

-spec find_and_fload(kz_term:ne_binary()) ->
                            {'ok', kz_json:object()} |
                            {'error', 'not_found'}.
find_and_fload(Schema) ->
    PrivDir = code:priv_dir('crossbar'),
    SchemaPath = filename:join([PrivDir, "couchdb", "schemas", maybe_add_ext(Schema)]),
    case filelib:is_regular(SchemaPath) of
        'true' -> fload_file(SchemaPath);
        'false'-> {'error', 'not_found'}
    end.

-spec fload_file(kz_term:ne_binary()) -> {'ok', kz_json:object()}.
fload_file(SchemaPath) ->
    Schema = filename:basename(SchemaPath, ".json"),
    case file:read_file(SchemaPath) of
        {'ok', SchemaJSON} -> {'ok', kz_json:insert_value(<<"id">>, Schema, kz_json:decode(SchemaJSON))};
        Error -> Error
    end.

-spec maybe_add_ext(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_add_ext(Schema) ->
    case filename:extension(Schema) of
        <<".json">> -> Schema;
        _Ext -> <<Schema/binary, ".json">>
    end.

-spec flush() -> 'ok'.
flush() ->
    kz_datamgr:flush_cache_docs(?KZ_SCHEMA_DB).

-spec flush(kz_term:ne_binary()) -> 'ok'.
flush(Schema) ->
    kz_datamgr:flush_cache_doc(?KZ_SCHEMA_DB, Schema).

-spec add_defaults(kz_term:api_object() | kz_term:ne_binary(), kz_term:ne_binary() | kz_json:object()) -> kz_term:api_object().
add_defaults(JObj, <<_/binary>> = Schema) ->
    {'ok', SchemaJObj} = load(Schema),
    add_defaults(JObj, SchemaJObj);
add_defaults(JObj, SchemaJObj) ->
    try validate(SchemaJObj, JObj) of
        {'ok', WithDefaultsJObj} -> WithDefaultsJObj;
        {'error', Err} ->
            lager:debug("schema has errors : ~p ", [Err]),
            JObj
    catch
        _Ex:_Err ->
            lager:debug("exception getting schema default ~p : ~p", [_Ex, _Err]),
            JObj
    end.

-spec validate(kz_term:ne_binary() | kz_json:object(), kz_json:object()) ->
                      {'ok', kz_json:object()} |
                      jesse_error:error().
validate(SchemaJObj, DataJObj) ->
    validate(SchemaJObj, DataJObj, ?DEFAULT_OPTIONS).

-ifdef(TEST).
-define(DEFAULT_LOADER, fun fload/1).
-else.
-define(DEFAULT_LOADER, fun load/1).
-endif.

-spec validate(kz_term:ne_binary() | kz_json:object(), kz_json:object(), jesse_options()) ->
                      {'ok', kz_json:object()} |
                      jesse_error:error().
validate(<<_/binary>> = Schema, DataJObj, Options) ->
    Fun = props:get_value('schema_loader_fun', Options, ?DEFAULT_LOADER),
    {'ok', SchemaJObj} = Fun(Schema),
    validate(SchemaJObj, DataJObj, props:insert_values([{'schema_loader_fun', ?DEFAULT_LOADER}], Options));
validate(SchemaJObj, DataJObj, Options0) when is_list(Options0) ->
    Options = props:insert_values(?DEFAULT_OPTIONS, Options0),
    jesse:validate_with_schema(SchemaJObj, DataJObj, Options).

-type option() :: {'version', kz_term:ne_binary()} |
                  {'error_code', integer()} |
                  {'error_message', kz_term:ne_binary()}.
-type options() :: [option()].

-type validation_error() :: {integer(), kz_term:ne_binary(), kz_json:object()}.
-type validation_errors() :: [validation_error()].

-spec errors_to_jobj([jesse_error:error_reason()]) ->
                            validation_errors().
errors_to_jobj(Errors) ->
    errors_to_jobj(Errors, [{'version', ?CURRENT_VERSION}]).

-spec errors_to_jobj([jesse_error:error_reason()], options()) ->
                            validation_errors().
errors_to_jobj(Errors, Options) ->
    [error_to_jobj(Error, Options) || Error <- Errors].

-spec error_to_jobj(jesse_error:error_reason()) ->
                           validation_error().
error_to_jobj(Error) ->
    error_to_jobj(Error, [{'version', ?CURRENT_VERSION}]).

-spec error_to_jobj(jesse_error:error_reason(), options()) ->
                           validation_error().
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,'external_error'
              ,Message
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"error">>
                    ,kz_json:from_list(
                       [{<<"message">>, Message}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_min_length'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Minimum = kz_json:get_value(<<"minLength">>, FailedSchemaJObj),
    MinLen = kz_term:to_binary(Minimum),

    validation_error(FailedKeyPath
                    ,<<"minLength">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"String must be at least ", MinLen/binary, " characters">>}
                       ,{<<"target">>, Minimum}
                       ,{<<"cause">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_max_length'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Maximum = kz_json:get_value(<<"maxLength">>, FailedSchemaJObj),
    MaxLen = kz_term:to_binary(Maximum),

    validation_error(FailedKeyPath
                    ,<<"maxLength">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"String must not be more than ", MaxLen/binary, " characters">>}
                       ,{<<"target">>, Maximum}
                       ,{<<"cause">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_length'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Min = kz_json:get_integer_value(<<"minLength">>, FailedSchemaJObj),
    FailedLength = byte_size(FailedValue),

    case is_integer(Min)
        andalso Min > FailedLength
    of
        'true' ->
            error_to_jobj({'data_invalid'
                          ,FailedSchemaJObj
                          ,'wrong_min_length'
                          ,FailedValue
                          ,FailedKeyPath
                          }
                         ,Options
                         );
        'false' ->
            error_to_jobj({'data_invalid'
                          ,FailedSchemaJObj
                          ,'wrong_max_length'
                          ,FailedValue
                          ,FailedKeyPath
                          }
                         ,Options
                         )
    end;
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_in_range'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Min = kz_json:get_integer_value(<<"minimum">>, FailedSchemaJObj),

    case is_integer(Min)
        andalso Min > FailedValue
    of
        'true' ->
            error_to_jobj({'data_invalid'
                          ,FailedSchemaJObj
                          ,'not_minimum'
                          ,FailedValue
                          ,FailedKeyPath
                          }
                         ,Options
                         );
        'false' ->
            error_to_jobj({'data_invalid'
                          ,FailedSchemaJObj
                          ,'not_maximum'
                          ,FailedValue
                          ,FailedKeyPath
                          }
                         ,Options
                         )
    end;
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_in_enum'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"enum">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Value not found in enumerated list of values">>}
                       ,{<<"target">>, kz_json:get_value(<<"enum">>, FailedSchemaJObj, [])}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_minimum'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Minimum = kz_json:get_first_defined([<<"minimum">>, <<"exclusiveMinimum">>], FailedSchemaJObj),
    Min = kz_term:to_binary(Minimum),

    validation_error(FailedKeyPath
                    ,<<"minimum">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Value must be at least ", Min/binary>>}
                       ,{<<"target">>, Minimum}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_maximum'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Maximum = kz_json:get_first_defined([<<"maximum">>, <<"exclusiveMaximum">>], FailedSchemaJObj),
    Max = kz_term:to_binary(Maximum),

    validation_error(FailedKeyPath
                    ,<<"maximum">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Value must be at most ", Max/binary>>}
                       ,{<<"target">>, Maximum}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_size'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Minimum = kz_json:get_value(<<"minItems">>, FailedSchemaJObj),
    Maximum = kz_json:get_value(<<"maxItems">>, FailedSchemaJObj),

    case length(FailedValue) of
        N when N < Minimum ->
            error_to_jobj({'data_invalid'
                          ,FailedSchemaJObj
                          ,'wrong_min_items'
                          ,FailedValue
                          ,FailedKeyPath
                          }
                         ,Options
                         );
        N when N > Maximum ->
            error_to_jobj({'data_invalid'
                          ,FailedSchemaJObj
                          ,'wrong_max_items'
                          ,FailedValue
                          ,FailedKeyPath
                          }
                         ,Options
                         )
    end;
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_min_items'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Minimum = kz_json:get_value(<<"minItems">>, FailedSchemaJObj),
    Min = kz_term:to_binary(Minimum),

    validation_error(FailedKeyPath
                    ,<<"minItems">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"The list must have at least ", Min/binary, " items">>}
                       ,{<<"target">>, Minimum}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_max_items'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Maximum = kz_json:get_value(<<"maxItems">>, FailedSchemaJObj),
    Max = kz_term:to_binary(Maximum),

    validation_error(FailedKeyPath
                    ,<<"maxItems">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"The list is more than ", Max/binary, " items">>}
                       ,{<<"target">>, Maximum}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_min_properties'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Minimum = kz_json:get_value(<<"minProperties">>, FailedSchemaJObj),
    Min = kz_term:to_binary(Minimum),

    validation_error(FailedKeyPath
                    ,<<"minProperties">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"The object must have at least ", Min/binary, " keys">>}
                       ,{<<"target">>, Minimum}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,{'not_unique', _Item}
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"uniqueItems">>
                    ,kz_json:from_list([{<<"message">>, <<"List of items is not unique">>}])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,'no_extra_properties_allowed'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"additionalProperties">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Strict checking of data is enabled; only include schema-defined properties">>}]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,'no_extra_items_allowed'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"additionalItems">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Strict checking of data is enabled; only include schema-defined items">>}]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'no_match'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Pattern = kz_json:get_value(<<"pattern">>, FailedSchemaJObj),
    validation_error(FailedKeyPath
                    ,<<"pattern">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Failed to match pattern '", Pattern/binary, "'">>}
                       ,{<<"target">>, Pattern}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,'missing_required_property'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath ++ [FailedValue]
                    ,<<"required">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Field is required but missing">>}]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,'missing_dependency'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"dependencies">>
                    ,kz_json:from_list([{<<"message">>, <<"Dependencies were not validated">>}])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_divisible'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    DivBy = kz_json:get_binary_value(<<"divisibleBy">>, FailedSchemaJObj),
    validation_error(FailedKeyPath
                    ,<<"divisibleBy">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Value not divisible by ", DivBy/binary>>}
                       ,{<<"target">>, DivBy}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_allowed'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Disallow = get_disallow(FailedSchemaJObj),
    validation_error(FailedKeyPath
                    ,<<"disallow">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Value is disallowed by ", Disallow/binary>>}
                       ,{<<"target">>, Disallow}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_type'
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Types = get_types(FailedSchemaJObj),
    validation_error(FailedKeyPath
                    ,<<"type">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Value did not match type(s): ", Types/binary>>}
                       ,{<<"target">>, Types}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,{'missing_required_property', FailKey}
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath ++ [FailKey]
                    ,<<"required">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Field is required but missing">>}]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,FailMsg
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    lager:debug("failed message: ~p", [FailMsg]),
    lager:debug("failed schema: ~p", [_FailedSchemaJObj]),
    lager:debug("failed value: ~p", [_FailedValue]),
    lager:debug("failed keypath: ~p", [FailedKeyPath]),
    validation_error(FailedKeyPath
                    ,kz_term:to_binary(FailMsg)
                    ,kz_json:from_list([{<<"message">>, <<"failed to validate">>}])
                    ,Options
                    );
error_to_jobj({'schema_invalid'
              ,Schema
              ,{schema_not_found, SchemaName}
              }
             ,Options
             ) ->
    Name = case kz_term:to_binary(SchemaName) of
               <<"file://", FileName/binary>> -> FileName;
               FileName -> FileName
           end,
    lager:error("schema has errors: ~p: ~p", [Name, Schema]),
    validation_error([<<"schema">>]
                    ,<<"schema">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"schema not found">>}
                       ,{<<"schema">>, Name}
                       ])
                    ,Options
                    );
error_to_jobj({'schema_invalid'
              ,Schema
              ,Error
              }
             ,Options
             ) ->
    lager:error("schema has errors: ~p: ~p", [Error, Schema]),
    validation_error([<<"schema">>]
                    ,<<"schema">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"schema is invalid">>}
                       ,{<<"schema">>, Error}
                       ])
                    ,Options
                    );
error_to_jobj(Other, _Options) ->
    throw({'schema_error', Other}).

%%------------------------------------------------------------------------------
%% @doc Add a validation error to the list of request errors
%% @end
%%------------------------------------------------------------------------------
-spec validation_error(kz_json:path(), kz_term:ne_binary(), kz_json:object(), options()) ->
                                  validation_error().
validation_error(Property, <<"type">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"items">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"required">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"minimum">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"maximum">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"minItems">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"maxItems">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"uniqueItems">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"pattern">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"minLength">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"maxLength">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"maxSize">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"enum">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"format">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"divisibleBy">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"too_few_properties">>=C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);

%% Not unique within the datastore
validation_error(Property, <<"unique">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
%% User is not authorized to update the property
validation_error(Property, <<"forbidden">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
%% Date range is invalid, too small, or too large
validation_error(Property, <<"date_range">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
%% Value was required to locate a resource, but failed (like account_name)
validation_error(Property, <<"not_found">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
%% Value's keys didn't match property
validation_error(Property, <<"patternProperties">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"disabled">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"expired">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
%% Generic
validation_error(Property, <<"invalid">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"schema">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"additionalProperties">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, Code, Message, Options) ->
    lager:warning("UNKNOWN ERROR CODE: ~p", [Code]),
    depreciated_validation_error(Property, Code, Message, Options).

-spec depreciated_validation_error(kz_json:path(), kz_term:ne_binary(), kz_json:object(), options()) ->
                                          validation_error().
depreciated_validation_error(<<"account">>, <<"expired">>, Message, Options) ->
    build_validate_error([<<"account">>]
                        ,<<"expired">>
                        ,Message
                        ,props:set_values([{'error_code', 423}
                                          ,{'error_message', <<"locked">>}
                                          ]
                                         ,Options
                                         )
                        );
depreciated_validation_error(<<"account">>, <<"disabled">>, Message, Options) ->
    build_validate_error([<<"account">>]
                        ,<<"disabled">>
                        ,Message
                        ,props:set_values([{'error_code', 423}
                                          ,{'error_message', <<"locked">>}
                                          ]
                                         ,Options
                                         )
                        );
depreciated_validation_error(Property, Code, Message, Options) ->
    build_validate_error(Property, Code, Message
                        ,set_default_options(Options)
                        ).

-spec set_default_options(options()) -> options().
set_default_options(Options) ->
    props:set_values([{'version', ?CURRENT_VERSION}
                     ,{'error_code', 400}
                     ,{'error_message', <<"validation error">>}
                     ]
                    ,Options
                    ).

-spec build_validate_error(kz_json:path(), kz_term:ne_binary(), kz_json:object(), options()) ->
                                  validation_error().
build_validate_error(Property, Code, Message, Options) ->
    %% Maintain the same error format we are currently using until we are ready to
    %% convert to something that makes sense....
    Version = props:get_value('version', Options, ?CURRENT_VERSION),

    Error = build_error_message(Version, Message),

    Key = kz_binary:join(Property, <<".">>),

    {props:get_value('error_code', Options)
    ,props:get_value('error_message', Options)
    ,kz_json:set_values([{[Key, Code], Error}], kz_json:new())
    }.

-spec build_error_message(kz_term:ne_binary(), kz_json:object()) -> kz_json:object() | kz_term:ne_binary().
build_error_message(?VERSION_1, JObj) ->
    kz_json:get_value(<<"message">>, JObj);
build_error_message(_Version, JObj) ->
    JObj.

-spec get_disallow(kz_json:object()) -> kz_term:ne_binary().
get_disallow(JObj) ->
    case kz_json:get_value(<<"disallow">>, JObj) of
        <<_/binary>> = Disallow -> Disallow;
        Disallows when is_list(Disallows) -> kz_binary:join(Disallows)
    end.

-spec get_types(kz_json:object()) -> kz_term:ne_binary().
get_types(JObj) ->
    case kz_json:get_first_defined([<<"type">>, <<"types">>], JObj) of
        <<_/binary>> = Type -> Type;
        Types when is_list(Types) -> kz_binary:join(Types);
        _TypeSchema -> <<"type schema">>
    end.

-spec flatten(kz_json:object()) -> kz_json:flat_object().
flatten(?EMPTY_JSON_OBJECT=Empty) -> Empty;
flatten(?JSON_WRAPPER(L) = Schema) when is_list(L) ->
    kz_json:from_list(
      lists:flatten(
        flatten_props(kz_json:get_json_value(<<"properties">>, Schema)
                     ,[]
                     ,Schema
                     )
       )
     ).

flatten_props('undefined', Path, Obj) -> flatten_prop(Path, Obj);
flatten_props(?JSON_WRAPPER(L), Path, Obj) when is_list(L) ->
    [maybe_flatten_props(K, V, Path, Obj)
     || {K, V} <- L
    ].

maybe_flatten_props(K, ?JSON_WRAPPER(_)=V, Path, _Obj) ->
    flatten_props(kz_json:get_json_value(<<"properties">>, V), Path ++ [K], V);
maybe_flatten_props(K, _V, Path, Obj) ->
    flatten_prop(Path ++ [K], Obj).

flatten_prop(Path, ?JSON_WRAPPER(L) = Value) when is_list(L) ->
    case lists:last(Path) of
        <<"default">> -> [{Path, Value}];
        _ -> [{Path ++ [K], V} || {K,V} <- L]
    end.

-spec default_object(string() | kz_term:ne_binary() | kz_json:object()) -> kz_json:object().
default_object([_|_]=SchemaId) ->
    default_object(list_to_binary(SchemaId));
default_object(?NE_BINARY=SchemaId) ->
    {'ok', Schema} = load(SchemaId),
    default_object(Schema);
default_object(Schema) ->
    try validate(Schema, kz_json:new()) of
        {'ok', JObj} -> JObj;
        {'error', Err} ->
            lager:debug("schema has errors : ~p ", [Err]),
            kz_json:new()
    catch
        _Ex:_Err ->
            lager:debug("exception getting schema default ~p : ~p", [_Ex, _Err]),
            kz_util:log_stacktrace(erlang:get_stacktrace()),
            kz_json:new()
    end.

-spec filtering_list(kz_json:object()) -> list(kz_json:keys() | []).
filtering_list(Schema) ->
    Flattened = flatten(Schema),
    lists:usort([lists:droplast(Keys)
                 || Keys <- kz_json:get_keys(Flattened),
                    [] =/= Keys
                ]).

-spec filter(kz_json:object(), kz_json:object()) -> kz_json:object().
filter(JObj, Schema) ->
    Filter = filtering_list(Schema),

    FilteredFlat = kz_json:filter(fun({K, _}) -> lists:member(K, Filter) end
                                 ,kz_json:flatten(JObj)
                                 ),
    kz_json:expand(FilteredFlat).

set_value(Path, Value, JObj) ->
    FixedPath = fix_path(Path),
    kz_json:set_value(FixedPath, Value, JObj).

-spec fix_path(kz_json:path()) -> kz_json:path().
fix_path(Path) ->
    [fix_el(El) || El <- Path].

%% JSON array indicies are 0-indexed, Erlang's are 1-indexed
%% If an indicie is found, convert (incr) from JSON- to Erlang-based indicie
-spec fix_el(kz_json:key() | non_neg_integer()) -> kz_json:key() | non_neg_integer().
fix_el(I) when is_integer(I) -> I+1;
fix_el(El) -> El.
