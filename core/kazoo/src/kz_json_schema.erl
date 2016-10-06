%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz INC
%%% @doc
%%% Module for interacting with JSON schema docs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_json_schema).

-export([add_defaults/2
        ,load/1, fload/1
        ,flush/0, flush/1
        ,validate/2, validate/3
        ,errors_to_jobj/1, errors_to_jobj/2
        ,error_to_jobj/1, error_to_jobj/2
        ,validation_error/4
        ,build_error_message/2
        ]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-spec load(ne_binary() | string()) -> {'ok', kz_json:object()} |
                                      {'error', any()}.
load(<<"./", Schema/binary>>) -> load(Schema);
load(<<_/binary>> = Schema) ->
    case kz_datamgr:open_cache_doc(?KZ_SCHEMA_DB, Schema, [{'cache_failures', ['not_found']}]) of
        {'error', _E}=E -> E;
        {'ok', JObj} -> {'ok', kz_json:insert_value(<<"id">>, Schema, JObj)}
    end;
load(Schema) -> load(kz_util:to_binary(Schema)).

-spec fload(ne_binary() | string()) -> {'ok', kz_json:object()} |
                                       {'error', any()}.
fload(<<"./", Schema/binary>>) -> fload(Schema);
fload(<<_/binary>> = Schema) ->
    io:format("loading schema ~s~n", [Schema]),
    Path = <<"/opt/kazoo/applications/crossbar/priv/couchdb/schemas/", Schema/binary, ".json">>,
    case file:read_file(Path) of
        {'error', _E}=E -> E;
        {'ok', Bin} -> {'ok', kz_json:insert_value(<<"id">>, Schema, kz_json:decode(Bin))}
    end;
fload(Schema) -> fload(kz_util:to_binary(Schema)).

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
    kz_json:foldl(fun defaults_foldl/3
                 ,JObj
                 ,kz_json:get_value(<<"properties">>, SchemaJObj, kz_json:new())
                 ).

-spec defaults_foldl(kz_json:path(), kz_json:object(), api_object()) -> api_object().
defaults_foldl(SchemaKey, SchemaValue, JObj) ->
    case kz_json:get_value(<<"default">>, SchemaValue) of
        'undefined' ->
            maybe_sub_properties(SchemaKey, SchemaValue, JObj);
        Default ->
            maybe_sub_properties(SchemaKey
                                ,SchemaValue
                                ,maybe_default(SchemaKey, Default, JObj)
                                )
    end.

-spec maybe_sub_properties(kz_json:path(), kz_json:object(), api_object()) -> api_object().
maybe_sub_properties(SchemaKey, SchemaValue, JObj) ->
    case kz_json:get_value(<<"type">>, SchemaValue) of
        <<"object">> ->
            maybe_update_data_with_sub(SchemaKey, SchemaValue, JObj);
        <<"array">> ->
            {_, JObj1} = lists:foldl(fun(SubJObj, Acc) ->
                                             maybe_sub_properties_foldl(SchemaKey, SchemaValue, SubJObj, Acc)
                                     end
                                    ,{1, JObj}
                                    ,kz_json:get_value(SchemaKey, JObj, [])
                                    ),
            JObj1;
        _Type -> JObj
    end.

-spec maybe_sub_properties_foldl(kz_json:path(), kz_json:object(), kz_json:json_term(), {pos_integer(), kz_json:object()}) ->
                                        {pos_integer(), kz_json:object()}.
maybe_sub_properties_foldl(SchemaKey, SchemaValue, SubJObj, {Idx, JObj}) ->
    case add_defaults(SubJObj, kz_json:get_value(<<"items">>, SchemaValue, kz_json:new())) of
        'undefined' -> {Idx+1, JObj};
        NewSubJObj -> {Idx+1, kz_json:set_value([SchemaKey, Idx], NewSubJObj, JObj)}
    end.

-spec maybe_update_data_with_sub(kz_json:path(), kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_update_data_with_sub(SchemaKey, SchemaValue, JObj) ->
    case add_defaults(kz_json:get_value(SchemaKey, JObj), SchemaValue) of
        'undefined' -> JObj;
        SubJObj ->  kz_json:set_value(SchemaKey, SubJObj, JObj)
    end.

-spec maybe_default(kz_json:path(), kz_json:json_term(), api_object()) -> api_object().
maybe_default(Key, Default, JObj) ->
    case kz_json:is_json_object(JObj)
        andalso kz_json:get_value(Key, JObj)
    of
        'undefined' when JObj =/= 'undefined' -> kz_json:set_value(Key, Default, JObj);
        'undefined' ->  kz_json:set_value(Key, Default, kz_json:new());
        _Value -> JObj
    end.

-type jesse_option() :: {'parser_fun', fun((_) -> _)} |
{'error_handler', fun((jesse_error:error_reason(), [jesse_error:error_reason()], non_neg_integer()) ->
 [jesse_error:error_reason()])} |
{'allowed_errors', non_neg_integer() | 'infinity'} |
{'default_schema_ver', binary()} |
{'schema_loader_fun', fun((binary()) -> {'ok', jesse:json_term()} | jesse:json_term() | 'not_found')}.

-type jesse_options() :: [jesse_option()].

-spec validate(ne_binary() | kz_json:object(), kz_json:object()) ->
                      {'ok', kz_json:object()} |
                      jesse_error:error().
-spec validate(ne_binary() | kz_json:object(), kz_json:object(), jesse_options()) ->
                      {'ok', kz_json:object()} |
                      jesse_error:error().
validate(SchemaJObj, DataJObj) ->
    validate(SchemaJObj, DataJObj, [{'schema_loader_fun', fun load/1}
                                   ,{'allowed_errors', 'infinity'}
                                   ]).

validate(<<_/binary>> = Schema, DataJObj, Options) ->
    Fun = props:get_value('schema_loader_fun', Options, fun load/1),
    {'ok', SchemaJObj} = Fun(Schema),
    validate(SchemaJObj, DataJObj, Options);
validate(SchemaJObj, DataJObj, Options) ->
    jesse:validate_with_schema(SchemaJObj, DataJObj, Options).

-type option() :: {'version', ne_binary()} |
                  {'error_code', integer()} |
                  {'error_message', ne_binary()}.
-type options() :: [option()].

-type validation_error() :: {integer(), ne_binary(), kz_json:object()}.

-spec errors_to_jobj([jesse_error:error_reason()]) ->
                            [validation_error()].
-spec errors_to_jobj([jesse_error:error_reason()], options()) ->
                            [validation_error()].
errors_to_jobj(Errors) ->
    errors_to_jobj(Errors, [{'version', ?CURRENT_VERSION}]).

errors_to_jobj(Errors, Options) ->
    [error_to_jobj(Error, Options) || Error <- Errors].

-spec error_to_jobj(jesse_error:error_reason()) ->
                           validation_error().
-spec error_to_jobj(jesse_error:error_reason(), options()) ->
                           validation_error().
error_to_jobj(Error) ->
    error_to_jobj(Error, [{'version', ?CURRENT_VERSION}]).

error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_min_length'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    Minimum = kz_json:get_value(<<"minLength">>, FailedSchemaJObj),
    MinLen = kz_util:to_binary(Minimum),

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
    MaxLen = kz_util:to_binary(Maximum),

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
    Min = kz_util:to_binary(Minimum),

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
    Max = kz_util:to_binary(Maximum),

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
    Min = kz_util:to_binary(Minimum),

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
    Max = kz_util:to_binary(Maximum),

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
    Min = kz_util:to_binary(Minimum),

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
              ,_FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
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
                    ,kz_util:to_binary(FailMsg)
                    ,kz_json:from_list([{<<"message">>, <<"failed to validate">>}])
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a validation error to the list of request errors
%% @end
%%--------------------------------------------------------------------
-spec validation_error(kz_json:path(), ne_binary(), kz_json:object(), options()) ->
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
validation_error(Property, Code, Message, Options) ->
    lager:warning("UNKNOWN ERROR CODE: ~p", [Code]),
    depreciated_validation_error(Property, Code, Message, Options).

-spec depreciated_validation_error(kz_json:path(), ne_binary(), kz_json:object(), options()) ->
                                          validation_error().
depreciated_validation_error(<<"account">>, <<"expired">>, Message, Options) ->
    build_validate_error([<<"account">>]
                        ,<<"expired">>
                        ,Message
                        ,props:insert_values([{'error_code', 423}
                                             ,{'error_message', <<"locked">>}
                                             ]
                                            ,Options
                                            )
                        );
depreciated_validation_error(<<"account">>, <<"disabled">>, Message, Options) ->
    build_validate_error([<<"account">>]
                        ,<<"disabled">>
                        ,Message
                        ,props:insert_values([{'error_code', 423}
                                             ,{'error_message', <<"locked">>}
                                             ]
                                            ,Options
                                            )
                        );
depreciated_validation_error(Property, Code, Message, Options) ->
    build_validate_error(Property, Code, Message
                        ,insert_default_options(Options)
                        ).

-spec insert_default_options(options()) -> options().
insert_default_options(Options) ->
    props:insert_values([{'version', ?CURRENT_VERSION}
                         ,{'error_code', 400}
                         ,{'error_message', <<"invalid_request">>}
                        ]
                       ,Options
                       ).

-spec build_validate_error(kz_json:path(), ne_binary(), kz_json:object(), options()) ->
                                  validation_error().
build_validate_error(Property, Code, Message, Options) ->
    %% Maintain the same error format we are currently using until we are ready to
    %% convert to something that makes sense....
    Version = props:get_value('version', Options, ?CURRENT_VERSION),

    Error = build_error_message(Version, Message),

    Key = kz_util:join_binary(Property, <<".">>),

    {props:get_value('error_code', Options)
    ,props:get_value('error_message', Options)
    ,kz_json:set_values([{[Key, Code], Error}], kz_json:new())
    }.

-spec build_error_message(ne_binary(), kz_json:object()) -> kz_json:object() | ne_binary().
build_error_message(?VERSION_1, JObj) ->
    kz_json:get_value(<<"message">>, JObj);
build_error_message(_Version, JObj) ->
    JObj.

-spec get_disallow(kz_json:object()) -> ne_binary().
get_disallow(JObj) ->
    case kz_json:get_value(<<"disallow">>, JObj) of
        <<_/binary>> = Disallow -> Disallow;
        Disallows when is_list(Disallows) -> kz_util:join_binary(Disallows)
    end.

-spec get_types(kz_json:object()) -> ne_binary().
get_types(JObj) ->
    case kz_json:get_first_defined([<<"type">>, <<"types">>], JObj) of
        <<_/binary>> = Type -> Type;
        Types when is_list(Types) -> kz_util:join_binary(Types);
        _TypeSchema -> <<"type schema">>
    end.
