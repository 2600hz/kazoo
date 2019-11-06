%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Module for interacting with JSON schema docs
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_json_schema).

-export([add_defaults/2
        ,load/1, fload/1
        ,flush/0, flush/1
        ,diff/0, diff/1
        ,diff_schema/1, diff_schema/2
        ,delete/1

        ,validate/2, validate/3
        ,fix_js_types/2
        ,errors_to_jobj/1, errors_to_jobj/2
        ,error_to_jobj/1, error_to_jobj/2
        ,validation_error/4
        ,build_error_message/2
        ,default_object/1
        ,filter/2
        ]).

-ifdef(TEST).
-export([flatten/1]).
-endif.

-export_type([validation_error/0
             ,validation_errors/0
             ,extra_validator_options/0
             ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-type extra_validator() :: fun((jesse:json_term(), jesse_state:state()) -> jesse_state:state()).
-type validator_option() :: 'use_defaults' | 'apply_defaults_to_empty_objects'.
-type validator_options() :: [validator_option()].
-type extra_validator_option() :: {'stability_level', kz_term:ne_binary()}.
-type extra_validator_options() :: [extra_validator_option()].

-type parser_fun() :: fun((kz_term:ne_binary()) -> kz_json:json_term()).
-type error_handler_fun() :: fun((jesse_error:error_reason(), [jesse_error:error_reason()], non_neg_integer()) ->
                                        [jesse_error:error_reason()]).
-type schema_loader_fun() :: fun((kz_term:ne_binary()) -> {'ok', kz_json:object()} | kz_json:object() | 'not_found').
-type setter_fun() :: fun((kz_json:path(), kz_json:json_term(), kz_json:object()) -> kz_json:object()).
-type getter_fun() :: fun((kz_json:path(), kz_json:object(), jesse:json_term()) -> jesse:json_term()).

-type jesse_option() :: {'parser_fun', parser_fun()} |
                        {'error_handler', error_handler_fun()} |
                        {'allowed_errors', non_neg_integer() | 'infinity'} |
                        {'default_schema_ver', binary()} |
                        {'schema_loader_fun', schema_loader_fun()} |
                        {'external_validator', extra_validator()} |
                        {'setter_fun', setter_fun()} |
                        {'getter_fun', getter_fun()} |
                        {'validator_options', validator_options()} |
                        {'extra_validator_options', extra_validator_options()}.

-type jesse_options() :: [jesse_option()].

-define(DEFAULT_OPTIONS, [{'schema_loader_fun', fun load/1}
                         ,{'allowed_errors', 'infinity'}
                         ,{'setter_fun', fun set_value/3}
                         ,{'getter_fun', fun get_value/3}
                         ,{'validator_options', ['use_defaults'
                                                ,'apply_defaults_to_empty_objects'
                                                ]}
                         ]).

setup_extra_validator(Options) ->
    ExtraOptions = props:get_value('extra_validator_options', Options, []),
    Fun = fun(Value, State) ->
                  kz_json_schema_extensions:extra_validator(Value, State, ExtraOptions)
          end,
    props:set_value({'external_validator', Fun}, Options).

-type load_return() :: {'ok', kz_json:object()} |
                       {'error', 'not_found'} |
                       kz_datamgr:data_error().
-ifdef(TEST).
load(Schema) -> fload(Schema).
-else.
-spec load(kz_term:ne_binary() | string()) -> load_return().
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
                                               {'error', 'not_found' | file:posix()}.
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
                            {'error', 'not_found' | file:posix()}.
find_and_fload(Schema) ->
    PrivDir = code:priv_dir('crossbar'),
    SchemaPath = filename:join([PrivDir, "couchdb", "schemas", maybe_add_ext(Schema)]),
    case filelib:is_regular(SchemaPath) of
        'true' -> fload_file(SchemaPath);
        'false'-> {'error', 'not_found'}
    end.

-spec fload_file(kz_term:ne_binary()) -> {'ok', kz_json:object()} |
                                         {'error', 'not_found' | file:posix()}.
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

-type diff_verbosity() :: 'undefined' | 'diff' | 'schema'.
-spec diff() -> 'ok'.
diff() ->
    diff('schema').

-spec diff(diff_verbosity()) -> 'ok'.
diff(Verbosity) ->
    PrivDir = code:priv_dir('crossbar'),
    Path = filename:join([PrivDir, "couchdb", "schemas"]),
    _ = filelib:fold_files(Path
                          ,"json$"
                          ,'false'
                          ,fun diff_schema/2
                          ,Verbosity
                          ),
    'ok'.

-spec diff_schema(file:filename_all()) -> 'ok'.
diff_schema(Filename) ->
    _ = diff_schema(Filename, 'diff'),
    'ok'.

-spec diff_schema(file:filename_all(), Verbosity) ->
                         Verbosity when Verbosity :: diff_verbosity().
diff_schema(Filename, Verbosity) ->
    SchemaName = filename:basename(Filename, ".json"),
    diff_schema(Filename, Verbosity, SchemaName, load(SchemaName)).

-spec diff_schema(file:filename_all(), Verbosity, kz_term:ne_binary(), load_return()) ->
                         Verbosity when Verbosity :: diff_verbosity().
diff_schema(Filename, Verbosity, SchemaName, {'ok', Schema}) ->
    {'ok', File} = fload(Filename),

    Public = kz_doc:public_fields(Schema, 'false'),

    JustSchemaDiff = kz_json:diff(Public, File),
    JustDiskDiff = kz_json:diff(kz_json:delete_key(<<"_id">>, File), Public),
    maybe_log_diff(Verbosity, SchemaName, JustSchemaDiff, JustDiskDiff),
    Verbosity;
diff_schema(_Filename, Verbosity, SchemaName, {'error', E}) ->
    maybe_log_diff_error(Verbosity, SchemaName, E),
    Verbosity.

maybe_log_diff('undefined', _Name, _JustSchemaDiff, _JustDiskDiff) -> 'ok';
maybe_log_diff(Verbosity, SchemaName, JustSchemaDiff, JustDiskDiff) ->
    case {kz_json:is_empty(JustSchemaDiff), kz_json:is_empty(JustDiskDiff)} of
        {'true', 'true'}  -> 'ok';
        _ when Verbosity =:= 'schema' ->
            io:format("'~s' differs from on-disk file:~n~s~s"
                     ,[SchemaName
                      ,printable_diff_keys(" in schema", JustSchemaDiff)
                      ,printable_diff_keys(" on disk", JustDiskDiff)
                      ]
                     );
        _ when Verbosity =:= 'diff' ->
            io:format("~s has keys that on-disk doesn't:~nin schema: ~s~non disk: ~s~n"
                     ,[SchemaName
                      ,kz_json:encode(JustSchemaDiff, ['pretty'])
                      ,kz_json:encode(JustDiskDiff, ['pretty'])
                      ]
                     );
        _ -> 'ok'
    end.

printable_diff_keys(Where, Diff) ->
    case kz_json:is_empty(Diff) of
        'true' -> [];
        'false' ->
            Paths = kz_json:get_keys(kz_json:flatten(Diff)),
            [Where, " only: \n", [["  ", kz_binary:join(Path, <<".">>), $\n] || Path <- Paths]]
    end.

maybe_log_diff_error('undefined', _Name, _E) -> 'ok';
maybe_log_diff_error(_, _SchemaName, _E) ->
    io:format("failed to load ~s: ~p~n", [_SchemaName, _E]).

-spec delete(kz_term:ne_binary()) -> 'ok'.
delete(<<Schema/binary>>) ->
    case load(Schema) of
        {'ok', Doc} ->
            {'ok', _} = kz_datamgr:del_doc(?KZ_SCHEMA_DB, Doc),
            io:format("deleted schema ~s~n", [Schema]);
        {'error', _E} ->
            io:format("failed to find schema ~s: ~p~n", [Schema, _E])
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
        {'error', Errors} ->
            lager:info("errors applying defaults : ~p", [errors_to_jobj(Errors)]),
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
    Options1 = props:insert_values(?DEFAULT_OPTIONS, Options0),
    Options = setup_extra_validator(Options1),
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
              ,{'external_error', Message}
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"error">>
                    ,kz_json:from_list(
                       [{<<"message">>, Message}
                       ,{<<"value">>, FailedValue}
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
                       ,{<<"value">>, FailedValue}
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
                       ,{<<"value">>, FailedValue}
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
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"enum">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Value not found in enumerated list of values">>}
                       ,{<<"target">>, kz_json:get_value(<<"enum">>, FailedSchemaJObj, [])}
                       ,{<<"value">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_minimum'
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_maximum'
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
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
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_max_items'
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_min_properties'
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,{'not_unique', _Item}
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"uniqueItems">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"List of items is not unique">>}
                       ,{<<"value">>, FailedValue}
                       ]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,'no_extra_properties_allowed'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"additionalProperties">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Strict checking of data is enabled; only include schema-defined properties">>}
                       ,{<<"value">>, FailedValue}
                       ]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,'no_extra_items_allowed'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"additionalItems">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Strict checking of data is enabled; only include schema-defined items">>}
                       ,{<<"value">>, FailedValue}
                       ]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'no_match'
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
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
                       [{<<"message">>, <<"Field is required but missing">>}
                       ,{<<"value">>, FailedValue}
                       ]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,'missing_dependency'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"dependencies">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Dependencies were not validated">>}
                       ,{<<"value">>, FailedValue}
                       ]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_divisible'
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'not_allowed'
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_type'
              ,FailedValue
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
                       ,{<<"value">>, FailedValue}
                       ])
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,{'missing_required_property', FailKey}
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath ++ [FailKey]
                    ,<<"required">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Field is required but missing">>}
                       ,{<<"value">>, FailedValue}
                       ]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,FailedSchemaJObj
              ,'wrong_format'
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    validation_error(FailedKeyPath
                    ,<<"wrong_format">>
                    ,kz_json:from_list(
                       [{<<"message">>, <<"Field is not in the correct format">>}
                       ,{<<"value">>, FailedValue}
                       ,{<<"format">>, kz_json:get_value(<<"format">>, FailedSchemaJObj)}
                       ]
                      )
                    ,Options
                    );
error_to_jobj({'data_invalid'
              ,_FailedSchemaJObj
              ,FailMsg
              ,FailedValue
              ,FailedKeyPath
              }
             ,Options
             ) ->
    lager:debug("failed message: ~p", [FailMsg]),
    lager:debug("failed schema: ~p", [_FailedSchemaJObj]),
    lager:debug("failed value: ~p", [FailedValue]),
    lager:debug("failed keypath: ~p", [FailedKeyPath]),
    validation_error(FailedKeyPath
                    ,kz_term:to_binary(FailMsg)
                    ,kz_json:from_list(
                       [{<<"message">>, <<"failed to validate">>}
                       ,{<<"value">>, FailedValue}
                       ]
                      )
                    ,Options
                    );
error_to_jobj({'schema_invalid'
              ,Schema
              ,{'schema_not_found', SchemaName}
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
error_to_jobj({'schema_invalid', Schema, Error}
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
validation_error(Property, <<"superfluous">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"invalid">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"schema">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"additionalProperties">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"additionalItems">> = C, Message, Options) ->
    depreciated_validation_error(Property, C, Message, Options);
validation_error(Property, <<"wrong_format">> = C, Message, Options) ->
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

    Keys = [binary:replace(Bin, <<".">>, <<"%2E">>, ['global'])
            || P <- Property,
               Bin <- [kz_term:to_binary(P)]
           ],
    Key = kz_binary:join(Keys, <<".">>),

    {props:get_value('error_code', Options)
    ,props:get_value('error_message', Options)
    ,kz_json:set_value([Key, Code], Error, kz_json:new())
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
    try validate(kz_json:delete_key(<<"required">>, Schema), kz_json:new()) of
        {'ok', JObj} -> JObj;
        {'error', Err} ->
            lager:debug("failed to build default object for ~s due to errors: ~p"
                       ,[kz_doc:id(Schema), Err]
                       ),
            kz_json:new()
    catch
        ?STACKTRACE(_Ex, _Err, ST)
        lager:error("exception getting schema default ~p : ~p", [_Ex, _Err]),
        kz_log:log_stacktrace(ST),
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

get_value(Path, JObj, Default) ->
    FixedPath = fix_path(Path),
    kz_json:get_value(FixedPath, JObj, Default).

-spec fix_path(kz_json:path()) -> kz_json:path().
fix_path(Path) ->
    [fix_el(El) || El <- Path].

%% JSON array indicies are 0-indexed, Erlang's are 1-indexed
%% If an index is found, convert (incr) from JSON- to Erlang-based index
-spec fix_el(kz_json:key() | non_neg_integer()) -> kz_json:key() | non_neg_integer().
fix_el(I) when is_integer(I) -> I+1;
fix_el(El) -> El.

-spec fix_js_types(kz_json:object(), [jesse_error:error_reason()]) ->
                          {'true', kz_json:object()} |
                          'false'.
fix_js_types(JObj, ValidationErrors) ->
    case lists:foldl(fun maybe_fix_js_type/2, {'false', JObj}, ValidationErrors) of
        {'false', _} -> 'false';
        {'true', _}=Fixed -> Fixed
    end.

-spec maybe_fix_js_type(validation_error(), {boolean(), kz_json:object()}) ->
                               {boolean(), kz_json:object()}.
maybe_fix_js_type({'data_invalid', SchemaJObj, 'wrong_type', Value, Key}, {WasFixed, JObj}) ->
    case kz_json:get_value(<<"type">>, SchemaJObj) of
        <<"integer">> -> maybe_fix_js_integer(Key, Value, WasFixed, JObj);
        <<"number">> -> maybe_fix_js_number(Key, Value, WasFixed, JObj);
        <<"boolean">> -> maybe_fix_js_boolean(Key, Value, WasFixed, JObj);
        _Type -> {WasFixed, JObj}
    end;
maybe_fix_js_type(_, Acc) -> Acc.

-spec maybe_fix_js_integer(kz_json:get_key(), kz_json:json_term(), boolean(), kz_json:object()) ->
                                  {boolean(), kz_json:object()}.
maybe_fix_js_integer(Key, Value, WasFixed, JObj) ->
    try kz_term:to_integer(Value) of
        V ->
            {'true', kz_json:set_value(maybe_fix_index(Key), V, JObj)}
    catch
        _E:_R ->
            lager:debug("error converting ~p to integer ~p: ~p", [Value, _E, _R]),
            {WasFixed, JObj}
    end.

-spec maybe_fix_js_boolean(kz_json:get_key(), kz_json:json_term(), boolean(), kz_json:object()) ->
                                  {boolean(), kz_json:object()}.
maybe_fix_js_boolean(Key, Value, WasFixed, JObj) ->
    try kz_term:to_boolean(Value) of
        V ->
            {'true', kz_json:set_value(maybe_fix_index(Key), V, JObj)}
    catch
        _E:_R ->
            lager:debug("error converting ~p to boolean ~p: ~p", [Value, _E, _R]),
            {WasFixed, JObj}
    end.

-spec maybe_fix_js_number(kz_json:get_key(), kz_json:json_term(), boolean(), kz_json:object()) ->
                                 {boolean(), kz_json:object()}.
maybe_fix_js_number(Key, Value, WasFixed, JObj) ->
    try kz_term:to_number(Value) of
        V -> {'true', kz_json:set_value(maybe_fix_index(Key), V, JObj)}
    catch
        _E:_R ->
            lager:debug("error converting ~p to number ~p: ~p", [Value, _E, _R]),
            {WasFixed, JObj}
    end.

-spec maybe_fix_index(kz_json:get_key()) -> kz_json:get_key().
maybe_fix_index(Keys)
  when is_list(Keys) ->
    [case is_integer(K) of
         'true' -> K + 1;
         'false' -> K
     end
     || K <- Keys
    ];
maybe_fix_index(Key) ->
    Key.
