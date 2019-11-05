%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc Generate schema for Kazoo AMQP APIs.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_schemas).

-export([process/0, process_app/1, process_module/1
        ,to_schemas/0, to_schemas/1, to_schema/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_amqp/src/api/kapi_presence.hrl").
-include_lib("kazoo_amqp/src/api/kapi_route.hrl").

-define(DEBUG(_Fmt, _Args), 'ok').
%%-define(DEBUG(Fmt, Args), io:format([$~, $p, $  | Fmt], [?LINE | Args])).

-record(acc, {kapi_name = <<"empty">> :: kz_term:ne_binary() %% s/kapi_(.+)/\1/
             ,api_name = <<"empty">> :: kz_term:api_ne_binary() %% api function
             ,app_schemas = kz_json:new() :: kz_json:object()
             ,project_schemas = kz_json:new() :: kz_json:object()
             ,schema_dir = kz_ast_util:default_schema_priv_dir() :: file:filename_all()
             }).
-type acc() :: #acc{}.

-spec to_schemas() -> 'ok'.
to_schemas() ->
    kz_json:foreach(fun update_schema/1, process()).

-spec to_schemas(atom()) -> 'ok'.
to_schemas(App) ->
    kz_json:foreach(fun update_schema/1, process_app(App)).

-spec to_schema(module()) -> 'ok'.
to_schema(KapiModule) ->
    kz_json:foreach(fun update_schema/1, process_module(KapiModule)).

update_schema({PrivDir, KapiModule}) ->
    kz_json:foreach(fun({_KAPI, API}) -> update_schema(PrivDir, API) end, KapiModule).

update_schema(PrivDir, API) ->
    kz_json:foreach(fun({_A, Schema}) -> maybe_update_schema(PrivDir, Schema) end, API).

maybe_update_schema(PrivDir, API) ->
    maybe_update_schema(PrivDir, API, kz_doc:id(API)).

maybe_update_schema(_PrivDir, _JObj, 'undefined') -> 'ok';
maybe_update_schema(PrivDir, GeneratedJObj, ID) ->
    ?DEBUG("adding ~s to ~s: ~s", [ID, PrivDir, kz_json:encode(GeneratedJObj)]),
    Path = kz_ast_util:schema_path(<<ID/binary, ".json">>, PrivDir),
    ExistingJObj = existing_schema(Path),
    MergedJObj = kz_json:merge(fun kz_json:merge_left/2
                              ,ExistingJObj
                              ,GeneratedJObj
                              ),
    UpdatedSchema = kz_json:delete_key(<<"id">>, MergedJObj),
    case kz_json:are_equal(ExistingJObj, UpdatedSchema) of
        'true' -> 'ok';
        'false' ->
            'ok' = file:write_file(Path, kz_json:encode(UpdatedSchema))
    end.

-spec existing_schema(file:filename_all()) -> kz_json:object().
existing_schema(Name) ->
    case kz_json_schema:fload(Name) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} ->
            kz_json:new();
        {'error', _E} ->
            io:format("failed to find ~s: ~p~n", [Name, _E]),
            kz_json:new()
    end.

-spec process() -> kz_json:object().
process() ->
    io:format("process kapi modules: "),
    Options = [{'expression', fun expression_to_schema/2}
              ,{'function', fun set_function/3}
              ,{'module', fun print_dot/2}
              ,{'accumulator', #acc{}}
              ,{'application', fun add_app_config/2}
              ,{'after_application', fun add_schemas_to_bucket/2}
              ],
    #acc{project_schemas=Schemas} = kazoo_ast:walk_project(Options),
    io:format(" done~n", []),
    Schemas.

-spec process_app(atom()) -> kz_json:object().
process_app(App) ->
    io:format("process kapi modules: "),
    Options = [{'expression', fun expression_to_schema/2}
              ,{'function', fun set_function/3}
              ,{'module', fun print_dot/2}
              ,{'accumulator', #acc{}}
              ,{'application', fun add_app_config/2}
              ,{'after_application', fun add_schemas_to_bucket/2}
              ],
    #acc{project_schemas=Schemas} = kazoo_ast:walk_app(App, Options),
    io:format(" done~n", []),
    Schemas.

-spec process_module(module()) -> kz_json:object().
process_module(KapiModule) ->
    io:format("process kapi module ~s: ", [KapiModule]),
    Options = [{'expression', fun expression_to_schema/2}
              ,{'function', fun set_function/3}
              ,{'module', fun print_dot/2}
              ,{'accumulator', #acc{}}
              ,{'application', fun add_app_config/2}
              ,{'after_application', fun add_schemas_to_bucket/2}
              ],
    Acc = kazoo_ast:walk_modules([KapiModule], Options),
    #acc{project_schemas=Schemas} = add_schemas_to_bucket(KapiModule, Acc),

    io:format(" done~n", []),
    Schemas.

-spec print_dot(kz_term:ne_binary() | module(), acc()) ->
                       acc() |
                       {'skip', acc()}.
print_dot(<<"kapi_fs">>, #acc{}=Acc) ->
    {'skip', Acc};
print_dot(<<"kapi_schemas">>, #acc{}=Acc) ->
    {'skip', Acc};
print_dot(<<"kapi_definition">>, #acc{}=Acc) ->
    {'skip', Acc};
print_dot(<<"kapi_", Module/binary>>, #acc{}=Acc) ->
    io:format("."),
    ?DEBUG("process kapi_~s~n", [Module]),
    Acc#acc{kapi_name=Module};
print_dot(<<_/binary>>, #acc{}=Acc) ->
    {'skip', Acc};
print_dot(Module, #acc{}=Acc) ->
    print_dot(kz_term:to_binary(Module), Acc).

-spec add_app_config(atom(), acc()) -> acc().
add_app_config(App, Acc) ->
    case application:get_env(App, 'schemas_to_priv') of
        {'ok', 'true'} ->
            ?DEBUG("detected schemas will go in ~s/priv~n", [App]),
            Acc#acc{schema_dir = kz_term:to_binary(code:priv_dir(App))};
        _ ->
            ?DEBUG("schemas for ~s go in default location~n", [App]),
            Acc#acc{schema_dir = kz_ast_util:default_schema_priv_dir()}
    end.

add_schemas_to_bucket(_App, #acc{schema_dir = PrivDir
                                ,app_schemas = AppSchemas
                                ,project_schemas = ProjectSchemas
                                }=Acc) ->
    ProjectSchema = kz_json:get_json_value(PrivDir, ProjectSchemas, kz_json:new()),

    ?DEBUG("merging dir ~s / app ~p into proj ~p~n", [PrivDir, AppSchemas, ProjectSchema]),

    UpdatedSchema = kz_json:merge(ProjectSchema, AppSchemas),
    ?DEBUG("merged: ~p~n", [UpdatedSchema]),

    Acc#acc{app_schemas = kz_json:new()
           ,project_schemas = kz_json:set_value(PrivDir, UpdatedSchema, ProjectSchemas)
           }.

-spec set_function(kz_term:ne_binary() | function(), integer(), acc()) -> acc().
set_function(<<_/binary>> = Function, 0, #acc{kapi_name = <<"notifications">>}=Acc) ->
    case kz_binary:reverse(Function) of
        <<"noitinifed_", Nuf/binary>> ->
            ?DEBUG("api definition for ~s~n", [Function]),
            Acc#acc{api_name=kz_binary:reverse(Nuf)};
        _ ->
            ?DEBUG("ignoring non api definition function ~p/0~n", [Function]),
            {'skip', Acc}
    end;
set_function(<<_/binary>> = _Function, _Arity, #acc{kapi_name = <<"notifications">>}=Acc) ->
    ?DEBUG("ignoring non api definition function ~p/~p~n", [_Function, _Arity]),
    {'skip', Acc};
set_function(<<_/binary>> = Function, _Arity, #acc{}=Acc) ->
    case kz_binary:reverse(Function) of
        <<"v_", Nuf/binary>> ->
            ?DEBUG("validator ~s~n", [Function]),
            Acc#acc{api_name=kz_binary:reverse(Nuf)};
        _ ->
            ?DEBUG("builder ~s~n", [Function]),
            Acc#acc{api_name=Function}
    end;
set_function(Function, Arity, Acc) ->
    set_function(kz_term:to_binary(Function), Arity, Acc).

expression_to_schema(?RECORD('kapi_definition', Fields), Acc) ->
    kapi_definition_to_schema(Fields, Acc);
expression_to_schema(_, #acc{kapi_name = <<"notifications">>}=Acc) ->
    Acc;
expression_to_schema(?MOD_FUN_ARGS('kz_api', 'build_message', [_Prop, Required, Optional]), Acc) ->
    properties_to_schema(kz_ast_util:ast_to_list_of_binaries(Required)
                        ,optional_validators(Optional)
                        ,Acc
                        );
expression_to_schema(?MOD_FUN_ARGS('kz_api', 'build_message_specific_headers', [_Prop, Required, Optional]), Acc) ->
    properties_to_schema(kz_ast_util:ast_to_list_of_binaries(Required)
                        ,optional_validators(Optional)
                        ,Acc
                        );
expression_to_schema(?MOD_FUN_ARGS('kz_api', 'validate', [_Prop, _Required, Values, Types]), Acc) ->
    validators_to_schema(ast_to_proplist(Values), ast_to_proplist(Types), Acc);
expression_to_schema(?MOD_FUN_ARGS('kz_api', 'validate_message', [_Prop, _Required, Values, Types]), Acc) ->
    validators_to_schema(ast_to_proplist(Values), ast_to_proplist(Types), Acc);
expression_to_schema(_Expr, Acc) ->
    Acc.

kapi_definition_to_schema(Fields, Acc) ->
    lists:foldl(fun kapi_definition_field_to_schema/2, Acc, Fields).

kapi_definition_field_to_schema(?RECORD_FIELD_BIND('required_headers', Required), Acc) ->
    properties_to_schema(kz_ast_util:ast_to_list_of_binaries(Required), [], Acc);
kapi_definition_field_to_schema(?RECORD_FIELD_BIND('optional_headers', Optional), Acc) ->
    properties_to_schema([], optional_validators(Optional), Acc);
kapi_definition_field_to_schema(?RECORD_FIELD_BIND('values', Values), Acc) ->
    validators_to_schema(ast_to_proplist(Values), [], Acc);
kapi_definition_field_to_schema(?RECORD_FIELD_BIND('types', Types), Acc) ->
    validators_to_schema([], ast_to_proplist(Types), Acc);
kapi_definition_field_to_schema(_, Acc) ->
    Acc.

optional_validators(?EMPTY_LIST) -> [];
optional_validators(Optional) ->
    kz_ast_util:ast_to_list_of_binaries(Optional).

properties_to_schema(RequiredHs, OptionalHs, #acc{}=Acc) ->
    Schema = kapi_schema(Acc),
    {DetectedRequired, OptHs} = just_required(RequiredHs),
    SchemaRequired = kz_json:get_list_value(<<"required">>, Schema, []),

    Required = lists:merge(lists:sort(SchemaRequired), lists:sort(DetectedRequired)),

    Optional = OptHs ++ OptionalHs,

    WithFields = lists:foldl(fun add_field/2, Schema, Required ++ Optional),

    Updated = set_required(WithFields, Required),

    set_kapi_schema(Acc, Updated).

set_required(Schema, []) -> Schema;
set_required(Schema, Required) ->
    kz_json:set_value(<<"required">>, Required, Schema).

%% see kapi_pusher for [Required, [Opt1, Opt2]]
just_required(Required) ->
    lists:foldl(fun flatten_required/2, {[], []}, Required).

flatten_required(<<_/binary>>=R, {Req, Opt}) ->
    {[R | Req], Opt};
flatten_required([R, <<_/binary>>=Optional], {Req, Opt}) ->
    {[R | Req], [Optional | Opt]};
flatten_required([R, Optional], {Req, Opt}) ->
    {[R | Req], Optional ++ Opt}.

kapi_schema(#acc{app_schemas=Schemas
                ,kapi_name=KAPI
                ,api_name=API
                }) ->
    case kz_json:get_json_value([KAPI, API], Schemas, kz_json:new()) of
        ?EMPTY_JSON_OBJECT -> base_schema(KAPI, API);
        Schema -> Schema
    end.

base_schema(KAPI, API) ->
    kz_json:from_list([{<<"_id">>, <<"kapi.", KAPI/binary, ".", API/binary>>}
                      ,{<<"$schema">>, <<"http://json-schema.org/draft-04/schema#">>}
                      ,{<<"description">>, <<"AMQP API for ", KAPI/binary, ".", API/binary>>}
                      ,{<<"type">>, <<"object">>}
                      ]).

set_kapi_schema(#acc{app_schemas=Schemas
                    ,kapi_name=KAPI
                    ,api_name=API
                    }=Acc, Schema) ->
    case kz_json:is_empty(kz_json:get_json_value(<<"properties">>, Schema, kz_json:new())) of
        'true' -> Acc;
        'false' ->
            Acc#acc{app_schemas=kz_json:set_value([KAPI, API], Schema, Schemas)}
    end.

add_field([_|_]=Fields, Schema) ->
    Path = property_path(Fields),

    Guessed = guess_field_default(lists:last(Fields)),

    ?DEBUG("adding path ~p guessed type ~p~n", [Path, Guessed]),

    Properties = kz_json:get_json_value(Path, Schema, kz_json:new()),

    kz_json:set_value(Path, kz_json:merge(Guessed, Properties), Schema);
add_field(Field, Schema) ->
    add_field([Field], Schema).

guess_field_default(<<"Call">>) ->
    kz_json:from_list([{<<"type">>, <<"object">>}]);
guess_field_default(<<"Notifications">>) ->
    kz_json:from_list([{<<"type">>, <<"object">>}]);
guess_field_default(<<"Reason">>) ->
    kz_json:from_list([{<<"type">>, <<"string">>}]);
guess_field_default(<<"Preview">>) ->
    kz_json:from_list([{<<"type">>, <<"boolean">>}]);
guess_field_default(<<"Endpoints">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"$ref">>, <<"kapi.dialplan.bridge_endpoint">>}
                                                       ,{<<"type">>, <<"object">>}
                                                       ])}
                      ]);
guess_field_default(<<"Commands">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"type">>, <<"object">>}])}
                      ]);
guess_field_default(<<"Tones">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"$ref">>, <<"kapi.dialplan.tones_req_tone_headers">>}])}
                      ]);
guess_field_default(<<"Additional-Headers">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"type">>, <<"object">>}])}
                      ]);
guess_field_default(<<"Routes">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"type">>, <<"object">>}
                                                       ,{<<"$ref">>, <<"kapi.route.resp_route">>}
                                                       ])}
                      ]);
guess_field_default(<<"On-Success">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"type">>, <<"object">>}])}
                      ]);
guess_field_default(<<"Accounts">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"type">>, <<"string">>}])}
                      ]);
guess_field_default(<<"Usernames">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"type">>, <<"string">>}])}
                      ]);
guess_field_default(<<"Flags">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"type">>, <<"string">>}])}
                      ]);
guess_field_default(<<"Bridge-Actions">>) ->
    kz_json:from_list([{<<"type">>, <<"object">>}]);
guess_field_default(<<"Endpoint-Actions">>) ->
    kz_json:from_list([{<<"type">>, <<"object">>}]);
guess_field_default(Field) ->
    guess_field_default_rev(kz_json:normalize_key(kz_binary:reverse(Field))).

guess_field_default_rev(<<"di_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"string">>}]);
guess_field_default_rev(<<"sdi_">>) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>, kz_json:from_list([{<<"type">>, <<"string">>}])}
                      ]);
guess_field_default_rev(<<"tuoemit_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"integer">>}]);
guess_field_default_rev(<<"emit_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"integer">>}]);
guess_field_default_rev(<<"lru_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"string">>}]);
guess_field_default_rev(<<"nosaer_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"string">>}]);
guess_field_default_rev(<<"yek_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"string">>}]);
guess_field_default_rev(<<"timil_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"integer">>}]);
guess_field_default_rev(<<"tfel_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"integer">>}]);
guess_field_default_rev(<<"srav_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"object">>}]);
guess_field_default_rev(<<"sredaeh_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"object">>}]);
guess_field_default_rev(<<"snoitca_", _/binary>>) ->
    kz_json:from_list([{<<"type">>, <<"object">>}]);
guess_field_default_rev(_Dleif) ->
    ?DEBUG("failed to guess ~s~n", [kz_binary:reverse(_Dleif)]),
    kz_json:from_list([{<<"type">>, <<"string">>}]).

validators_to_schema(Values, Types, Acc) ->
    Schema = lists:foldl(fun add_validator/2, kapi_schema(Acc), Values ++ Types),
    set_kapi_schema(Acc, Schema).

add_validator({Field, 'undefined'}, Schema) ->
    Properties = kz_json:get_json_value([<<"properties">>, Field], Schema, kz_json:new()),
    kz_json:set_value([<<"properties">>, Field], Properties, Schema);
add_validator({[_|_]=Fields, Value}, Schema) ->
    Path = property_path(Fields),
    Properties = kz_json:get_json_value(Path, Schema, kz_json:new()),
    ValidatorProperties = validator_properties(Value),
    Updated = kz_json:merge(ValidatorProperties, Properties),
    kz_json:set_value(Path, Updated, Schema);
add_validator({Field, Value}, Schema) ->
    add_validator({[Field], Value}, Schema).

property_path(Fields) ->
    lists:foldr(fun(Field, Acc) -> [<<"properties">>, Field | Acc] end, [], Fields).

validator_properties(<<_/binary>>=Value) ->
    kz_json:from_list([{<<"type">>, <<"string">>}
                      ,{<<"enum">>, [Value]}
                      ]);
validator_properties([<<_/binary>>|_]=Values) ->
    kz_json:from_list([{<<"type">>, <<"string">>}
                      ,{<<"enum">>, Values}
                      ]);
validator_properties({_, 'is_pos_integer', 1}) ->
    kz_json:from_list([{<<"type">>, <<"integer">>}
                      ,{<<"minimum">>, 1}
                      ]);
validator_properties({_, 'is_integer', 1}) ->
    kz_json:from_list([{<<"type">>, <<"integer">>}]);
validator_properties({_, 'is_binary', 1}) ->
    kz_json:from_list([{<<"type">>, <<"string">>}]);
validator_properties({_, 'is_boolean', 1}) ->
    kz_json:from_list([{<<"type">>, <<"boolean">>}]);
validator_properties({_, 'is_list', 1}) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>
                       ,kz_json:from_list([{<<"type">>, <<"string">>}])
                       }
                      ]);
validator_properties({'kz_json', 'is_json_object', 1}) ->
    kz_json:from_list([{<<"type">>, <<"object">>}]);
validator_properties({'kz_json', 'are_json_objects', 1}) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>
                       ,kz_json:from_list([{<<"type">>, <<"object">>}])
                       }
                      ]);
validator_properties({'kz_term', 'is_boolean', 1}) ->
    kz_json:from_list([{<<"type">>, <<"boolean">>}]);
validator_properties({'kz_term', 'is_ne_binary', 1}) ->
    kz_json:from_list([{<<"type">>, <<"string">>}
                      ,{<<"minLength">>, 1}
                      ]);
validator_properties({'kz_term', 'is_ne_binaries', 1}) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>
                       ,kz_json:from_list([{<<"type">>, <<"string">>}])
                       }
                      ]);
validator_properties({'kapi_dialplan', 'terminators_v', 1}) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>
                       ,kz_json:from_list([{<<"type">>, <<"string">>}
                                          ,{<<"enum">>, ?ANY_DIGIT}
                                          ])
                       }
                      ]);
validator_properties({'kz_term', 'is_ne_list', 1}) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"minItems">>, 1}
                      ]);
validator_properties({'kapi_dialplan', 'b_leg_events_v', 1}) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>
                       ,kz_json:from_list([{<<"type">>, <<"string">>}
                                          ,{<<"enum">>, ?CALL_EVENTS}
                                          ])
                       }
                      ]);
validator_properties({'function', 'b_leg_events_v', 1}) ->
    kz_json:from_list([{<<"type">>, <<"array">>}
                      ,{<<"items">>
                       ,kz_json:from_list([{<<"type">>, <<"string">>}
                                          ,{<<"enum">>, ?CALL_EVENTS}
                                          ])
                       }
                      ]);
validator_properties({'function', 'tone_timeout_v', 1}) ->
    kz_json:from_list([{<<"type">>, <<"integer">>}
                      ,{<<"minimum">>, 0}
                      ]);
validator_properties({'function', 'binding_digit_timeout_v', 1}) ->
    kz_json:from_list([{<<"type">>, <<"integer">>}
                      ,{<<"minimum">>, 0}
                      ]);
validator_properties({'function', 'has_cost_parameters', 1}) ->
    kz_json:from_list([{<<"type">>, <<"object">>}
                      ,{<<"properties">>, cost_parameters_schema()}
                      ]);
validator_properties({'function', 'store_media_content_v', 1}) ->
    kz_json:from_list([{<<"type">>, <<"string">>}]);
validator_properties({'function', _F, _A}) ->
    ?LOG_DEBUG("  no properties for fun ~p/~p~n", [_F, _A]),
    kz_json:from_list([{<<"type">>, <<"string">>}]).

cost_parameters_schema() ->
    kz_json:from_list(
      [cost_parameter_schema(K) || K <- ?ROUTE_REQ_COST_PARAMS]
     ).

cost_parameter_schema(Parameter) ->
    {Parameter, kz_json:from_list([{<<"type">>, <<"integer">>}])}.

ast_to_proplist(ASTList) ->
    ast_to_proplist(ASTList, []).

ast_to_proplist(?EMPTY_LIST, Acc) ->
    lists:reverse(Acc);
ast_to_proplist(?VAR(_), Acc) ->
    lists:reverse(Acc);
ast_to_proplist(?LIST(H, ?MOD_FUN_ARGS('props', 'delete', [ASTKey, ASTProps])), Acc) ->
    Key = kz_ast_util:binary_match_to_binary(ASTKey),
    Props = ast_to_proplist(ASTProps),
    [ast_to_kv(H) | props:set_values(props:delete(Key, Props), Acc)];
ast_to_proplist(?LIST(H, ?MOD_FUN_ARGS('props', 'delete_keys', [ASTKeys, ASTProps])), Acc) ->
    Keys = kz_ast_util:ast_to_list_of_binaries(ASTKeys),
    Props = ast_to_proplist(ASTProps),
    [ast_to_kv(H) | props:set_values(props:delete_keys(Keys, Props), Acc)];
ast_to_proplist(?LIST(H, T), Acc) ->
    ast_to_proplist(T, [ast_to_kv(H) | Acc]).

ast_to_kv(?TUPLE([Key, Value])) ->
    {kz_ast_util:binary_match_to_binary(Key)
    ,ast_to_value(Value)
    }.

ast_to_value(?MOD_FUN_ARGS('kapi_presence', 'presence_states', [])) ->
    ?PRESENCE_STATES;
ast_to_value(?BINARY(_)=Bin) ->
    kz_ast_util:binary_match_to_binary(Bin);
ast_to_value(?FA(F, A)) ->
    {'function', F, A};
ast_to_value(?MFA(M, F, A)) ->
    {M, F, A};
ast_to_value(?LIST(_, _)=ASTList) ->
    kz_ast_util:ast_to_list_of_binaries(ASTList);
ast_to_value(?ANON(Clauses)) ->
    clauses_to_value(Clauses);
ast_to_value(?VAR(_)) ->
    'undefined'.

clauses_to_value(?CLAUSE([?BINARY_STRING(Value)
                         ,?BINARY_VAR('_')
                         ]
                        ,_Guard
                        ,[?ATOM('true')])
                ) ->
    {'regex', list_to_binary([$^, kz_term:to_binary(Value), ".+$"])};
clauses_to_value(_Clause) -> 'undefined'.
