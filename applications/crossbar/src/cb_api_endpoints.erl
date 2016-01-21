-module(cb_api_endpoints).
-compile([debug_info]).

-compile({'no_auto_import', [get/0]}).

-export([get/0
         ,to_swagger_json/0
        ]).
-include("crossbar.hrl").

-define(SWAGGER_INFO
       ,wh_json:from_list([{<<"title">>, <<"Crossbar">>}
                          ,{<<"description">>, <<"The Crossbar APIs">>}
                          ,{<<"license">>, wh_json:from_list([{<<"name">>, <<"Mozilla Public License 1.1">>}])}
                          ,{<<"version">>, <<"2.0">>}
                          ])
       ).

to_swagger_json() ->
    BaseSwagger = read_swagger_json(),
    BasePaths = wh_json:get_value(<<"paths">>, BaseSwagger, wh_json:new()),

    Paths = format_as_path_centric(get()),

    Swagger = wh_json:set_values([{<<"paths">>, to_swagger_paths(Paths, BasePaths)}
                                 ,{<<"definitions">>, to_swagger_definitions()}
                                 ,{<<"swagger">>, <<"2.0">>}
                                 ,{<<"info">>, ?SWAGGER_INFO}
                                 ]
                                ,BaseSwagger
                                ),
    write_swagger_json(Swagger).

-define(SCHEMAS_PATH(Schema), filename:join([code:priv_dir('crossbar')
                                            ,"couchdb"
                                            ,"schemas"
                                             ,Schema
                                            ])
       ).

to_swagger_definitions() ->
    SchemasPath = ?SCHEMAS_PATH(<<>>),
    filelib:fold_files(SchemasPath, ".json$", 'false', fun process_schema/2, wh_json:new()).

process_schema(SchemaJSONFile, Definitions) ->
    SchemaName = wh_util:to_binary(filename:basename(SchemaJSONFile, ".json")),

    {'ok', SchemaJSON} = file:read_file(SchemaJSONFile),
    SchemaJObj = wh_json:decode(SchemaJSON),

    wh_json:set_value(SchemaName, SchemaJObj, Definitions).

-define(SWAGGER_JSON, filename:join([code:priv_dir('crossbar')
                                    ,"couchdb"
                                    ,"swagger"
                                    ,"swagger.json"
                                    ])
       ).

read_swagger_json() ->
    case file:read_file(?SWAGGER_JSON) of
        {'ok', Bin} -> wh_json:decode(Bin);
        {'error', 'enoent'} -> wh_json:new()
    end.

-define(FORMAT_JSON, filename:join([code:lib_dir('crossbar')
                                    ,".."
                                    ,".."
                                    ,"scripts"
                                    ,"format-json.sh"
                                   ])
       ).

write_swagger_json(Swagger) ->
    'ok' = file:write_file(?SWAGGER_JSON, wh_json:encode(Swagger)),
    Res = os:cmd([?FORMAT_JSON, " ", ?SWAGGER_JSON]),
    io:format("os returned ~p~n", [Res]).

to_swagger_paths(Paths, BasePaths) ->
    wh_json:foldl(fun to_swagger_path/3, BasePaths, Paths).

to_swagger_path(Path, PathMeta, Acc) ->
    Methods = wh_json:get_value(<<"allowed_methods">>, PathMeta, []),
    Parameters = swagger_params(PathMeta),
    lists:foldl(fun(Method, Acc1) ->
                        add_swagger_path(Method, Acc1, Path, Parameters)
                end
               ,Acc
               ,Methods
               ).

add_swagger_path(Method, Acc, Path, Parameters) ->
    MethodJObj = wh_json:get_value([Path, Method], Acc),
    Vs = props:filter_undefined(
           [{[Path, Method], MethodJObj}
           ,maybe_add_schema(Path, Method, Parameters)
           ]
          ),
    io:format("add ~p~n", [Vs]),
    wh_json:insert_values(Vs, Acc).

maybe_add_schema(_Path, _Method, 'undefined') ->
    {'undefined', 'undefined'};
maybe_add_schema(Path, <<"put">> = Method, Parameters) ->
    {[Path, Method, <<"parameters">>], Parameters};
maybe_add_schema(Path, <<"post">> = Method, Parameters) ->
    {[Path, Method, <<"parameters">>], Parameters};
maybe_add_schema(_Path, _Method, _Parameters) ->
    {'undefined', 'undefined'}.

swagger_params(PathMeta) ->
    case wh_json:get_value(<<"schema">>, PathMeta) of
        'undefined' -> 'undefined';
        Schema ->
            wh_json:from_list([{<<"name">>, Schema}
                              ,{<<"in">>, <<"body">>}
                              ,{<<"required">>, 'true'}
                              ,{<<"schema">>, wh_json:from_list([{<<"$ref">>, <<"#/definitions/", Schema/binary>>}])}
                              ])
    end.

format_as_path_centric(Data) ->
    lists:foldl(fun format_pc_module/2
                ,wh_json:new()
                ,Data
               ).

format_pc_module({Module, Config}, Acc) ->
    ModuleName = module_name(Module),

    lists:foldl(fun(ConfigData, Acc1) ->
                        format_pc_config(ConfigData, Acc1, Module, ModuleName)
                end
               ,Acc
               ,Config
               );
format_pc_module(_MC, Acc) ->
    io:format("skipping ~p~n", [_MC]),
    Acc.

format_pc_config({Callback, Paths}, Acc, Module, ModuleName) ->
    lists:foldl(fun(Path, Acc1) ->
                        format_pc_callback(Path, Acc1, Module, ModuleName, Callback)
                end
               ,Acc
               ,Paths
               ).

format_pc_callback({[], []}, Acc, _Module, _ModuleName, _Callback) ->
    Acc;
format_pc_callback({Path, []}, Acc, _Module, ModuleName, Callback) ->
    PathName = path_name(Path, ModuleName),
    wh_json:set_value([PathName, wh_util:to_binary(Callback)], <<"not supported">>, Acc);
format_pc_callback({Path, Vs}, Acc, Module, ModuleName, Callback) ->
    PathName = path_name(Path, ModuleName),
    wh_json:set_values(
      props:filter_undefined(
        [{[PathName, wh_util:to_binary(Callback)]
         ,[wh_util:to_lower_binary(V) || V <- Vs]
         }
        ,maybe_include_schema(PathName, Module)
        ]
       )
                      ,Acc
       ).

maybe_include_schema(PathName, Module) ->
    M = base_module_name(Module),
    case filelib:is_file(?SCHEMAS_PATH(M)) of
        'true' ->
            {[PathName, <<"schema">>], base_module_name(Module)};
        'false' ->
            {'undefined', 'undefined'}
    end.

path_name(Path, ModuleName) ->
    wh_util:join_binary([<<>>, ModuleName | format_path_tokens(Path)], <<"/">>).

format_path_tokens(<<"/">>) -> [];
format_path_tokens(<<_/binary>> = Token) ->
    [format_path_token(Token)];
format_path_tokens(Tokens) ->
    [format_path_token(Token) || Token <- Tokens, Token =/= <<"/">>].

format_path_token(<<"_">>) ->
    <<"{ID}">>;
format_path_token(<<"_", Rest/binary>>) ->
    <<"{", (wh_util:to_upper_binary(Rest))/binary, "}">>;
format_path_token(Token) -> Token.

base_module_name(Module) when is_atom(Module) ->
    base_module_name(wh_util:to_binary(Module));
base_module_name(<<_/binary>>=Module) ->
    {'match', [Name|_]} = re:run(Module
                                ,<<"^cb_([a-z_]+?)(?:_v([0-9]))?$">>
                                ,[{'capture', 'all_but_first', 'binary'}]
                                ),
    Name.

module_name(Module) when is_atom(Module) ->
    module_name(wh_util:to_binary(Module));
module_name(<<_/binary>>=Module) ->
    case re:run(Module
               ,<<"^cb_([a-z_]+?)(?:_v([0-9]))?$">>
               ,[{'capture', 'all_but_first', 'binary'}]
               )
    of
        {'match', [<<"about">>]} -> <<?CURRENT_VERSION/binary, "/about">>;
        {'match', [<<"accounts">>]} -> <<?CURRENT_VERSION/binary, "/accounts">>;
        {'match', [<<"api_auth">>]} -> <<?CURRENT_VERSION/binary, "/api_auth">>;
        {'match', [<<"basic_auth">>]} -> <<?CURRENT_VERSION/binary, "/basic_auth">>;
        {'match', [<<"google_auth">>]} -> <<?CURRENT_VERSION/binary, "/google_auth">>;
        {'match', [<<"ip_auth">>]} -> <<?CURRENT_VERSION/binary, "/ip_auth">>;
        {'match', [<<"schemas">>]} -> <<?CURRENT_VERSION/binary, "/schemas">>;
        {'match', [<<"shared_auth">>]} -> <<?CURRENT_VERSION/binary, "/shared_auth">>;
        {'match', [<<"sup">>]} -> <<?CURRENT_VERSION/binary, "/sup">>;
        {'match', [<<"system_configs">>]} -> <<?CURRENT_VERSION/binary, "/system_configs">>;
        {'match', [<<"token_auth">>]} -> <<?CURRENT_VERSION/binary, "/shared_auth">>;
        {'match', [<<"ubiquiti_auth">>]} -> <<?CURRENT_VERSION/binary, "/ubiquiti_auth">>;
        {'match', [<<"user_auth">>]} -> <<?CURRENT_VERSION/binary, "/user_auth">>;
        {'match', [Name]} -> <<?CURRENT_VERSION/binary, "/accounts/{ACCOUNTID}/", Name/binary>>;
        {'match', [Name, Version]} ->
            <<"v", Version/binary, "/accounts/{ACCOUNTID}/", Name/binary>>
    end.

%% API

%% get() -> [{module, config()}]
%% config() :: [{callback(), [{path(), methods()}]}]
get() ->
    process_application('crossbar').

process_application(App) ->
    EBinDir = code:lib_dir(App, 'ebin'),
    io:format("looking in ~p~n", [EBinDir]),
    filelib:fold_files(EBinDir, "^cb_.*.beam$", 'false', fun process_module/2, []).

process_module(File, Acc) ->
    io:format("processing file ~p~n", [File]),
    {'ok', {Module, [{'exports', Fs}]}} = beam_lib:chunks(File, ['exports']),

    case process_exports(File, Module, Fs) of
        'undefined' -> Acc;
        Exports -> [Exports | Acc]
    end.

is_api_function({'allowed_methods', _Arity}) -> 'true';
%is_api_function({'content_types_provided', _Arity}) -> 'true';
is_api_function(_) ->  'false'.

process_exports(_File, 'api_resource', _) -> [];
process_exports(_File, 'cb_context', _) -> [];
process_exports(File, Module, Fs) ->
    case lists:any(fun is_api_function/1, Fs) of
        'false' ->
            io:format("skipping non-api module ~s~n", [Module]),
            'undefined';
        'true' ->
            process_api_module(File, Module)
    end.

process_api_module(File, Module) ->
    {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(File, ['abstract_code']),
    process_api_ast(Module, AST).

process_api_ast(Module, {'raw_abstract_v1', Attributes}) ->
    APIFunctions = [{F, A, Clauses}
                    || {'function', _Line, F, A, Clauses} <- Attributes,
                       is_api_function({F, A})
                   ],
    process_api_ast_functions(Module, APIFunctions).

process_api_ast_functions(Module, Functions) ->
    {Module
      ,[process_api_ast_function(Module, F, A, Cs) || {F, A, Cs} <- Functions]
     }.

process_api_ast_function(_Module, 'allowed_methods', _Arity, Clauses) ->
    Methods = find_http_methods(Clauses),
    io:format("found in ~p: ~p~n", [_Module, Methods]),
    {'allowed_methods', Methods};
process_api_ast_function(_Module, 'content_types_provided', _Arity, Clauses) ->
    ContentTypes = find_http_methods(Clauses),
    io:format("found ctp in ~p: ~p~n", [_Module, ContentTypes]),
    {'content_types_provided', ContentTypes}.

find_http_methods(Clauses) ->
    lists:foldl(fun find_http_methods_from_clause/2, [], Clauses).

find_http_methods_from_clause({'clause', _Line, ArgsList, _, ClauseBody}, Methods) ->
    [{args_list_to_path(ArgsList), find_methods(ClauseBody)}
     | Methods
    ].

args_list_to_path([]) ->
    <<"/">>;
args_list_to_path(Args) ->
    lists:reverse(lists:foldl(fun arg_to_path/2, [], Args)).

-define(BINARY(Value), {'bin',_, [{'bin_element', _, {'string', _, Value}, 'default', 'default'}]}).
-define(VAR_NAME(Name), {'var', _, Name}).

arg_to_path(?BINARY(Name), Acc) ->
    [wh_util:to_binary(Name) | Acc];
arg_to_path(?VAR_NAME('Context'), Acc) ->
    Acc;
arg_to_path(?VAR_NAME(Name), Acc) ->
    [wh_util:to_binary(Name) | Acc];
arg_to_path({'match', _, {'bin', _, _}, ?VAR_NAME(Name)}, Acc) ->
    [wh_util:to_binary(Name) | Acc].

find_methods(ClauseBody) ->
    find_methods(ClauseBody, []).
find_methods(ClauseBody, Acc) ->
    lists:usort(lists:foldl(fun find_methods_in_clause/2, Acc, ClauseBody)).

-define(LAGER_CALL, {'remote', _, {'atom', _, 'lager'}, _}).
-define(CB_CONTEXT_CALL(Fun)
        ,{'remote', _
         ,{'atom', _, 'cb_context'}
         ,{'atom', _, Fun}
         }
       ).

find_methods_in_clause(?VAR_NAME('Context'), Acc) ->
    Acc;
find_methods_in_clause(?VAR_NAME('Context1'), Acc) ->
    Acc;
find_methods_in_clause({'call', _, ?LAGER_CALL, _}, Acc) ->
    Acc;
find_methods_in_clause({'call', _, ?CB_CONTEXT_CALL('add_content_types_provided')
                        ,[?VAR_NAME('Context')
                          ,Args
                         ]
                       }, Acc) ->
    find_methods_in_clause(Args, Acc);
find_methods_in_clause({'call', _, ?CB_CONTEXT_CALL('set_content_types_provided')
                        ,[?VAR_NAME(_)
                          ,Args
                         ]
                       }, Acc) ->
    find_methods_in_clause(Args, Acc);
find_methods_in_clause({'call', _, {'fun', _, _}, []}, Acc) ->
    Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_fax'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_faxes:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_media'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_media:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_notifications'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_notifications:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_attachments'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_whitelabel:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_domain_attachments'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_whitelabel:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_provisioner'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_global_provisioner_templates:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_vm_download'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_vmboxes:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_get'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_port_requests:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'atom', _, 'ok'}, Acc) ->
    Acc;
find_methods_in_clause({'match', _, _, _}, Acc) ->
    Acc;
find_methods_in_clause({'nil', _}, Acc) ->
    [<<"nil">> | Acc];
find_methods_in_clause({'cons',_
                       ,{'bin',_
                        ,[{'bin_element',_,{'string',_,Method},'default','default'}]
                        }
                        ,{'nil',_}
                       }
                      ,Acc
                      ) ->
    [list_to_binary(Method) | Acc];
find_methods_in_clause({'cons',_
                       ,{'bin',_
                        ,[{'bin_element',_,{'string',_,Method},'default','default'}]
                        }
                        ,Cons
                       }
                      ,Acc
                      ) ->
    [list_to_binary(Method) | find_methods_in_clause(Cons, Acc)];

%% Matches the content_types_provided to_json list
find_methods_in_clause({'cons', _
                        ,{'tuple', _
                          ,[{'atom', _, 'to_json'}
                            ,JSONList
                           ]
                         }
                        ,Rest
                       }
                      ,Acc) ->
    CTPs = find_content_types_in_clause(JSONList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_csv list
find_methods_in_clause({'cons', _
                        ,{'tuple', _
                          ,[{'atom', _, 'to_csv'}
                            ,CSVList
                           ]
                         }
                        ,Rest
                       }
                      ,Acc) ->
    CTPs = find_content_types_in_clause(CSVList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_binary list
find_methods_in_clause({'cons', _
                        ,{'tuple', _
                          ,[{'atom', _, 'to_binary'}
                            ,BinaryList
                           ]
                         }
                        ,Rest
                       }
                      ,Acc) ->
    CTPs = find_content_types_in_clause(BinaryList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_pdf list
find_methods_in_clause({'cons', _
                        ,{'tuple', _
                          ,[{'atom', _, 'to_pdf'}
                            ,PDFList
                           ]
                         }
                        ,Rest
                       }
                      ,Acc) ->
    CTPs = find_content_types_in_clause(PDFList, Acc),
    find_methods_in_clause(Rest, CTPs);

find_methods_in_clause({'case', _
                        ,_CaseConditional
                        ,CaseClauses
                       }
                       ,Acc0
                      ) ->
    lists:foldl(fun({'clause', _Line, _Args, _, ClauseBody}, Acc1) ->
                        find_methods(ClauseBody, Acc1)
                end
                ,Acc0
                ,CaseClauses
               ).

-define(CONTENT_TYPE_BINS(Type, SubType), [?BINARY(Type), ?BINARY(SubType)]).
-define(CONTENT_TYPE_VARS(Type, SubType), [?VAR_NAME(Type), ?VAR_NAME(SubType)]).

find_content_types_in_clause({'nil', _}, Acc) -> Acc;
find_content_types_in_clause({'cons', _
                              ,{'tuple', _
                                ,?CONTENT_TYPE_VARS(_Type, _SubType)
                               }
                              ,Rest
                             }
                             ,Acc) ->
    find_content_types_in_clause(Rest, Acc);
find_content_types_in_clause({'cons', _
                             ,{'tuple', _
                              ,?CONTENT_TYPE_BINS(Type, SubType)
                              }
                             ,Rest
                             }, Acc) ->
    CT = wh_util:join_binary([Type, SubType], <<"/">>),
    find_content_types_in_clause(Rest, [CT | Acc]).


%% End of Module

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
