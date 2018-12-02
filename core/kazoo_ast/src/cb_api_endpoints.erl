%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc Modules to create a reference documents from Crossbar API modules.
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_api_endpoints).

-compile({'no_auto_import', [get/0]}).

-export([get/0
        ,get_app/2
        ,process_module/2
        ,to_swagger_file/0
        ,to_oas3_file/0
        ,to_ref_doc/0, to_ref_doc/1
        ,schema_to_doc/2, ref_tables_to_doc/1

        ,read_swagger_json/1
        ,format_as_path_centric/1
        ,to_swagger_paths/2
        ,to_swagger_definitions/1
        ,to_swagger_parameters/1

        ,convert_to_oas_schema/2
        ]).

-ifdef(TEST).
-export([sort_methods/1]).
-endif.

-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_web/include/kazoo_web.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-include_lib("kazoo_ast/src/kz_ast.hrl").

-define(DEBUG(F, A), ?DEV_LOG(F, A)).
-define(DEBUG(F), ?DEV_LOG(F)).

%% -define(DEBUG(F, A), ok).
%% -define(DEBUG(F), ok).

-define(REF_PATH
       ,code:lib_dir('crossbar'), "doc", "ref"
       ).
-define(REF_PATH(Module)
       ,filename:join([?REF_PATH, <<Module/binary,".md">>])
       ).

-define(SWAGGER_2_JSON_FILE
       ,filename:join([code:priv_dir('crossbar'), "api", "swagger.json"])
       ).

-define(OAS3_YML_FILE
       ,filename:join([code:priv_dir('crossbar'), "oas3", "openapi.yml"])
       ).

-define(OAS3_SCHEMAS_FILE
       ,filename:join([code:priv_dir('crossbar'), "oas3", "oas3-schemas.yml"])
       ).

-define(OAS3_PARAMETERS_FILE
       ,filename:join([code:priv_dir('crossbar'), "oas3", "oas3-parameters.yml"])
       ).

-define(ACCOUNTS_PREFIX, "accounts/{ACCOUNT_ID}").
-define(ENDPOINTS_PREFIX, "{ENDPOINT_TYPE}/{ENDPOINT_ID}").

-define(X_AUTH_TOKEN, "auth_token_header").
-define(X_AUTH_TOKEN_NOT_REQUIRED, "auth_token_header_or_none").

-spec to_ref_doc() -> 'ok'.
to_ref_doc() ->
    lists:foreach(fun api_to_ref_doc/1, ?MODULE:get()).

-spec to_ref_doc(atom()) -> 'ok'.
to_ref_doc('crossbar_filter'=Module) ->
    Filters = filters_from_module(Module),
    filters_to_ref_doc(Filters);
to_ref_doc(CBModule) ->
    Path = code:which(CBModule),
    api_to_ref_doc(hd(process_module(Path, []))).

-define(FILTER_ROW(Filter, OperatesOn, Description)
       ,[kz_binary:join([Filter, OperatesOn, Description], <<" | ">>), $\n]
       ).
-define(FILTER_HEADER
       ,["## Available Filters\n\n"
        ,?FILTER_ROW(<<"Filter">>, <<"Operates On">>, <<"Description">>)
        ,?FILTER_ROW(<<"------">>, <<"-----------">>, <<"-----------">>)
        ]
       ).
-define(FILTER_DOC
       ,["# Query String Filters\n\n"
         "## About Filters\n\n"
        ]).

-spec filters_to_ref_doc(kz_json:object()) -> 'ok'.
filters_to_ref_doc(Filters) ->
    ReversedTable = kz_json:foldl(fun filter_to_ref_doc/3, [], Filters),
    'ok' = file:write_file(?REF_PATH(<<"filters">>)
                          ,[?FILTER_DOC
                           ,?FILTER_HEADER
                           ,lists:reverse(ReversedTable)]
                          ).

filter_to_ref_doc(Filter, OperatesOn, Acc) ->
    [?FILTER_ROW(<<"`", Filter/binary, "`">>, <<"`", OperatesOn/binary, "`">>, <<>>) | Acc].

api_to_ref_doc([]) -> 'ok';
api_to_ref_doc({Module, Paths}) ->
    api_to_ref_doc(Module, Paths, module_version(Module)).

api_to_ref_doc(Module, Paths, ?CURRENT_VERSION) ->
    BaseName = base_module_name(Module),

    PathToSection = fun(Path, Acc) -> api_path_to_section(Module, Path, Acc) end,
    Sections = lists:foldl(PathToSection, ref_doc_header(BaseName), Paths),

    Doc = lists:reverse(Sections),
    DocPath = ?REF_PATH(BaseName),
    'ok' = file:write_file(DocPath, Doc);
api_to_ref_doc(_Module, _Paths, _Version) ->
    'ok'.

-spec api_path_to_section(atom(), {atom(), http_methods()}, iolist()) -> iolist().
api_path_to_section(Module, {'allowed_methods', Paths}, Acc) ->
    ModuleName = path_name(Module),
    F = fun(Path, Acc1) -> methods_to_section(ModuleName, Path, Acc1) end,
    lists:foldl(F, Acc, Paths);
api_path_to_section(_MOdule, _Paths, Acc) -> Acc.

%%------------------------------------------------------------------------------
%% @doc Creates a Markdown section for each API methods.
%% ```
%% ## Fetch/Create/Change
%% > Verb Path
%% ```shell
%% curl -v http://{SERVER}:8000/Path
%% ```
%% '''
%% @end
%%------------------------------------------------------------------------------
methods_to_section('undefined', _Path, Acc) ->
    io:format("skipping path ~p\n", [_Path]),
    Acc;
methods_to_section(ModuleName, {Path, Methods}, Acc) ->
    API = kz_util:iolist_join($/, [?CURRENT_VERSION, ModuleName | format_path_tokens(Path)]),
    APIPath = iolist_to_binary([$/, API]),
    lists:foldl(fun(Method, Acc1) ->
                        method_to_section(Method, Acc1, APIPath)
                end
               ,Acc
               ,sort_methods(Methods)
               ).

-spec sort_methods(kz_term:ne_binaries()) -> kz_term:ne_binaries().
sort_methods(Methods) ->
    Ordering = [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE],
    sort_methods(Methods, Ordering, []).

sort_methods([], _Ordering, Acc) -> lists:reverse(Acc);
sort_methods(Methods, [], Acc) -> lists:reverse(Methods ++ Acc);
sort_methods(Methods, [Method | Ordering], Acc) ->
    case lists:member(Method, Methods) of
        'false' -> sort_methods(Methods, Ordering, Acc);
        'true' ->
            sort_methods(lists:delete(Method, Methods)
                        ,Ordering
                        ,[Method | Acc]
                        )
    end.

method_to_section(Method, Acc, APIPath) ->
    [[ "## ", method_as_action(Method), "\n\n"
     ,"> ", Method, " ", APIPath, "\n\n"
     , "```shell\ncurl -v -X ", Method, " \\\n"
       "    -H \"X-Auth-Token: {AUTH_TOKEN}\" \\\n"
       "    http://{SERVER}:8000", APIPath, "\n"
       "```\n\n"
     ]
     | Acc
    ].

-spec method_as_action(kz_term:ne_binary()) -> kz_term:ne_binary().
method_as_action(?HTTP_GET) -> <<"Fetch">>;
method_as_action(?HTTP_PUT) -> <<"Create">>;
method_as_action(?HTTP_POST) -> <<"Change">>;
method_as_action(?HTTP_DELETE) -> <<"Remove">>;
method_as_action(?HTTP_PATCH) -> <<"Patch">>.

-spec ref_doc_header(kz_term:ne_binary()) -> iolist().
ref_doc_header(BaseName) ->
    CleanedUpName = kz_ast_util:smash_snake(BaseName),
    [[maybe_add_schema(BaseName)]
    ,["## About ", CleanedUpName, "\n\n"]
    ,["# ", CleanedUpName, "\n\n"]
    ].

-spec maybe_add_schema(kz_term:ne_binary()) -> iolist().
maybe_add_schema(BaseName) ->
    case kz_ast_util:load_ref_schema(BaseName) of
        'undefined' -> [?SCHEMA_SECTION, "\n\n"];
        SchemaJObj -> kz_ast_util:schema_to_table(SchemaJObj)
    end.

%%------------------------------------------------------------------------------
%% @doc This looks for `## Schema' in the doc file and adds the JSON schema
%% formatted as the markdown table.
%% ```
%% Schema = "vmboxes" or "devices"
%% Doc = "voicemail.md" or "devices.md"
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec schema_to_doc(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
schema_to_doc(Schema, Doc) ->
    SchemaJObj = kz_ast_util:load_ref_schema(Schema),
    DocFile = filename:join([code:lib_dir('crossbar'), "doc", Doc]),
    {'ok', DocContents} = file:read_file(DocFile),
    case SchemaJObj =/= 'undefined'
        andalso binary:split(DocContents, <<?SCHEMA_SECTION/binary, "\n">>)
    of
        [Before, After] ->
            Data = [Before, kz_ast_util:schema_to_table(SchemaJObj), After],
            ok = file:write_file(DocFile, Data);
        _Else ->
            io:format("file ~s appears to have a schema section already~n", [DocFile])
    end.

-type ref_table() :: {kz_term:ne_binary(), ref_tables()} | kz_term:ne_binary().
-type ref_tables() :: [ref_table()] | [].

-spec ref_tables_to_doc(ref_tables()) -> iolist().
ref_tables_to_doc(Tables) ->
    [ref_table_to_doc(Table) || Table <- Tables].

-spec ref_table_to_doc(ref_table()) -> iodata().
ref_table_to_doc({Schema, [SchemaTable | RefTables]}) ->
    [?SUB_SCHEMA_SECTION_HEADER, " ", Schema, "\n\n"
    ,SchemaTable, $\n
     | ref_tables_to_doc(RefTables)
    ];
ref_table_to_doc(RefTable) ->
    RefTable.

-define(SWAGGER_INFO
       ,kz_json:from_list([{<<"title">>, <<"Crossbar">>}
                          ,{<<"description">>, <<"The Crossbar APIs">>}
                          ,{<<"license">>, kz_json:from_list([{<<"name">>, <<"Mozilla Public License 1.1">>}])}
                          ,{<<"version">>, ?CURRENT_VERSION}
                          ])
       ).

-define(SWAGGER_EXTERNALDOCS
       ,kz_json:from_list([{<<"description">>, <<"Kazoo documentation's Git repository">>}
                          ,{<<"url">>, <<"https://docs.2600hz.com/dev">>}
                          ])
       ).

-spec to_swagger_file() -> 'ok'.
to_swagger_file() ->
    OASs = generate_oas_json(get(), <<"oas_two_and_three">>),
    write_swagger_file(OASs, <<"oas_two_and_three">>).

-spec to_oas3_file() -> 'ok'.
to_oas3_file() ->
    OAS3 = generate_oas_json(get(), <<"oas3">>),
    write_swagger_file(OAS3, <<"oas3">>).


-spec generate_oas_json(callback_configs(), kz_term:ne_binary()) -> kz_json:object() | kz_json:objects().
generate_oas_json(Callbacks, OasVersion) ->
    Paths = format_as_path_centric(Callbacks),
    generate_oas_paths_json(Paths, OasVersion).

-spec generate_oas_paths_json(kz_json:object(), kz_term:ne_binary()) -> kz_json:object() | kz_json:objects().
generate_oas_paths_json(Paths, <<"oas_two_and_three">>) ->
    kz_json:from_list([{<<"oas3">>, generate_oas_paths_json(Paths, <<"oas3">>)}
                      ,{<<"swagger2">>, generate_oas_paths_json(Paths, <<"swagger2">>)}
                      ]);
generate_oas_paths_json(Paths, <<"swagger2">> = OasVersion) ->
    BaseSwagger = read_swagger_json(?SWAGGER_2_JSON_FILE),
    BasePaths = kz_json:get_value(<<"paths">>, BaseSwagger),

    kz_json:set_values([{<<"paths">>, to_swagger_paths(Paths, BasePaths)}
                       ,{<<"definitions">>, to_swagger_definitions(OasVersion)}
                       ,{<<"parameters">>, to_swagger_parameters(kz_json:get_keys(Paths))}
                       ,{<<"host">>, <<"localhost:8000">>}
                       ,{<<"basePath">>, <<"/", (?CURRENT_VERSION)/binary>>}
                       ,{<<"swagger">>, <<"2.0">>}
                       ,{<<"info">>, ?SWAGGER_INFO}
                       ,{<<"consumes">>, [<<"application/json">>]}
                       ,{<<"produces">>, [<<"application/json">>]}
                       ,{<<"externalDocs">>, ?SWAGGER_EXTERNALDOCS}
                       ]
                      ,BaseSwagger
                      );
generate_oas_paths_json(Paths, <<"oas3">> = OasVersion) ->
     BaseOas = kz_json:from_map(read_oas3_yaml(?OAS3_YML_FILE)),
     _OasPaths = to_oas3_paths(Paths),

     'ok' = file:write_file(?OAS3_PARAMETERS_FILE, kz_yaml:encode(to_swagger_parameters(kz_json:get_keys(Paths)))),
     'ok' = file:write_file(?OAS3_SCHEMAS_FILE, kz_yaml:encode(to_swagger_definitions(OasVersion))),

     PathNames = kz_json:foldl(fun(Path, PathMeta, Acc) ->
                                       [{kz_json:get_value(<<"module_name">>, PathMeta), Path}|Acc]
                               end
                              ,[]
                              ,Paths
                              ),

     kz_json:set_values([{[<<"components">>, <<"parameters">>, <<"$ref">>], kz_term:to_binary(?OAS3_PARAMETERS_FILE)}
                        ,{[<<"components">>, <<"schemas">>, <<"$ref">>], kz_term:to_binary(?OAS3_SCHEMAS_FILE)}
                         | [{[<<"paths">>, Path, <<"$ref">>], <<"paths/", Name/binary, ".yml#/paths/", (escape_json_pointer(Path))/binary>>}
                            || {Name, Path} <- lists:reverse(PathNames)
                           ]
                        ]
                       ,BaseOas
                       ).
escape_json_pointer(Pointer) ->
    binary:replace(Pointer, <<"/">>, <<"~1">>, [global]).

-spec write_swagger_file(kz_json:object() | kz_json:objects(), kz_term:ne_binary()) -> 'ok'.
write_swagger_file(Swaggers, <<"oas_two_and_three">>) ->
    write_swagger_file(kz_json:get_json_value(<<"swagger2">>, Swaggers), <<"swagger2">>),
    write_swagger_file(kz_json:get_json_value(<<"oas3">>, Swaggers), <<"oas3">>);
write_swagger_file(Swagger2, <<"swagger2">>) ->
    'ok' = file:write_file(?SWAGGER_2_JSON_FILE, kz_json:encode(Swagger2));
write_swagger_file(Oas3, <<"oas3">>) ->
    'ok' = file:write_file(?OAS3_YML_FILE, kz_yaml:encode(Oas3, #{sort_keys => 'true'
                                                                 ,key_string_style => 'single_quote'
                                                                 })).

-spec to_swagger_definitions(kz_term:ne_binary()) -> kz_json:object().
to_swagger_definitions(OasVersion) ->
    SchemasPath = kz_ast_util:schema_path(<<>>),
    {OasSchemas, Report, HasError} =
        filelib:fold_files(kz_term:to_list(SchemasPath)
                          ,"\\.json\$"
                          ,'false'
                          ,fun(FileName, {Definitions, Report, HasError}) -> process_schema(FileName, Definitions, OasVersion, Report, HasError) end
                          ,{kz_json:new(), #{}, 'false'}
                          ),
    print_report(Report),
    case HasError of
        'false' -> OasSchemas;
        'true' -> throw({'error', 'failed_to_convert_oas'})
    end.

-spec print_report(map()) -> 'ok'.
print_report(Report) ->
    _ = maps:map(fun(F, R) ->
                         print_report(F, maps:get(warn, R, []), maps:get(err, R, [])) end
                , Report
                ),
    'ok'.

-spec print_report(string(), kz_term:ne_binaries(), kz_term:ne_binaries()) -> 'ok'.
print_report(File, Warn, Err) ->
    print_messages(<<"Warnings">>, File, Warn),
    print_messages(<<"Errors">>, File, Err).

-spec print_messages(kz_term:ne_binary(), string(), kz_term:ne_binaries()) -> 'ok'.
print_messages(<<"Warnings">> = Title, File, Msgs) ->
    Ignore = [<<"path '._id': unsupported keyword.">>
             ,<<"path '.$schema': unsupported keyword.">>
             ],
    FilterFun = fun(Msg) -> not lists:any(fun(Elem) -> Elem =:= Msg end, Ignore) end,
    Head = iolist_to_binary(io_lib:format("~s in file: ~s", [Title, File])),
    print_messages(Head, lists:filter(FilterFun, Msgs));
print_messages(Title, File, Msgs) ->
    Head = iolist_to_binary(io_lib:format("~s in file: ~s", [Title, File])),
    print_messages(Head, Msgs).

-spec print_messages(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
print_messages(_, []) -> 'ok';
print_messages(Head, Msgs) ->
    print_messages([Head | Msgs]),
    io:format(user, "~n", []).

-spec print_messages(kz_term:ne_binaries()) -> 'ok'.
print_messages([]) -> 'ok';
print_messages([Msg | Msgs]) ->
    io:format(user, "~s~n", [Msg]),
    print_messages(Msgs).

-spec process_schema(string(), kz_json:object(), kz_term:ne_binary(), map(), boolean()) ->
                            {kz_json:object(), map(), boolean()}.
process_schema(Filename, Definitions, OasVersion, Warn, Err) ->
    process_schema(Filename, Definitions, OasVersion, filename:basename(Filename, ".json"), Warn, Err).

-spec process_schema(string(), kz_json:object(), kz_term:ne_binary(), string(), map(), boolean()) ->
                            {kz_json:object(), map(), boolean()}.
process_schema(_Filename, Definitions, <<"oas3">>, "kapi."++_ = Name, Report, HasError) ->
    {kz_json:delete_key(kz_term:to_binary(Name), Definitions), Report, HasError};
process_schema(Filename, Definitions, OasVersion, Name, Report, HasError) ->
    case convert_to_oas_schema(Filename, OasVersion) of
        {'ok', OasSchema, Warn} ->
            {kz_json:set_value(kz_term:to_binary(Name), OasSchema, Definitions), maps:put(Filename, #{warn => Warn}, Report), HasError};
        {'error', Warn, Err} ->
            {Definitions, maps:put(Filename, #{warn => Warn, err => Err}, Report), 'true'}
    end.

-type oas_schema_ret() :: {'ok', kz_json:object(), kz_term:ne_binaries()} |
                          {'error', kz_term:ne_binaries(), kz_term:ne_binaries()}.

-spec convert_to_oas_schema(kz_term:text(), kz_term:ne_binary()) -> oas_schema_ret().
convert_to_oas_schema(File, OasVersion) ->
    {'ok', Bin} = file:read_file(File),
    KVs = kz_json:to_proplist(kz_json:flatten(kz_json:decode(Bin))),
    case to_oas_schema(KVs, OasVersion) of
        {'ok', OasSchema, Warn} ->
            {'ok', kz_json:expand(kz_json:from_list(OasSchema)), Warn};
        {'error', Warn, Err} ->
            {'error', Warn, Err}
    end.

-spec to_oas_schema(kz_term:proplist(), kz_term:ne_binary()) -> oas_schema_ret().
to_oas_schema(KVs, <<"swagger2">>) ->
    {'ok'
    ,[case lists:last(Path) =:= <<"$ref">> of
          'false' -> KV;
          'true' -> {Path, maybe_fix_ref(V, <<"swagger2">>)}
      end
      || {Path, V}=KV <- KVs,
         is_supported_by_swagger2(Path)
     ]
    ,[]
    };
to_oas_schema(KVs, <<"oas3">>) ->
    case to_oas3_schema(KVs, [], KVs, [], []) of
        {'ok', NewKVs, Warn} ->
            {'ok', NewKVs, format_path_msg(Warn)};
        {'error', Warn, Err} ->
            {'error', format_path_msg(Warn), format_path_msg(Err)}
    end.

-spec format_path_msg(kz_term:proplist()) -> kz_term:ne_binaries().
format_path_msg(PathMsgs) ->
    lists:reverse([iolist_to_binary(io_lib:format("path '~s': ~s", [Path, Msg])) || {Path, Msg} <- PathMsgs]).

-spec is_supported_by_swagger2(kz_term:ne_binaries()) -> boolean().
is_supported_by_swagger2(Path) ->
    %% keep swagger2 behaviour as before
    [<<"_id">>] =/= Path
        andalso [<<"$schema">>] =/= Path
        andalso [<<"additionalProperties">>] =/= Path
        andalso not lists:member(<<"patternProperties">>, Path)
        andalso not is_kazoo_prefixed(Path).

-spec is_kazoo_prefixed(kz_term:ne_binaries()) -> boolean().
is_kazoo_prefixed([]) -> 'false';
is_kazoo_prefixed([<<"kazoo-", _/binary>>|_]) -> 'true';
is_kazoo_prefixed([<<"support_level">>|_]) -> 'true';
is_kazoo_prefixed([_Field|Path]) -> is_kazoo_prefixed(Path).

-define(IS_OAS_PROPERTIES(P), (P =:= <<"properties">>
                                   orelse P =:= <<"additionalProperties">>
                                   orelse P =:= <<"x-patternProperties">>
                              )
       ).

-type oas3_schema_ret() :: {'ok', kz_term:proplist(), kz_term:proplist()} |
                           {'error', kz_term:proplist(), kz_term:proplist()}.

-spec to_oas3_schema(kz_term:proplist(), kz_term:proplist(), kz_term:proplist(), kz_term:proplist(), kz_term:proplist()) -> oas3_schema_ret().
to_oas3_schema([{Path, Val} | PVs], KVs, OrigKVs, Warn, Err) ->
    case to_oas3_schema(Path, Path, [], Val, KVs, OrigKVs, Warn, Err) of
        {'ok', NewKVs, NewWarn} -> to_oas3_schema(PVs, NewKVs, OrigKVs, NewWarn, Err);
        {'error', NewWarn, NewErr} -> to_oas3_schema(PVs, [{Path, Val} | KVs], OrigKVs, NewWarn, NewErr)
    end;
to_oas3_schema([], KVs, _, Warn, Err) ->
    return_error_on_error(lists:reverse(KVs), Warn, Err).

-spec to_oas3_schema(OriginalPath::kz_term:ne_binaries(), Path::kz_term:ne_binaries(), VisitedPath::kz_term:ne_binaries()
                    ,Value::kz_term:proplist_value(), KVAcc::kz_term:proplist(), OrigKVs::kz_term:proplist()
                    ,Warnings::kz_term:proplist(), Errors::kz_term:proplist()) -> oas3_schema_ret().
%% remove unsupported keywords
to_oas3_schema(_, [<<"_id">> = P], ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"$id">>  = P| _], [] = ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"$id">>  = P| _], [_, Properties | _] = ReverseP, _, KVs, _, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"$schema">> = P], ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"additionalItems">> = P | _], [] = ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"additionalItems">> = P | _], [_, Properties | _] = ReverseP, _, KVs, _, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"const">> = P | _], [] = ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"const">> = P | _], [_, Properties | _] = ReverseP, _, KVs, _, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"contains">> = P | _], [] = ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"contains">> = P | _], [_, Properties | _] = ReverseP, _, KVs, _, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"dependencies">> = P | _], [] = ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"dependencies">> = P | _], [_, Properties | _] = ReverseP, _, KVs, _, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"id">> = P | _], [] = ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"id">> = P | _], [_, Properties | _] = ReverseP, _, KVs, _, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"propertyNames">> = P | _], [] = ReverseP, _, KVs, _, Warn, Err) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
to_oas3_schema(_, [<<"propertyNames">> = P | _], [_, Properties | _] = ReverseP, _, KVs, _, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    ret_unsupported_key(KVs, P, ReverseP, Warn, Err);

%% let's rename this instead of removing
to_oas3_schema(OrigP, [<<"patternProperties">> = P | Ps], [], Val, KVs, OrigKVs, Warn, Err) ->
    to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>>], Val, KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"patternProperties">> = P | Ps], [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>> | ReverseP], Val, KVs, OrigKVs, Warn, Err);

%% add prefix to kazoo specific fields
to_oas3_schema(OrigP, [<<"kazoo-", _/binary>> = P | Ps], [], Val, KVs, OrigKVs, Warn, Err) ->
    to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>>], Val, KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"kazoo-", _/binary>> = P | Ps], [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>> | ReverseP], Val, KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"support_level">> = P | Ps], [], Val, KVs, OrigKVs, Warn, Err) ->
    to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>>], Val, KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"support_level">> = P | Ps], [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>> | ReverseP], Val, KVs, OrigKVs, Warn, Err);

%% since `kz_json:flatten/1' is not flattening a list if JObjs, we have to flatten them here.
to_oas3_schema(OrigP, [<<"allOf">> = P], [], Val, KVs, OrigKVs, Warn, Err) ->
    {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P], Val, Warn, Err),
    to_oas3_schema(OrigP, [], [P], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);
to_oas3_schema(_OrigP, [<<"allOf">> = P | _], []=ReverseP, _Val, _KVs, _OrigKVs, Warn, Err) ->
    Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
    {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};
to_oas3_schema(OrigP, [<<"allOf">> = P], [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P | ReverseP], Val, Warn, Err),
    to_oas3_schema(OrigP, [], [P | ReverseP], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);
to_oas3_schema(_OrigP, [<<"allOf">> = P | _], [_, Properties|_]=ReverseP, _Val, _KVs, _OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
    {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};

%% since `kz_json:flatten/1' is not flattening a list if JObjs, we have to flatten them here.
to_oas3_schema(OrigP, [<<"anyOf">> = P], [], Val, KVs, OrigKVs, Warn, Err) ->
    {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P], Val, Warn, Err),
    to_oas3_schema(OrigP, [], [P], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);
to_oas3_schema(_OrigP, [<<"anyOf">> = P | _], []=ReverseP, _Val, _KVs, _OrigKVs, Warn, Err) ->
    Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
    {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};
to_oas3_schema(OrigP, [<<"anyOf">> = P], [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P | ReverseP], Val, Warn, Err),
    to_oas3_schema(OrigP, [], [P | ReverseP], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);
to_oas3_schema(_OrigP, [<<"anyOf">> = P | _], [_, Properties|_]=ReverseP, _Val, _KVs, _OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
    {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};

%% since `kz_json:flatten/1' is not flattening a list if JObjs, we have to flatten them here.
to_oas3_schema(OrigP, [<<"items">> = P], [], Val, KVs, OrigKVs, Warn, Err) ->
    {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P], Val, Warn, Err),
    to_oas3_schema(OrigP, [], [P], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);
to_oas3_schema(OrigP, [<<"items">> = P], [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P | ReverseP], Val, Warn, Err),
    to_oas3_schema(OrigP, [], [P | ReverseP], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);

%% since `kz_json:flatten/1' is not flattening a list if JObjs, we have to flatten them here.
to_oas3_schema(OrigP, [<<"oneOf">> = P], [], Val, KVs, OrigKVs, Warn, Err) ->
    {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P], Val, Warn, Err),
    to_oas3_schema(OrigP, [], [P], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);
to_oas3_schema(_OrigP, [<<"oneOf">> = P | _], []=ReverseP, _Val, _KVs, _OrigKVs, Warn, Err) ->
    Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
    {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};
to_oas3_schema(OrigP, [<<"oneOf">> = P], [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P | ReverseP], Val, Warn, Err),
    to_oas3_schema(OrigP, [], [P | ReverseP], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);
to_oas3_schema(_OrigP, [<<"oneOf">> = P | _], [_, Properties|_]=ReverseP, _Val, _KVs, _OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
    {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};

%% other checks
to_oas3_schema(OrigP, [<<"$ref">> = P], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    to_oas3_schema(OrigP, [], [P | ReverseP], maybe_fix_ref(Val, <<"oas3">>), KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"additionalProperties">> = P], ReverseP, Val, KVs, OrigKVs, Warn, Err) when is_boolean(Val) ->
    to_oas3_schema(OrigP, [], [P | ReverseP], Val, KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"additionalProperties">> = P | Ps], [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"type">> | _] = Ps, [] = ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    to_oas3_type(OrigP, Ps, ReverseP, Val, KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"type">> | _] = Ps, [_, Properties|_]=ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when ?IS_OAS_PROPERTIES(Properties) ->
    to_oas3_type(OrigP, Ps, ReverseP, Val, KVs, OrigKVs, Warn, Err);

%% accept everything else as-is, we don't care about them
to_oas3_schema(OrigP, [P|Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err);

%% return the result
to_oas3_schema(OrigP, [], [], Val, KVs, _, Warn, Err) ->
    return_error_on_error([{OrigP, Val} | KVs], Warn, Err);
to_oas3_schema(_, [], ReverseP, Val, KVs, _, Warn, Err) ->
    return_error_on_error([{lists:reverse(ReverseP), Val} | KVs], Warn, Err).


oas3_deep_flatten(ReverseP, [H|_]=Val, Warn, Err) ->
    case kz_json:is_json_object(H) of
        'true' ->
            {_, DeepValFlat, DeepWarn, DeepErr} =
                lists:foldl(fun(J, Acc) -> oas3_deep_flatten(J, ReverseP, Acc) end
                           ,{0, [], Warn, Err}
                           ,Val
                           ),
            {lists:reverse(DeepValFlat), DeepWarn, DeepErr};
        'false' ->
            Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
            {Val, Warn, [{join_oas3_path_reverse(ReverseP), Msg} | Err]}
    end;
oas3_deep_flatten(ReverseP, Val, Warn, Err) ->
    Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
    {Val, Warn, [{join_oas3_path_reverse(ReverseP), Msg} | Err]}.

oas3_deep_flatten(JObj, ReverseP,  {Index, Acc, Warn, Err}) ->
    KVs = kz_json:to_proplist(kz_json:flatten(JObj)),
    ArrayPath = join_oas3_path_reverse([<<"[", (kz_term:to_binary(Index))/binary, "]">> | ReverseP]),
    case to_oas3_schema(KVs, [], KVs, [], []) of
        {'ok', DeepKVs, DeepWarn} ->
            JObj1 = kz_json:expand(kz_json:from_list(DeepKVs)),
            {Index+1, [JObj1 | Acc], [{<<ArrayPath/binary, Path/binary>>, Msg} || {Path, Msg} <- DeepWarn] ++ Warn, Err};
        {'error', DeepWarn, DeepErr} ->
            NewWarn = [{<<ArrayPath/binary, Path/binary>>, Msg} || {Path, Msg} <- DeepWarn] ++ Warn,
            NewErr = [{<<ArrayPath/binary, Path/binary>>, Msg} || {Path, Msg} <- DeepErr] ++ Err,
            {Index+1, [JObj | Acc], NewWarn, NewErr}
    end.

-spec join_oas3_path_reverse(kz_term:ne_binaries()) -> kz_term:ne_binary().
join_oas3_path_reverse([]) ->
    <<$.>>;
join_oas3_path_reverse(Path) ->
    kz_binary:join([<<>> | lists:reverse(Path)], <<$.>>).

-spec return_error_on_error(kz_term:proplist(), kz_term:proplist(), kz_term:proplist()) -> oas3_schema_ret().
return_error_on_error(KVs, Warn, []) ->
    {'ok', KVs, Warn};
return_error_on_error(_, Warn, Err) ->
    {'error', Warn, Err}.

-spec ret_unsupported_key(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binaries()) -> oas3_schema_ret().
ret_unsupported_key(KVs, Key, ReverseP, Warn, Err) ->
    Msg = <<"unsupported keyword.">>,
    return_error_on_error(KVs, [{join_oas3_path_reverse([Key | ReverseP]), Msg} | Warn], Err).

to_oas3_type(OrigP, [P | Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    case oas3_type_type(ReverseP, Val, OrigKVs) of
        'array' ->
            Msg = <<"the value must be a single type and not an array of types.">>,
            {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};
        'binary' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err);
        'missing_items' ->
            Msg = <<"'items' must be present if type is array.">>,
            {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};
        'null' ->
            Msg = <<"converting type 'null' to '\"nullable\": true'.">>,
            to_oas3_schema(OrigP, Ps, [<<"nullable">> | ReverseP], 'true', KVs, OrigKVs, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Warn], Err);
        'null_in_array' ->
            Msg = <<"type has 'null', adding '\"nullable\": true' instead.">>,
            Nullable = {lists:reverse([<<"nullable">> | ReverseP]), 'true'},
            [Type] = [T || T <- Val,
                           T =/= 'null',
                           T =/= <<"null">>
                     ],
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Type, [Nullable | KVs ], OrigKVs, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Warn], Err);
        'undefined' ->
            Msg = iolist_to_binary(io_lib:format("invalid type '~p'.", [Val])),
            {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]}
    end.

-spec oas3_type_type(kz_term:ne_binaries(), kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:proplist()) ->
                            'array' |
                            'binary' |
                            'missing_items' |
                            'null' |
                            'null_in_array' |
                            'undefined'.
oas3_type_type(Path, Value, KVs) when is_list(Value) ->
    HasArray = lists:member(<<"array">>, Value),
    case lists:any(fun(Null) -> lists:member(Null, Value) end, ['null', <<"null">>])
        andalso length(Value) =:= 2
    of
        'true' when HasArray ->
            case oas3_type_type(Path, <<"array">>, KVs) of
                'binary' -> null_in_array;
                'missing_items' -> 'missing_items'
            end;
        'true' -> 'null_in_array';
        'false' -> 'array'
    end;
oas3_type_type(Path, <<"array">>, KVs) ->
    RenameFun = fun(<<"x-patternProperties">>, Acc) -> [<<"patternProperties">> | Acc];
                   (Other, Acc) -> [Other | Acc]
                end,
    PartialItemsPath = lists:foldl(RenameFun, [], [<<"items">> | Path]),
    Fun = fun(Elem) -> lists:prefix(PartialItemsPath, Elem) end,
    case lists:any(Fun, [P || {P, _} <- KVs]) of
        'true' -> 'binary';
        'false' -> 'missing_items'
    end;
oas3_type_type(_Path, 'null', _KVs) ->
    'null';
oas3_type_type(_Path, <<"boolean">>, _KVs) ->
    'binary';
oas3_type_type(_Path, <<"integer">>, _KVs) ->
    'binary';
oas3_type_type(_Path, <<"null">>, _KVs) ->
    'null';
oas3_type_type(_Path, <<"number">>, _KVs) ->
    'binary';
oas3_type_type(_Path, <<"object">>, _KVs) ->
    'binary';
oas3_type_type(_Path, <<"string">>, _KVs) ->
    'binary';
oas3_type_type(_Path, _Value, _KVs) ->
    'undefined'.

-spec maybe_fix_ref(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_fix_ref(<<"#",_/binary>>=Ref, _) -> Ref;
maybe_fix_ref(RelativePath=?NE_BINARY, <<"swagger2">>) ->
    <<"#/definitions/", RelativePath/binary>>;
maybe_fix_ref(RelativePath=?NE_BINARY, <<"oas3">>) ->
    <<"#/components/schemas/", RelativePath/binary>>.

-spec read_swagger_json(kz_term:ne_binary()) -> kz_json:object().
read_swagger_json(SwaggerFile) ->
    case file:read_file(SwaggerFile) of
        {'ok', Bin} -> kz_json:decode(Bin);
        {'error', 'enoent'} -> kz_json:new()
    end.

-spec read_oas3_yaml(kz_term:ne_binary()) -> kz_yaml:yaml_node().
read_oas3_yaml(SwaggerFile) ->
    try kz_yaml:decode_file(SwaggerFile)
    catch
        'throw':{'error', 'enoent'} -> #{};
        'thorw':Throw -> throw(Throw)
    end.

to_oas3_paths(_Paths) ->
    kz_json:new().

-spec to_swagger_paths(kz_json:object(), kz_json:object()) -> kz_json:object().
to_swagger_paths(Paths, BasePaths) ->
    Endpoints =
        [{[Path, Method], kz_json:get_value([Path, Method], BasePaths, kz_json:new())}
         || {Path, AllowedMethods} <- kz_json:to_proplist(Paths),
            Method <- kz_json:get_list_value(<<"allowed_methods">>, AllowedMethods, [])
        ],
    kz_json:merge(kz_json:set_values(Endpoints, kz_json:new())
                 ,kz_json:foldl(fun to_swagger_path/3, kz_json:new(), Paths)
                 ).

-spec to_swagger_path(kz_json:key(), kz_json:object(), kz_json:object()) -> kz_json:object().
to_swagger_path(Path, PathMeta, Acc) ->
    Methods = kz_json:get_list_value(<<"allowed_methods">>, PathMeta, []),
    SchemaParameter = swagger_params(PathMeta),
    F = fun(Method, Acc1) -> add_swagger_path(Method, Acc1, Path, SchemaParameter) end,
    lists:foldl(F, Acc, Methods).

-spec add_swagger_path(kz_term:ne_binary(), kz_json:object(), kz_json:key(), kz_term:api_object()) ->
                              kz_json:object().
add_swagger_path(Method, Acc, Path, SchemaParameter) ->
    MethodJObj = kz_json:get_value([Path, Method], Acc, kz_json:new()),
    Parameters = make_parameters(Path, Method, SchemaParameter),
    BaseResponse = kz_json:from_list_recursive([{<<"200">>, [{<<"description">>, <<"request succeeded">>}]}]),

    Vs = props:filter_empty([{[Path, Method], MethodJObj}
                            ,{[Path, Method, <<"parameters">>], Parameters}
                            ,{[Path, Method, <<"responses">>], BaseResponse}
                            ]),
    kz_json:insert_values(Vs, Acc).

-spec make_parameters(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_object()) -> kz_term:ne_binaries().
make_parameters(Path, Method, SchemaParameter) ->
    lists:usort(fun compare_parameters/2
               ,lists:flatten(
                  [Parameter
                   || F <- [fun (P, M) -> maybe_add_schema(P, M, SchemaParameter) end
                           ,fun auth_token_param/2
                           ,fun path_params/2
                           ],
                      Parameter <- [F(Path, Method)],
                      not kz_term:is_empty(Parameter)
                  ])).

-spec compare_parameters(kz_json:object(), kz_json:object()) -> boolean().
compare_parameters(Param1, Param2) ->
    Keys = [<<"name">>, <<"$ref">>],
    kz_json:get_first_defined(Keys, Param1) >= kz_json:get_first_defined(Keys, Param2).

-spec maybe_add_schema(any(), kz_term:ne_binary(), kz_json:object()) -> kz_term:api_object().
maybe_add_schema(_Path, Method, Schema)
  when Method =:= <<"put">>;
       Method =:= <<"post">> ->
    Schema;
maybe_add_schema(_Path, _Method, _Parameters) ->
    'undefined'.

-spec swagger_params(kz_json:object()) -> kz_term:api_object().
swagger_params(PathMeta) ->
    case kz_json:get_ne_binary_value(<<"schema">>, PathMeta) of
        'undefined' -> 'undefined';
        %% These do not have schemas
        <<"ip_auth">> -> undefined;
        %% These have schemas
        Schema ->
            kz_json:from_list([{<<"name">>, Schema}
                              ,{<<"in">>, <<"body">>}
                              ,{<<"required">>, 'true'}
                              ,{<<"schema">>, kz_json:from_list([{<<"$ref">>, <<"#/definitions/", Schema/binary>>}])}
                              ])
    end.

-spec auth_token_param(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
auth_token_param(Path, _Method) ->
    case is_authtoken_required(Path) of
        'undefined' -> 'undefined';
        'true' -> kz_json:from_list([{<<"$ref">>, <<"#/parameters/"?X_AUTH_TOKEN>>}]);
        'false' -> kz_json:from_list([{<<"$ref">>, <<"#/parameters/"?X_AUTH_TOKEN_NOT_REQUIRED>>}])
    end.

-spec is_authtoken_required(kz_term:ne_binary()) -> kz_term:api_boolean().
is_authtoken_required(<<"/"?ACCOUNTS_PREFIX"/", _/binary>>=Path) ->
    not is_api_c2c_connect(Path);
is_authtoken_required(_Path) -> 'undefined'.

-spec is_api_c2c_connect(kz_term:ne_binary()) -> boolean().
is_api_c2c_connect(<<"/"?ACCOUNTS_PREFIX"/clicktocall/", _/binary>>=Path) ->
    kz_binary:suffix(<<"/connect">>, Path);
is_api_c2c_connect(_) -> 'false'.

-spec path_params(kz_term:ne_binary(), any()) -> kz_json:objects().
path_params(Path, _Method) ->
    [path_param(Param) || Param <- split_url(Path),
                          is_path_variable(Param)
    ].

-spec path_param(kz_term:ne_binary()) -> kz_json:object().
path_param(PathToken) ->
    Param = unbrace_param(PathToken),
    kz_json:from_list([{<<"$ref">>, <<"#/parameters/", Param/binary>>}]).

-spec split_url(kz_term:ne_binary()) -> kz_term:ne_binaries().
split_url(Path) ->
    binary:split(Path, <<$/>>, ['global']).

-spec is_path_variable(kz_term:ne_binary()) -> boolean().
is_path_variable(Param) ->
    -1 =/= kz_binary:pos(${, Param).

-spec format_as_path_centric(callback_configs()) -> kz_json:object().
format_as_path_centric(Configs) ->
    lists:foldl(fun format_pc_module/2, kz_json:new(), Configs).

-spec format_pc_module(callback_config(), kz_json:object()) -> kz_json:object().
format_pc_module({Module, CallbackConfig}, Acc) ->
    ModuleName = path_name(Module),
    F = fun(ConfigData, Acc1) -> format_pc_config(ConfigData, Acc1, Module, ModuleName) end,
    lists:foldl(F, Acc, CallbackConfig);
format_pc_module(_MC, Acc) ->
    Acc.

-spec format_pc_config(path_with_methods(), kz_json:object(), module(), kz_term:api_ne_binary()) ->
                              kz_json:object().
format_pc_config(_ConfigData, Acc, _Module, 'undefined') -> Acc;
format_pc_config({Callback, Paths}, Acc, Module, ModuleName) ->
    F = fun(Path, Acc1) -> format_pc_callback(Path, Acc1, Module, ModuleName, Callback) end,
    lists:foldl(F, Acc, Paths).

format_pc_callback({[], []}, Acc, _Module, _ModuleName, _Callback) -> Acc;
format_pc_callback({_Path, []}, Acc, _Module, _ModuleName, _Callback) ->
    io:format("module ~s supported path ~s~nm: ~p c: ~p~n"
             ,[_ModuleName, _Path, _Module, _Callback]
             ),
    Acc;
format_pc_callback({Path, Vs}, Acc, Module, ModuleName, Callback) ->
    PathName = swagger_api_path(Path, ModuleName),
    BaseModule = base_module_name(Module),
    Values = props:filter_undefined(
               [{[PathName, kz_term:to_binary(Callback)], [kz_term:to_lower_binary(V) || V <- Vs]}
               ,{[PathName, <<"module_name">>], BaseModule}
               ,maybe_include_schema(PathName, BaseModule)
               ]),
    kz_json:set_values(Values, Acc).

maybe_include_schema(PathName, BaseModule) ->
    case filelib:is_file(kz_ast_util:schema_path(<<BaseModule/binary, ".json">>)) of
        'false' -> {'undefined', 'undefined'};
        'true' -> {[PathName, <<"schema">>], BaseModule}
    end.

format_path_tokens(<<"/">>) -> [];
format_path_tokens(<<_/binary>> = Token) ->
    [format_path_token(Token)];
format_path_tokens(Tokens) ->
    [format_path_token(Token) || Token <- Tokens, Token =/= <<"/">>].

format_path_token(<<"_", Rest/binary>>) -> format_path_token(Rest);
format_path_token(Token = <<Prefix:1/binary, _/binary>>)
  when byte_size(Token) >= 3 ->
    case is_all_upper(Token) of
        'true' -> brace_token(Token);
        'false' ->
            case is_all_upper(Prefix) of
                'true' -> brace_token(camel_to_snake(Token));
                'false' -> Token
            end
    end;
format_path_token(BadToken) ->
    Fmt = "Please pick a good allowed_methods/N variable name: '~s' is too short.\n",
    io:format('standard_error', Fmt, [BadToken]),
    halt(1).

camel_to_snake(Bin) ->
    Options = ['global', {'return', 'binary'}],
    re:replace(Bin, <<"(?!^)([A-Z][a-z])">>, <<"_\\1">>, Options).

is_all_upper(Bin) ->
    Bin =:= kz_term:to_upper_binary(Bin).

brace_token(Token=?NE_BINARY) ->
    <<"{", (kz_term:to_upper_binary(Token))/binary, "}">>.

unbrace_param(<<"{", Param/binary>>) ->
    Size = byte_size(Param) - 1,
    <<Unbraced:Size/binary, "}">> = Param,
    Unbraced;
unbrace_param(Param=?NE_BINARY) ->
    PosLHS = kz_binary:pos(${, Param),
    PosRHS = kz_binary:pos($}, Param),
    binary:part(Param, PosLHS + 1, PosRHS - (PosLHS + 1)).

base_module_name(Module) ->
    {'match', [Name|_]} = grep_cb_module(Module),
    Name.

module_version(Module) ->
    case grep_cb_module(Module) of
        {'match', [_Name, Version]} -> Version;
        {'match', [_Name]} -> ?CURRENT_VERSION
    end.

swagger_api_path(Path, ModuleName) ->
    API = kz_util:iolist_join($/, [ModuleName | format_path_tokens(Path)]),
    iolist_to_binary([$/, API]).

-spec path_name(atom()) -> kz_term:api_ne_binary().
path_name(Module) ->
    case grep_cb_module(Module) of
        {'match', [<<"about">>=Name]} -> Name;
        {'match', [<<"accounts">>=Name]} -> Name;
        {'match', [<<"api_auth">>=Name]} -> Name;
        {'match', [<<"auth">>=Name]} -> Name;
        {'match', [<<"basic_auth">>=Name]} -> Name;
        {'match', [<<"ip_auth">>=Name]} -> Name;
        {'match', [<<"rates">>=Name]} -> Name;
        {'match', [<<"schemas">>=Name]} -> Name;
        {'match', [<<"shared_auth">>=Name]} -> Name;
        {'match', [<<"sup">>=Name]} -> Name;
        {'match', [<<"system_configs">>=Name]} -> Name;
        {'match', [<<"system_status">>=Name]} -> Name;
        {'match', [<<"templates">>=Name]} -> Name;
        {'match', [<<"token_auth">>=Name]} -> Name;
        {'match', [<<"ubiquiti_auth">>=Name]} -> Name;
        {'match', [<<"user_auth">>=Name]} -> Name;
        {'match', [<<"quickcall">>=Name]} -> <<?ACCOUNTS_PREFIX"/"?ENDPOINTS_PREFIX"/", Name/binary>>;
        {'match', [Name]} -> <<?ACCOUNTS_PREFIX"/", Name/binary>>;
        {'match', [Name, ?CURRENT_VERSION]} -> <<?ACCOUNTS_PREFIX"/", Name/binary>>;
        {'match', _M} -> 'undefined'
    end.

%% API

-type callback_config() :: {callback(), [allowed_methods() | content_types_provided()]}.
-type callback_configs() :: [callback_config()].
-type callback() :: module().
-spec get() -> callback_configs().
get() ->
    Apps = ['crossbar', 'acdc', 'frontier', 'cccp'],
    lists:foldl(fun get_app/2, [], Apps).

-spec get_app(module(), callback_configs()) -> callback_configs().
get_app(App, Acc) ->
    process_application(App, Acc).

-spec process_application(atom(), callback_configs()) -> callback_configs().
process_application(App, Acc) ->
    EBinDir = code:lib_dir(App, 'ebin'),
    io:format("processing ~s modules: ", [App]),
    Processed = filelib:fold_files(EBinDir, "^cb_.*.beam\$", 'false', fun process_module/2, Acc),
    io:format(" done~n"),
    Processed.

-spec process_module(file:filename_all(), callback_configs()) -> callback_configs().
process_module(File, Acc) ->
    {'ok', {Module, [{'exports', Fs}]}} = beam_lib:chunks(File, ['exports']),
    io:format("."),

    case process_exports(File, Module, Fs) of
        'undefined' -> Acc;
        Exports -> [Exports | Acc]
    end.

-type fun_arity() :: {atom(), arity()}.
-type fun_arities() :: [fun_arity()].

-spec is_api_function(fun_arity()) -> boolean().
is_api_function({'allowed_methods', _Arity}) -> 'true';
%%is_api_function({'content_types_provided', _Arity}) -> 'true';
is_api_function(_) ->  'false'.

-spec process_exports(file:filename_all(), module(), fun_arities()) ->
                             callback_config() | 'undefined' | [].
process_exports(_File, 'api_resource', _) -> [];
process_exports(_File, 'cb_context', _) -> [];
process_exports(File, Module, Fs) ->
    case lists:any(fun is_api_function/1, Fs) of
        'false' -> 'undefined';
        'true' -> process_api_module(File, Module)
    end.

-spec process_api_module(file:filename_all(), module()) -> callback_config() | 'undefined'.
process_api_module(File, Module) ->
    {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(File, ['abstract_code']),
    try process_api_ast(Module, AST)
    catch
        ?STACKTRACE(_E, _R, ST)
        io:format("failed to process ~p(~p): ~s: ~p\n", [File, Module, _E, _R]),
        io:format("~p\n", [ST]),
        'undefined'
        end.

-spec process_api_ast(module(), kz_ast_util:abstract_code()) ->
                             {module(), [allowed_methods() | content_types_provided()]}.
process_api_ast(Module, {'raw_abstract_v1', Attributes}) ->
    APIFunctions = [{F, A, Clauses}
                    || ?AST_FUNCTION(F, A, Clauses) <- Attributes,
                       is_api_function({F, A})
                   ],
    process_api_ast_functions(Module, APIFunctions).

-type http_methods() :: kz_term:ne_binaries().
-type path_with_methods() :: {iodata(), http_methods()}.
-type paths_with_methods() :: [path_with_methods()].
-type allowed_methods() :: {'allowed_methods', paths_with_methods()}.
-type content_types_provided() :: {'content_types_provided', paths_with_methods()}.

-spec process_api_ast_functions(module(), [{atom(), arity(), [erl_parse:abstract_clause()]}]) ->
                                       {module(), [allowed_methods() | content_types_provided()]}.
process_api_ast_functions(Module, Functions) ->
    {Module
    ,[process_api_ast_function(Module, F, A, Cs) || {F, A, Cs} <- Functions]
    }.

-spec process_api_ast_function(module(), atom(), arity(), [erl_parse:abstract_clause()]) ->
                                      allowed_methods() |
                                      content_types_provided().
process_api_ast_function(_Module, 'allowed_methods', _Arity, Clauses) ->
    Methods = find_http_methods(Clauses),
    {'allowed_methods', Methods};
process_api_ast_function(_Module, 'content_types_provided', _Arity, Clauses) ->
    ContentTypes = find_http_methods(Clauses),
    {'content_types_provided', ContentTypes}.

-spec find_http_methods([erl_parse:abstract_clause()]) -> paths_with_methods().
find_http_methods(Clauses) ->
    lists:foldl(fun find_http_methods_from_clause/2, [], Clauses).

-spec find_http_methods_from_clause(erl_parse:abstract_clause(), paths_with_methods()) ->
                                           paths_with_methods().
find_http_methods_from_clause(?CLAUSE(ArgsList, _Guards, ClauseBody), Methods) ->
    [{args_list_to_path(ArgsList), find_methods(ClauseBody)}
     | Methods
    ].

-spec args_list_to_path([erl_parse:abstract_expr()]) -> iodata().
args_list_to_path([]) -> <<"/">>;
args_list_to_path(Args) ->
    lists:reverse(lists:foldl(fun arg_to_path/2, [], Args)).

-spec arg_to_path(erl_parse:abstract_expr(), iodata()) -> iodata().
arg_to_path(?BINARY_MATCH(Matches), Acc) ->
    [binary_match_to_path(Matches) | Acc];
arg_to_path(?VAR('Context'), Acc) ->
    Acc;
arg_to_path(?VAR(Name), Acc) ->
    [kz_term:to_binary(Name) | Acc];
arg_to_path(?MATCH(?BINARY_MATCH(_), ?VAR(Name)), Acc) ->
    [kz_term:to_binary(Name) | Acc].

-spec binary_match_to_path([?BINARY_STRING(atom()) | ?BINARY_VAR(atom())]) ->
                                  kz_term:ne_binary().
binary_match_to_path(Matches) ->
    iolist_to_binary([binary_to_path(Match) || Match <- Matches]).

-spec binary_to_path(?BINARY_STRING(Name) | ?BINARY_VAR(atom())) ->
                            Name | iodata().
binary_to_path(?BINARY_STRING(Name)) ->
    Name;
binary_to_path(?BINARY_VAR(VarName)) ->
    format_path_token(kz_term:to_binary(VarName)).

find_methods(ClauseBody) ->
    find_methods(ClauseBody, []).
find_methods(ClauseBody, Acc) ->
    lists:usort(lists:foldl(fun find_methods_in_clause/2, Acc, ClauseBody)).

-define(CB_CONTEXT_CALL(Fun), ?MOD_FUN('cb_context', Fun)).

-spec find_methods_in_clause(erl_parse:abstract_expr(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
find_methods_in_clause(?VAR('Context'), Acc) ->
    Acc;
find_methods_in_clause(?VAR('Context1'), Acc) ->
    Acc;
find_methods_in_clause({'call', _, ?LAGER_CALL, _}, Acc) ->
    Acc;
find_methods_in_clause(?MOD_FUN_ARGS('cb_context'
                                    ,'add_content_types_provided'
                                    ,[?VAR('Context'), Args]
                                    )
                      ,Acc) ->
    find_methods_in_clause(Args, Acc);
find_methods_in_clause(?MOD_FUN_ARGS('cb_context'
                                    ,'set_content_types_provided'
                                    ,[?VAR(_), Args]
                                    )
                      ,Acc) ->
    find_methods_in_clause(Args, Acc);
find_methods_in_clause(?MOD_FUN_ARGS(_Mod, _Fun, _Args), Acc) ->
    Acc;
find_methods_in_clause(?LAGER, Acc) ->
    Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_fax', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType, _} <- cb_faxes:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_media', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType, _} <- cb_media:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_notifications', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType, _} <- cb_notifications:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_attachments', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType, _} <- cb_whitelabel:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_domain_attachments', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType, _} <- cb_whitelabel:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_provisioner', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType, _} <- cb_global_provisioner_templates:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_vm_download', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType, _} <- cb_vmboxes:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_get', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType, _} <- cb_port_requests:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('allowed_methods_on_account', _Args), Acc) ->
    AccountMethods = cb_accounts:allowed_methods_on_account(<<"account">>, {'ok', <<"master">>}),
    AccountMethods ++ Acc;
find_methods_in_clause(?FUN_ARGS(_Fun, _Args), Acc) ->
    Acc;
find_methods_in_clause(?ATOM('ok'), Acc) ->
    Acc;
find_methods_in_clause(?MATCH(_Left, _Right), Acc) ->
    Acc;
find_methods_in_clause(?EMPTY_LIST, Acc) -> Acc;
find_methods_in_clause(?LIST(?BINARY(Method), ?EMPTY_LIST)
                      ,Acc) ->
    [list_to_binary(Method) | Acc];
find_methods_in_clause(?LIST(?BINARY(Method), Cons)
                      ,Acc) ->
    [list_to_binary(Method) | find_methods_in_clause(Cons, Acc)];

%% Matches the content_types_provided to_json list
find_methods_in_clause(?LIST(?TUPLE([?ATOM('to_json'), JSONList])
                            ,Rest
                            )
                      ,Acc) ->
    CTPs = find_content_types_in_clause(JSONList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_csv list
find_methods_in_clause(?LIST(?TUPLE([?ATOM('to_csv'), CSVList])
                            ,Rest
                            )
                      ,Acc) ->
    CTPs = find_content_types_in_clause(CSVList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_binary list
find_methods_in_clause(?LIST(?TUPLE([?ATOM('to_binary'), BinaryList])
                            ,Rest
                            )
                      ,Acc) ->
    CTPs = find_content_types_in_clause(BinaryList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_pdf list
find_methods_in_clause(?LIST(?TUPLE([?ATOM('to_pdf'), PDFList])
                            ,Rest
                            )
                      ,Acc) ->
    CTPs = find_content_types_in_clause(PDFList, Acc),
    find_methods_in_clause(Rest, CTPs);

find_methods_in_clause(?CASE(_CaseConditional, CaseClauses), Acc0) ->
    lists:foldl(fun(?CLAUSE(_Args, _Guards, ClauseBody), Acc1) ->
                        find_methods(ClauseBody, Acc1)
                end
               ,Acc0
               ,CaseClauses
               ).

-define(CONTENT_TYPE_BINS(Type, SubType), [?BINARY(Type), ?BINARY(SubType)]).
-define(CONTENT_TYPE_VARS(Type, SubType), [?VAR(Type), ?VAR(SubType)]).

-spec find_content_types_in_clause(erl_parse:abstract_expr(), kz_term:ne_binaries()) ->
                                          kz_term:ne_binaries().
find_content_types_in_clause(?EMPTY_LIST, Acc) -> Acc;
find_content_types_in_clause(?LIST(?TUPLE(?CONTENT_TYPE_VARS(_Type, _SubType))
                                  ,Rest
                                  )
                            ,Acc) ->
    find_content_types_in_clause(Rest, Acc);
find_content_types_in_clause(?LIST(?TUPLE(?CONTENT_TYPE_BINS(Type, SubType))
                                  ,Rest
                                  )
                            ,Acc) ->
    CT = kz_binary:join([Type, SubType], <<"/">>),
    find_content_types_in_clause(Rest, [CT | Acc]).

-spec grep_cb_module(atom() | kz_term:ne_binary()) ->
                            {'match', kz_term:ne_binaries()} |
                            'nomatch'.
grep_cb_module(Module) when is_atom(Module) ->
    grep_cb_module(kz_term:to_binary(Module));
grep_cb_module(?NE_BINARY=Module) ->
    re:run(Module
          ,<<"^cb_([a-z_]+?)(?:_(v[0-9]))?\$">>
          ,[{'capture', 'all_but_first', 'binary'}]
          ).

-spec to_swagger_parameters(kz_term:ne_binaries()) -> kz_json:object().
to_swagger_parameters(Paths) ->
    Params = [Param || Path <- Paths,
                       Param <- split_url(Path),
                       is_path_variable(Param)
             ],
    kz_json:from_list(
      [{<<?X_AUTH_TOKEN>>, parameter_auth_token(true)}
      ,{<<?X_AUTH_TOKEN_NOT_REQUIRED>>, parameter_auth_token(false)}
      ]
      ++ [{kz_json:get_ne_binary_value(<<"name">>, ParamProps), ParamProps}
          || Param <- lists:usort(lists:flatten(Params)),
             ParamProps <- [kz_json:from_list(def_path_param(Param))]
         ]).

-spec parameter_auth_token(boolean()) -> kz_json:object().
parameter_auth_token(IsRequired) ->
    kz_json:from_list([{<<"name">>, <<"X-Auth-Token">>}
                      ,{<<"in">>, <<"header">>}
                      ,{<<"type">>, <<"string">>}
                      ,{<<"minLength">>, 32}
                      ,{<<"required">>, IsRequired}
                      ,{<<"description">>, <<"request authentication token">>}
                      ]).

-spec generic_id_path_param(kz_term:ne_binary()) -> kz_json:json_proplist().
generic_id_path_param(Name) ->
    [{<<"minLength">>, 32}
    ,{<<"maxLength">>, 32}
    ,{<<"pattern">>, <<"^[0-9a-f]+\$">>}
     | base_path_param(Name)
    ].

-spec base_path_param(kz_term:ne_binary()) -> kz_json:json_proplist().
base_path_param(Param) ->
    [{<<"name">>, unbrace_param(Param)}
    ,{<<"in">>, <<"path">>}
    ,{<<"required">>, true}
    ,{<<"type">>, <<"string">>}
    ].

-spec modb_id_path_param(kz_term:ne_binary()) -> kz_json:json_proplist().
modb_id_path_param(Param) ->
    %% Matches an MoDB id:
    [{<<"pattern">>, <<"^[0-9a-f-]+\$">>}
    ,{<<"minLength">>, 39}
    ,{<<"maxLength">>, 39}
     | base_path_param(Param)
    ].

%% When param represents an account id (i.e. 32 bytes of hexa):
-spec def_path_param(kz_term:ne_binary()) -> kz_json:json_proplist().
def_path_param(<<"{ACCOUNT_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{ADDRESS_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{ALERT_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{APP_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{AUTH_TOKEN}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{BLACKLIST_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{C2C_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{CALLFLOW_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{CARD_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{CCCP_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{CONFERENCE_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{CONFIG_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{CONNECTIVITY_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{DEVICE_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{DIRECTORY_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{FAXBOX_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{FAX_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{GROUP_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{KEY_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{LEDGER_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{LINK_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{LIST_ENTRY_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{LIST_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{MEDIA_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{MENU_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{NOTIFICATION_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{PORT_REQUEST_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{QUEUE_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{RATE_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{RESOURCE_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{RESOURCE_TEMPLATE_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{SMS_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{STORAGE_PLAN_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{TEMPLATE_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{TEMPORAL_RULE_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{TEMPORAL_RULE_SET}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{USER_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{VM_BOX_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{WEBHOOK_ID}">>=P) -> generic_id_path_param(P);
def_path_param(<<"{MIGRATION_ID}">>=P) -> generic_id_path_param(P);

%% When param represents an MoDB id (i.e. 32+4+2 bytes of hexa & 1 dash):
def_path_param(<<"{CDR_ID}">>=P) -> modb_id_path_param(P);
def_path_param(<<"{RECORDING_ID}">>=P) -> modb_id_path_param(P);

%% When you don't know (ideally you do know):
def_path_param(<<"{ARGS}">>=P) -> base_path_param(P);
def_path_param(<<"{ATTACHMENT_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{ATTEMPT_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{CALL_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{COMMENT_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{ERROR_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{EXTENSION}">>=P) -> base_path_param(P);
def_path_param(<<"{FAX_JOB_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{HANDLER_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{INTERACTION_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{JOB_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{LANGUAGE}">>=P) -> base_path_param(P);
def_path_param(<<"{LEDGER_ENTRY_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{PLAN_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{PROMPT_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{PROVIDER_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{SAMPLE_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{SELECTOR_NAME}">>=P) -> base_path_param(P);
def_path_param(<<"{SMTP_LOG_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{SOCKET_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{SYSTEM_CONFIG_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{TEMPLATE_NAME}">>=P) -> base_path_param(P);
def_path_param(<<"{THING}">>=P) -> base_path_param(P);
def_path_param(<<"{TRANSACTION_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{USERNAME}">>=P) -> base_path_param(P);
def_path_param(<<"{VM_MSG_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{WHITELABEL_DOMAIN}">>=P) -> base_path_param(P);

%% For all the edge cases out there:
def_path_param(<<"{MODB_SUFFIX}">>=P) ->
    [{<<"minLength">>, 6}
    ,{<<"maxLength">>, 6}
    ,{<<"pattern">>, <<"^[0-9]{6}">>}
     | base_path_param(P)
    ];
def_path_param(<<"report-{REPORT_ID}">>) ->
    Prefix = <<"report-">>,
    PrefixSize = byte_size(Prefix),
    [{<<"minLength">>, 32 + PrefixSize}
    ,{<<"maxLength">>, 32 + PrefixSize}
    ,{<<"pattern">>, <<"^report\\-[0-9a-f]+\$">>}
     | base_path_param(<<"{REPORT_ID}">>)
    ];

def_path_param(<<"{APP_SCREENSHOT_INDEX}">>=P) ->
    [{<<"pattern">>, <<"^[0-9]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{FUNCTION}">>=P) ->
    [{<<"pattern">>, <<"^[a-zA-Z0-9]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{IP_ADDRESS}">>=P) ->
    [{<<"minLength">>, 7}
    ,{<<"maxLength">>, 15}
    ,{<<"pattern">>, <<"^[0-9.]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{MODULE}">>=P) ->
    [{<<"pattern">>, <<"^[a-zA-Z0-9]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{NODE}">>=P) ->
    [{<<"pattern">>, <<"^[a-zA-Z0-9]+@[a-zA-Z0-9]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{PARTICIPANT_ID}">>=P) ->
    [{<<"pattern">>, <<"^[0-9]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{PHONE_NUMBER}">>=P) ->
    [{<<"minLength">>, 13}
    ,{<<"pattern">>, <<"^%2[Bb][0-9]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{SCHEMA_NAME}">>=P) ->
    [{<<"pattern">>, <<"^[a-z0-9._-]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{TASK_ID}">>=P) ->
    [{<<"minLength">>, 15}
    ,{<<"maxLength">>, 15}
    ,{<<"pattern">>, <<"^[0-9a-f]+\$">>}
     | base_path_param(P)
    ];

def_path_param(<<"{ENDPOINT_ID}">>=P) ->
    generic_id_path_param(P);
def_path_param(<<"{ENDPOINT_TYPE}">>=P) ->
    [{<<"enum">>, [<<"users">>, <<"devices">>]}
     | base_path_param(P)
    ];

def_path_param(<<"{UUID}">>=P) ->
    [{<<"pattern">>, <<"^[a-f0-9-]+\$">>}
     | base_path_param(P)
    ];
def_path_param(<<"{NUMBER}">> = P) ->
    [{<<"pattern">>, <<"^\\+?[0-9]+">>}
     | base_path_param(P)
    ];
def_path_param(<<"{AUDIT_ID}">> = P) ->
    generic_id_path_param(P);
def_path_param(<<"{SOURCE_SERVICE}">> = P) ->
    base_path_param(P);

def_path_param(_Param) ->
    io:format('standard_error'
             ,"~s/" ?FILE ":~p: No Swagger definition of path parameter '~s'.\n"
             ,[code:lib_dir('kazoo_ast'), ?LINE, _Param]
             ),
    halt(1).

filters_from_module(Module) ->
    Options = [{'accumulator', kz_json:new()}
              ,{'clause', fun handle_filter_clause/3}
              ,{'function', fun maybe_process_function/3}
              ],
    kazoo_ast:walk_modules([Module], Options).

maybe_process_function('filter_prop', 3, Acc) -> Acc;
maybe_process_function(_F, _A, Acc) -> {'skip', Acc}.

handle_filter_clause([?VAR('Doc'), ?BINARY_MATCH([?BINARY_STRING(FilterName)]), ?VAR('Val')], _Guards, Acc) ->
    kz_json:set_value(kz_term:to_binary(FilterName), <<"{VALUE}">>, Acc);
handle_filter_clause([?VAR('Doc'), ?BINARY_MATCH([?BINARY_STRING(FilterName)]), ?VAR('Key')], _Guards, Acc) ->
    kz_json:set_value(kz_term:to_binary(FilterName), <<"{KEY}">>, Acc);
handle_filter_clause([?VAR('Doc')
                     ,?BINARY_MATCH([?BINARY_STRING(FilterName)
                                    ,?BINARY_VAR('Key')
                                    ])
                     ,?VAR('Val')
                     ], _Guards, Acc) ->
    kz_json:set_value(kz_term:to_binary(FilterName ++ "{KEY}"), <<"{VALUE}">>, Acc);
handle_filter_clause([?VAR('_') | _], _Guards, Acc) ->
    Acc.
