-module(cb_api_endpoints).

-compile({'no_auto_import', [get/0]}).

-export([get/0
        ,to_swagger_json/0
        ,to_ref_doc/0, to_ref_doc/1
        ,schema_to_doc/2
        ]).

-ifdef(TEST).
-export([sort_methods/1]).

-endif.

-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("crossbar/src/crossbar.hrl").

-define(REF_PATH
       ,filename:join([code:lib_dir('crossbar'), "doc", "ref"])
       ).
-define(REF_PATH(Module)
       ,filename:join([?REF_PATH, <<Module/binary,".md">>])
       ).

-define(SCHEMA_SECTION, <<"#### Schema\n\n">>).
-define(SUB_SCHEMA_SECTION_HEADER, <<"#####">>).

-define(ACCOUNTS_PREFIX, "accounts/{ACCOUNT_ID}").

-define(X_AUTH_TOKEN, "auth_token_header").

-spec to_ref_doc() -> 'ok'.
-spec to_ref_doc(atom()) -> 'ok'.
to_ref_doc() ->
    ensure_ref_dir(),
    lists:foreach(fun api_to_ref_doc/1, ?MODULE:get()).

to_ref_doc(CBModule) ->
    ensure_ref_dir(),
    Path = code:which(CBModule),
    api_to_ref_doc(hd(process_module(Path, []))).

-spec ensure_ref_dir() -> 'ok'.
ensure_ref_dir() ->
    'ok' = filelib:ensure_dir(filename:join([?REF_PATH, ".placeholder"])).

api_to_ref_doc([]) -> 'ok';
api_to_ref_doc({Module, Paths}) ->
    api_to_ref_doc(Module, Paths, module_version(Module)).

api_to_ref_doc(Module, Paths, ?CURRENT_VERSION) ->
    BaseName = base_module_name(Module),
    Sections = lists:foldl(fun(K, Acc) -> api_path_to_section(Module, K, Acc) end
                          ,ref_doc_header(BaseName)
                          ,Paths
                          ),
    Doc = lists:reverse(Sections),
    DocPath = ?REF_PATH(BaseName),
    'ok' = file:write_file(DocPath, Doc);
api_to_ref_doc(_Module, _Paths, _Version) ->
    'ok'.

api_path_to_section(Module, {'allowed_methods', Paths}, Acc) ->
    ModuleName = path_name(Module),
    lists:foldl(fun(Path, Acc1) ->
                        methods_to_section(ModuleName, Path, Acc1)
                end
               ,Acc
               ,Paths
               );
api_path_to_section(_MOdule, _Paths, Acc) -> Acc.

%% #### Fetch/Create/Change
%% > Verb Path
%% ```shell
%% curl -v http://{SERVER}:8000/Path
%% ```
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

-spec sort_methods(ne_binaries()) -> ne_binaries().
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
    [[ "#### ", method_as_action(Method), "\n\n"
     ,"> ", Method, " ", APIPath, "\n\n"
     , "```shell\ncurl -v -X ", Method, " \\\n"
       "    -H \"X-Auth-Token: {AUTH_TOKEN}\" \\\n"
       "    http://{SERVER}:8000", APIPath, "\n"
       "```\n\n"
     ]
     | Acc
    ].

method_as_action(?HTTP_GET) -> <<"Fetch">>;
method_as_action(?HTTP_PUT) -> <<"Create">>;
method_as_action(?HTTP_POST) -> <<"Change">>;
method_as_action(?HTTP_DELETE) -> <<"Remove">>;
method_as_action(?HTTP_PATCH) -> <<"Patch">>.

ref_doc_header(BaseName) ->
    CleanedUpName = kz_ast_util:smash_snake(BaseName),
    [[maybe_add_schema(BaseName)]
    ,["#### About ", CleanedUpName, "\n\n"]
    ,["### ", CleanedUpName, "\n\n"]
    ].

maybe_add_schema(BaseName) ->
    case kz_ast_util:load_ref_schema(BaseName) of
        'undefined' -> [?SCHEMA_SECTION, "\n\n"];
        SchemaJObj ->
            [Table | RefTables] = kz_ast_util:schema_to_table(SchemaJObj),
            [?SCHEMA_SECTION, Table, "\n\n", ref_tables_to_doc(RefTables), "\n\n"]
    end.

%% This looks for "#### Schema" in the doc file and adds the JSON schema formatted as the markdown table
%% Schema = "vmboxes" or "devices"
%% Doc = "voicemail.md" or "devices.md"
-spec schema_to_doc(ne_binary(), ne_binary()) -> 'ok'.
schema_to_doc(Schema, Doc) ->
    {'ok', SchemaJObj} = kz_json_schema:load(Schema),
    [Table|RefTables] = kz_ast_util:schema_to_table(SchemaJObj),

    DocFile = filename:join([code:lib_dir('crossbar'), "doc", Doc]),
    {'ok', DocContents} = file:read_file(DocFile),

    case binary:split(DocContents, <<?SCHEMA_SECTION/binary, "\n">>) of
        [Before, After] ->
            file:write_file(DocFile, [Before, ?SCHEMA_SECTION, Table, $\n, ref_tables_to_doc(RefTables), After]);
        _Else ->
            io:format("file ~s appears to have a schema section already~n", [DocFile])
    end.

ref_tables_to_doc([]) -> [];
ref_tables_to_doc(Tables) ->
    ref_tables_to_doc(Tables, []).

ref_tables_to_doc([], Acc) -> lists:reverse(Acc);
ref_tables_to_doc([Table | Tables], Acc) ->
    ref_tables_to_doc(Tables, [[ref_table_to_doc(Table)] | Acc]).

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
                          ,{<<"url">>, <<"https://github.com/2600hz/kazoo/tree/master/applications/crossbar/doc">>}
                          ])
       ).

-spec to_swagger_json() -> 'ok'.
to_swagger_json() ->
    BaseSwagger = read_swagger_json(),
    BasePaths = kz_json:get_value(<<"paths">>, BaseSwagger),

    Paths = format_as_path_centric(get()),

    Swagger = kz_json:set_values([{<<"paths">>, to_swagger_paths(Paths, BasePaths)}
                                 ,{<<"definitions">>, to_swagger_definitions()}
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
                                ),
    write_swagger_json(Swagger).

-spec to_swagger_definitions() -> kz_json:object().
to_swagger_definitions() ->
    SchemasPath = kz_ast_util:schema_path(<<>>),
    filelib:fold_files(SchemasPath, "\\.json\$", 'false', fun process_schema/2, kz_json:new()).

-spec process_schema(ne_binary(), kz_json:object()) -> kz_json:object().
process_schema(Filename, Definitions) ->
    {'ok', Bin} = file:read_file(Filename),
    JObj = kz_json:delete_keys([<<"_id">>, <<"$schema">>], kz_json:decode(Bin)),
    Name = kz_term:to_binary(filename:basename(Filename, ".json")),
    kz_json:set_value(Name, JObj, Definitions).

-define(SWAGGER_JSON,
        filename:join([code:priv_dir('crossbar'), "api", "swagger.json"])).

read_swagger_json() ->
    case file:read_file(?SWAGGER_JSON) of
        {'ok', Bin} -> kz_json:decode(Bin);
        {'error', 'enoent'} -> kz_json:new()
    end.

write_swagger_json(Swagger) ->
    file:write_file(?SWAGGER_JSON, kz_json:encode(Swagger)).

to_swagger_paths(Paths, BasePaths) ->
    Endpoints =
        [{[Path,Method], kz_json:get_value([Path,Method], BasePaths, kz_json:new())}
         || {Path,AllowedMethods} <- kz_json:to_proplist(Paths),
            Method <- kz_json:get_list_value(<<"allowed_methods">>, AllowedMethods, [])
        ],
    kz_json:merge_recursive(kz_json:set_values(Endpoints, kz_json:new())
                           ,kz_json:foldl(fun to_swagger_path/3, kz_json:new(), Paths)
                           ).

to_swagger_path(Path, PathMeta, Acc) ->
    Methods = kz_json:get_value(<<"allowed_methods">>, PathMeta, []),
    SchemaParameter = swagger_params(PathMeta),
    lists:foldl(fun(Method, Acc1) ->
                        add_swagger_path(Method, Acc1, Path, SchemaParameter)
                end
               ,Acc
               ,Methods
               ).

add_swagger_path(Method, Acc, Path, SchemaParameter) ->
    MethodJObj = kz_json:get_value([Path, Method], Acc, kz_json:new()),
    Parameters = make_parameters(Path, Method, SchemaParameter),
    Vs = props:filter_empty(
           [{[Path, Method], MethodJObj}
           ,{[Path, Method, <<"parameters">>], Parameters}
           ]),
    kz_json:insert_values(Vs, Acc).

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
                  ]
                 )
               ).

compare_parameters(Param1, Param2) ->
    Keys = [<<"name">>, <<"$ref">>],
    kz_json:get_first_defined(Keys, Param1) >= kz_json:get_first_defined(Keys, Param2).

maybe_add_schema(_Path, Method, Schema)
  when Method =:= <<"put">>;
       Method =:= <<"post">> ->
    Schema;
maybe_add_schema(_Path, _Method, _Parameters) ->
    'undefined'.

swagger_params(PathMeta) ->
    case kz_json:get_value(<<"schema">>, PathMeta) of
        'undefined' -> 'undefined';
        Schema ->
            kz_json:from_list([{<<"name">>, Schema}
                              ,{<<"in">>, <<"body">>}
                              ,{<<"required">>, 'true'}
                              ,{<<"schema">>, kz_json:from_list([{<<"$ref">>, <<"#/definitions/", Schema/binary>>}])}
                              ])
    end.

auth_token_param(Path, _Method) ->
    case is_authtoken_required(Path) of
        'undefined' -> 'undefined';
        Required ->
            kz_json:from_list(
              [{<<"$ref">>, <<"#/parameters/"?X_AUTH_TOKEN>>}
              ,{<<"required">>, Required}
              ])
    end.

-spec is_authtoken_required(ne_binary()) -> api_boolean().
is_authtoken_required(<<"/"?ACCOUNTS_PREFIX"/", _/binary>>=Path) ->
    not is_api_c2c_connect(Path);
is_authtoken_required(_Path) -> 'undefined'.

is_api_c2c_connect(<<"/"?ACCOUNTS_PREFIX"/clicktocall/", _/binary>>=Path) ->
    kz_binary:suffix(<<"/connect">>, Path);
is_api_c2c_connect(_) -> 'false'.

path_params(Path, _Method) ->
    [path_param(Param) || <<"{",_/binary>> = Param <- split_url(Path)].

path_param(PathToken) ->
    Param = unbrace_param(PathToken),
    kz_json:from_list([{<<"$ref">>, <<"#/parameters/", Param/binary>>}]).

split_url(Path) ->
    binary:split(Path, <<$/>>, [global]).


format_as_path_centric(Data) ->
    lists:foldl(fun format_pc_module/2, kz_json:new(), Data).

format_pc_module({Module, Config}, Acc) ->
    ModuleName = path_name(Module),
    lists:foldl(fun(ConfigData, Acc1) ->
                        format_pc_config(ConfigData, Acc1, Module, ModuleName)
                end
               ,Acc
               ,Config
               );
format_pc_module(_MC, Acc) ->
    Acc.

format_pc_config(_ConfigData, Acc, _Module, 'undefined') -> Acc;
format_pc_config({Callback, Paths}, Acc, Module, ModuleName) ->
    lists:foldl(fun(Path, Acc1) ->
                        format_pc_callback(Path, Acc1, Module, ModuleName, Callback)
                end
               ,Acc
               ,Paths
               ).

format_pc_callback({[], []}, Acc, _Module, _ModuleName, _Callback) -> Acc;
format_pc_callback({_Path, []}, Acc, _Module, _ModuleName, _Callback) ->
    io:format("~s not supported ~s\n", [_ModuleName, _Path]),
    Acc;
format_pc_callback({Path, Vs}, Acc, Module, ModuleName, Callback) ->
    PathName = swagger_api_path(Path, ModuleName),
    kz_json:set_values(props:filter_undefined(
                         [{[PathName, kz_term:to_binary(Callback)]
                          ,[kz_term:to_lower_binary(V) || V <- Vs]
                          }
                         ,maybe_include_schema(PathName, Module)
                         ]
                        )
                      ,Acc
                      ).

maybe_include_schema(PathName, Module) ->
    M = base_module_name(Module),
    case filelib:is_file(kz_ast_util:schema_path(<<M/binary, ".json">>)) of
        'false' -> {'undefined', 'undefined'};
        'true' ->
            {[PathName, <<"schema">>], base_module_name(Module)}
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
        true -> brace_token(Token);
        false ->
            case is_all_upper(Prefix) of
                true -> brace_token(camel_to_snake(Token));
                false -> Token
            end
    end;
format_path_token(BadToken) ->
    io:format('standard_error'
             ,"Please pick a good allowed_methods/N variable name: '~s' is too short.\n"
             ,[BadToken]),
    halt(1).

camel_to_snake(Bin) ->
    Options = ['global', {'return', 'binary'}],
    re:replace(Bin, <<"(?!^)([A-Z][a-z])">>, <<"_\\1">>, Options).

is_all_upper(Bin) ->
    Bin =:= kz_term:to_upper_binary(Bin).

brace_token(Token=?NE_BINARY) ->
    <<"{", (kz_term:to_upper_binary(Token))/binary, "}">>.

unbrace_param(Param) ->
    Size = byte_size(Param) - 2,
    <<"{", Param0:Size/binary, "}">> = Param,
    kz_term:to_lower_binary(Param0).

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

path_name(Module) ->
    case grep_cb_module(Module) of
        {'match', [<<"about">>=Name]} -> Name;
        {'match', [<<"accounts">>=Name]} -> Name;
        {'match', [<<"api_auth">>=Name]} -> Name;
        {'match', [<<"auth">>=Name]} -> Name;
        {'match', [<<"basic_auth">>=Name]} -> Name;
        {'match', [<<"google_auth">>=Name]} -> Name;
        {'match', [<<"ip_auth">>=Name]} -> Name;
        {'match', [<<"rates">>=Name]} -> Name;
        {'match', [<<"schemas">>=Name]} -> Name;
        {'match', [<<"shared_auth">>=Name]} -> Name;
        {'match', [<<"sup">>=Name]} -> Name;
        {'match', [<<"system_configs">>=Name]} -> Name;
        {'match', [<<"templates">>=Name]} -> Name;
        {'match', [<<"token_auth">>=Name]} -> Name;
        {'match', [<<"ubiquiti_auth">>=Name]} -> Name;
        {'match', [<<"user_auth">>=Name]} -> Name;
        {'match', [Name]} -> <<?ACCOUNTS_PREFIX"/", Name/binary>>;
        {'match', [Name, ?CURRENT_VERSION]} -> <<?ACCOUNTS_PREFIX"/", Name/binary>>;
        {'match', _M} ->
            'undefined'
    end.

%% API

-type config() :: [{callback(), [{path(), methods()}]}].
-type callback() :: module().
-type path() :: ne_binary().
-type methods() :: ne_binaries().
-spec get() -> config().
get() ->
    Apps = ['crossbar', 'acdc'],
    lists:foldl(fun get_app/2, [], Apps).

get_app(App, Acc) ->
    process_application(App, Acc).

process_application(App, Acc) ->
    EBinDir = code:lib_dir(App, 'ebin'),
    io:format("processing ~s modules: ", [App]),
    Processed = filelib:fold_files(EBinDir, "^cb_.*.beam\$", 'false', fun process_module/2, Acc),
    io:format(" done~n"),
    Processed.

process_module(File, Acc) ->
    {'ok', {Module, [{'exports', Fs}]}} = beam_lib:chunks(File, ['exports']),
    io:format("."),

    case process_exports(File, Module, Fs) of
        'undefined' -> Acc;
        Exports -> [Exports | Acc]
    end.

is_api_function({'allowed_methods', _Arity}) -> 'true';
%%is_api_function({'content_types_provided', _Arity}) -> 'true';
is_api_function(_) ->  'false'.

process_exports(_File, 'api_resource', _) -> [];
process_exports(_File, 'cb_context', _) -> [];
process_exports(File, Module, Fs) ->
    case lists:any(fun is_api_function/1, Fs) of
        'false' -> 'undefined';
        'true' -> process_api_module(File, Module)
    end.

process_api_module(File, Module) ->
    {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(File, ['abstract_code']),
    try process_api_ast(Module, AST)
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            io:format("failed to process ~p(~p): ~s: ~p\n", [File, Module, _E, _R]),
            io:format("~p\n", [ST]),
            'undefined'
    end.

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
    {'allowed_methods', Methods};
process_api_ast_function(_Module, 'content_types_provided', _Arity, Clauses) ->
    ContentTypes = find_http_methods(Clauses),
    {'content_types_provided', ContentTypes}.

find_http_methods(Clauses) ->
    lists:foldl(fun find_http_methods_from_clause/2, [], Clauses).

find_http_methods_from_clause(?CLAUSE(ArgsList, _Guards, ClauseBody), Methods) ->
    [{args_list_to_path(ArgsList), find_methods(ClauseBody)}
     | Methods
    ].

args_list_to_path([]) -> <<"/">>;
args_list_to_path(Args) ->
    lists:reverse(lists:foldl(fun arg_to_path/2, [], Args)).

arg_to_path(?BINARY_MATCH(Matches), Acc) ->
    [binary_match_to_path(Matches) | Acc];
arg_to_path(?VAR('Context'), Acc) ->
    Acc;
arg_to_path(?VAR(Name), Acc) ->
    [kz_term:to_binary(Name) | Acc];
arg_to_path(?MATCH(?BINARY_MATCH(_), ?VAR(Name)), Acc) ->
    [kz_term:to_binary(Name) | Acc].

binary_match_to_path(Matches) ->
    iolist_to_binary([binary_to_path(Match) || Match <- Matches]).
binary_to_path(?BINARY_STRING(Name)) ->
    Name;
binary_to_path(?BINARY_VAR(VarName)) ->
    format_path_token(kz_term:to_binary(VarName)).

find_methods(ClauseBody) ->
    find_methods(ClauseBody, []).
find_methods(ClauseBody, Acc) ->
    lists:usort(lists:foldl(fun find_methods_in_clause/2, Acc, ClauseBody)).

-define(CB_CONTEXT_CALL(Fun), ?MOD_FUN('cb_context', Fun)).

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
find_methods_in_clause(?LAGER, Acc) ->
    Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_fax', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_faxes:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_media', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_media:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_notifications', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_notifications:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_attachments', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_whitelabel:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_domain_attachments', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_whitelabel:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_provisioner', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_global_provisioner_templates:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_for_vm_download', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_vmboxes:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause(?FUN_ARGS('content_types_provided_get', _Args), Acc) ->
    [kz_binary:join([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_port_requests:acceptable_content_types()
    ] ++ Acc;
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
find_methods_in_clause(?LIST(?TUPLE([?ATOM('to_json')
                                    ,JSONList
                                    ]
                                   )
                            ,Rest
                            )
                      ,Acc) ->
    CTPs = find_content_types_in_clause(JSONList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_csv list
find_methods_in_clause(?LIST(?TUPLE([?ATOM('to_csv')
                                    ,CSVList
                                    ]
                                   )
                            ,Rest
                            )
                      ,Acc) ->
    CTPs = find_content_types_in_clause(CSVList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_binary list
find_methods_in_clause(?LIST(?TUPLE([?ATOM('to_binary')
                                    ,BinaryList
                                    ]
                                   )
                            ,Rest
                            )
                      ,Acc) ->
    CTPs = find_content_types_in_clause(BinaryList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_pdf list
find_methods_in_clause(?LIST(?TUPLE([?ATOM('to_pdf')
                                    ,PDFList
                                    ]
                                   )
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

grep_cb_module(Module) when is_atom(Module) ->
    grep_cb_module(kz_term:to_binary(Module));
grep_cb_module(?NE_BINARY=Module) ->
    re:run(Module
          ,<<"^cb_([a-z_]+?)(?:_(v[0-9]))?\$">>
          ,[{'capture', 'all_but_first', 'binary'}]
          ).


to_swagger_parameters(Paths) ->
    Params = [Param || Path <- Paths,
                       Param = <<"{",_/binary>> <- split_url(Path)
             ],
    kz_json:from_list(
      [{<<?X_AUTH_TOKEN>>, kz_json:from_list(
                             [{<<"name">>, <<"X-Auth-Token">>}
                             ,{<<"in">>, <<"header">>}
                             ,{<<"type">>, <<"string">>}
                             ,{<<"minLength">>, 32}
                             ])}
      ,{<<"id">>, kz_json:from_list([{<<"minLength">>, 32}
                                    ,{<<"maxLength">>, 32}
                                    ,{<<"pattern">>, <<"^[0-9a-f]+\$">>}
                                     | base_path_param(<<"id">>)
                                    ])}
      ]
      ++ [{unbrace_param(Param), kz_json:from_list(def_path_param(Param))}
          || Param <- lists:usort(lists:flatten(Params))
         ]).

generic_id_path_param(Name) ->
    [{<<"$ref">>, <<"#/parameters/id">>}
    ,{<<"name">>, Name}
    ].

base_path_param(Param) ->
    [{<<"name">>, Param}
    ,{<<"in">>, <<"path">>}
    ,{<<"required">>, true}
    ,{<<"type">>, <<"string">>}
    ].

modb_id_path_param(Param) ->
    %% Matches an MoDB id:
    [{<<"pattern">>, <<"^[0-9a-f-]+\$">>}
    ,{<<"minLength">>, 39}
    ,{<<"maxLength">>, 39}
     | base_path_param(Param)
    ].

%% When param represents an account id (i.e. 32 bytes of hexa):
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
def_path_param(<<"{LEDGER_ID}">>=P) -> generic_id_path_param(P);
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

%% When param represents an MoDB id (i.e. 32+4+2 bytes of hexa & 1 dash):
def_path_param(<<"{CDR_ID}">>=P) -> modb_id_path_param(P);
def_path_param(<<"{RECORDING_ID}">>=P) -> modb_id_path_param(P);

%% When you don't know (ideally you do know):
def_path_param(<<"{ARGS}">>=P) -> base_path_param(P);
def_path_param(<<"{ATTACHMENT_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{ATTEMPT_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{CALL_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{COMMENT_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{EXTENSION}">>=P) -> base_path_param(P);
def_path_param(<<"{FAX_JOB_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{INTERACTION_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{JOB_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{LANGUAGE}">>=P) -> base_path_param(P);
def_path_param(<<"{LEDGER_ENTRY_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{PLAN_ID}">>=P) -> base_path_param(P);
def_path_param(<<"{PROMPT_ID}">>=P) -> base_path_param(P);
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

def_path_param(<<"{UUID}">>=P) ->
    [{<<"pattern">>, <<"^[a-f0-9-]+\$">>}
     | base_path_param(P)
    ];

def_path_param(_Param) ->
    io:format(standard_error
             ,"No Swagger definition of path parameter '~s'.\n"
             ,[_Param]),
    halt(1).
