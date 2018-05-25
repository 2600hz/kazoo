%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% The origin of this file is the edoc module `edoc_doclet.erl'
%%% written by Richard Carlsson.
%%% @doc Kazoo standard doclet module for EDoc.
%%%
%%% TODO: copy "doc-files" subdirectories, recursively.
%%% TODO: generate summary page of TODO-notes
%%% TODO: generate summary page of deprecated things
%%% TODO: generate decent indexes over modules, methods, records, etc.
%%%
%%% @author Richard Carlsson <carlsson.richard@gmail.com>
%%% @author Hesaam Farhang
%%% @see kz_edoc_layout
%%% @see edoc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_edoc_doclet).

%% Note that this is written so that it is *not* depending on edoc.hrl!

-export([run/1]).

-export([process_cmds/2
        ,sidebar_apps_list/2
        ,render_apps/3
        ,build_search_index/2
        ,render_apps_index/3
        ,index_file/2
        ,copy_files/1]).

-export([compile_templates/0
        ,default_context/0
        ,init_context/1
        ,init_context/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(DEFAULT_TEMPLATE_DIR, "doc/edoc-template").

-define(INLINE_SVGS, [{kz_2600hz_logo_svg_file, "2600hz.svg"}
                     ,{kz_erlang_logo, "erlang.svg"}
                     ]).

-define(APP_OVERVIEW_FILE, "overview.edoc").
-define(APPS_OVERVIEW_FILE, "apps_overview.edoc").
-define(PROJ_OVERVIEW_FILE, "doc/edoc_overview.edoc").

-define(INDEX_FILE, "index.html").
-define(APP_SUMMARY_FILE, "index.html").

-define(DEFAULT_TEMPLATES, [{kz_edoc_mod_template, "doc/edoc-template/module.html"}
                           ,{kz_edoc_app_template, "doc/edoc-template/app_overview.html"}
                           ,{kz_edoc_apps_template, "doc/edoc-template/apps_index.html"}
                           ,{kz_edoc_index_template, "doc/edoc-template/index.html"}
                           ]).


-type gen_app() :: {atom(), atom(), string(), [string()]}.
%% Contains of source files to process.
%%
%% <strong>Tuple description:</strong>
%% ```{ModuleName, AppName, "core" | "applications", ErlFiles}'''

-type build_search() :: app | module | funcs | func | types | type.

-type search_doc() :: kz_json:object().
%% Contains JSON objects of fields/value to be index for search.
%%
%% <strong>JSON object description:</strong>
%%
%% ```
%% [
%% {
%%   "ref": "{{AppName}}/index.html",
%%   "app": "{{AppName}}"
%% },
%% {
%%   "ref": "{{AppName}}/{{ModuleName}}.html",
%%   "module": "{{ModuleName}}",
%%   "desc": "{{ModuleDescription}}"
%% },
%% {
%%   "ref": "{{AppName}}/index.html#{{FuncTagID}}",
%%   "fun": "{{FuncName}}"
%%   "desc": "{{FuncDescription}}"
%% },
%% {
%%   "ref": "{{AppName}}/{{ModuleName}}.html#{{TypeTagID}}",
%%   "type": "{{TypeNameName}}",
%%   "desc": "{{TypeNameDescription}}"
%% }
%% ]
%% '''

-type search_docs() :: [kz_json:object()].

-type sidebar_props() :: kz_term:proplist().
%% Contains sidebar proplist.
%%
%% <strong>Proplist description:</strong>
%% ```[{"core" | "applications", [{AppName, [ModuleName, ModuleDescription, ModuleOutfilePath]}]}]'''

-type processed_mod() :: {string(), atom(), atom(), binary()}.
%% Contains module information necessary for computing sidebar and render.
%%
%% <strong>Tuple description:</strong>
%% ```{"core" | "applications, AppName, ModuleName, ModuleDescription}'''

-type mod_desc() :: {atom(), binary()}.
%% Contains module name and description for further doc generation.
%%
%% <strong>Tuple description:</strong>
%% ```{ModuleName, ModuleDescription}'''

-type mods_map_key() :: {string(), atom()}.
%% Map key is: `{"core" | "applications", AppName}'.

-type mods_map() :: #{mods_map_key() => mod_desc()}.
%% Contains module information necessary for computing sidebar and render.
%% See {@link mod_desc()} and {@link mods_map_key()}.

%%------------------------------------------------------------------------------
%% @doc Main Kazoo doclet entry point.
%%
%% We only care about kazoo documentation, so not all options/features are
%% implemented.
%%
%% Someone please document extra Kazoo options.
%% @end
%%------------------------------------------------------------------------------
-spec run(kz_term:proplist()) -> ok.
run(Options) ->
    run(Options, props:get_value(kz_gen_apps, Options)).

-spec run(kz_term:proplist(), [gen_app()]) -> ok.
run(_, []) ->
    ?DEV_LOG("no source files were found...", []),
    exit(error);
run(Options, GenApps) ->
    gen_multi_app(init_context(Options, GenApps), GenApps).

gen_multi_app(Context, GenApps) ->
    {Modules, HasError} = process_cmds(Context, GenApps),
    Sidebar = sidebar_apps_list(Modules, Context),
    IndexData = render_apps(Modules, Sidebar, Context),
    build_search_index(IndexData, Context),
    render_apps_index(Modules, Sidebar, Context),
    index_file(Sidebar, Context),
    copy_files(Context),

    %% handle postponed error during processing of source files
    case HasError of
        true ->
            ?DEV_LOG("doc generation is completed with some error~n", []),
            exit(error);
        false ->
            io:format("~n~n Done.~n")
    end.

%%------------------------------------------------------------------------------
%% @doc Processing source files and save their EDoc as JSON. Returns a map of
%% successful processed modules and their infos and whether or not all source files
%% are successfully processed or not.
%% @end
%%------------------------------------------------------------------------------
-spec process_cmds(map(), [gen_app()]) -> {mods_map(), boolean()}.
process_cmds(Context, GenApps) ->
    Private = maps:get(private, Context, false),
    Hidden = maps:get(hidden, Context, false),

    GenAppsLength = length(GenApps),

    SourceFun = fun(Obj, Acc) -> source(Context, Obj, Acc, Private, Hidden) end,

    io:format("~n:: Start processing ~b source file(s)~n", [GenAppsLength]),

    Malt = [{'processes', 'schedulers'}],
    Result = plists:fold(SourceFun, fun fuse_together/2, [], GenApps, Malt),

    Fun = fun({AppCat, App, Module, ShortDesc}, Map) ->
                  maps:put({AppCat, App}, [{Module, ShortDesc} | maps:get({AppCat, App}, Map, [])], Map)
          end,

    io:format("~n:: Successfully processed: ~b~n~n", [length(Result)]),
    {lists:foldl(Fun, #{}, Result), GenAppsLength /= length(Result)}.

fuse_together(A, B) when is_list(A), is_list(B) ->
    A ++ B.

%%------------------------------------------------------------------------------
%% @doc Generating documentation (as a JSON) for a source file, adding its name to the
%% `Acc' if it was successful. Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.
%% @end
%%------------------------------------------------------------------------------
-spec source(map(), gen_app(), [processed_mod()], boolean(), boolean()) -> [processed_mod()].
source(#{kz_doc_site := OutDir, kz_apps_uri := AppsUri
        ,edoc_env := Env, edoc_opts := EDocOpts
        } = Ctx
      ,{M, App, AppCat, ErlFile}
      ,Result
      ,Private, Hidden
      ) ->
    io:format("."),
    Context = Ctx#{kz_rel_path => make_rel_path(filename:split(filename:join(AppsUri, App)))
                  ,kz_app_cat => AppCat
                  ,kz_app_name => atom_to_list(App)
                  ,kz_mod_name => atom_to_list(M)
                  },
    try edoc:get_doc(ErlFile, Env, EDocOpts) of
        {Module, Doc} ->
            check_name(Module, M, ErlFile),
            case (not is_private(Doc)
                  orelse Private)
                andalso (not is_hidden(Doc)
                         orelse Hidden)
            of
                true ->
                    Props = run_layout(module, ErlFile, Doc, Context),
                    JObj = kz_json:from_list_recursive(Props),
                    Name1 = atom_to_list(M) ++ ".json",

                    Encoding = [{encoding, utf8}],
                    edoc_lib:write_file(kz_json:encode(JObj), filename:join([OutDir, "tmp", AppsUri, App]), Name1, Encoding),

                    ShortDesc = props:get_value(short_desc, Props, []),
                    [{AppCat, App, Module, ShortDesc} | Result];
                false ->
                    Result
            end
    catch
        _:R ->
            ?DEV_LOG("skipping source file '~ts': ~P.", [ErlFile, R, 15]),
            Result
    end.

check_name([$? | _], _, _) ->
    %% A module name of the form '?...' is assumed to be caused
    %% by the epp_dodger parser when the module declaration has
    %% the form '-module(?MACRO).'; skip the filename check.
    ok;
check_name(Module, Module, _) ->
    ok;
check_name(Module, _, File) ->
    ?DEV_LOG("file '~ts' actually contains module '~s'.", [File, Module]).

%%------------------------------------------------------------------------------
%% @doc Processing the result of {@link process_cmds/2} and create sidebar
%% proplist ready for render.
%% @end
%%------------------------------------------------------------------------------
-spec sidebar_apps_list(mods_map(), map()) -> sidebar_props().
sidebar_apps_list(Modules, Context) ->
    lists:map(fun(AppCat) -> {AppCat, sidebar_apps_cat(AppCat, Modules, Context)} end, ["core", "applications"]).

sidebar_apps_cat(AppCat, Modules, #{kz_apps_uri := AppsUri, file_suffix := Suffix}) ->
    Fun = fun({Cat, App}, Mods, Acc, Cat) ->
                  [{App, lists:sort([{Module, Desc, AppsUri ++ "/" ++ atom_to_list(App) ++ "/" ++ atom_to_list(Module) ++ Suffix}
                                     || {Module, Desc} <- Mods
                                    ])
                   }
                   | Acc
                  ];
             (_, _, Acc, _) ->
                  Acc
          end,
    lists:sort(maps:fold(fun(K, V, A) -> Fun(K, V, A, AppCat) end, [], Modules)).

%%------------------------------------------------------------------------------
%% @doc Processing the result of {@link process_cmds/2} and renders each
%% application root index file and each module files.
%% @end
%%------------------------------------------------------------------------------
-spec render_apps(mods_map(), sidebar_props(), map()) -> [kz_json:object()].
render_apps(Modules, Sidebar, Context) ->
    io:format(":: Start rendering~n", []),

    Malt = [{'processes', 'schedulers'}],
    plists:fold(fun({App, Ms}, Acc) -> render_app(App, Ms, Sidebar, Context) ++ Acc end
               ,fun fuse_together/2
               ,[]
               ,maps:to_list(Modules)
               ,Malt
               ).

-spec render_app({string(), atom()}, [mod_desc()], sidebar_props(), map()) -> [kz_json:object()].
render_app({AppCat, App}, Modules, Sidebar, Context) ->
    render_app_overview(AppCat, App, Modules, Sidebar, Context),
    [build_search_index_data(app, {list_to_binary(AppCat), atom_to_binary(App, utf8)}, Context, [])
     | lists:flatten([render_module(AppCat, App, Ms, Sidebar, Context) || Ms <- Modules])
    ].

-spec render_app_overview(string(), atom(), [mod_desc()], sidebar_props(), map()) -> ok.
render_app_overview(AppCat, App, Modules, Sidebar, #{kz_doc_site := OutDir , kz_apps_uri := AppsUri}=Ctx) ->
    io:format("."),
    File = filename:join([AppCat, App, "doc", ?APP_OVERVIEW_FILE]),
    Context = Ctx#{kz_rel_path => make_rel_path(filename:split(filename:join(AppsUri, App)))
                  ,kz_app_cat => AppCat
                  ,kz_app_name => atom_to_list(App)
                  },
    Props = get_overview_data(File, "Application: " ++ atom_to_list(App), Context),
    Rendered = render([{kz_sidebar_apps, Sidebar}
                      ,{kz_modules, lists:usort(Modules)}
                       | Props
                      ], Context, kz_edoc_app_template),

    EncOpts = [{encoding, utf8}],
    edoc_lib:write_file(Rendered, filename:join([OutDir, AppsUri, App]), ?INDEX_FILE, EncOpts).

-spec render_module(string(), atom(), mod_desc(), sidebar_props(), map()) -> search_docs().
render_module(AppCat, App, {Module, _Desc}, Sidebar, #{kz_doc_site := OutDir, kz_apps_uri := AppsUri, file_suffix := Suffix}=Ctx) ->
    io:format("."),
    Context = Ctx#{kz_rel_path => make_rel_path(filename:split(filename:join(AppsUri, App)))
                  ,kz_app_cat => AppCat
                  ,kz_app_name => atom_to_list(App)
                  ,kz_mod_name => atom_to_list(Module)
                  },
    Props = read_tmp_file(Context, App, Module),

    Rendered = render([{kz_sidebar_apps, Sidebar}
                       | Props
                      ], Context, kz_edoc_mod_template),

    Name = atom_to_list(Module) ++ Suffix,
    EncOpts = [{encoding, utf8}],
    ok = edoc_lib:write_file(Rendered, filename:join([OutDir, AppsUri, App]), Name, EncOpts),
    build_search_index_data(module, {atom_to_binary(App, utf8), atom_to_binary(Module, utf8)}, Context, Props).

-spec render_apps_index(mods_map(), sidebar_props(), map()) -> ok.
render_apps_index(Modules, Sidebar, #{kz_doc_site := OutDir, kz_apps_uri := AppsUri}=Ctx) ->
    io:format("~n:: Rendering apps index file~n"),
    Apps = [App || {_, App} <- maps:keys(Modules)],
    Context = Ctx#{kz_rel_path => make_rel_path(filename:split(AppsUri))},
    Props = get_overview_data(?APPS_OVERVIEW_FILE, "Kazoo Applications Index", Context),
    Rendered = render([{kz_sidebar_apps, Sidebar}
                      ,{kz_apps, lists:usort(Apps)}
                       | Props
                      ], Context, kz_edoc_apps_template),

    EncOpts = [{encoding, utf8}],
    edoc_lib:write_file(Rendered, filename:join([OutDir, AppsUri]), ?INDEX_FILE, EncOpts).

%%------------------------------------------------------------------------------
%% @doc Renders root HTML index file.
%% @end
%%------------------------------------------------------------------------------
-spec index_file(sidebar_props(), map()) -> ok.
index_file(Sidebar, #{kz_doc_site := OutDir}=Ctx) ->
    io:format(":: Rendering index file~n~n"),
    Context = Ctx#{kz_rel_path => make_rel_path([])},
    Props = get_overview_data(?PROJ_OVERVIEW_FILE, "Kazoo Erlang Reference", Context),
    Rendered = render([{kz_sidebar_apps, Sidebar}
                       | Props
                      ], Context, kz_edoc_index_template),

    EncOpts = [{encoding, utf8}],
    edoc_lib:write_file(Rendered, OutDir, ?INDEX_FILE, EncOpts).

-spec run_layout(module | overview, string(), kz_types:xml_el(), map()) -> kz_term:proplist().
run_layout(module, File, Doc, Context) ->
    try kz_edoc_layout:module(Doc, Context)
    catch
        _E:_T ->
            ?DEV_LOG("failed to layout ~s: ~p:~p", [File, _E, _T]),
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST),
            exit(error)
    end;
run_layout(overview, File, Doc, Context) ->
    try kz_edoc_layout:overview(Doc, Context)
    catch
        _E:_T ->
            ?DEV_LOG("failed to layout overview file ~s: ~p:~p", [File, _E, _T]),
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST),
            exit(error)
    end.

-spec render(kz_term:proplist(), map(), atom()) -> iolist().
render(Props0, Context, Template) ->
    Name = props:get_value(name, Props0),
    Props = Props0 ++ maps:to_list(Context),
    %% ?DEV_LOG("Props ~p", [Props]),
    case render(Template, Props) of
        {ok, Rendered} ->
            Rendered;
        {error, _Reason} ->
            ?DEV_LOG("failed to render ~s: ~p", [Name, _Reason]),
            exit(error)
    end.

-spec make_rel_path(string()) -> string().
make_rel_path([]) -> "";
make_rel_path(Ps) ->
    lists:append(["../" || _ <- lists:seq(1, length(Ps))]).

-spec render(atom(), kz_term:proplist()) -> {ok, iolist() | atom()} | {error, any()}.
render(Module, Props) when is_atom(Module) ->
    kz_template:render(Module, Props).
%%render(TemplateFilePath, Props) when is_list(TemplateFilePath) ->
%%    kz_template:render(TemplateFilePath, Props, [{auto_escape, false}]).

-spec read_tmp_file(map(), atom(), atom()) -> kz_term:proplist().
read_tmp_file(#{kz_doc_site := OutDir, kz_apps_uri := AppsUri}, App, Module) ->
    Path = filename:join([OutDir, "tmp", AppsUri, App, Module]) ++ ".json",
    case file:read_file(Path) of
        {ok, Bin} ->
            kz_json:recursive_to_proplist(kz_json:decode(Bin));
        {error, Reason} ->
            ?DEV_LOG("can not read temporary file ~p: ~p", [Path, Reason]),
            exit(error)
    end.

-spec get_overview_data(string(), string(), map()) -> kz_term:proplist().
get_overview_data(File, Title, #{edoc_env := Env, edoc_opts := Options}=Context) ->
    Tags = extract_overview(File, Env, Options),
    Data0 = edoc_data:overview(Title, Tags, Env, Options),
    EncodingAttribute = #xmlAttribute{name = encoding
                                     ,value = "utf8"
                                     },
    #xmlElement{attributes = As} = Data0,

    run_layout(overview, File, Data0#xmlElement{attributes = [EncodingAttribute | As]}, Context).

%% Read external source file. Fails quietly, returning empty tag list.
-spec extract_overview(string(), any(), any()) -> any().
extract_overview(File, Env, Opts) ->
    case edoc_extract:file(File, overview, Env, Opts) of
        {ok, Tags} ->
            Tags;
        {error, _} ->
            []
    end.

-spec build_search_index_data(build_search(), {binary(), binary()}, map(), list()) -> search_doc() | search_docs().
build_search_index_data(module, {App, Module}=AppMod, #{file_suffix := Suffix}=Context, Props) ->
    Desc = props:get_value(<<"full_desc">>, Props, <<>>),
    FunsDesc = build_search_index_data(funcs, AppMod, Context, props:get_value(<<"functions">>, Props, [])),
    TypesDesc = build_search_index_data(types, AppMod, Context, props:get_value(<<"types">>, Props, [])),
    [kz_json:from_list(
       [{<<"ref">>, <<App/binary, "/", Module/binary, (list_to_binary(Suffix))/binary>>}
       ,{<<"module">>, Module}
       ,{<<"desc">>, extract_xml_texts(Desc, <<"module ", App/binary, "/", Module/binary>>)}
       ])
     | FunsDesc ++ TypesDesc
    ];
build_search_index_data(funcs, AppMod, Context, Props) ->
    [build_search_index_data(func, AppMod, Context, FunProps) || FunProps <- Props];
build_search_index_data(func, {App, Module}, #{file_suffix := Suffix}, Props) ->
    Id = props:get_value(<<"id">>, Props, <<>>),
    Name = props:get_value(<<"name">>, Props, <<>>),
    Desc = props:get_value(<<"full_desc">>, Props, <<>>),
    Ref = <<App/binary, "/", Module/binary, (list_to_binary(Suffix))/binary, "#", Id/binary>>,
    kz_json:from_list(
      [{<<"ref">>, Ref}
      ,{<<"fun">>, Name}
      ,{<<"desc">>, extract_xml_texts(Desc, <<"func ", Ref/binary>>)}
      ]
     );
build_search_index_data(types, AppMod, Context, Props) ->
    [build_search_index_data(type, AppMod, Context, FunProps) || FunProps <- Props];
build_search_index_data(type, {App, Module}, #{file_suffix := Suffix}, Props) ->
    Id = props:get_value(<<"id">>, Props, <<>>),
    Name = props:get_value(<<"name">>, Props, <<>>),
    Desc = props:get_value(<<"full_desc">>, Props, <<>>),
    Ref = <<App/binary, "/", Module/binary, (list_to_binary(Suffix))/binary, "#", Id/binary>>,
    kz_json:from_list(
      [{<<"ref">>, Ref}
      ,{<<"type">>, Name}
      ,{<<"desc">>, extract_xml_texts(Desc, <<"type ", Ref/binary>>)}
      ]
     );
build_search_index_data(app, {_AppCat, App}, #{file_suffix := Suffix}, _) ->
    kz_json:from_list(
      [{<<"ref">>, <<App/binary, "/index", (list_to_binary(Suffix))/binary>>}
      ,{<<"app">>, App}
      ]
     ).

-spec extract_xml_texts(binary() | string(), binary()) -> binary().
extract_xml_texts(<<>>, _) ->
    <<>>;
extract_xml_texts(Bin, Where) when is_binary(Bin) ->
    extract_xml_texts(binary_to_list(Bin), Where);
extract_xml_texts(Text, Where) ->
    try xmerl_scan:string("<doc>" ++ Text ++ "</doc>", [{encoding, 'utf-8'}]) of
        {E0, _} ->
            iolist_to_binary([extract_xml_texts(E1) || E1 <- E0#xmlElement.content])
    catch
        _T:_E ->
            ?DEV_LOG("exception occurred when converting to xml for extracting text for search index~nwhere: ~s~nxmerl exception: ~p:~p~n"
                    ,[Where, _T, _E]),
            <<>>
    end.

-spec extract_xml_texts(kz_types:xml_text() | kz_types:xml_el() | any()) -> iolist().
extract_xml_texts(#xmlText{value = Value}) ->
    [C || C <- Value, C =/= $\n, C =/= $\r];
extract_xml_texts(#xmlElement{content = Content}) ->
    [extract_xml_texts(C) || C <- Content];
extract_xml_texts(Text) when not is_tuple(Text) ->
    unicode:characters_to_list(Text);
extract_xml_texts(_) ->
    [].

%%------------------------------------------------------------------------------
%% @doc Runs search index builder command on given index docs. Required
%% `nodejs' and `lunr.js' to be installed on system.
%%
%% Default index builder command is `scripts/edoc_build_search_index.js'.
%%
%% If node-js can't find globally installed `lunr' package probably it because
%% of `NODE_PATH' is not set. Try to export `node_modules' path before running
%% this. By default `/usr/lib/node_modules' is being used for this. Exported
%% `NODE_PATH' would be prefixed to this.
%% @end
%%------------------------------------------------------------------------------
-spec build_search_index(search_docs() | {ok, string()} | {error, any()}, map()) -> ok.
build_search_index([], _) ->
    ?DEV_LOG("no data to build search index", []);
build_search_index(IndexData, Context) when is_list(IndexData) ->
    build_search_index(save_search_docs(IndexData, Context), Context);
build_search_index({error, Reason}, _) ->
    ?DEV_LOG("failed to save search index doc: ~p", [Reason]);
build_search_index({ok, DocFile}, #{kz_doc_site := OutDir, kz_search_index_cmd := Builder}) ->
    IndexFile = filename:join([OutDir, "js/search_index.js"]),

    ok = filelib:ensure_dir(IndexFile),

    Cmd = Builder ++ " " ++ DocFile ++ " " ++ IndexFile,

    io:format("~n~n:: Building search index with command ~s~n", [Cmd]),

    Options = [exit_status
              ,use_stdio
              ,stderr_to_stdout
              ,{env, [{"NODE_PATH", get_node_path() ++ "/usr/lib/node_modules"}]}
              ],
    Port = erlang:open_port({spawn, Cmd}, Options),
    listen_to_index_builder(Port, []).

-spec get_node_path() -> string().
get_node_path() ->
    case os:getenv("NODE_PATH", "noenv") of
        "noenv" -> "";
        "" -> "";
        Path -> Path ++ ":"
    end.

-spec listen_to_index_builder(port(), string()) -> ok.
listen_to_index_builder(Port, Acc) ->
    receive
        {Port, {data, Msg}} -> listen_to_index_builder(Port, Acc ++ Msg);
        {Port, {exit_status, 0}} ->
            case Acc of
                "created"++_ -> ok;
                _ ->
                    ?DEV_LOG("build index command failed:", []),
                    io:put_chars(Acc)
            end;
        {Port, {exit_status, _Exit}} ->
            ?DEV_LOG("build index command failed with exit_status ~p:", [_Exit]),
            io:put_chars(Acc)
    end.

-spec save_search_docs(search_docs(), map()) -> {ok, string()} | {error, any()}.
save_search_docs(Docs, #{kz_doc_site := OutDir}) ->
    Path = filename:join([OutDir, "tmp", "search-docs"]) ++ ".json",
    case file:write_file(Path, kz_json:encode(Docs)) of
        ok -> {ok, Path};
        {error, _}=Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Compiles all default ErlyDTL templates. Useful if you're testing things in
%% Erlang shell, otherwise it is useless, since templates are compiled to
%% memory and are not saved to file system.
%% @end
%%------------------------------------------------------------------------------
-spec compile_templates() -> ok.
compile_templates() ->
    compile_templates(?DEFAULT_TEMPLATES).

-spec compile_templates([{atom(), string()}]) -> ok.
compile_templates([]) ->
    ok;
compile_templates([{Mod, File}|Keys]) ->
    Mod = compile_template(Mod, File),
    compile_templates(Keys).

-spec compile_template(atom(), string()) -> atom().
compile_template(Module, File) ->
    case kz_template:compile(File, Module, [{auto_escape, false}]) of
        {ok, Module} -> Module;
        {error, Reason} ->
            ?DEV_LOG("failed to compile template ~p (~p): ~p", [Module, File, Reason]),
            error(Reason)
    end.

%%------------------------------------------------------------------------------
%% @doc Copy every files found in template directory to doc-site.
%% All `.html' files are ignored. All `.svg' files which are in-lined by this
%% module will be ignored too.
%% @end
%%------------------------------------------------------------------------------
-spec copy_files(map()) -> ok.
copy_files(#{kz_template_dir := TemplateDir}=Context) ->
    Files = filelib:fold_files(TemplateDir, ".*", true, fun maybe_copy_file/2, []),
    io:format(":: Copying ~b from template directory to doc-site~n", [length(Files)]),
    do_copy_files(Files, Context).

-spec do_copy_files([string()], map()) -> ok.
do_copy_files([], _) ->
    ok;
do_copy_files([File | Files], #{kz_template_dir := TemplateDir, kz_doc_site := OutDir}=Context) ->
    io:format("."),
    RelPath = case File -- TemplateDir of
                  "/"++Rest -> Rest;
                  Rest -> Rest
              end,
    NewPath = filename:join(OutDir, RelPath),
    ok = filelib:ensure_dir(NewPath),
    case file:copy(File, {NewPath, [write]}) of
        {ok, _} -> do_copy_files(Files, Context);
        {error, _Reason}=Error ->
            ?DEV_LOG("failed to copy ~s to ~s: ~p", [File, NewPath, _Reason]),
            exit(Error)
    end.

-spec maybe_copy_file(string(), [string()]) -> [string()].
maybe_copy_file(File, Acc) ->
    case filename:extension(File) of
        ".html" -> Acc;
        ".svg" ->
            Name = filename:basename(File),
            case lists:keyfind(Name, 2, ?INLINE_SVGS) of
                false -> [File | Acc];
                _ -> Acc
            end;
        _ -> [File | Acc]
    end.

-spec is_private(kz_types:xml_el()) -> boolean().
is_private(E) ->
    case kz_xml:get_attrval(private, E) of
        "yes" -> true;
        _ -> false
    end.

-spec is_hidden(kz_types:xml_el()) -> boolean().
is_hidden(E) ->
    case kz_xml:get_attrval(hidden, E) of
        "yes" -> true;
        _ -> false
    end.

%%------------------------------------------------------------------------------
%% @doc Returns a default `Context' to help calling this module functions.
%% @see init_context/1
%% @see init_context/2
%% @end
%%------------------------------------------------------------------------------
-spec default_context() -> map().
default_context() ->
    init_context(default_edoc_options(), []).

-spec default_edoc_options() -> kz_term:proplist().
default_edoc_options() ->
    [{file_suffix, ".html"}
    ,{preprocess, true}
    ,{pretty_printer, erl_pp}
    ,{sort_functions, true}
    ,{todo, true}
    ].

%%------------------------------------------------------------------------------
%% @doc Returns a default `Context' with your given `Options' to help calling
%% this module functions.
%% @end
%%------------------------------------------------------------------------------
-spec init_context(kz_term:proplist()) -> map().
init_context(Opts) ->
    init_context(lists:usort(Opts ++ default_edoc_options()), []).

%%------------------------------------------------------------------------------
%% @doc Merges your given `Options' with default values and {@link gen_apps()}
%% then returns a `Context'.
%% @end
%%------------------------------------------------------------------------------
-spec init_context(kz_term:proplist(), any()) -> map().
init_context(Opts, GenApps) ->
    EDocOpts = [KV
                || {K, _}=KV <- Opts,
                   not is_kazoo_option(atom_to_list(K))
               ],
    Env = edoc_lib:get_doc_env(EDocOpts),
    {AppLink, ModLink} = make_doc_links(GenApps),
    Context0 = maps:from_list(
                 [{edoc_opts, EDocOpts}
                 ,{edoc_env, Env}
                 ,{file_suffix, ".html"}
                 ,{preprocess, true}
                 ,{pretty_printer, erl_pp}
                 ,{sort_functions, true}
                 ,{todo, true}

                  %% kazoo specific options
                 ,{kz_apps_uri, "apps"}
                 ,{kz_base_uri, ""}
                 ,{kz_doc_site, "doc/edoc"}
                 ,{kz_export_type, xmerl_html}
                 ,{kz_ga, undefined}
                 ,{kz_gendate, kz_time:format_date()}
                 ,{kz_search_index_cmd, "scripts/edoc_build_search_index.js"}
                 ,{kz_link_apps, AppLink}
                 ,{kz_link_mods, ModLink}
                 ,{kz_template_dir, ?DEFAULT_TEMPLATE_DIR}
                 ,{kz_vsn, master}
                 ] ++ make_svgs_inline(Opts) ++ ?DEFAULT_TEMPLATES),
    compile_templates(),
    Opts1 = [O
             || {K, _}=O <- Opts,
                K =/= kz_gen_apps,
                K =/= includes
            ],
    maps:merge(Context0, maps:from_list(Opts1)).

-spec is_kazoo_option(string()) -> boolean().
is_kazoo_option("kz_"++_) -> true;
is_kazoo_option(_) -> false.

-spec make_doc_links([gen_app()]) -> {fun((string()) -> string() | undefined), fun((string()) -> string() | undefined)}.
make_doc_links(GenApps) ->
    Mods = maps:from_list(lists:sort([{atom_to_list(M), {atom_to_list(App), AppCat}} || {M, App, AppCat, _} <- GenApps])),
    Apps = maps:from_list(lists:usort([{atom_to_list(App), AppCat} || {_, App, AppCat, _} <- GenApps])),
    Module = fun(M) -> maps:get(M, Mods, undefined) end,
    App = fun(A) -> maps:get(A, Apps, undefined) end,
    {App, Module}.

-spec make_svgs_inline(kz_term:proplist()) -> kz_term:proplist().
make_svgs_inline(Options) ->
    TemplateDir = props:get_value(kz_template_dir, Options, ?DEFAULT_TEMPLATE_DIR),
    [{K, read_svg_file(TemplateDir, File)} || {K, File} <- ?INLINE_SVGS].

-spec read_svg_file(string(), string()) -> binary().
read_svg_file(TemplateDir, File) ->
    Path = filename:join([TemplateDir, "img"]) ++ "/" ++ File,
    case file:read_file(Path) of
        {ok, Svg} -> Svg;
        {error, _Reason} ->
            ?DEV_LOG("failed to read svg file from ~p: ~p", [Path, _Reason]),
            <<>>
    end.
