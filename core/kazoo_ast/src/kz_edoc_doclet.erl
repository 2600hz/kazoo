%%%=============================================================================
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
%%%=============================================================================
-module(kz_edoc_doclet).

%% Note that this is written so that it is *not* depending on edoc.hrl!

-export([run/1]).

-export([compile_templates/0
        ]).

-include_lib("xmerl/include/xmerl.hrl").

-define(DEV_LOG(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).

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

%%------------------------------------------------------------------------------
%% @doc Main Kazoo doclet entry point.
%%
%% We only care about kazoo documentation, so not all options/features are
%% implemented.
%%
%% Someone please document extra Kazoo options.
%% @end
%%------------------------------------------------------------------------------
-spec run(proplists:proplist()) -> ok.
run(Options) ->
    run(Options, proplists:get_value(kz_gen_apps, Options)).

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

%% Processing the individual source files
process_cmds(Context, GenApps) ->
    Private = maps:get(private, Context, false),
    Hidden = maps:get(hidden, Context, false),

    GenAppsLength = length(GenApps),

    SourceFun = fun(Obj, Acc) -> source(Context, Obj, Acc, Private, Hidden) end,

    io:format(":: Start processing ~b source file(s)~n", [GenAppsLength]),

    Malt = [{'processes', 'schedulers'}],
    Result = plists:fold(SourceFun, fun fuse_together/2, [], GenApps, Malt),

    Fun = fun({AppCat, App, Module, ShortDesc}, Map) ->
                  maps:put({AppCat, App}, [{Module, ShortDesc} | maps:get({AppCat, App}, Map, [])], Map)
          end,

    io:format("~n:: Successfully processed: ~b~n~n", [length(Result)]),
    {lists:foldl(Fun, #{}, Result), GenAppsLength /= length(Result)}.

fuse_together(A, B) when is_list(A), is_list(B) ->
    A ++ B.

%% Generating documentation (as a JSON) for a source file, adding its name to the
%% set if it was successful. Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.
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

                    ShortDesc = proplists:get_value(short_desc, Props, []),
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

render_apps(Modules, Sidebar, Context) ->
    io:format(":: Start rendering~n", []),

    Malt = [{'processes', 'schedulers'}],
    plists:fold(fun({App, Ms}, Acc) -> render_app(App, Ms, Sidebar, Context) ++ Acc end
               ,fun fuse_together/2
               ,[]
               ,maps:to_list(Modules)
               ,Malt
               ).

render_app({AppCat, App}, Modules, Sidebar, Context) ->
    render_app_overview(AppCat, App, Modules, Sidebar, Context),
    [build_search_index_data(app, {list_to_binary(AppCat), atom_to_binary(App, utf8)}, Context, [])
     | lists:flatten([render_module(AppCat, App, Ms, Sidebar, Context) || Ms <- Modules])
    ].

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

%% Creating an index file.
index_file(Sidebar, #{kz_doc_site := OutDir}=Ctx) ->
    io:format(":: Rendering index file~n~n"),
    Context = Ctx#{kz_rel_path => make_rel_path([])},
    Props = get_overview_data(?PROJ_OVERVIEW_FILE, "Kazoo Erlang Reference", Context),
    Rendered = render([{kz_sidebar_apps, Sidebar}
                       | Props
                      ], Context, kz_edoc_index_template),

    EncOpts = [{encoding, utf8}],
    edoc_lib:write_file(Rendered, OutDir, ?INDEX_FILE, EncOpts).

run_layout(module, File, Doc, Context) ->
    try kz_edoc_layout:module(Doc, Context)
    catch
        _E:_T ->
            ?DEV_LOG("failed to layout ~s: ~p:~p", [File, _E, _T]),
            log_stacktrace(),
            exit(error)
    end;
run_layout(overview, File, Doc, Context) ->
    try kz_edoc_layout:overview(Doc, Context)
    catch
        _E:_T ->
            ?DEV_LOG("failed to layout overview file ~s: ~p:~p", [File, _E, _T]),
            log_stacktrace(),
            exit(error)
    end.

render(Props0, Context, Template) ->
    Name = proplists:get_value(name, Props0),
    Props = Props0 ++ maps:to_list(Context),
    %% ?DEV_LOG("Props ~p", [Props]),
    case render(Template, Props) of
        {ok, Rendered} ->
            Rendered;
        {error, _Reason} ->
            ?DEV_LOG("failed to render ~s: ~p", [Name, _Reason]),
            exit(error)
    end.

make_rel_path([]) -> "";
make_rel_path(Ps) ->
    ["../" || _ <- lists:seq(1, length(Ps))].

render(Module, Props) when is_atom(Module) ->
    kz_template:render(Module, Props).
%%render(TemplateFilePath, Props) when is_list(TemplateFilePath) ->
%%    kz_template:render(TemplateFilePath, Props, [{auto_escape, false}]).

read_tmp_file(#{kz_doc_site := OutDir, kz_apps_uri := AppsUri}, App, Module) ->
    Path = filename:join([OutDir, "tmp", AppsUri, App, Module]) ++ ".json",
    case file:read_file(Path) of
        {ok, Bin} ->
            kz_json:recursive_to_proplist(kz_json:decode(Bin));
        {error, Reason} ->
            ?DEV_LOG("can not read temporary file ~p: ~p", [Path, Reason]),
            exit(error)
    end.

get_overview_data(File, Title, #{edoc_env := Env, edoc_opts := Options}=Context) ->
    Tags = extract_overview(File, Env, Options),
    Data0 = edoc_data:overview(Title, Tags, Env, Options),
    EncodingAttribute = #xmlAttribute{name = encoding
                                     ,value = "utf8"
                                     },
    #xmlElement{attributes = As} = Data0,

    run_layout(overview, File, Data0#xmlElement{attributes = [EncodingAttribute | As]}, Context).

%% Read external source file. Fails quietly, returning empty tag list.
extract_overview(File, Env, Opts) ->
    case edoc_extract:file(File, overview, Env, Opts) of
        {ok, Tags} ->
            Tags;
        {error, _} ->
            []
    end.

build_search_index_data(module, {App, Module}=AppMod, #{file_suffix := Suffix}=Context, Props) ->
    Desc = proplists:get_value(<<"full_desc">>, Props, <<>>),
    FunsDesc = build_search_index_data(funcs, AppMod, Context, proplists:get_value(<<"functions">>, Props, [])),
    TypesDesc = build_search_index_data(types, AppMod, Context, proplists:get_value(<<"types">>, Props, [])),
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
    Id = proplists:get_value(<<"id">>, Props, <<>>),
    Name = proplists:get_value(<<"name">>, Props, <<>>),
    Desc = proplists:get_value(<<"full_desc">>, Props, <<>>),
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
    Id = proplists:get_value(<<"id">>, Props, <<>>),
    Name = proplists:get_value(<<"name">>, Props, <<>>),
    Desc = proplists:get_value(<<"full_desc">>, Props, <<>>),
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

extract_xml_texts(<<>>, _) ->
    <<>>;
extract_xml_texts(Bin, Where) when is_binary(Bin) ->
    extract_xml_texts(unicode:characters_to_list(Bin), Where);
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

extract_xml_texts(#xmlText{value = Value}) ->
    [C || C <- Value, C =/= $\n, C =/= $\r];
extract_xml_texts(#xmlElement{content = Content}) ->
    [extract_xml_texts(C) || C <- Content];
extract_xml_texts(Text) when not is_tuple(Text) ->
    unicode:characters_to_list(Text);
extract_xml_texts(_) ->
    [].

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
              ,{env, [{"NODE_PATH", "$NODE_PATH:/usr/lib/node_modules"}]}
              ],
    Port = erlang:open_port({spawn, Cmd}, Options),
    listen_to_index_builder(Port, []).

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

save_search_docs(Docs, #{kz_doc_site := OutDir}) ->
    Path = filename:join([OutDir, "tmp", "search-docs"]) ++ ".json",
    case file:write_file(Path, kz_json:encode(Docs)) of
        ok -> {ok, Path};
        {error, _}=Error -> Error
    end.

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

copy_files(#{kz_template_dir := TemplateDir}=Context) ->
    Files = filelib:fold_files(TemplateDir, ".*", true, fun maybe_copy_file/2, []),
    io:format(":: Copying ~b from template directory to doc-site~n", [length(Files)]),
    do_copy_files(Files, Context).

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

is_private(E) ->
    case kz_edoc_layout:get_attrval(private, E) of
        "yes" -> true;
        _ -> false
    end.

is_hidden(E) ->
    case kz_edoc_layout:get_attrval(hidden, E) of
        "yes" -> true;
        _ -> false
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init_context(proplists:proplist(), any()) -> map().
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

is_kazoo_option("kz_"++_) -> true;
is_kazoo_option(_) -> false.

make_doc_links(GenApps) ->
    Mods = maps:from_list(lists:sort([{atom_to_list(M), {atom_to_list(App), AppCat}} || {M, App, AppCat, _} <- GenApps])),
    Apps = maps:from_list(lists:usort([{atom_to_list(App), AppCat} || {_, App, AppCat, _} <- GenApps])),
    Module = fun(M) -> maps:get(M, Mods, undefined) end,
    App = fun(A) -> maps:get(A, Apps, undefined) end,
    {App, Module}.

make_svgs_inline(Options) ->
    TemplateDir = proplists:get_value(kz_template_dir, Options, ?DEFAULT_TEMPLATE_DIR),
    [{K, read_svg_file(TemplateDir, File)} || {K, File} <- ?INLINE_SVGS].

read_svg_file(TemplateDir, File) ->
    Path = filename:join([TemplateDir, "img"]) ++ "/" ++ File,
    case file:read_file(Path) of
        {ok, Svg} -> Svg;
        {error, _Reason} ->
            ?DEV_LOG("failed to read svg file from ~p: ~p", [Path, _Reason]),
            ""
    end.

-spec log_stacktrace() -> 'ok'.
log_stacktrace() ->
    ST = erlang:get_stacktrace(),
    log_stacktrace(ST).

-spec log_stacktrace(list()) -> ok.
log_stacktrace(ST) ->
    log_stacktrace(ST, "", []).

log_stacktrace(ST, Fmt, Args) ->
    ?DEV_LOG("stacktrace: " ++ Fmt, Args),
    _ = [log_stacktrace_mfa(M, F, A, Info)
         || {M, F, A, Info} <- ST
        ],
    'ok'.

log_stacktrace_mfa(M, F, Arity, Info) when is_integer(Arity) ->
    ?DEV_LOG("st: ~s:~s/~b at (~b)", [M, F, Arity, props:get_value('line', Info, 0)]);
log_stacktrace_mfa(M, F, Args, Info) ->
    ?DEV_LOG("st: ~s:~s at ~p", [M, F, props:get_value('line', Info, 0)]),
    lists:foreach(fun (Arg) -> ?DEV_LOG("args: ~p", [Arg]) end, Args).
