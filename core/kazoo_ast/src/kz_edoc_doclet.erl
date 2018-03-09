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
    render_apps(Modules, Sidebar, Context),
    render_apps_index(Modules, Sidebar, Context),
    index_file(Sidebar, Context),
    copy_files(Context),

    %% handle postponed error during processing of source files
    case HasError of
        true -> exit(error);
        false -> ok
    end.

%% Processing the individual source files
process_cmds(Context, GenApps) ->
    Private = maps:get(private, Context, false),
    Hidden = maps:get(hidden, Context, false),

    Fun = fun(Obj, Acc) -> source(Context, Obj, Acc, Private, Hidden) end,
    lists:foldl(Fun, {#{}, false}, GenApps).

%% Generating documentation (as a JSON) for a source file, adding its name to the
%% set if it was successful. Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.
source(#{kz_doc_site := OutDir, kz_apps_uri := AppsUri
        ,edoc_env := Env, edoc_opts := EDocOpts
        } = Ctx
      ,{M, App, AppCat, ErlFile}
      ,{Map, HasError}
      ,Private, Hidden
      ) ->
    ?DEV_LOG("processing source file '~ts'", [ErlFile]),
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
                    {maps:put({AppCat, App}, [{Module, ShortDesc} | maps:get({AppCat, App}, Map, [])], Map), HasError};
                false ->
                    {Map, HasError}
            end
    catch
        _:R ->
            ?DEV_LOG("skipping source file '~ts': ~P.", [ErlFile, R, 15]),
            {Map, true}
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

sidebar_apps_list(Modules, #{kz_apps_uri := AppsUri, file_suffix := Suffix}) ->
    Side = maps:fold(fun({_AppCat, App}, Mods, Acc) ->
                             [{App, lists:sort([{Module, Desc, AppsUri ++ "/" ++ atom_to_list(App) ++ "/" ++ atom_to_list(Module) ++ Suffix}
                                               || {Module, Desc} <- Mods]
                                               )
                              }
                              | Acc
                             ]
                     end
                    ,[]
                    ,Modules),
    lists:sort(Side).

render_apps(Modules, Sidebar, Context) ->
    _ = maps:map(fun(App, Ms) -> render_app(App, Ms, Sidebar, Context) end, Modules),
    ok.

render_app({AppCat, App}, Modules, Sidebar, Context) ->
    render_app_overview(AppCat, App, Modules, Sidebar, Context),
    [render_module(AppCat, App, Ms, Sidebar, Context) || Ms <- Modules].

render_app_overview(AppCat, App, Modules, Sidebar, #{kz_doc_site := OutDir , kz_apps_uri := AppsUri}=Ctx) ->
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
    edoc_lib:write_file(Rendered, filename:join([OutDir, AppsUri, App]), Name, EncOpts).

render_apps_index(Modules, Sidebar, #{kz_doc_site := OutDir, kz_apps_uri := AppsUri}=Ctx) ->
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
    kz_template:render(Module, Props);
render(TemplateFilePath, Props) when is_list(TemplateFilePath) ->
    kz_template:render(TemplateFilePath, Props, [{auto_escape, false}]).

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

-spec compile_templates() -> ok.
compile_templates() ->
    compile_templates(?DEFAULT_TEMPLATES).

-spec compile_templates([{string(), string()}]) -> ok.
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
    do_copy_files(Files, Context).

do_copy_files([], _) ->
    ok;
do_copy_files([File | Files], #{kz_template_dir := TemplateDir, kz_doc_site := OutDir}=Context) ->
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
                 ,{kz_link_apps, AppLink}
                 ,{kz_link_mods, ModLink}
                 ,{kz_template_dir, "doc/edoc-template"}
                 ,{kz_vsn, master}
                 ] ++ ?DEFAULT_TEMPLATES),
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
