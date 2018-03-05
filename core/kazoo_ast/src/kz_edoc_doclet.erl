%%%=============================================================================
%%% @copyright 2003-2006 Richard Carlsson
%%% @copyright (C) 2018-, 2600Hz
%%% The origin of this file is the edoc module `edoc_doclet.erl'
%%% written by Richard Carlsson.
%%%
%%% This library is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
%%% USA
%%%
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

-export([run/2]).
-export([init_context/2
        ,doclet_apps_gen/3
        ,get_modules_app_path/3

        ,compile_templates/0
        ]).

-include_lib("xmerl/include/xmerl.hrl").

-import(edoc_report, [report/2, warning/2]).

-define(NO_APP, []).

-define(DEFAULT_FILE_SUFFIX, ".html").

-define(DEFAULT_APPS_OUT_DIR, "apps").

-define(APP_OVERVIEW_FILE, "overview.edoc").
-define(APPS_OVERVIEW_FILE, "apps_overview.edoc").
-define(PROJ_OVERVIEW_FILE, "doc/edoc_overview.edoc").

-define(INDEX_FILE, "index.html").
-define(APP_SUMMARY_FILE, "index.html").

-record(context, {dir = "" :: string()
                 ,env :: edoc_lib:edoc_env()
                 ,opts = [] :: [term()]
                 }).
%% Context for doclets

-record(doclet_apps_gen, {sources = [] :: [{atom(), string(), string()}]
                         ,app = ?NO_APP :: no_app() | atom()
                         ,modules = [] :: [atom()]
                         ,dir :: string()
                         }).

-record(doclet_gen, {sources = [] :: [{atom(), string(), string()}]
                    ,app = ?NO_APP :: no_app() | atom()
                    ,modules = [] :: [atom()]
                    }).

-record(doclet_toc, {paths :: [string()]
                    ,indir :: string()
                    }).

-type no_app() :: list().
%% A value used to mark absence of an Erlang application
%% context. Use the macro `NO_APP' defined in {@link kz_edoc_doclet.erl}
%% to produce this value.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init_context(edoc_lib:edoc_env(), list()) -> #context{}.
init_context(Env, Opts) ->
    #context{dir = proplists:get_value(dir, Opts)
            ,opts = Opts
            ,env = Env
            }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec doclet_apps_gen(atom(), string(), [string()]) -> #doclet_apps_gen{}.
doclet_apps_gen(Atom, Dir, Erls) ->
    Sources = [{list_to_atom(filename:basename(Erl, ".erl")), filename:basename(Erl), filename:dirname(Dir)}
               || Erl <- Erls
              ],
    #doclet_apps_gen{sources = Sources
                    ,app = Atom
                    ,dir = Dir
                    ,modules = [list_to_atom(filename:basename(Erl, ".erl")) || Erl <- Erls]
                    }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_modules_app_path({atom(), string()}, #doclet_apps_gen{}, {atom(), string()}) -> [{atom(), string()}].
get_modules_app_path({_, Dir}, #doclet_apps_gen{modules = Modules}, Acc) ->
    [{Module, Dir} || Module <- Modules] ++ Acc.

%% Sources is the list of inputs in the order they were found.
%% Modules are sorted lists of atoms without duplicates. (They
%% usually include the data from the edoc-info file in the target
%% directory, if it exists.)

%%------------------------------------------------------------------------------
%% %% @spec (Command::doclet_gen() | doclet_toc(), edoc_context()) -> ok
%% @doc Main doclet entry point. See the file <a
%% href="../include/edoc_doclet.hrl">`edoc_doclet.hrl'</a> for the data
%% structures used for passing parameters.
%%
%% Also see {@link edoc:layout/2} for layout-related options, and
%% {@link edoc:get_doc/2} for options related to reading source
%% files.
%%
%% Options:
%% <dl>
%%  <dt>{@type {file_suffix, string()@}}
%%  </dt>
%%  <dd>Specifies the suffix used for output files. The default value is
%%      `".html"'.
%%  </dd>
%%  <dt>{@type {hidden, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of hidden modules and
%%      functions will also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {overview, edoc:filename()@}}
%%  </dt>
%%  <dd>Specifies the name of the overview-file. By default, this doclet
%%      looks for a file `"overview.edoc"' in the target directory.
%%  </dd>
%%  <dt>{@type {private, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of private modules and
%%      functions will also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {stylesheet, string()@}}
%%  </dt>
%%  <dd>Specifies the URI used for referencing the stylesheet. The
%%      default value is `"stylesheet.css"'. If an empty string is
%%      specified, no stylesheet reference will be generated.
%%  </dd>
%%  <dt>{@type {stylesheet_file, edoc:filename()@}}
%%  </dt>
%%  <dd>Specifies the name of the stylesheet file. By default, this
%%      doclet uses the file `"stylesheet.css"' in the `priv'
%%      subdirectory of the EDoc installation directory. The named file
%%      will be copied to the target directory.
%%  </dd>
%%  <dt>{@type {title, string()@}}
%%  </dt>
%%  <dd>Specifies the title of the overview-page.
%%  </dd>
%% </dl>
%%------------------------------------------------------------------------------
-spec run(#doclet_gen{} | [#doclet_gen{}] | #doclet_toc{}, any()) -> any().
run(#doclet_gen{}=_Cmd, _Context) ->
    %% gen(Cmd#doclet_gen.sources,
    %% Cmd#doclet_gen.app,
    %% Cmd#doclet_gen.modules,
    %% Context);
    io:format("doclet_gen not implemented.~n"),
    exit(1);
run(#doclet_toc{}=_Cmd, _Context) ->
    %% toc(Cmd#doclet_toc.paths, Context);
    io:format("doclet_toc not implemented.~n"),
    exit(1);
run([], #context{}) ->
    io:format("no modules were found.~n");
run(Cmds, #context{opts = Options}=Context) when is_list(Cmds) ->
    gen_multi_app(Cmds, Context#context{opts = compile_templates(Options)}).

gen_multi_app(Cmds, Context) ->
    {Modules, HasError} = process_cmds(Cmds, Context, {#{}, false}),
    Sidebar = sidebar(Modules, Context),
    render_apps(Modules, Sidebar, Context),
    render_apps_index(Modules, Sidebar, Context),
    index_file(Sidebar, Context),
    copy_files(Context),

    %% handle postponed error during processing of source files
    case HasError of
        true -> exit(error);
        false -> ok
    end.

%% Processing the individual source files for each applications.
process_cmds([], _, Acc) ->
    Acc;
process_cmds([#doclet_apps_gen{sources = Sources}=Cmd|Cmds]
            ,#context{opts = Options} = Context
            ,{Map, HasError}) ->
    Private = proplists:get_bool(private, Options),
    Hidden = proplists:get_bool(hidden, Options),

    {NewMap, E} = lists:foldl(fun(Src, Acc) ->
                                     source(Context, Cmd, Src, Acc, Private, Hidden)
                              end
                             ,{Map, HasError}
                             ,Sources
                             ),
    process_cmds(Cmds, Context, {NewMap, E}).


%% Generating documentation for a source file, adding its name to the
%% set if it was successful. Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.

source(#context{dir = OutDir, env = Env, opts = Options}
      ,#doclet_apps_gen{app = App, dir = AppDir}
      ,{M, Name, Path}
      ,{Map, HasError}
      ,Private, Hidden
      ) ->
    AppsOutDir = proplists:get_value(apps_out_dir, Options, ?DEFAULT_APPS_OUT_DIR),
    File = filename:join(Path, Name),
    try edoc:get_doc(File, Env, Options) of
        {Module, Doc} ->
            check_name(Module, M, File),
            case (not is_private(Doc)
                  orelse Private)
                andalso (not is_hidden(Doc)
                         orelse Hidden)
            of
                true ->
                    Props = edoc:layout(Doc, Options),
                    JObj = kz_json:from_list_recursive(),
                    Name1 = atom_to_list(M) ++ ".json",
                    Encoding = [{encoding, encoding(Doc)}],
                    edoc_lib:write_file(kz_json:encode(JObj), filename:join([OutDir, "tmp", AppsOutDir, App]), Name1, Encoding),
                    ShortDesc = proplists:get_value(short_desc, Props, []),
                    {maps:put({App, AppDir}, [{Module, ShortDesc} | maps:get(App, Map, [])]), HasError};
                false ->
                    {Map, HasError}
            end
    catch
        _:R ->
            report("skipping source file '~ts': ~P.", [File, R, 15]),
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
    warning("file '~ts' actually contains module '~s'.", [File, Module]).

sidebar(Modules, #context{dir = OutDir, opts = Options}) ->
    AppsOutDir = proplists:get_value(apps_out_dir, Options, ?DEFAULT_APPS_OUT_DIR),
    Suffix = proplists:get_value(file_suffix, Options, ?DEFAULT_FILE_SUFFIX),

    Side = maps:fold(fun({App, _}, Mods, Acc) ->
                            [{App, [{Module, filename:join([OutDir, AppsOutDir, App, Module]) ++ Suffix} || {Module, _} <- Mods]}
                             | Acc
                            ]
                     end
                    ,[]
                    ,Modules),
    lists:usort(Side).

render_apps(Modules, Sidebar, Context) ->
    _ = maps:map(fun(App, Ms) -> render_app(App, Ms, Sidebar, Context) end, Modules),
    ok.

render_app({App, AppDir}, Modules, Sidebar, #context{opts = Options}=Context) ->
    render_app_overview(App, AppDir, Modules, Sidebar, Context),
    Suffix = proplists:get_value(file_suffix, Options, ?DEFAULT_FILE_SUFFIX),
    [render_modules(App, Ms, Sidebar, Suffix, Context) || Ms <- Modules].

render_app_overview(App, AppDir, Modules, Sidebar, #context{dir = OutDir, opts = Options}=Context) ->
    File = proplists:get_value(overview, Options, filename:join([AppDir, "doc", ?APP_OVERVIEW_FILE])),
    Data = get_overview_data(File, "Application: " ++ atom_to_list(App), Context),
    F = fun (M) ->
                M:overview(Data, Options)
        end,
    Props = edoc_lib:run_layout(F, Options),
    Rendered = render([{sidebar_apps, Sidebar}
                      ,{application, App}
                      ,{modules, lists:usort(Modules)}
                       | proplists:delete(application, Props)
                      ], Options, proplists:get_value(app_template, Options)),

    AppsOutDir = proplists:get_value(apps_out_dir, Options, ?DEFAULT_APPS_OUT_DIR),
    EncOpts = [{encoding, utf8}],
    edoc_lib:write_file(Rendered, filename:join([OutDir, AppsOutDir, App]), ?INDEX_FILE, EncOpts).

render_modules(App, Module, Sidebar, Suffix, #context{dir = OutDir, opts = Options}) ->
    AppsOutDir = proplists:get_value(apps_out_dir, Options, ?DEFAULT_APPS_OUT_DIR),
    Props = read_tmp_file(OutDir, AppsOutDir, App, Module),

    Rendered = render([{sidebar_apps, Sidebar}
                      ,{application, App}
                       | proplists:delete(application, Props)
                      ], Options, proplists:get_value(mod_template, Options)),

    Name = atom_to_list(Module) ++ Suffix,
    EncOpts = [{encoding, utf8}],
    edoc_lib:write_file(Rendered, filename:join([OutDir, AppsOutDir, App]), Name, EncOpts).

render_apps_index(Modules, Sidebar, #context{dir = OutDir, opts = Options}=Context) ->
    Apps = [App || {App, _} <- maps:keys(Modules)],
    Data = get_overview_data(?APPS_OVERVIEW_FILE, "Kazoo Applications Index", Context),
    F = fun (M) ->
                M:overview(Data, Options)
        end,
    Props = edoc_lib:run_layout(F, Options),
    Rendered = render([{sidebar_apps, Sidebar}
                      ,{apps, lists:usort(Apps)}
                       | Props
                      ], Options, proplists:get_value(app_template, Options)),

    EncOpts = [{encoding, utf8}],
    edoc_lib:write_file(Rendered, OutDir, ?INDEX_FILE, EncOpts).

%% Creating an index file.
index_file(Sidebar, #context{dir = OutDir, opts = Options}=Context) ->
    Data = get_overview_data(?PROJ_OVERVIEW_FILE, "Kazoo Erlang Reference", Context),
    F = fun (M) ->
                M:overview(Data, Options)
        end,
    Props = edoc_lib:run_layout(F, Options),
    Rendered = render([{sidebar_apps, Sidebar}
                       | Props
                      ], Options, proplists:get_value(app_template, Options)),

    EncOpts = [{encoding, utf8}],
    edoc_lib:write_file(Rendered, OutDir, ?INDEX_FILE, EncOpts).

render(Props0, Options, Template) ->
    Name = proplists:get_value(name, Props0),
    Props = [{sidebar_links, proplists:get_value(sidebar_links, Options, [])} | Props0],
    %% io:format("~n Props ~p~n~n", [Props]),
    case kz_template:render(Template, Props) of
        {ok, Rendered} ->
            Rendered;
        {error, _Reason} ->
            io:format("~nfailed to render ~s: ~p~n", [Name, _Reason]),
            exit(error)
    end.

-spec compile_templates() -> ok.
compile_templates() ->
    compile_templates([]).

-spec compile_templates(proplists:proplist()) -> proplists:proplist().
compile_templates(Options) ->
    Keys = [{"mod_template", "module.html"}
           ,{"app_template", "app_overview.html"}
           ,{"apps_template", "apps_index.html"}
           ,{"index_template", "index.html"}
           ],
    compile_templates(Keys, Options).

-spec compile_templates([atom()], proplists:proplist()) -> proplists:proplist().
compile_templates([], Options) ->
    Options;
compile_templates([{Key, DefaultFile}|Keys], Options) ->
    KeyAtom = list_to_atom(Key),
    Mod = proplists:get_value(KeyAtom, Options, list_to_atom("kz_edoc_" ++ Key)),
    File = proplists:get_value(list_to_atom(Key ++ "_file"), Options, DefaultFile),
    compile_templates(Keys, proplists:delete(KeyAtom, Options) ++ [compile_template(Mod, File)]).

-spec compile_template(atom(), string()) -> atom().
compile_template(Module, File) ->
    case kz_template:compile(File, Module, [{auto_escape, false}]) of
        {ok, _} -> Module;
        {error, Reason} ->
            io:format("~nfailed to compile template ~p (~p): ~p~n", [Module, File, Reason]),
            error(Reason)
    end.

copy_files(#context{opts = Options, dir = OutDir}) ->
    Base = proplists:get_value(template_dir, Options, "doc/edoc-template"),
    Paths = ["css"
            ,"img"
            ,"js"
            ,"404.html"
            ,"favicon.ico"
            ,"icon.png"
            ,"robot.txt"
            ,"site.webmanifest"
            ,"tile-wide.png"
            ,"tile.png"
            ],
    _ = [edoc_lib:copy_file(filename:join(Base, P), filename:join(OutDir, P)) || P <- Paths],
    ok.

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

encoding(E) ->
    case kz_edoc_layout:get_attrval(encoding, E) of
        "latin1" -> latin1;
        _ -> utf8
    end.

%% Read external source file. Fails quietly, returning empty tag list.
read_file(File, Context, Env, Opts) ->
    case edoc_extract:file(File, Context, Env, Opts) of
        {ok, Tags} ->
            Tags;
        {error, _} ->
            []
    end.

read_tmp_file(OutDir, AppsOutDir, App, Module) ->
    Path = filename:join([OutDir, "tmp", AppsOutDir, App, Module]) ++ ".json",
    case file:read_file(Path) of
        {ok, Bin} ->
            kz_json:recursive_to_proplist(kz_json:decode(Bin));
        {error, Reason} ->
            io:format("~ncan not read temporary file ~p: ~p~n", [Path, Reason]),
            exit(error)
    end.

get_overview_data(File, Title, #context{env = Env, opts = Options}) ->
    Tags = read_file(File, overview, Env, Options),
    Data0 = edoc_data:overview(Title, Tags, Env, Options),
    EncodingAttribute = #xmlAttribute{name = encoding
                                     ,value = "utf8"
                                     },
    #xmlElement{attributes = As} = Data0,
    Data0#xmlElement{attributes = [EncodingAttribute | As]}.
