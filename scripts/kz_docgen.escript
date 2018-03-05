#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').
-compile(nowarn_unused_function).
-compile(nowarn_unused_vars).

-export([main/1]).

-define(CONFIG, #{dirs => []
                 ,out_dir => "doc/edoc"
                 ,apps_out_dir => "apps"
                 ,template_dir => "doc/edoc-template"
                 ,mod_template => kz_edoc_mod_template
                 ,mod_template_file => "doc/edoc-template/module.html"
                 ,app_template => kz_edoc_app_template
                 ,app_template_file => "doc/edoc-template/app_overview.html"
                 ,apps_template => kz_edoc_apps_template
                 ,apps_template_file => "doc/edoc-template/apps_index.html"
                 ,index_template => kz_edoc_index_template
                 ,index_template_file => "doc/edoc-template/index.html"
                 ,base => ""
                 }).

-record(env, {module = []
             ,root = ""
             ,file_suffix
             ,apps
             ,modules
             ,app_default
             ,macros = []
             ,includes = []
         }).
%% EDoc internal environment. Should not be here, but we need it to to make links relative.

-spec main(_) -> boolean().
main(Args) ->
    _ = io:setopts(user, [{encoding, unicode}]),
    ScriptsDir = filename:dirname(escript:script_name()),
    ok = file:set_cwd(filename:absname(ScriptsDir ++ "/..")),
    io:format("cwd: ~p~n", [file:get_cwd()]),

    run(parse_args(Args, ?CONFIG)).

%% This is copy pasta from `edoc:run/2' and modified to get
%% a relative path to source modules, so links are relative.
%%
%% E.g. instead of `/home/user/kazoo/core/kazoo_amqp/src/api/kapi_rate.erl'
%% we have `kazoo_amqp/src/api/kapi_rate.erl' for linkage.
%%
%% Generally this is considered dangerous, but there is no other way to make Edoc
%% works properly with multi-application situation (except you are willingly happy
%% to mess around with application dependency)
%%
%% Side effect: ALL kazoo project app/module/function/type links are relatives, even
%% if links to the same document.
run(Opts0) ->
    EDocOpts = edoc_options(Opts0),
    Sources = sources(Opts0),

    Env = get_doc_env(Sources, EDocOpts),
    Ctxt1 = kz_edoc_doclet:init_context(Env, EDocOpts),

    Cmd = maps:values(Sources),
    F = fun(M) -> M:run(Cmd, Ctxt1) end,

    edoc_lib:run_doclet(F, EDocOpts).

%%% Internals

include_paths() ->
    lists:usort(["core"]
        ++ ["applications/tasks"]
        ++ [filename:dirname(Path) || Path <- filelib:wildcard("core/*/{src,include}/**/*.hrl")]
        ++ ["applications"]
        ++ [filename:dirname(Path) || Path <- filelib:wildcard("applications/*/{src,include}/**/*.hrl")]
        ++ ["deps"]).

edoc_options(Opts) ->
    [{dir, maps:get(out_dir, Opts)}
    ,{includes, include_paths()}
    ,{applications, "Kazoo"}
    ,{preprocess, true}
    ,{sort_functions, true}
    ,{pretty_printer, erl_pp}
    ,{todo, true}
    ,{layout, kz_edoc_layout}
    ,{doclet, kz_edoc_doclet}
    ,{sidebar_links, sidebar_links()}
     | maps:to_list(maps:remove(apps, Opts))
    ].

%% [{id, label, href}]
sidebar_links() ->
    [].

sources(#{apps := AppDirs}) ->
    maps:from_list([expand_sources(App, Dir) || {App, Dir} <- AppDirs]).

expand_sources(App, Dir) ->
    Erls = lists:usort(filelib:fold_files(filename:join(Dir, "src"), "\\.erl$", true, fun(H, T) -> [H|T] end, [])),
    Atom = list_to_atom(App),
    {{App, Dir}, kz_edoc_doclet:doclet_apps_gen(Atom, Dir, Erls)}.

%% something like `edoc_lib:get_doc_env/3'
get_doc_env(Sources, EDocOpts) ->
    Includes = proplists:append_values(includes, EDocOpts),
    {A, M} = get_doc_links(Sources),

    #env{file_suffix = ".html"
        ,apps = A
        ,modules = M
        ,app_default = "http://www.erlang.org/edoc/doc"
        ,includes = Includes
    }.

%% something like `edoc_lib:get_doc_links/3'
get_doc_links(Sources) ->
    A = maps:from_list(maps:keys(Sources)),
    M = maps:from_list(lists:usort(maps:fold(fun kz_edoc_doclet:get_modules_app_path/3, [], Sources))),

    Fun = fun(D) -> fun(K) -> maps:get(K, D, "") end end,
    {Fun(A), Fun(M)}.

parse_args([], #{dirs := []}=Config) ->
    C1 = Config#{apps => [{filename:basename(D), D}
                          || D <- filelib:wildcard("{core,applications}/*"),
                                  filelib:is_dir(D)
                         ]
                },
    maps:remove(dirs, C1);

parse_args(["--out-dir", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{out_dir => DocSite});

parse_args(["--doc-apps-dir", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{apps_out_dir => DocSite});

parse_args(["--template-dir", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{template_dir => DocSite});

parse_args(["--mod-template", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{mod_template => list_to_atom(DocSite)});
parse_args(["--mod-template-file", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{mod_template_file => DocSite});

parse_args(["--app-template", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{app_template => list_to_atom(DocSite)});
parse_args(["--app-template-file", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{app_template_file => DocSite});

parse_args(["--apps-template", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{apps_template => list_to_atom(DocSite)});
parse_args(["--apps-template-file", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{apps_template_file => DocSite});

parse_args(["--index-template", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{index_template => list_to_atom(DocSite)});
parse_args(["--index-template-file", DocSite | Rest], Config) ->
    parse_args(Rest, Config#{index_template_file => DocSite});

parse_args(["--base", Base | Rest], Config) ->
    parse_args(Rest, Config#{base => Base});

parse_args(["--ga", GA | Rest], Config) ->
    parse_args(Rest, Config#{ga => GA});

parse_args([_ | Rest], Config) ->
    parse_args(Rest, Config).
%% parse_args([Dir | Rest], #{dirs := Dirs}=Config) ->
%%     parse_args(Rest, Config#{dirs => [filename:absname(Dir) | Dirs]}).

usage() ->
    ok = io:setopts([{encoding, unicode}]),
    io:format("Usage: \n\t~s  [--out-dir <output dir>]  <source path>+\n", [filename:basename(escript:script_name())]),
    halt(1).
