#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode(compile).
-compile(nowarn_unused_function).
-compile(nowarn_unused_record).
-compile(nowarn_unused_vars).

-export([main/1]).

-spec main(_) -> boolean().
main(Args) ->
    _ = io:setopts(user, [{encoding, unicode}]),
    ScriptsDir = filename:dirname(escript:script_name()),
    ok = file:set_cwd(filename:absname(ScriptsDir ++ "/..")),

    %% io:format("~n~p~n", [doclet_options(parse_args(Args))]).
    kz_edoc_doclet:run(doclet_options(parse_args(Args))).

doclet_options(Opts) ->
    [{file_suffix, ".html"}
    ,{includes, include_paths()}
    ,{preprocess, true}
    ,{pretty_printer, erl_pp}
    ,{sort_functions, true}
    ,{todo, true}

     %% kazoo specific options
     | proplists:delete(all_apps, Opts)
    ].

include_paths() ->
    lists:usort(["core"]
                ++ ["applications"]
                ++ ["applications/tasks"]
                ++ [filename:dirname(Path) || Path <- filelib:wildcard("{applications,core}/*/{src,include}/**/*.hrl")]
                ++ ["deps"]).

option_spec_list() ->
    [{help, $?, "help", undefined, "Show the program options"}

    ,{all_apps, $a, "all-apps", {boolean, false}, "Produce documentation for all applications"}

    ,{kz_doc_site, $o, "doc-site", {string, "doc/edoc"}, "Path to directory for produced html files and all images, css, ..."}
    ,{kz_template_dir, $t, "template-dir", {string, "doc/edoc-template"}, "Path to directory where templates are"}

    ,{kz_base_uri, undefined, "base-uri", {string, ""}, "Base URI for documentation site"}
    ,{kz_apps_uri, undefined, "apps-uri", {string, "erlref"}, "Base URI to put application docs"}

    ,{kz_ga, undefined, "ga", {string, undefined}, "Google Analytics site's ID"}
    ,{kz_gendate, undefined, "gendate", string, "The date string in YYYY-MM-DD format that will be used in the documentation, [default: current date]"}
    ,{kz_vsn, undefined, "vsn", {string, "master"}, "The Kazoo version string"}
    ].

parse_args(CmdArgs) ->
    try getopt:parse(option_spec_list(), CmdArgs) of
        {ok, {Options, Args}} ->
            maybe_print_help(Options),
            parse_args(Options, Args);
        {error, {_, _}} ->
            usage()
    catch
        error:undef ->
            simple_usage()
    end.

parse_args(Options, Args) ->
    case proplists:get_bool(all_apps, Options) of
        true ->
            add_edoc_infos(Options, all_apps);
        false ->
            add_edoc_infos(Options, Args)
    end.

add_edoc_infos(Options, all_apps) ->
    Apps = [filename:basename(D) || D <- filelib:wildcard("{applications,core}/*"), filelib:is_dir(D)],
    add_edoc_infos(Options, Apps);
add_edoc_infos(_, []) ->
    io:format("no application is given, either set 'all_apps' or give some app names.~n"),
    halt(1);
add_edoc_infos(Options, Args) ->
    [{kz_gen_apps
     ,lists:append([EdocInfo
                    || App <- Args,
                       EdocInfo <- [maybe_get_app_edoc(App)],
                       EdocInfo =/= []
                   ])
     }
     | Options
    ].

maybe_get_app_edoc(App) ->
    case code:lib_dir(App) of
        {error, bad_name} ->
            io:format("invalid app name: ~p~n", [App]),
            [];
        Path ->
            get_app_edoc(Path)
    end.

get_app_edoc(Path) ->
    {AppName, AppCat} = app_cat(lists:reverse(filename:split(Path))),
    case [Erl || Erl <- filelib:wildcard(filename:join(Path, "src/**/*.erl"))] of
        [] ->
            io:format("no source files found for: ~p~n", [Path]),
            [];
        Erls ->
            [{list_to_atom(filename:basename(Erl, ".erl")), list_to_atom(AppName), AppCat, Erl}
             || Erl <- Erls
            ]
    end.

app_cat([AppName, "core" | _]) ->
    {AppName, "core"};
app_cat([AppName, "applications" | _]) ->
    {AppName, "applications"};
app_cat(_Other) ->
    io:format("bad kazoo app name: ~p~n", [_Other]),
    halt(1).

maybe_print_help(Options) ->
    case proplists:get_value(help, Options) of
        true -> usage();
        _ -> ok
    end.

simple_usage() ->
    io:format("Usage: ERL_LIBS=deps/:core/:applications/ ./scripts/kz_docgen.escript --help~n"),
    halt(1).

usage() ->
    getopt:usage(option_spec_list(), "ERL_LIBS=deps/:core/:applications/ ./scripts/kz_docgen.escript", "[app_name1 app_name2 ...]"),
    halt(1).
