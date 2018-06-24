#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode(compile).

-export([main/1]).

main(Args) ->
    _ = io:setopts(user, [{encoding, unicode}]),
    run_eunit(parse_args(Args, #{modules => []})).

parse_args([], Opts) ->
    Opts;
parse_args(["--with-cover" | Rest], Opts) ->
    parse_args(Rest, Opts#{cover => true});
parse_args(["--cover-project-name", ProjectName | Rest], Opts) ->
    parse_args(Rest, Opts#{coverdata => ProjectName ++ ".coverdata"});
parse_args(["--cover-report-dir", Dir | Rest], Opts) ->
    parse_args(Rest, Opts#{report_dir => Dir});
parse_args([Mod | Rest], #{modules := Modules}=Opts) ->
    parse_args(Rest, Opts#{modules => [Mod | Modules]}).

run_eunit(#{modules := []}) ->
    io:format(user, "No modules are specified.\n", []),
    usage();
run_eunit(#{modules := Modules}=Opts) ->
    maybe_start_cover(Opts),

    TestMods = filter_same_name_test_modules(Modules),
    case eunit:test([TestMods], [verbose]) of
        ok ->
            maybe_stop_cover(Opts),
            erlang:halt();
        _ ->
            erlang:halt(1)
    end.

filter_same_name_test_modules(Modules) ->
    Fun = fun(Mod, Acc) ->
                  Module = filename:rootname(filename:basename(Mod)),
                  case lists:reverse(Module) of
                      "stset_"++M ->
                          case not lists:member(lists:reverse(M), Modules) of
                              true -> [list_to_atom(Module) | Acc];
                              false -> Acc
                          end;
                      _ ->
                          [list_to_atom(Module) | Acc]
                  end
          end,
    lists:usort(lists:foldl(Fun, [], Modules)).

maybe_start_cover(#{cover := true
                   ,coverdata := _
                   }=Opts) ->
    ok = filelib:ensure_dir(maps:get(report_dir, Opts, "cover") ++ "/dummy"),
    _ = cover:start(),
    cover:compile_beam_directory("ebin");
maybe_start_cover(#{cover := true}) ->
    io:format(user, "No project name is specified.\n", []),
    usage();
maybe_start_cover(_) ->
    ok.

maybe_stop_cover(#{cover := true
                  ,coverdata := Coverdata
                  }=Opts) ->
    cover:export(Coverdata),
    cover:analyse_to_file([html, {outdir, maps:get(report_dir, Opts, "cover")}]);
maybe_stop_cover(_) ->
    ok.

usage() ->
    Arg0 = escript:script_name(),
    io:format(user, "Usage: ~s [--with-cover] [--cover-project-name <project name>] [--cover-report-dir <cover report dir>] <erlang module name/>+\n", [filename:basename(Arg0)]),
    halt(1).
