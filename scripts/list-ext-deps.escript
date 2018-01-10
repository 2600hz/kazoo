#!/usr/bin/env escript
%%! -noinput

%%% ===========================================================================
%%% Analyze every .beam file in the given paths, find out which external calls
%%% each make, and list those calls that are to the standard Erlang apps (such
%%% as stdlib).
%%%
%%% When multiple paths are given, cumulative results are displayed (i.e. the
%%% union of the sets of modules used by the beam files in question).
%%%
%%% The beam files must be compiled with debug_info otherwise those files will
%%% be skipped with an error message.
%%% ===========================================================================
-mode(compile).

main([_|_]=DirList) ->
    GoodDirs = check_dirs_exist(DirList),
    list_deps(GoodDirs);
main(_) ->
    usage().

list_deps(Dirs) ->
    Xref = 'list-ext-deps',
    {ok, _Pid} = xref:start(Xref),
    AppCalls = lists:foldl(fun(Dir, Acc) ->
                                   analyze_beams(Xref, Dir) ++ Acc
                           end, [], Dirs),
    xref:stop(Xref),
    ErlApps = all_erlang_apps(),
    Deps = find_erlang_deps(lists:usort(AppCalls), ErlApps),
    lists:foreach(fun(Dep) ->
                          io:format("~p\n", [Dep])
                  end, Deps).

%% Compare the list of module calls made by the app
%% with the modules provided by erlang, and return
%% a list of Erlang applications used by the AppCalls.
-type app_calls() :: [module()].
-type erl_app() :: {app_name(), [module()]}.
-type erl_apps() :: [erl_app()].
-type app_name() :: atom().
-spec find_erlang_deps(app_calls(), erl_apps()) -> [app_name()].
find_erlang_deps(AppCalls, ErlApps) ->
    Index = make_inverted_index(ErlApps),
    lists:usort([Mod
                 || AppCall <- AppCalls,
                    case maps:get(AppCall, Index, false) of
                        false -> Mod = false;
                        Mod -> true
                    end]).

%% Take a proplist of keys and associated arrays and invert
%% it to a map of array elements to keys.
%% In other words, [{key1, [el1, el2, el3]}] ->
%% %{el1: key1, el2: key1, el3: key1}
make_inverted_index(ErlApps) ->
    lists:foldl(fun({App, Mods}, Map) ->
                        lists:foldl(fun(Mod, Acc) ->
                                            maps:put(Mod, App, Acc)
                                    end, Map, Mods)
                end, #{}, ErlApps).

-spec analyze_beams(atom(), string()) -> app_calls().
analyze_beams(Xref, BaseDir) ->
    Beams = filelib:wildcard(filename:join([BaseDir, "**", "ebin", "*.beam"])),
    Deps = lists:foldl(fun(Beam, Acc) -> gb_sets:insert(Beam, Acc) end,
                       gb_sets:empty(), Beams),
    Mods = gb_sets:fold(fun(Beam, Acc) -> gather_deps(Xref, Beam) ++ Acc end,
                        [], Deps),
    lists:usort(Mods).

-spec gather_deps(atom(), string()) -> app_calls().
gather_deps(Xref, FileName) ->
    XrefOpts = [{verbose, 'false'},
                {builtins, 'true'},
                {warnings, false}],
    case xref:add_module(Xref, FileName, XrefOpts) of
        {ok, Mod} ->
            Query = "XC",
            {ok, Calls} = xref:q(Xref, Query),
            ok = xref:remove_module(Xref, Mod),
            [ToModule
             || {_From, {ToModule,_,_}} <- Calls];
        {error, MissingMod, {no_such_module,_}} ->
            out_stderr("Missing ~p\n", [MissingMod]),
            [];
        {error, NoDbgMod, {no_debug_info,_Path}} ->
            out_stderr("~p has no debug info, skipping\n", [NoDbgMod]),
            [];
        {error, Mod, Error} ->
            out_stderr("Error in ~p: ~p\n", [Mod, Error]),
            []
    end.

-spec all_erlang_apps() -> erl_apps().
all_erlang_apps() ->
    [{beam_to_app(Path), find_beams(Path)}
     || Path <- code:get_path(), Path =/= "."].

-compile({inline,
          [
           {find_beams, 1},
           {beams_to_mods, 1},
           {beam_to_mod, 1},
           {beam_to_app, 1},
           {app_vsn_to_app, 1}
          ]}).

beam_to_app(Beam) ->
    %% $ERL_ROOT/lib/App-Vsn/ebin -> AppVsn
    AppVsn = filename:basename(filename:dirname(Beam)),
    app_vsn_to_app(AppVsn).

find_beams(EbinPath) ->
    beams_to_mods(filelib:wildcard(filename:join(EbinPath, "*.beam"))).

beams_to_mods(Beams) ->
    [beam_to_mod(Beam) || Beam <- Beams].

beam_to_mod(Beam) ->
    list_to_atom(filename:basename(Beam, ".beam")).

app_vsn_to_app(AppVsn) ->
    [App, _Vsn] = string:tokens(AppVsn, "-"),
    list_to_atom(App).

check_dirs_exist(Dirs) ->
    lists:foldl(fun(Dir, Acc) ->
                        case filelib:is_dir(Dir) of
                            true ->
                                [Dir | Acc];
                            false ->
                                out_stderr("Skipping missing directory [~s]\n",
                                           [Dir]),
                                Acc
                        end
                end, [], Dirs).

usage() ->
    out_stderr("usage: ~s path [path...]\n\n"
               "Find OTP dependencies of .beam files "
               "(compiled with debug info) in list of paths\n",
               [filename:basename(escript:script_name())]),
    halt(1).

out_stderr(Fmt, Args) ->
    io:format(standard_error, Fmt, Args).

%% ex: ft=erlang ts=4 sts=4 sw=4 et
