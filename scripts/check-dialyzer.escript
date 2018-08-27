#!/usr/bin/env escript
%%! +A0 -sname kazoo_dialyzer
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([_KazooPLT]) -> 'ok';
main([KazooPLT, "--hard" | Args]) ->
    main(KazooPLT, Args, 'true');
main([KazooPLT | Args]) ->
    main(KazooPLT, Args, 'false');
main(_Args) ->
    io:format('user', "invalid usage: ~p~n", [_Args]),
    usage(0, _Args).

main(KazooPLT, Args, GoHard) ->
    'false' =:= lists:suffix(".plt", KazooPLT)
        andalso usage(1, [KazooPLT, GoHard | Args]),

    Env = string:tokens(os:getenv("TO_DIALYZE", ""), " "),

    case filter_for_erlang_files(lists:usort(Args ++ Env)) of
        [] ->
            io:format("No files to process\n"),
            usage(0);
        Paths ->
            case warn(KazooPLT, Paths, GoHard) of
                0 -> halt(0);
                Count ->
                    io:format("~p Dialyzer warnings~n", [Count]),
                    halt(Count)
            end
    end.

filter_for_erlang_files(Files) ->
    [Arg || Arg <- Files,
            not is_test(Arg)
                andalso (
                  is_ebin_dir(Arg)
                  orelse is_beam(Arg)
                  orelse is_erl(Arg)
                 )
                andalso filelib:is_file(Arg)
    ].

%% Internals

is_test(Path) ->
    lists:member("test", string:tokens(Path, "/")).

is_erl(Path) ->
    ".erl" == filename:extension(Path).

is_beam(Path) ->
    ".beam" == filename:extension(Path).

is_ebin_dir(Path) ->
    "ebin" == filename:basename(Path).

root_dir(Path) ->
    filename:join(lists:takewhile(fun is_not_src/1
                                 ,string:tokens(Path, "/")
                                 )
                 ).

is_not_src("src") -> 'false';
is_not_src(_) -> 'true'.

file_exists(Filename) ->
    case file:read_file_info(Filename) of
        {'ok', _}           -> 'true';
        {'error', 'enoent'} -> 'false';
        {'error', _Reason}  -> 'false';
        _ -> 'false'
    end.

warn(PLT, Paths, 'true') ->
    %% take the beams in Paths and run a dialyzer pass to get unknown functions
    %% add the modules from unknown functions
    %% then run do_warm without GoHard
    {BeamPaths, 'true'} = lists:foldl(fun get_beam_path/2, {[], 'true'}, Paths),
    Modules = lists:usort([M || {'warn_unknown', _, {'unknown_function',{M, _F, _Arity}}} <- do_scan_unknown(PLT, BeamPaths)]),
    do_warn(PLT, [fix_path(code:which(M)) || M <- Modules] ++ BeamPaths, 'true');
warn(PLT, Paths, 'false') ->
    {BeamPaths, 'false'} = lists:foldl(fun get_beam_path/2, {[], 'false'}, Paths),
    do_warn(PLT, BeamPaths, 'false').

get_beam_path(Path, {BPs, GoHard}) ->
    {maybe_fix_path(Path, BPs, GoHard), GoHard}.

maybe_fix_path(Path, BPs, GoHard) ->
    case {is_beam(Path), is_erl(Path)} of
        {'true', 'false'} ->
            [fix_path(Path) | BPs];
        {'false', 'true'} ->
            RootDir = root_dir(Path),
            Module  = filename:basename(Path, ".erl"),
            Beam = filename:join([RootDir, "ebin", Module++".beam"]),
            case file_exists(Beam) of
                'true' -> [fix_path(Beam) | BPs];
                'false' -> BPs
            end;
        {'false', 'false'} when GoHard ->
            lists:foldl(fun(F, Acc) -> maybe_fix_path(F, Acc, GoHard) end
                       ,BPs
                       ,filelib:wildcard(filename:join(Path, "*.beam"))
                       );
        {'false', 'false'} ->
            [{'app', filelib:wildcard(filename:join(Path, "*.beam"))} | BPs]
    end.

fix_path(Path) ->
    {'ok', CWD} = file:get_cwd(),
    fix_path(Path, CWD).
fix_path(Path, CWD) ->
    case re:run(Path, CWD) of
        'nomatch' -> filename:join([CWD, Path]);
        _ -> Path
    end.

do_warn(PLT, Paths, GoHard) ->
    {Apps, Beams} = lists:partition(fun({'app', _}) -> 'true'; (_) -> 'false' end, Paths),

    {N, _PLT, GoHard} = lists:foldl(fun do_warn_path/2
                                   ,{0, PLT, GoHard}
                                   ,[{'beams', Beams} | Apps]
                                   ),
    N.

%% explicitly adding `kz_types' so dialyzer knows about `sup_init_ret', `handle_call_ret_state' and other supervisor,
%% gen_server, ... critical types defined in `kz_types'. Dialyzer is strict about types for these `init', `handle_*'
%% functions and if we don't add `kz_types' here, dialyzer thinks their types are `any()' and will warn about it.
ensure_kz_types(Beams) ->
    case lists:any(fun(F) -> filename:basename(F, ".beam") =:= "kz_types" end, Beams) of
        'true' -> Beams;
        'false' -> [code:which('kz_types') | Beams]
    end.

do_warn_path({_, []}, Acc) -> Acc;
do_warn_path({'beams', Beams}, {N, PLT, 'true'}) ->
    {N + scan_and_print(PLT, Beams), PLT, 'true'};
do_warn_path({'beams', Beams}, {N, PLT, 'false'}) ->
    try lists:split(5, Beams) of
        {Ten, Rest} ->
            do_warn_path({'beams', Rest}
                        ,{N + scan_and_print(PLT, Ten), PLT, 'false'}
                        )
    catch
        'error':'badarg' ->
            {N + scan_and_print(PLT, Beams), PLT, 'false'}
    end;
do_warn_path({'app', Beams}, {N, PLT, 'false'}) ->
    try lists:split(5, Beams) of
        {Ten, Rest} ->
            do_warn_path({'app', Rest}
                        ,{N + scan_and_print(PLT, Ten), PLT, 'false'}
                        )
    catch
        'error':'badarg' ->
            {N + scan_and_print(PLT, Beams), PLT, 'false'}
    end.

scan_and_print(PLT, Bs) ->
    Beams = ensure_kz_types(Bs),
    %% io:format("scanning ~s~n", [string:join(Beams, " ")]),
    length([print(Beams, W)
            || W <- scan(PLT, Beams),
               filter(W)
           ]).

filter({'warn_contract_supertype', _, _}) -> 'false';
filter({'warn_undefined_callbacks', _, _}) -> 'false';
filter({'warn_contract_types', _, {'overlapping_contract',_}}) -> 'false';
filter({'warn_umatched_return', _, {'unmatched_return', ["'ok' | {'error','lager_not_running' | {'sink_not_configured','lager_event'}}"]}}) -> 'false';
filter({'warn_unmatched_return', _, {'unmatched_return', ["'false' | 'ok' | {'error','lager_not_running' | {'sink_not_configured','lager_event'}}"]}}) -> 'false';
filter({'warn_umatched_return', _, {'unmatched_return',["'ok' | {'error','invalid_db_name'}"]}}) -> 'false';
filter(_W) -> 'true'.

print(Beams, {Tag, {"src/" ++ _=File, Line}, _W}=Warning) ->
    Filename = filename:basename(File, ".erl"),
    [Beam] = [Beam || Beam <- Beams, Filename =:= filename:basename(Beam, ".beam")],
    AppDir = filename:dirname(filename:dirname(Beam)),
    SrcFile = filename:join([AppDir, File]),
    io:format("~s:~p: ~s~n  ~s~n", [SrcFile, Line, Tag, dialyzer:format_warning(Warning)]);
print(_Beams, {Tag, {File, Line}, _W}=Warning) ->
    io:format("~s:~p: ~s~n  ~s~n", [File, Line, Tag, dialyzer:format_warning(Warning)]);
print(_Beams, _Err) ->
    io:format("error: ~p~n", [_Err]).

scan(PLT, Things) ->
    try do_scan(PLT, Things) of
        Ret -> Ret
    catch 'throw':{'dialyzer_error',Error} ->
            io:format("~s\n", [Error]),
            []
    end.

do_scan_unknown(PLT, Paths) ->
    dialyzer:run([{'init_plt', PLT}
                 ,{'analysis_type', 'succ_typings'}
                  %% ,{'files_rec', [Path]}
                 ,{'from', 'byte_code'}
                 ,{'files', Paths}
                 ,{'warnings', ['unknown']}
                 ]).

do_scan(PLT, Paths) ->
    dialyzer:run([{'init_plt', PLT}
                 ,{'analysis_type', 'succ_typings'}
                  %% ,{'files_rec', [Path]}
                 ,{'from', 'byte_code'}
                 ,{'files', Paths}
                 ,{'warnings', ['error_handling' %% functions that only return via exception
                                %% ,no_behaviours  %% suppress warnings about behaviour callbacks
                                %% ,no_contracts   %% suppress warnings about invalid contracts
                                %% ,no_fail_call   %% suppress warnings for failing calls
                                %% ,no_fun_app     %% suppress warnings for failing fun applications
                                %% ,no_improper_lists %% suppress warnings for improper list construction
                                %% ,no_match          %% suppress warnings for patterns that are unused
                                %% ,'no_missing_calls'  %% suppress warnings about calls to missing functions
                                %% ,no_opaque         %% suppress warnings for violating opaque data structures
                                %% ,no_return         %% suppress warnings for functions that never return a value
                                %% ,no_undefined_callbacks %% suppress warnings about behaviours with no -callback
                                %% ,no_unused         %% suppress warnings for unused functions
                               ,'race_conditions'   %% include warnings for possible race conditions
                               ,'underspecs'        %% warn when the spec is too loose
                                %% ,'unknown'           %% let warnings about unknown functions/types change exit status
                               ,'unmatched_returns' %% warn when function calls ignore structure return values
                                %% ,overspecs %% ignorable, mostly for Dialyzer devs
                                %% ,specdiffs
                               ]}
                 ]).

usage(Exit) ->
    usage(Exit, "").

usage(Exit, Msg) ->
    Arg0 = escript:script_name(),
    io:format("Usage: ~s <path to .kazoo.plt> <path to ebin/>+~n~p~n"
             ,[filename:basename(Arg0), Msg]
             ),
    halt(Exit).

%% End of Module
