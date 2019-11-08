#!/usr/bin/env escript
%%! +A0 -sname kazoo_dialyzer
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([]) ->
    print_help(1);
main([_KazooPLT]) -> 'ok';
main([KazooPLT | CommandLineArgs]) ->
    {'ok', Options, Args} = parse_args(CommandLineArgs),
    handle(KazooPLT, Options, Args).

parse_args(CommandLineArgs) ->
    case getopt:parse(option_spec_list(), CommandLineArgs) of
        {'ok', {Options, Args}} when is_list(Options) ->
            {'ok', Options, Args};
        {'ok', {_O, _A}} ->
            print_help(1);
        {'error', {_O, _A}} ->
            print_help(1)
    end.

-spec option_spec_list() -> list().
option_spec_list() ->
    [{'help', $?, "help", 'undefined', "Show the program options"}
    ,{'hard', $h, "hard", {'boolean', 'false'}, "Include remote modules called by the supplied modules"}
    ,{'bulk', $b, "bulk", {'boolean', 'false'}, "Dialyze all files together (requires more memory/CPU)"}
    ].

-spec print_help(integer()) -> no_return().
print_help(Halt) ->
    Script = escript:script_name(),
    getopt:usage(option_spec_list(), "ERL_LIBS=deps/:core/:applications/ " ++ Script ++ " .kazoo.plt [args] [file.beam | path/ebin/ ...]"),
    halt(Halt).

handle(_KazooPLT, _Options, []) ->
    print_help(0);
handle(KazooPLT, Options, Args) ->
    ".plt" = filename:extension(KazooPLT),

    Env = string:tokens(os:getenv("TO_DIALYZE", ""), " "),

    handle_paths(KazooPLT
                ,Options
                ,filter_for_erlang_files(lists:usort(Env ++ Args))
                ).

handle_paths(_KazooPLT, _Options, []) ->
    io:format("No Erlang files found to process\n"),
    print_help(0);
handle_paths(KazooPLT, Options, Paths) ->
    case warn(KazooPLT, Options, Paths) of
        0 -> halt(0);
        1 ->
            io:format("1 Dialyzer warning~n"),
            halt(1);
        Count ->
            io:format("~p Dialyzer warnings~n", [Count]),
            halt(Count)
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

root_dir("/"++Path) ->
    filename:join(["/" | lists:takewhile(fun is_not_src/1
                                        ,string:tokens(Path, "/")
                                        )
                  ]
                 );
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

warn(PLT, Options, Paths) ->
    GoHard = props:get_value('hard', Options),
    Bulk = GoHard
        orelse props:get_value('bulk', Options),

    %% take the beams in Paths and run a dialyzer pass to get unknown functions
    %% add the modules from unknown functions
    %% then run do_warm without GoHard
    {BeamPaths, GoHard} = lists:foldl(fun get_beam_path/2, {[], GoHard}, Paths),

    AllModules = find_unknown_modules(PLT, BeamPaths, GoHard),

    log_work_to_do(BeamPaths, AllModules, GoHard),

    do_warn(PLT, AllModules, Bulk).

log_work_to_do([BeamPath], _AllModules, 'false') ->
    io:format("analyzing 1 path...~n~p~n~n", [BeamPath]);
log_work_to_do(BeamPaths, _AllModules, 'false') ->
    io:format("analyzing ~p paths...~n", [length(BeamPaths)]),
    _ = [io:format("~s~n", [File]) || File <- lists:usort(BeamPaths)],
    io:format("\n"),
    'ok';
log_work_to_do(BeamPaths, AllModules, 'true') ->
    Len = length(BeamPaths),
    io:format("analyzing ~p paths + ~p called modules...~n~n", [Len, length(AllModules)-Len]),
    _ = [io:format("~s~n", [File]) || File <- lists:usort(BeamPaths ++ AllModules)],
    io:format("\n"),
    'ok'.

find_unknown_modules(_PLT, BeamPaths, 'false') -> BeamPaths;
find_unknown_modules(PLT, BeamPaths, 'true') ->
    UnknownModules = [M ||
                         {'warn_unknown', _, {'unknown_function',{M, _F, _Arity}}} <- do_scan_unknown(PLT, BeamPaths),
                         M =/= 'localtime' % excluded cause raw dict makes dialyzer sad
                     ],

    [fix_path(MPath) ||
        M <- lists:usort(UnknownModules),
        MPath <- [code:which(M)],
        MPath =/= 'non_existing'
    ] ++ BeamPaths.

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

fix_path('non_existing', _CWD) -> 'undefined';
fix_path(Path, CWD) ->
    case re:run(Path, CWD) of
        'nomatch' -> filename:join([CWD, Path]);
        _ -> Path
    end.

do_warn(PLT, Paths, InBulk) ->
    {Apps, Beams} = maybe_separate_steps(Paths, InBulk),

    {N, _PLT, InBulk} = lists:foldl(fun do_warn_path/2
                                   ,{0, PLT, InBulk}
                                   ,[{'beams', Beams} | Apps]
                                   ),
    N.

maybe_separate_steps(Paths, InBulk) ->
    lists:foldl(fun(Path, Acc) -> maybe_separate_step(Path, Acc, InBulk) end
               ,{[], []}
               ,Paths
               ).

maybe_separate_step({'app', AppFiles}, {Apps, Beams}, 'true') ->
    {Apps, AppFiles ++ Beams};
maybe_separate_step({'app', AppFiles}, {Apps, Beams}, 'false') ->
    {[{'app', AppFiles} | Apps], Beams};
maybe_separate_step({'beam', Bs}, {Apps, Beams}, _InBulk) ->
    {Apps, Bs ++ Beams};
maybe_separate_step(Beam, {Apps, Beams}, _InBulk) ->
    {Apps, [Beam | Beams]}.

%% explicitly adding `kz_types' so dialyzer knows about `sup_init_ret', `handle_call_ret_state' and other supervisor,
%% gen_server, ... critical types defined in `kz_types'. Dialyzer is strict about types for these `init', `handle_*'
%% functions and if we don't add `kz_types' here, Dialyzer thinks their types are `any()' and will warn about it.
ensure_kz_types(Beams) ->
    case lists:any(fun(F) -> filename:basename(F, ".beam") =:= "kz_types" end, Beams) of
        'true' -> Beams;
        'false' -> [code:which('kz_types') | Beams]
    end.

do_warn_path({_, []}, Acc) -> Acc;
do_warn_path({_, Beams}, {N, PLT, 'true'}) ->
    {N + scan_and_print(PLT, Beams), PLT, 'true'};
do_warn_path({Type, Beams}, {N, PLT, 'false'}) ->
    try lists:split(5, Beams) of
        {Ten, Rest} ->
            do_warn_path({Type, Rest}
                        ,{N + scan_and_print(PLT, Ten), PLT, 'false'}
                        )
    catch
        'error':'badarg' ->
            {N + scan_and_print(PLT, Beams), PLT, 'false'}
    end.

scan_and_print(PLT, Bs) ->
    Beams = ensure_kz_types(Bs),
    length([print(Beams, W)
            || W <- scan(PLT, Beams),
               filter(W)
           ]).

filter({'warn_contract_supertype',  _, _}) -> 'false';
filter({'warn_undefined_callbacks', _, _}) -> 'false';
filter({'warn_contract_types',      _, {'overlapping_contract',_}}) -> 'false';
filter({'warn_umatched_return',     _, {'unmatched_return', ["'ok' | {'error','lager_not_running' | {'sink_not_configured','lager_event'}}"]}}) -> 'false';
filter({'warn_unmatched_return',    _, {'unmatched_return', ["'false' | 'ok' | {'error','lager_not_running' | {'sink_not_configured','lager_event'}}"]}}) -> 'false';
filter({'warn_umatched_return',     _, {'unmatched_return',["'ok' | {'error','invalid_db_name'}"]}}) -> 'false';
filter({'warn_return_no_exit',      _, {'no_return',['only_normal','kz_log_md_clear',0]}}) -> 'false';
filter({'warn_failing_call',        _, {'call',['lager','md',"([])" | _]}}) -> 'false';
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
                                %% ,'race_conditions'   %% include warnings for possible race conditions
                               ,'underspecs'        %% warn when the spec is too loose
                                %% ,'unknown'           %% let warnings about unknown functions/types change exit status
                               ,'unmatched_returns' %% warn when function calls ignore structure return values
                                %% ,overspecs %% ignorable, mostly for Dialyzer devs
                                %% ,specdiffs
                               ]}
                 ]).

%% End of Module
