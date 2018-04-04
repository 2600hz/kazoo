#!/usr/bin/env escript
%%! +A0 -sname kazoo_dialyzer
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([_KazooPLT]) -> 'ok';
main([KazooPLT | Args]) ->
    'false' =:= lists:suffix(".plt", KazooPLT)
        andalso usage(1),

    Env = string:tokens(os:getenv("TO_DIALYZE", ""), " "),

    case filter_for_erlang_files(lists:usort(Args ++ Env)) of
        [] ->
            io:format("No files to process\n"),
            usage(0);
        Paths ->
            case warn(KazooPLT, Paths) of
                0 -> halt(0);
                Count ->
                    io:format("~p Dialyzer warnings\n", [Count]),
                    halt(Count)
            end
    end;
main(_) ->
    usage(0).

filter_for_erlang_files(Files) ->
    [Arg || Arg <- Files,
            not is_test(Arg)
                andalso (
                  is_ebin_dir(Arg)
                  orelse is_beam(Arg)
                  orelse is_erl(Arg)
                 )
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

warn(PLT, Paths) ->
    BeamPaths = lists:foldl(fun get_beam_path/2, [], Paths),
    do_warn(PLT, BeamPaths).

get_beam_path(Path, BPs) ->
    case maybe_fix_path(Path) of
        'undefined' -> BPs;
        Beam -> [Beam | BPs]
    end.

maybe_fix_path(Path) ->
    case {is_beam(Path), is_erl(Path)} of
        {'true', 'false'} ->
            fix_path(Path);
        {'false', 'true'} ->
            RootDir = root_dir(Path),
            Module  = filename:basename(Path, ".erl"),
            Beam = filename:join([RootDir, "ebin", Module++".beam"]),
            case file_exists(Beam) of
                'true' -> fix_path(Beam);
                'false' -> 'undefined'
            end;
        {'false', 'false'} ->
            {'app', filelib:wildcard(filename:join(Path, "*.beam"))}
    end.

fix_path(Path) ->
    {'ok', CWD} = file:get_cwd(),
    fix_path(Path, CWD).
fix_path(Path, CWD) ->
    case re:run(Path, CWD) of
        'nomatch' -> filename:join([CWD, Path]);
        _ -> Path
    end.

do_warn(PLT, Paths) ->
    {Apps, Beams} = lists:partition(fun({'app', _}) -> 'true'; (_) -> 'false' end, Paths),

    {N, _} = lists:foldl(fun do_warn_path/2
                        ,{0, PLT}
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
do_warn_path({'beams', Beams}, {N, PLT}) ->
    try lists:split(5, Beams) of
        {Ten, Rest} ->
            do_warn_path({'beams', Rest}
                        ,{N + scan_and_print(PLT, Ten), PLT}
                        )
    catch
        'error':'badarg' ->
            {N + scan_and_print(PLT, Beams), PLT}
    end;
do_warn_path({'app', Beams}, {N, PLT}) ->
    try lists:split(5, Beams) of
        {Ten, Rest} ->
            do_warn_path({'app', Rest}
                        ,{N + scan_and_print(PLT, Ten), PLT}
                        )
    catch
        'error':'badarg' ->
            {N + scan_and_print(PLT, Beams), PLT}
    end.

scan_and_print(PLT, Bs) ->
    Beams = ensure_kz_types(Bs),
    io:format("scanning ~s~n", [string:join(Beams, " ")]),
    length([print(W)
            || W <- scan(PLT, Beams),
               filter(W)
           ]).

filter({'warn_contract_supertype', _, _}) -> 'false';
filter({'warn_undefined_callbacks', _, _}) -> 'false';
filter({'warn_contract_types', _, {'overlapping_contract',_}}) -> 'false';
filter(_W) -> 'true'.

print({Tag, {File, Line}, _W}=Warning) ->
    io:format("~s:~p: ~s~n  ~s~n", [File, Line, Tag, dialyzer:format_warning(Warning)]);
print(_Err) ->
    io:format("error: ~p~n", [_Err]).

scan(PLT, Things) ->
    try do_scan(PLT, Things) of
        Ret -> Ret
    catch 'throw':{'dialyzer_error',Error} ->
            io:format("~s\n", [Error]),
            []
    end.

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
                                %% ,no_missing_calls  %% suppress warnings about calls to missing functions
                                %% ,no_opaque         %% suppress warnings for violating opaque data structures
                                %% ,no_return         %% suppress warnins for functions that never return a value
                                %% ,no_undefined_callbacks %% suppress warnings about behaviours with no -callback
                                %% ,no_unused         %% suppress warnings for unused functions
                               ,'race_conditions'   %% include warnings for possible race conditions
                               ,'underspecs'        %% warn when the spec is too loose
                                %% ,unknown           %% let warnings about unknown functions/types change exit status
                               ,'unmatched_returns' %% warn when function calls ignore structure return values
                                %% ,overspecs %% ignorable, mostly for Dialyzer devs
                                %% ,specdiffs
                               ]}
                 ]).

usage(Exit) ->
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  <path to .kazoo.plt> <path to ebin/>+\n", [filename:basename(Arg0)]),
    halt(Exit).

%% End of Module
