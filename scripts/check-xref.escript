#!/usr/bin/env escript
%%! +A0 -sname kazoo_xref
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

-define(SERVER, 'kazoo').
-define(IGNORED_UNDEFINED_FUNCTION_CALLS, 'ignored_undefined_function_calls').

%% API

main([]) ->
    usage(),
    halt(-1);
main(Paths) ->
    GlobalErrors = xref("global", Paths),
    %% F = fun (Path) -> is_in_path("applications", Path) end,
    %% {Applications, Core} = lists:partition(F, Paths),
    %% LocalErrors = lists:sum([xref(Application, [Application|Core])
    %%                          || Application <- Applications
    %%                         ]),
    halt(GlobalErrors
         %% + LocalErrors
        ).

%% Internals

xref(Pass, Paths) ->
    put(?IGNORED_UNDEFINED_FUNCTION_CALLS, sets:new()),
    io:format("Pass: ~s\n", [Pass]),
    AllPaths = all_paths(Paths),
    {'ok', _Pid} = xref:start(?SERVER),
    'ok' = xref:set_library_path(?SERVER, AllPaths),
    'ok' = xref:set_default(?SERVER, [{'warnings', 'false'}
                                     ,{'verbose', 'false'}
                                     ]),
    io:format("Loading modules...\n"),
    lists:foreach(fun add_dir/1, AllPaths),
    io:format("Running xref analysis...\n"),
    ErrorsCount =
        lists:sum(
          [begin
               {'ok', Res} = xref:analyze(?SERVER, Xref),
               Filtered = filter(Xref, Res),
               print(Xref, Filtered),
               length(Filtered)
           end
           || Xref <- xrefs()
          ]),
    'stopped' = xref:stop(?SERVER),
    io:format("Done\n"),
    ErrorsCount.

all_paths(Paths) ->
    case lists:any(fun (Path) -> is_in_path("_rel", Path) end, Paths) of
        true -> Paths;
        false ->
            %% ie: we are not Xref-ing an Erlang release.
            'ok' = code:add_pathsa(Paths),
            code:get_path()
    end.

is_in_path(Name, Path) ->
    lists:member(Name, filename:split(Path)).

add_dir(Dir) ->
    case Dir =/= "."
        %% Don't include deps
        andalso not ('match' =:= re:run(Dir, "/deps/", [{'capture', 'none'}]))
        %% Note: OTP's dirs usually start with "/"
        andalso xref:add_directory(?SERVER, Dir)
    of
        false -> ok;
        {'ok', _Modules} -> 'ok';
        {'error', _XrefModule, Reason} ->
            show_error('add_directory', Reason)
    end.

xrefs() ->
    ['undefined_function_calls'
    ,'undefined_functions'        %%
     %% ,'locals_not_used'            %% Compilation discovers this
     %% ,'exports_not_used'           %% Compilation discovers this
     %% ,'deprecated_function_calls'  %% Concerns not kazoo
     %% ,'deprecated_functions'       %% Concerns not kazoo
     %% Want moar? http://www.erlang.org/doc/man/xref.html
    ].

filter('undefined_function_calls', Results) ->
    ToKeep = fun
                 %% OTP Xref errors
                 ({{eunit_test,_,_}, {_,_,_}}) -> 'false';
        ({{cerl_to_icode,_,_}, {_,_,_}}) -> 'false';
        ({{compile,_,_}, {_,_,_}}) -> 'false';
        ({{dialyzer_cl,_,_}, {_,_,_}}) -> 'false';
        ({{diameter_lib,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_beam_to_icode,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_consttab,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_icode_bincomp,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_icode_mulret,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_icode_pp,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_icode_split_arith,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_icode_type,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_main,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_x86_main,_,_}, {_,_,_}}) -> 'false';
        ({{hipe_unified_loader,_,_}, {_,_,_}}) -> 'false';
        ({{init,_,_}, {_,_,_}}) -> 'false';

                 %% DTL modules that only exist at runtime
        ({{_,_,_}, {sub_package_dialog,_,_}}) -> 'false';
        ({{_,_,_}, {sub_package_message_summary,_,_}}) -> 'false';
        ({{_,_,_}, {sub_package_presence,_,_}}) -> 'false';

                 %% False positives due to RabbitMQ clashing with EVERYTHING
        ({{_,_,_}, {cowboy,start_http,4}}) -> 'false';
        ({{_,_,_}, {cowboy,start_https,4}}) -> 'false';

                 %% Missing deps of an old-deprecated app: pusher
        ({{_,_,_}, {qdate,to_unixtime,1}}) -> 'false';
        ({{_,_,_}, {qdate,unixtime,0}}) -> 'false';

        ({Caller, Callee}) -> ignore_xref_filter(Caller, Callee)
                                            end,
lists:filter(ToKeep, Results);
filter(undefined_functions, Results) ->
    ToKeep = fun
                 %% OTP Xref errors
                 ({eunit_test, nonexisting_function, 0}) -> false;
        ({hipe, _, _}) -> false;
        ({hipe_amd64_main, _, _}) -> false;
        ({hipe_arm_main, _, _}) -> false;
        ({hipe_x86_main, _, _}) -> false;
        ({hipe_bifs, _, _}) -> false;
        ({hipe_data_pp, _, _}) -> false;
        ({hipe_icode2rtl, _, _}) -> false;
        ({hipe_icode_heap_test, _, _}) -> false;
        ({hipe_llvm_liveness, _, _}) -> false;
        ({hipe_llvm_main, _, _}) -> false;
        ({hipe_ppc_main, _, _}) -> false;
        ({hipe_rtl_arch, _, _}) -> false;
        ({hipe_rtl_cfg, _, _}) -> false;
        ({hipe_rtl_cleanup_const, _, _}) -> false;
        ({hipe_rtl_lcm, _, _}) -> false;
        ({hipe_rtl_ssa, _, _}) -> false;
        ({hipe_rtl_ssa_avail_expr, _, _}) -> false;
        ({hipe_rtl_ssa_const_prop, _, _}) -> false;
        ({hipe_rtl_ssapre, _, _}) -> false;
        ({hipe_rtl_symbolic, _, _}) -> false;
        ({hipe_sparc_main, _, _}) -> false;
        ({hipe_tagscheme, _, _}) -> false;

                 %% Missing deps of an old-deprecated app: pusher
        ({qdate, to_unixtime, 1}) -> false;
        ({qdate, unixtime, 0}) -> false;

        (MFA) -> ignore_xref_filter(MFA)
                                     end,
lists:filter(ToKeep, Results);
filter(_Xref, Results) ->
    Results.

-spec ignore_xref_filter(mfa(), mfa()) -> boolean().
ignore_xref_filter({CallerModule, _, _}, Callee) ->
    ModuleAttrs = CallerModule:module_info('attributes'),
    case props:get_value('ignore_xref', ModuleAttrs) of
        'undefined' -> 'true';
        IgnoreXref ->
            Ignored = lists:any(fun(Ignore) -> Ignore =:= Callee end, IgnoreXref),
            Ignored andalso add_to_ignored_undefined_function_calls(Callee),
            not Ignored
    end.

-spec ignore_xref_filter(mfa()) -> boolean().
ignore_xref_filter(MFA) ->
    not sets:is_element(MFA, get(?IGNORED_UNDEFINED_FUNCTION_CALLS)).

-spec add_to_ignored_undefined_function_calls(mfa()) -> term().
add_to_ignored_undefined_function_calls(MFA) ->
    IgnoredCalls = get(?IGNORED_UNDEFINED_FUNCTION_CALLS),
    put(?IGNORED_UNDEFINED_FUNCTION_CALLS, sets:add_element(MFA, IgnoredCalls)).

print('undefined_function_calls'=Xref, Results) ->
    io:format("Xref: listing ~p\n", [Xref]),
    Sorted = lists:keysort(1, Results),
    lists:foldl(fun print_undefined_function_call/2, 'undefined', Sorted);
print('undefined_functions'=Xref, Results) ->
    io:format("Xref: listing ~p\n", [Xref]),
    F = fun ({M, F, A}) -> io:format("~30.. s:~s/~p\n", [M, F, A]) end,
    lists:foreach(F, Results);
print(Xref, Results) ->
    io:format("Xref: listing ~p\n\t~p\n", [Xref, Results]).

print_undefined_function_call({{M1,F1,A1}, {M2,F2,A2}}, {M1, _Path}=Acc) ->
    io:format("~30.. s:~s/~p calls undefined ~s:~s/~p\n"
             ,[M1,F1,A1, M2,F2,A2]
             ),
    Acc;
print_undefined_function_call({{M1,_,_}, _}=Call, _OldAcc) ->
    {M1, M1Path} = module_path(M1),
    io:format("~s:1: ~s has the following error(s)~n", [M1Path, M1]),
    print_undefined_function_call(Call, {M1, M1Path}).

module_path(Module) ->
    {Module
    ,case Module:module_info('compile') of
         CompileOptions when is_list(CompileOptions) ->
             case proplists:get_value('source', CompileOptions) of
                 'undefined' -> beam_to_source(code:which(Module));
                 Source -> source_to_relative_path(Source)
             end;
         _ -> beam_to_source(code:which(Module))
     end
    }.

source_to_relative_path(Source) ->
    {'ok', CWD} = file:get_cwd(),
    source_to_relative_path(string:tokens(Source, "/")
                           ,string:tokens(CWD, "/")
                           ).

source_to_relative_path([Dir | Source], [Dir | CWD]) ->
    source_to_relative_path(Source, CWD);
source_to_relative_path(SourceParts, _CWD) ->
    string:join(SourceParts, "/").

beam_to_source(BeamPath) ->
    SourceFile = filename:basename(BeamPath, ".beam") ++ ".erl",
    AppDir = filename:dirname(filename:dirname(BeamPath)),
    RootDir = filename:dirname(filename:dirname(AppDir)) ++ "/",

    filelib:fold_files(AppDir
                      ,SourceFile
                      ,'true'
                      ,fun(X, _) -> iolist_to_binary(re:replace(X, RootDir, "")) end
                      ,""
                      ).

usage() ->
    %% ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  <path to ebin/>+\n", [filename:basename(Arg0)]).

show_error('add_directory', {'module_clash', {Module, BEAM1, BEAM2}}) ->
    io:format("Module clash: ~s (~s & ~s)\n"
             ,[Module, filename:dirname(BEAM1), filename:dirname(BEAM2)]);
show_error(Fun, Reason) ->
    io:format("Error with ~s: ~p\n", [Fun, Reason]).

%% End of Module
