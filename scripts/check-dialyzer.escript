#!/usr/bin/env escript
%%! -sname xref
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

-define(PLT, ".kazoo.plt").

%% API

main(Args) ->
    case [Arg || Arg <- Args,
                 is_ebin_dir(Arg)
                     orelse is_beam(Arg)
                     orelse is_erl(Arg)
         ]
    of
        [] ->
            io:format("No files to process\n"),
            usage(),
            halt(0);
        Paths ->
            Count = lists:sum([warn(Path) || Path <- Paths]),
            io:format("~p Dialyzer warnings\n", [Count]),
            halt(Count)
    end.

%% Internals

is_erl(Path) ->
    ".erl" == filename:extension(Path).

is_beam(Path) ->
    ".beam" == filename:extension(Path).

is_ebin_dir(Path) ->
    "ebin" == filename:basename(Path).

root_dir(Path) ->
    filename:join(
      lists:takewhile(fun ("src") -> 'false';
                          (_) -> 'true'
                      end, string:tokens(Path, "/"))).

warn(Path) ->
    case {is_beam(Path), is_erl(Path)} of
        {'true',_} ->
            do_warn(Path);
        {_,'true'} ->
            RootDir = root_dir(Path),
            Module  = filename:basename(Path, ".erl"),
            Beam = filename:join([RootDir, "ebin", Module++".beam"]),
            do_warn(Beam);
        {_,_} ->
            io:format("going through ~p\n", [Path]),
            Files = filelib:wildcard(filename:join(Path, "*.beam")),
            R = lists:sum([do_warn(File) || File <- Files]),
            io:format("~p warnings for ~p\n", [R, Path]),
            R
    end.

do_warn(Path) ->
    length([ print(W)
             || W <- scan(Path)
                    , filter(W)
           ]).


%% dialyzer <R17 will erroneously output too many of those
filter({warn_contract_supertype, _, _}) -> 'false';

filter({warn_callgraph, _, {call_to_missing,[application,ensure_all_started,1]}}) -> 'false';
filter({warn_matching, {"src/konami_code_fsm.erl",_}, {pattern_match,["pattern 'true'","'false'"]}}) -> 'false';

filter({_Tag, _Loc, _Msg}=_W) ->
    io:format("_W = ~p\n", [_W]),
    'true'.

print({Tag, _Loc, _Warning} = W) ->
    io:format("~-30.. s~s", [Tag, dialyzer:format_warning(W)]);
print(_Err) ->
    _Err.

scan(Thing) ->
    try do_scan(Thing) of
        Ret -> Ret
    catch throw:{dialyzer_error,Error} ->
            io:format("~s", [Error]),
            [255]
    end.

do_scan(Path) ->
    io:format("scanning ~p\n", [Path]),
    dialyzer:run([ {init_plt, ?PLT}
                 , {analysis_type, succ_typings}
                 %% , {files_rec, [Path]}
                 , {files, [Path]}
                 , {warnings, [ no_undefined_callbacks

                              , unmatched_returns
                              , error_handling
                              , race_conditions
                              %% , overspecs  %% "… is a subtype of any()"
                              , underspecs    %% Has issues for < R17
                              %% , specdiffs  %% "… is a subtype of any()"

                              , no_return  %% Suppress warnings for functions that will never return a value
                              %% , no_unused
                              %% , no_improper_lists
                              %% , no_fun_app
                              %% , no_match
                              %% , no_opaque
                              %% , no_fail_call
                              %% , no_contracts
                              %% , no_behaviours

                              ]}
                 ]).

usage() ->
    %% ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  <path to ebin/>+\n", [filename:basename(Arg0)]).

%% End of Module
