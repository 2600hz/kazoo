#!/usr/bin/env escript
%%! -sname xref
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

-define(SERVER, 'kazoo').

%% API

main([]) -> usage();
main(Paths) ->
    'ok' = code:add_pathsa(Paths),
    {'ok', _Pid} = xref:start(?SERVER),
    AllPaths = code:get_path(),
    'ok' = xref:set_library_path(?SERVER, AllPaths),
    'ok' = xref:set_default(?SERVER, [ {'warnings', 'false'}
                                     , {'verbose', 'false'}
                                     ]),
    io:format("Loading modules...\n"),
    _ = [{'ok', _Modules} = xref:add_directory(?SERVER, Dir)
         || Dir <- AllPaths
                , not lists:prefix("./deps/", Dir)
                %% Includes OTP's paths
                %% Removes deps' paths
        ],
    Xrefs = [ 'undefined_function_calls'
            %% , 'undefined_functions'        %%
            %% , 'locals_not_used'            %% Compilation discovers this
            %% , 'exports_not_used'           %% Compilation discovers this
            %% , 'deprecated_function_calls'  %% Concerns not kazoo
            %% , 'deprecated_functions'       %% Concerns not kazoo
              %% Want moar? http://www.erlang.org/doc/man/xref.html
            ],
    io:format("Running xref analysis...\n"),
    lists:foreach( fun (Xref) ->
                           {'ok', Res} = xref:analyze(?SERVER, Xref),
                           print(Xref, Res)
                   end
                 , Xrefs ),
    'stopped' = xref:stop(?SERVER).

%% Internals

print('undefined_function_calls'=Xref, Results) ->
    io:format("Xref: ~p\n", [Xref]),
    lists:foreach( fun ({{M1,F1,A1}, {M2,F2,A2}}) ->
                           %% FIXME: table-like output
                           io:format( "\t~p:~p/~p calls undefined ~p:~p/~p\n"
                                    , [M1,F1,A1, M2,F2,A2] )
                   end
                 , Results );
print(Xref, Results) ->
    io:format("Xref: ~p\n\t~p\n", [Xref, Results]).

usage() ->
    %% ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  <path to ebin/>+\n", [filename:basename(Arg0)]),
    halt(1).

%% End of Module
