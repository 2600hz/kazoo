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
    AppsMods = lists:foldl(fun add_directory_fold/2, [], AllPaths),
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
                           Filtered = filter(AppsMods, Xref, Res),
                           print(Xref, Filtered)
                   end
                 , Xrefs ),
    'stopped' = xref:stop(?SERVER),
    io:format("Done\n").

%% Internals

filter(AppsMods, 'undefined_function_calls', Results) ->
    KazooMods = sets:from_list(
                  lists:flatmap( fun ({"./deps/"++_, _Mods}) -> [];
                                     ({_Dir, Mods}) -> Mods
                                 end, AppsMods )),
    ToKeep = fun
                 %% apns:start/0 calls the fun only if it exists
                 ({{apns,start,0}, {application,ensure_all_started,1}}) -> 'false';

                 %% OTP Xref errors
                 ({{eunit_test,_,_}, {_,_,_}}) -> 'false';

                 %% DTL modules that only exist at runtime
                 ({{_,_,_}, {sub_package_dialog,_,_}}) -> 'false';
                 ({{_,_,_}, {sub_package_message_summary,_,_}}) -> 'false';
                 ({{_,_,_}, {sub_package_presence,_,_}}) -> 'false';

                 %% Non-kazoo errors
                 ({{M,_,_}, {_,_,_}}) -> sets:is_element(M, KazooMods);
                 (_) -> 'true'
             end,
    lists:filter(ToKeep, Results);
filter(_AppsMods, _Xref, Results) ->
    Results.

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

show_error('add_directory', {'module_clash', {Module, BEAM1, BEAM2}}) ->
    io:format( "Module clash: ~s (~s & ~s)\n"
             , [Module, filename:dirname(BEAM1), filename:dirname(BEAM2)] );
show_error(Fun, Reason) ->
    io:format("Error with ~s: ~p\n", [Fun, Reason]).

add_directory_fold("./deps/"++_, Acc) -> Acc;
add_directory_fold(".", Acc) -> Acc;
add_directory_fold(Dir, Acc) ->
    %% Note: OTP's dirs usually start with "/"
    case xref:add_directory(?SERVER, Dir) of
        {'ok', Modules} ->
            case Dir of
                "/"++_ -> Acc;
                _ -> [{Dir,Modules} | Acc]
            end;
        {'error', _XrefModule, Reason} ->
            show_error('add_directory', Reason),
            Acc
    end.

%% End of Module
