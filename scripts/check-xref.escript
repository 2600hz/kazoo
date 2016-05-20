#!/usr/bin/env escript
%%! -sname kazoo_xref
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

-define(SERVER, 'kazoo').

%% API

main([]) ->
    usage(),
    halt(-1);
main(Paths) ->
    AllPaths = all_paths(Paths),
    {'ok', _Pid} = xref:start(?SERVER),
    'ok' = xref:set_library_path(?SERVER, AllPaths),
    'ok' = xref:set_default(?SERVER, [ {'warnings', 'false'}
                                     , {'verbose', 'false'}
                                     ]),
    io:format("Loading modules...\n"),
    _ = [ case xref:add_directory(?SERVER, Dir) of
              {'ok', _Modules} -> 'ok';
              {'error', _XrefModule, Reason} ->
                  show_error('add_directory', Reason)
          end || Dir <- AllPaths
                     , Dir =/= "."
                 %% Don't include deps
                     , not lists:prefix("./deps/", Dir)
                 %% Note: OTP's dirs usually start with "/"
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
    ErrorsCount =
        lists:sum(
          lists:map( fun (Xref) ->
                             {'ok', Res} = xref:analyze(?SERVER, Xref),
                             Filtered = filter(Xref, Res),
                             print(Xref, Filtered),
                             length(Filtered)
                     end
                   , Xrefs )
         ),
    'stopped' = xref:stop(?SERVER),
    io:format("Done\n"),
    halt(ErrorsCount).

%% Internals

all_paths(Paths) ->
    OfARelease = fun (Path) -> lists:member("_rel", filename:split(Path)) end,
    case lists:any(OfARelease, Paths) of
        false ->
            %% ie: we are not Xref-ing an Erlang release.
            'ok' = code:add_pathsa(Paths),
            code:get_path();
        true -> Paths
    end.

filter('undefined_function_calls', Results) ->
    ToKeep = fun
                 %% apns:start/0 calls the fun only if it exists
                 ({{apns,start,0}, {application,ensure_all_started,1}}) -> 'false';

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

                 (_) -> 'true'
             end,
    lists:filter(ToKeep, Results);
filter(_Xref, Results) ->
    Results.

print('undefined_function_calls'=Xref, Results) ->
    io:format("Xref: listing ~p\n", [Xref]),
    lists:foreach(
      fun ({{M1,F1,A1}, {M2,F2,A2}}) ->
              io:format( "~30.. s:~-30..,s/~p ~30.. s ~30.. s:~s/~p\n"
                       , [M1,F1,A1, "calls undefined", M2,F2,A2] )
      end, Results );
print(Xref, Results) ->
    io:format("Xref: listing ~p\n\t~p\n", [Xref, Results]).

usage() ->
    %% ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  <path to ebin/>+\n", [filename:basename(Arg0)]).

show_error('add_directory', {'module_clash', {Module, BEAM1, BEAM2}}) ->
    io:format( "Module clash: ~s (~s & ~s)\n"
             , [Module, filename:dirname(BEAM1), filename:dirname(BEAM2)] );
show_error(Fun, Reason) ->
    io:format("Error with ~s: ~p\n", [Fun, Reason]).

%% End of Module
