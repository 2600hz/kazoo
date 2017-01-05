#!/usr/bin/env escript
%%! +A0 -sname kazoo_dialyzer
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([KazooPLT | Args]) ->
    case lists:suffix(".plt", KazooPLT) of
        'true' -> 'ok';
        'false' ->
            usage(),
            halt(1)
    end,
    case [Arg || Arg <- Args,
                 not is_test(Arg)
                     andalso (
                       is_ebin_dir(Arg)
                       orelse is_beam(Arg)
                       orelse is_erl(Arg)
                      )
         ]
    of
        [] ->
            io:format("No files to process\n"),
            usage(),
            halt(0);
        Paths ->
            Count = lists:sum([warn(KazooPLT,Path) || Path <- Paths]),
            io:format("~p Dialyzer warnings\n", [Count]),
            halt(Count)
    end;
main(_) ->
    usage(),
    halt(0).

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
    filename:join(
      lists:takewhile(fun ("src") -> 'false';
                          (_) -> 'true'
                      end, string:tokens(Path, "/"))).

file_exists(Filename) ->
    case file:read_file_info(Filename) of
        {'ok', _}           -> 'true';
        {'error', 'enoent'} -> 'false';
        {'error', _Reason}  -> 'false';
        _ -> 'false'
    end.

warn(PLT, Path) ->
    case {is_beam(Path), is_erl(Path)} of
        {'true',_} ->
            do_warn(PLT, Path);
        {_,'true'} ->
            RootDir = root_dir(Path),
            Module  = filename:basename(Path, ".erl"),
            Beam = filename:join([RootDir, "ebin", Module++".beam"]),
            case file_exists(Beam) of
                'true' -> do_warn(PLT, Beam);
                'false' -> io:format("file ~s doesn't exist~n", [Beam]),
                           0
            end;
        {_,_} ->
            io:format("going through ~p\n", [Path]),
            Files = filelib:wildcard(filename:join(Path, "*.beam")),
            R = lists:sum([do_warn(PLT, File) || File <- Files]),
            io:format("~p warnings for ~p\n", [R, Path]),
            R
    end.

do_warn(PLT, Path) ->
    length([ print(W)
             || W <- scan(PLT, Path)
                    , filter(W)
           ]).


filter(W) ->
    case W of %% {_Tag, _Loc, _Msg} ->

        %% dialyzer <R17 will erroneously output too many of those
        {warn_contract_supertype, _, _} -> 'false';
        {warn_unknown,{"",0},_} -> 'false';

        {warn_callgraph, _, {call_to_missing,[application,ensure_all_started,1]}} -> 'false';
        {warn_matching, {"src/konami_code_fsm.erl",_}, {pattern_match,["pattern 'true'","'false'"]}} -> 'false';

        %% MAYBE remove error_handling to disable those
        {warn_return_only_exit, {"src/modules/cb_sup.erl",_}, {no_return,[only_explicit,system_terminate,4]}} -> 'false';

        %% Dialyzer says Server can't match but it can
        {warn_matching, {"src/kz_nodes.erl",_}, {pattern_match_cov,["variable Server","{<<_:8,_:_*8>>,<<_:8,_:_*8>>}"]}} -> 'false';

        %% ETS false positives, from core/
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {record_match,["pattern {'kz_amqp_assignment', _, _, _, _, _, _, _, _, _, _, _}","{'error','no_channel'}"]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {record_match,["pattern {'kz_amqp_assignment', _, _, _, _, Channel, _, _, _, _, _, _}","{'error','no_channel'}"]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {record_match,["pattern {'kz_amqp_assignment', Timestamp, _, _, _, _, _, _, _, _, _, Watchers}","{'error','no_channel'}"]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', _, _, _, _, _, _, _, _, _, _, _}], _}",_]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern Assignment = {'kz_amqp_assignment', _, _, _, _, Channel, _, _, _, _, _, _}","{'error','no_channel'}"]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {'kz_amqp_assignment', _, _, _, _, _, _, _, _, _, _, _}","{'error','no_channel'}"]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', _, _, Ref, _, _, _, _, _, _, _, _}], Continuation}",_]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', _, _, _, _, _, Ref, _, _, _, _, _}], Continuation}",_]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', _, Consumer, _, _, Channel, _, _, _, _, _, _}], Continuation}",_]}} -> 'false';

        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[{'kz_amqp_assignment', _, _, _, _, Channel, _, _, _, _, _, _}], Continuation}",_]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[{'kz_amqp_assignment', _, _, _, _, _, _, _, _, _, _, _}], Continuation}",_]}} -> 'false';

        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[{'kz_amqp_assignment', Timestamp, Consumer, _, _, _, _, _, _, _, _, _}], Continuation}",_]}} -> 'false';
        {warn_contract_types, {"src/kz_amqp_assignments.erl",_}, {invalid_contract,[kz_amqp_assignments,maybe_reassign,1,"('$2' | '_' | 'undefined' | pid()) -> 'ok'"]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[{'kz_amqp_assignment', _, _, _, _, Channel, _, _, _, _, _, _}], _}",_]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', _, _, _, 'sticky', _, _, _, Broker, _, _, _}], _}",_]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', _, _, _, 'float', _, _, _, _, _, _, _}], _}",_]}} -> 'false';
        {warn_not_called, {"src/kz_amqp_assignments.erl",_}, {unused_fun,[maybe_reassign,2]}} -> 'false';
        {warn_contract_types, {"src/kz_amqp_assignments.erl",_}, {invalid_contract,[kz_amqp_assignments,assign_or_reserve,3,"('undefined','undefined','float' | 'sticky') -> #kz_amqp_assignment{timestamp::{non_neg_integer(),non_neg_integer(),non_neg_integer()},consumer::'$2' | '_' | 'undefined' | pid(),consumer_ref::reference(),type::'float' | 'sticky',broker::'$1' | '_' | 'undefined' | binary(),reconnect::'false',watchers::set()}"]}} -> 'false';
        {warn_not_called, {"src/kz_amqp_assignments.erl",_}, {unused_fun,[unregister_channel_handlers,1]}} -> 'false';
        {warn_not_called, {"src/kz_amqp_assignments.erl",_}, {unused_fun,[assign_consumer,3]}} -> 'false';
        {warn_not_called, {"src/kz_amqp_assignments.erl",_}, {unused_fun,[move_channel_to_consumer,2]}} -> 'false';
        {warn_not_called, {"src/kz_amqp_assignments.erl",_}, {unused_fun,[add_consumer_to_channel,3]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_}, {pattern_match,["pattern {[ExistingAssignment = {'kz_amqp_assignment', _, _, _, _, _, _, _, _, _, _, _}], _}",_]}} -> 'false';
        {warn_matching, {"src/kz_amqp_history.erl",_}, {pattern_match,["pattern <{[{'kz_amqp_history', Timestamp, _, Command}], Continuation}, NewTag>",_]}} -> 'false';

        {warn_matching, {"src/kazoo_amqp_maintenance.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', _, _, _, _, _, _, _, _, _, _, _}], Continuation}", _]}} -> 'false';
        {warn_not_called, {"src/kazoo_amqp_maintenance.erl",_}, {unused_fun,[log_invalid_assignment,1]}} -> 'false';

        {warn_matching, {"src/kazoo_amqp_maintenance.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', {_, _, _}, 'undefined', 'undefined', _, Channel, ChannelRef, Connection, <<_:8/integer-unit:1,_/binary-unit:8>>, 'undefined', _, _}], Continuation}",_]}} -> 'false';
        {warn_matching, {"src/kazoo_amqp_maintenance.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', {_, _, _}, Consumer, ConsumerRef, 'float', 'undefined', 'undefined', 'undefined', 'undefined', 'undefined', _, _}], Continuation}",_]}} -> 'false';
        {warn_matching, {"src/kazoo_amqp_maintenance.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', {_, _, _}, Consumer, ConsumerRef, 'sticky', 'undefined', 'undefined', 'undefined', <<_:8/integer-unit:1,_/binary-unit:8>>, 'undefined', _, _}], Continuation}",_]}} -> 'false';
        {warn_matching, {"src/kazoo_amqp_maintenance.erl",_}, {pattern_match,["pattern {[Assignment = {'kz_amqp_assignment', {_, _, _}, Consumer, ConsumerRef, _, Channel, ChannelRef, Connection, <<_:8/integer-unit:1,_/binary-unit:8>>, Assigned, _, _}], Continuation}",_]}} -> 'false';
        {warn_opaque, {"src/kazoo_amqp_maintenance.erl",_}, {call_without_opaque,_}} -> 'false';
        {warn_matching, {"src/kazoo_amqp_maintenance.erl",_}, {pattern_match,["pattern Assignment = {'kz_amqp_assignment', _, _, _, _, _, _, _, _, _, _, _}","[any()]"]}} -> 'false';
        {warn_matching, {"src/kazoo_amqp_maintenance.erl",_}, {pattern_match,["pattern <{[{'kz_amqp_connections', Connection, _, Broker, Available, _, Zone, _, _, _}], Continuation}, PrimaryBroker>",_]}} -> 'false';
        {warn_matching, {"src/kazoo_amqp_maintenance.erl",_}, {pattern_match,["pattern {'kz_amqp_assignment', _, _, _, _, _, _, _, _, _, _, _}","[any()]"]}} -> 'false';
        {warn_not_called, {"src/kazoo_amqp_maintenance.erl",_}, {unused_fun,[channel_summary_age,1]}} -> 'false';
        {warn_matching, {"src/kz_amqp_assignments.erl",_},{pattern_match,["pattern {'kz_amqp_assignment', Timestamp, _, _, _, _, _, _, _, _, _, Watchers}","{'error','no_channel'}"]}} -> 'false';

        %% More ETS false positives, from applications/
        {warn_matching, {"src/ecallmgr_fs_channels.erl",_}, {pattern_match,["pattern <{[Channel = {'channel', CallId, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _"++_,_]}} -> 'false';

        %% Dialyzer on 18 & 19 "such contracts are currently unsupported and are simply ignored"
        {warn_contract_types, _, {overlapping_contract,_}} -> 'false';

        _ ->
            %% io:format("W = ~p\n", [W]),
            'true'
    end.

print({Tag, _Loc, _Warning} = W) ->
    io:format("~-30.. s~s", [Tag, dialyzer:format_warning(W)]);
print(_Err) ->
    _Err.

scan(PLT, Thing) ->
    try do_scan(PLT, Thing) of
        Ret -> Ret
    catch 'throw':{'dialyzer_error',Error} ->
            io:format("~s\n", [Error]),
            []
    end.

do_scan(PLT, Path) ->
    io:format("scanning ~p\n", [Path]),
    dialyzer:run([ {'init_plt', PLT}
                 , {'analysis_type', 'succ_typings'}
                 %% , {'files_rec', [Path]}
                 , {'files', [Path]}
                 , {'warnings', [ 'no_undefined_callbacks'

                              , 'unmatched_returns'
                              , 'error_handling'  %% Warn about functions that only return with exception
                              , 'race_conditions'
                              %% , 'overspecs'  %% "… is a subtype of any()"
                              , 'underspecs'    %% Has issues for < R17
                              %% , 'specdiffs'  %% "… is a subtype of any()"

                              , 'no_return'  %% Suppress warnings for functions that will never return a value
                              %% , 'no_unused'
                              %% , 'no_improper_lists'
                              %% , 'no_fun_app'
                              %% , 'no_match'
                              %% , 'no_opaque'
                              %% , 'no_fail_call'
                              %% , 'no_contracts'
                              %% , 'no_behaviours'

                              ]}
                 ]).

usage() ->
    %% ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  <path to .kazoo.plt> <path to ebin/>+\n", [filename:basename(Arg0)]).

%% End of Module
