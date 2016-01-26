%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_misc).
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-export([method_record_type/1, polite_pause/0, polite_pause/1]).
-export([die/1, frame_error/2, amqp_error/4, quit/1,
         protocol_error/3, protocol_error/4, protocol_error/1]).
-export([not_found/1, absent/2]).
-export([type_class/1, assert_args_equivalence/4, assert_field_equivalence/4]).
-export([dirty_read/1]).
-export([table_lookup/2, set_table_value/4]).
-export([r/3, r/2, r_arg/4, rs/1]).
-export([enable_cover/0, report_cover/0]).
-export([enable_cover/1, report_cover/1]).
-export([start_cover/1]).
-export([confirm_to_sender/2]).
-export([throw_on_error/2, with_exit_handler/2, is_abnormal_exit/1,
         filter_exit_map/2]).
-export([with_user/2, with_user_and_vhost/3]).
-export([execute_mnesia_transaction/1]).
-export([execute_mnesia_transaction/2]).
-export([execute_mnesia_tx_with_tail/1]).
-export([ensure_ok/2]).
-export([tcp_name/3, format_inet_error/1]).
-export([upmap/2, map_in_order/2]).
-export([table_filter/3]).
-export([dirty_read_all/1, dirty_foreach_key/2, dirty_dump_log/1]).
-export([format/2, format_many/1, format_stderr/2]).
-export([unfold/2, ceil/1, queue_fold/3]).
-export([sort_field_table/1]).
-export([pid_to_string/1, string_to_pid/1,
         pid_change_node/2, node_to_fake_pid/1]).
-export([version_compare/2, version_compare/3]).
-export([version_minor_equivalent/2]).
-export([dict_cons/3, orddict_cons/3, gb_trees_cons/3]).
-export([gb_trees_fold/3, gb_trees_foreach/2]).
-export([all_module_attributes/1, build_acyclic_graph/3]).
-export([const/1]).
-export([ntoa/1, ntoab/1]).
-export([is_process_alive/1]).
-export([pget/2, pget/3, pget_or_die/2, pmerge/3, pset/3, plmerge/2]).
-export([format_message_queue/2]).
-export([append_rpc_all_nodes/4]).
-export([os_cmd/1]).
-export([is_os_process_alive/1]).
-export([gb_sets_difference/2]).
-export([version/0, otp_release/0, which_applications/0]).
-export([sequence_error/1]).
-export([json_encode/1, json_decode/1, json_to_term/1, term_to_json/1]).
-export([check_expiry/1]).
-export([base64url/1]).
-export([interval_operation/5]).
-export([ensure_timer/4, stop_timer/2, send_after/3, cancel_timer/1]).
-export([get_parent/0]).
-export([store_proc_name/1, store_proc_name/2]).
-export([moving_average/4]).
-export([get_env/3]).

%% Horrible macro to use in guards
-define(IS_BENIGN_EXIT(R),
        R =:= noproc; R =:= noconnection; R =:= nodedown; R =:= normal;
            R =:= shutdown).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-export_type([resource_name/0, thunk/1, channel_or_connection_exit/0]).

-type(ok_or_error() :: rabbit_types:ok_or_error(any())).
-type(thunk(T) :: fun(() -> T)).
-type(resource_name() :: binary()).
-type(channel_or_connection_exit()
      :: rabbit_types:channel_exit() | rabbit_types:connection_exit()).
-type(digraph_label() :: term()).
-type(graph_vertex_fun() ::
        fun (({atom(), [term()]}) -> [{digraph:vertex(), digraph_label()}])).
-type(graph_edge_fun() ::
        fun (({atom(), [term()]}) -> [{digraph:vertex(), digraph:vertex()}])).
-type(tref() :: {'erlang', reference()} | {timer, timer:tref()}).

-spec(method_record_type/1 :: (rabbit_framing:amqp_method_record())
                              -> rabbit_framing:amqp_method_name()).
-spec(polite_pause/0 :: () -> 'done').
-spec(polite_pause/1 :: (non_neg_integer()) -> 'done').
-spec(die/1 ::
        (rabbit_framing:amqp_exception()) -> channel_or_connection_exit()).

-spec(quit/1 :: (integer()) -> no_return()).

-spec(frame_error/2 :: (rabbit_framing:amqp_method_name(), binary())
                       -> rabbit_types:connection_exit()).
-spec(amqp_error/4 ::
        (rabbit_framing:amqp_exception(), string(), [any()],
         rabbit_framing:amqp_method_name())
        -> rabbit_types:amqp_error()).
-spec(protocol_error/3 :: (rabbit_framing:amqp_exception(), string(), [any()])
                          -> channel_or_connection_exit()).
-spec(protocol_error/4 ::
        (rabbit_framing:amqp_exception(), string(), [any()],
         rabbit_framing:amqp_method_name()) -> channel_or_connection_exit()).
-spec(protocol_error/1 ::
        (rabbit_types:amqp_error()) -> channel_or_connection_exit()).
-spec(not_found/1 :: (rabbit_types:r(atom())) -> rabbit_types:channel_exit()).
-spec(absent/2 :: (rabbit_types:amqqueue(), rabbit_amqqueue:absent_reason())
                  -> rabbit_types:channel_exit()).
-spec(type_class/1 :: (rabbit_framing:amqp_field_type()) -> atom()).
-spec(assert_args_equivalence/4 :: (rabbit_framing:amqp_table(),
                                    rabbit_framing:amqp_table(),
                                    rabbit_types:r(any()), [binary()]) ->
                                        'ok' | rabbit_types:connection_exit()).
-spec(assert_field_equivalence/4 ::
        (any(), any(), rabbit_types:r(any()), atom() | binary()) ->
                                         'ok' | rabbit_types:connection_exit()).
-spec(equivalence_fail/4 ::
        (any(), any(), rabbit_types:r(any()), atom() | binary()) ->
                                 rabbit_types:connection_exit()).
-spec(dirty_read/1 ::
        ({atom(), any()}) -> rabbit_types:ok_or_error2(any(), 'not_found')).
-spec(table_lookup/2 ::
        (rabbit_framing:amqp_table(), binary())
        -> 'undefined' | {rabbit_framing:amqp_field_type(), any()}).
-spec(set_table_value/4 ::
        (rabbit_framing:amqp_table(), binary(),
         rabbit_framing:amqp_field_type(), rabbit_framing:amqp_value())
        -> rabbit_framing:amqp_table()).
-spec(r/2 :: (rabbit_types:vhost(), K)
             -> rabbit_types:r3(rabbit_types:vhost(), K, '_')
                    when is_subtype(K, atom())).
-spec(r/3 ::
        (rabbit_types:vhost() | rabbit_types:r(atom()), K, resource_name())
        -> rabbit_types:r3(rabbit_types:vhost(), K, resource_name())
               when is_subtype(K, atom())).
-spec(r_arg/4 ::
        (rabbit_types:vhost() | rabbit_types:r(atom()), K,
         rabbit_framing:amqp_table(), binary()) ->
                      undefined |
                      rabbit_types:error(
                        {invalid_type, rabbit_framing:amqp_field_type()}) |
                      rabbit_types:r(K) when is_subtype(K, atom())).
-spec(rs/1 :: (rabbit_types:r(atom())) -> string()).
-spec(enable_cover/0 :: () -> ok_or_error()).
-spec(start_cover/1 :: ([{string(), string()} | string()]) -> 'ok').
-spec(report_cover/0 :: () -> 'ok').
-spec(enable_cover/1 :: ([file:filename() | atom()]) -> ok_or_error()).
-spec(report_cover/1 :: ([file:filename() | atom()]) -> 'ok').
-spec(throw_on_error/2 ::
        (atom(), thunk(rabbit_types:error(any()) | {ok, A} | A)) -> A).
-spec(with_exit_handler/2 :: (thunk(A), thunk(A)) -> A).
-spec(is_abnormal_exit/1 :: (any()) -> boolean()).
-spec(filter_exit_map/2 :: (fun ((A) -> B), [A]) -> [B]).
-spec(with_user/2 :: (rabbit_types:username(), thunk(A)) -> A).
-spec(with_user_and_vhost/3 ::
        (rabbit_types:username(), rabbit_types:vhost(), thunk(A))
        -> A).
-spec(execute_mnesia_transaction/1 :: (thunk(A)) -> A).
-spec(execute_mnesia_transaction/2 ::
        (thunk(A), fun ((A, boolean()) -> B)) -> B).
-spec(execute_mnesia_tx_with_tail/1 ::
        (thunk(fun ((boolean()) -> B))) -> B | (fun ((boolean()) -> B))).
-spec(ensure_ok/2 :: (ok_or_error(), atom()) -> 'ok').
-spec(tcp_name/3 ::
        (atom(), inet:ip_address(), rabbit_networking:ip_port())
        -> atom()).
-spec(format_inet_error/1 :: (atom()) -> string()).
-spec(upmap/2 :: (fun ((A) -> B), [A]) -> [B]).
-spec(map_in_order/2 :: (fun ((A) -> B), [A]) -> [B]).
-spec(table_filter/3:: (fun ((A) -> boolean()), fun ((A, boolean()) -> 'ok'),
                                                    atom()) -> [A]).
-spec(dirty_read_all/1 :: (atom()) -> [any()]).
-spec(dirty_foreach_key/2 :: (fun ((any()) -> any()), atom())
                             -> 'ok' | 'aborted').
-spec(dirty_dump_log/1 :: (file:filename()) -> ok_or_error()).
-spec(format/2 :: (string(), [any()]) -> string()).
-spec(format_many/1 :: ([{string(), [any()]}]) -> string()).
-spec(format_stderr/2 :: (string(), [any()]) -> 'ok').
-spec(unfold/2  :: (fun ((A) -> ({'true', B, A} | 'false')), A) -> {[B], A}).
-spec(ceil/1 :: (number()) -> integer()).
-spec(queue_fold/3 :: (fun ((any(), B) -> B), B, queue:queue()) -> B).
-spec(sort_field_table/1 ::
        (rabbit_framing:amqp_table()) -> rabbit_framing:amqp_table()).
-spec(pid_to_string/1 :: (pid()) -> string()).
-spec(string_to_pid/1 :: (string()) -> pid()).
-spec(pid_change_node/2 :: (pid(), node()) -> pid()).
-spec(node_to_fake_pid/1 :: (atom()) -> pid()).
-spec(version_compare/2 :: (string(), string()) -> 'lt' | 'eq' | 'gt').
-spec(version_compare/3 ::
        (string(), string(), ('lt' | 'lte' | 'eq' | 'gte' | 'gt'))
        -> boolean()).
-spec(version_minor_equivalent/2 :: (string(), string()) -> boolean()).
-spec(dict_cons/3 :: (any(), any(), dict:dict()) -> dict:dict()).
-spec(orddict_cons/3 :: (any(), any(), orddict:orddict()) -> orddict:orddict()).
-spec(gb_trees_cons/3 :: (any(), any(), gb_trees:tree()) -> gb_trees:tree()).
-spec(gb_trees_fold/3 :: (fun ((any(), any(), A) -> A), A, gb_trees:tree())
 -> A).
-spec(gb_trees_foreach/2 ::
        (fun ((any(), any()) -> any()), gb_trees:tree()) -> 'ok').
-spec(all_module_attributes/1 ::
        (atom()) -> [{atom(), atom(), [term()]}]).
-spec(build_acyclic_graph/3 ::
        (graph_vertex_fun(), graph_edge_fun(), [{atom(), [term()]}])
        -> rabbit_types:ok_or_error2(digraph:graph(),
                                     {'vertex', 'duplicate', digraph:vertex()} |
                                     {'edge', ({bad_vertex, digraph:vertex()} |
                                               {bad_edge, [digraph:vertex()]}),
                                      digraph:vertex(), digraph:vertex()})).
-spec(const/1 :: (A) -> thunk(A)).
-spec(ntoa/1 :: (inet:ip_address()) -> string()).
-spec(ntoab/1 :: (inet:ip_address()) -> string()).
-spec(is_process_alive/1 :: (pid()) -> boolean()).
-spec(pget/2 :: (term(), [term()]) -> term()).
-spec(pget/3 :: (term(), [term()], term()) -> term()).
-spec(pget_or_die/2 :: (term(), [term()]) -> term() | no_return()).
-spec(pmerge/3 :: (term(), term(), [term()]) -> [term()]).
-spec(plmerge/2 :: ([term()], [term()]) -> [term()]).
-spec(pset/3 :: (term(), term(), [term()]) -> [term()]).
-spec(format_message_queue/2 :: (any(), priority_queue:q()) -> term()).
-spec(append_rpc_all_nodes/4 :: ([node()], atom(), atom(), [any()]) -> [any()]).
-spec(os_cmd/1 :: (string()) -> string()).
-spec(is_os_process_alive/1 :: (non_neg_integer()) -> boolean()).
-spec(gb_sets_difference/2 :: (gb_sets:set(), gb_sets:set()) -> gb_sets:set()).
-spec(version/0 :: () -> string()).
-spec(otp_release/0 :: () -> string()).
-spec(which_applications/0 :: () -> [{atom(), string(), string()}]).
-spec(sequence_error/1 :: ([({'error', any()} | any())])
                       -> {'error', any()} | any()).
-spec(json_encode/1 :: (any()) -> {'ok', string()} | {'error', any()}).
-spec(json_decode/1 :: (string()) -> {'ok', any()} | 'error').
-spec(json_to_term/1 :: (any()) -> any()).
-spec(term_to_json/1 :: (any()) -> any()).
-spec(check_expiry/1 :: (integer()) -> rabbit_types:ok_or_error(any())).
-spec(base64url/1 :: (binary()) -> string()).
-spec(interval_operation/5 ::
        ({atom(), atom(), any()}, float(), non_neg_integer(), non_neg_integer(), non_neg_integer())
        -> {any(), non_neg_integer()}).
-spec(ensure_timer/4 :: (A, non_neg_integer(), non_neg_integer(), any()) -> A).
-spec(stop_timer/2 :: (A, non_neg_integer()) -> A).
-spec(send_after/3 :: (non_neg_integer(), pid(), any()) -> tref()).
-spec(cancel_timer/1 :: (tref()) -> 'ok').
-spec(get_parent/0 :: () -> pid()).
-spec(store_proc_name/2 :: (atom(), rabbit_types:proc_name()) -> ok).
-spec(store_proc_name/1 :: (rabbit_types:proc_type_and_name()) -> ok).
-spec(moving_average/4 :: (float(), float(), float(), float() | 'undefined')
                          -> float()).
-spec(get_env/3 :: (atom(), atom(), term())  -> term()).
-endif.

%%----------------------------------------------------------------------------

method_record_type(Record) ->
    element(1, Record).

polite_pause() ->
    polite_pause(3000).

polite_pause(N) ->
    receive
    after N -> done
    end.

die(Error) ->
    protocol_error(Error, "~w", [Error]).

frame_error(MethodName, BinaryFields) ->
    protocol_error(frame_error, "cannot decode ~w", [BinaryFields], MethodName).

amqp_error(Name, ExplanationFormat, Params, Method) ->
    Explanation = format(ExplanationFormat, Params),
    #amqp_error{name = Name, explanation = Explanation, method = Method}.

protocol_error(Name, ExplanationFormat, Params) ->
    protocol_error(Name, ExplanationFormat, Params, none).

protocol_error(Name, ExplanationFormat, Params, Method) ->
    protocol_error(amqp_error(Name, ExplanationFormat, Params, Method)).

protocol_error(#amqp_error{} = Error) ->
    exit(Error).

not_found(R) -> protocol_error(not_found, "no ~s", [rs(R)]).

absent(#amqqueue{name = QueueName, pid = QPid, durable = true}, nodedown) ->
    %% The assertion of durability is mainly there because we mention
    %% durability in the error message. That way we will hopefully
    %% notice if at some future point our logic changes s.t. we get
    %% here with non-durable queues.
    protocol_error(not_found,
                   "home node '~s' of durable ~s is down or inaccessible",
                   [node(QPid), rs(QueueName)]);

absent(#amqqueue{name = QueueName}, crashed) ->
    protocol_error(not_found,
                   "~s has crashed and failed to restart", [rs(QueueName)]).

type_class(byte)          -> int;
type_class(short)         -> int;
type_class(signedint)     -> int;
type_class(long)          -> int;
type_class(decimal)       -> int;
type_class(unsignedbyte)  -> int;
type_class(unsignedshort) -> int;
type_class(unsignedint)   -> int;
type_class(float)         -> float;
type_class(double)        -> float;
type_class(Other)         -> Other.

assert_args_equivalence(Orig, New, Name, Keys) ->
    [assert_args_equivalence1(Orig, New, Name, Key) || Key <- Keys],
    ok.

assert_args_equivalence1(Orig, New, Name, Key) ->
    {Orig1, New1} = {table_lookup(Orig, Key), table_lookup(New, Key)},
    case {Orig1, New1} of
        {Same, Same} ->
            ok;
        {{OrigType, OrigVal}, {NewType, NewVal}} ->
            case type_class(OrigType) == type_class(NewType) andalso
                 OrigVal == NewVal of
                 true  -> ok;
                 false -> assert_field_equivalence(OrigVal, NewVal, Name, Key)
            end;
        {OrigTypeVal, NewTypeVal} ->
            assert_field_equivalence(OrigTypeVal, NewTypeVal, Name, Key)
    end.

assert_field_equivalence(_Orig, _Orig, _Name, _Key) ->
    ok;
assert_field_equivalence(Orig, New, Name, Key) ->
    equivalence_fail(Orig, New, Name, Key).

equivalence_fail(Orig, New, Name, Key) ->
    protocol_error(precondition_failed, "inequivalent arg '~s' "
                   "for ~s: received ~s but current is ~s",
                   [Key, rs(Name), val(New), val(Orig)]).

val(undefined) ->
    "none";
val({Type, Value}) ->
    ValFmt = case is_binary(Value) of
                 true  -> "~s";
                 false -> "~p"
             end,
    format("the value '" ++ ValFmt ++ "' of type '~s'", [Value, Type]);
val(Value) ->
    format(case is_binary(Value) of
               true  -> "'~s'";
               false -> "'~p'"
           end, [Value]).

%% Normally we'd call mnesia:dirty_read/1 here, but that is quite
%% expensive due to general mnesia overheads (figuring out table types
%% and locations, etc). We get away with bypassing these because we
%% know that the tables we are looking at here
%% - are not the schema table
%% - have a local ram copy
%% - do not have any indices
dirty_read({Table, Key}) ->
    case ets:lookup(Table, Key) of
        [Result] -> {ok, Result};
        []       -> {error, not_found}
    end.

table_lookup(Table, Key) ->
    case lists:keysearch(Key, 1, Table) of
        {value, {_, TypeBin, ValueBin}} -> {TypeBin, ValueBin};
        false                           -> undefined
    end.

set_table_value(Table, Key, Type, Value) ->
    sort_field_table(
      lists:keystore(Key, 1, Table, {Key, Type, Value})).

r(#resource{virtual_host = VHostPath}, Kind, Name) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = Name};
r(VHostPath, Kind, Name) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = Name}.

r(VHostPath, Kind) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = '_'}.

r_arg(#resource{virtual_host = VHostPath}, Kind, Table, Key) ->
    r_arg(VHostPath, Kind, Table, Key);
r_arg(VHostPath, Kind, Table, Key) ->
    case table_lookup(Table, Key) of
        {longstr, NameBin} -> r(VHostPath, Kind, NameBin);
        undefined          -> undefined;
        {Type, _}          -> {error, {invalid_type, Type}}
    end.

rs(#resource{virtual_host = VHostPath, kind = Kind, name = Name}) ->
    format("~s '~s' in vhost '~s'", [Kind, Name, VHostPath]).

enable_cover() -> enable_cover(["."]).

enable_cover(Dirs) ->
    lists:foldl(fun (Dir, ok) ->
                        case cover:compile_beam_directory(
                               filename:join(lists:concat([Dir]),"ebin")) of
                            {error, _} = Err -> Err;
                            _                -> ok
                        end;
                    (_Dir, Err) ->
                        Err
                end, ok, Dirs).

start_cover(NodesS) ->
    {ok, _} = cover:start([rabbit_nodes:make(N) || N <- NodesS]),
    ok.

report_cover() -> report_cover(["."]).

report_cover(Dirs) -> [report_cover1(lists:concat([Dir])) || Dir <- Dirs], ok.

report_cover1(Root) ->
    Dir = filename:join(Root, "cover"),
    ok = filelib:ensure_dir(filename:join(Dir, "junk")),
    lists:foreach(fun (F) -> file:delete(F) end,
                  filelib:wildcard(filename:join(Dir, "*.html"))),
    {ok, SummaryFile} = file:open(filename:join(Dir, "summary.txt"), [write]),
    {CT, NCT} =
        lists:foldl(
          fun (M,{CovTot, NotCovTot}) ->
                  {ok, {M, {Cov, NotCov}}} = cover:analyze(M, module),
                  ok = report_coverage_percentage(SummaryFile,
                                                  Cov, NotCov, M),
                  {ok,_} = cover:analyze_to_file(
                             M,
                             filename:join(Dir, atom_to_list(M) ++ ".html"),
                             [html]),
                  {CovTot+Cov, NotCovTot+NotCov}
          end,
          {0, 0},
          lists:sort(cover:modules())),
    ok = report_coverage_percentage(SummaryFile, CT, NCT, 'TOTAL'),
    ok = file:close(SummaryFile),
    ok.

report_coverage_percentage(File, Cov, NotCov, Mod) ->
    io:fwrite(File, "~6.2f ~p~n",
              [if
                   Cov+NotCov > 0 -> 100.0*Cov/(Cov+NotCov);
                   true -> 100.0
               end,
               Mod]).

confirm_to_sender(Pid, MsgSeqNos) ->
    gen_server2:cast(Pid, {confirm, MsgSeqNos, self()}).

%% @doc Halts the emulator returning the given status code to the os.
%% On Windows this function will block indefinitely so as to give the io
%% subsystem time to flush stdout completely.
quit(Status) ->
    case os:type() of
        {unix,  _} -> halt(Status);
        {win32, _} -> init:stop(Status),
                      receive
                      after infinity -> ok
                      end
    end.

throw_on_error(E, Thunk) ->
    case Thunk() of
        {error, Reason} -> throw({E, Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

with_exit_handler(Handler, Thunk) ->
    try
        Thunk()
    catch
        exit:{R, _}      when ?IS_BENIGN_EXIT(R) -> Handler();
        exit:{{R, _}, _} when ?IS_BENIGN_EXIT(R) -> Handler()
    end.

is_abnormal_exit(R)      when ?IS_BENIGN_EXIT(R) -> false;
is_abnormal_exit({R, _}) when ?IS_BENIGN_EXIT(R) -> false;
is_abnormal_exit(_)                              -> true.

filter_exit_map(F, L) ->
    Ref = make_ref(),
    lists:filter(fun (R) -> R =/= Ref end,
                 [with_exit_handler(
                    fun () -> Ref end,
                    fun () -> F(I) end) || I <- L]).


with_user(Username, Thunk) ->
    fun () ->
            case mnesia:read({rabbit_user, Username}) of
                [] ->
                    mnesia:abort({no_such_user, Username});
                [_U] ->
                    Thunk()
            end
    end.

with_user_and_vhost(Username, VHostPath, Thunk) ->
    with_user(Username, rabbit_vhost:with(VHostPath, Thunk)).

execute_mnesia_transaction(TxFun) ->
    %% Making this a sync_transaction allows us to use dirty_read
    %% elsewhere and get a consistent result even when that read
    %% executes on a different node.
    case worker_pool:submit(
           fun () ->
                   case mnesia:is_transaction() of
                       false -> DiskLogBefore = mnesia_dumper:get_log_writes(),
                                Res = mnesia:sync_transaction(TxFun),
                                DiskLogAfter  = mnesia_dumper:get_log_writes(),
                                case DiskLogAfter == DiskLogBefore of
                                    true  -> file_handle_cache_stats:update(
                                              mnesia_ram_tx),
                                             Res;
                                    false -> file_handle_cache_stats:update(
                                              mnesia_disk_tx),
                                             {sync, Res}
                                end;
                       true  -> mnesia:sync_transaction(TxFun)
                   end
           end, single) of
        {sync, {atomic,  Result}} -> mnesia_sync:sync(), Result;
        {sync, {aborted, Reason}} -> throw({error, Reason});
        {atomic,  Result}         -> Result;
        {aborted, Reason}         -> throw({error, Reason})
    end.

%% Like execute_mnesia_transaction/1 with additional Pre- and Post-
%% commit function
execute_mnesia_transaction(TxFun, PrePostCommitFun) ->
    case mnesia:is_transaction() of
        true  -> throw(unexpected_transaction);
        false -> ok
    end,
    PrePostCommitFun(execute_mnesia_transaction(
                       fun () ->
                               Result = TxFun(),
                               PrePostCommitFun(Result, true),
                               Result
                       end), false).

%% Like execute_mnesia_transaction/2, but TxFun is expected to return a
%% TailFun which gets called (only) immediately after the tx commit
execute_mnesia_tx_with_tail(TxFun) ->
    case mnesia:is_transaction() of
        true  -> execute_mnesia_transaction(TxFun);
        false -> TailFun = execute_mnesia_transaction(TxFun),
                 TailFun()
    end.

ensure_ok(ok, _) -> ok;
ensure_ok({error, Reason}, ErrorTag) -> throw({error, {ErrorTag, Reason}}).

tcp_name(Prefix, IPAddress, Port)
  when is_atom(Prefix) andalso is_number(Port) ->
    list_to_atom(
      format("~w_~s:~w", [Prefix, inet_parse:ntoa(IPAddress), Port])).

format_inet_error(E) -> format("~w (~s)", [E, format_inet_error0(E)]).

format_inet_error0(address) -> "cannot connect to host/port";
format_inet_error0(timeout) -> "timed out";
format_inet_error0(Error)   -> inet:format_error(Error).

%% This is a modified version of Luke Gorrie's pmap -
%% http://lukego.livejournal.com/6753.html - that doesn't care about
%% the order in which results are received.
%%
%% WARNING: This is is deliberately lightweight rather than robust -- if F
%% throws, upmap will hang forever, so make sure F doesn't throw!
upmap(F, L) ->
    Parent = self(),
    Ref = make_ref(),
    [receive {Ref, Result} -> Result end
     || _ <- [spawn(fun () -> Parent ! {Ref, F(X)} end) || X <- L]].

map_in_order(F, L) ->
    lists:reverse(
      lists:foldl(fun (E, Acc) -> [F(E) | Acc] end, [], L)).

%% Apply a pre-post-commit function to all entries in a table that
%% satisfy a predicate, and return those entries.
%%
%% We ignore entries that have been modified or removed.
table_filter(Pred, PrePostCommitFun, TableName) ->
    lists:foldl(
      fun (E, Acc) ->
              case execute_mnesia_transaction(
                     fun () -> mnesia:match_object(TableName, E, read) =/= []
                                   andalso Pred(E) end,
                     fun (false, _Tx) -> false;
                         (true,   Tx) -> PrePostCommitFun(E, Tx), true
                     end) of
                  false -> Acc;
                  true  -> [E | Acc]
              end
      end, [], dirty_read_all(TableName)).

dirty_read_all(TableName) ->
    mnesia:dirty_select(TableName, [{'$1',[],['$1']}]).

dirty_foreach_key(F, TableName) ->
    dirty_foreach_key1(F, TableName, mnesia:dirty_first(TableName)).

dirty_foreach_key1(_F, _TableName, '$end_of_table') ->
    ok;
dirty_foreach_key1(F, TableName, K) ->
    case catch mnesia:dirty_next(TableName, K) of
        {'EXIT', _} ->
            aborted;
        NextKey ->
            F(K),
            dirty_foreach_key1(F, TableName, NextKey)
    end.

dirty_dump_log(FileName) ->
    {ok, LH} = disk_log:open([{name, dirty_dump_log},
                              {mode, read_only},
                              {file, FileName}]),
    dirty_dump_log1(LH, disk_log:chunk(LH, start)),
    disk_log:close(LH).

dirty_dump_log1(_LH, eof) ->
    io:format("Done.~n");
dirty_dump_log1(LH, {K, Terms}) ->
    io:format("Chunk: ~p~n", [Terms]),
    dirty_dump_log1(LH, disk_log:chunk(LH, K));
dirty_dump_log1(LH, {K, Terms, BadBytes}) ->
    io:format("Bad Chunk, ~p: ~p~n", [BadBytes, Terms]),
    dirty_dump_log1(LH, disk_log:chunk(LH, K)).

format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

format_many(List) ->
    lists:flatten([io_lib:format(F ++ "~n", A) || {F, A} <- List]).

format_stderr(Fmt, Args) ->
    case os:type() of
        {unix, _} ->
            Port = open_port({fd, 0, 2}, [out]),
            port_command(Port, io_lib:format(Fmt, Args)),
            port_close(Port);
        {win32, _} ->
            %% stderr on Windows is buffered and I can't figure out a
            %% way to trigger a fflush(stderr) in Erlang. So rather
            %% than risk losing output we write to stdout instead,
            %% which appears to be unbuffered.
            io:format(Fmt, Args)
    end,
    ok.

unfold(Fun, Init) ->
    unfold(Fun, [], Init).

unfold(Fun, Acc, Init) ->
    case Fun(Init) of
        {true, E, I} -> unfold(Fun, [E|Acc], I);
        false -> {Acc, Init}
    end.

ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

queue_fold(Fun, Init, Q) ->
    case queue:out(Q) of
        {empty, _Q}      -> Init;
        {{value, V}, Q1} -> queue_fold(Fun, Fun(V, Init), Q1)
    end.

%% Sorts a list of AMQP table fields as per the AMQP spec
sort_field_table(Arguments) ->
    lists:keysort(1, Arguments).

%% This provides a string representation of a pid that is the same
%% regardless of what node we are running on. The representation also
%% permits easy identification of the pid's node.
pid_to_string(Pid) when is_pid(Pid) ->
    {Node, Cre, Id, Ser} = decompose_pid(Pid),
    format("<~s.~B.~B.~B>", [Node, Cre, Id, Ser]).

%% inverse of above
string_to_pid(Str) ->
    Err = {error, {invalid_pid_syntax, Str}},
    %% The \ before the trailing $ is only there to keep emacs
    %% font-lock from getting confused.
    case re:run(Str, "^<(.*)\\.(\\d+)\\.(\\d+)\\.(\\d+)>\$",
                [{capture,all_but_first,list}]) of
        {match, [NodeStr, CreStr, IdStr, SerStr]} ->
            [Cre, Id, Ser] = lists:map(fun list_to_integer/1,
                                       [CreStr, IdStr, SerStr]),
            compose_pid(list_to_atom(NodeStr), Cre, Id, Ser);
        nomatch ->
            throw(Err)
    end.

pid_change_node(Pid, NewNode) ->
    {_OldNode, Cre, Id, Ser} = decompose_pid(Pid),
    compose_pid(NewNode, Cre, Id, Ser).

%% node(node_to_fake_pid(Node)) =:= Node.
node_to_fake_pid(Node) ->
    compose_pid(Node, 0, 0, 0).

decompose_pid(Pid) when is_pid(Pid) ->
    %% see http://erlang.org/doc/apps/erts/erl_ext_dist.html (8.10 and
    %% 8.7)
    <<131,103,100,NodeLen:16,NodeBin:NodeLen/binary,Id:32,Ser:32,Cre:8>>
        = term_to_binary(Pid),
    Node = binary_to_term(<<131,100,NodeLen:16,NodeBin:NodeLen/binary>>),
    {Node, Cre, Id, Ser}.

compose_pid(Node, Cre, Id, Ser) ->
    <<131,NodeEnc/binary>> = term_to_binary(Node),
    binary_to_term(<<131,103,NodeEnc/binary,Id:32,Ser:32,Cre:8>>).

version_compare(A, B, lte) ->
    case version_compare(A, B) of
        eq -> true;
        lt -> true;
        gt -> false
    end;
version_compare(A, B, gte) ->
    case version_compare(A, B) of
        eq -> true;
        gt -> true;
        lt -> false
    end;
version_compare(A, B, Result) ->
    Result =:= version_compare(A, B).

version_compare(A, A) ->
    eq;
version_compare([], [$0 | B]) ->
    version_compare([], dropdot(B));
version_compare([], _) ->
    lt; %% 2.3 < 2.3.1
version_compare([$0 | A], []) ->
    version_compare(dropdot(A), []);
version_compare(_, []) ->
    gt; %% 2.3.1 > 2.3
version_compare(A,  B) ->
    {AStr, ATl} = lists:splitwith(fun (X) -> X =/= $. end, A),
    {BStr, BTl} = lists:splitwith(fun (X) -> X =/= $. end, B),
    ANum = list_to_integer(AStr),
    BNum = list_to_integer(BStr),
    if ANum =:= BNum -> version_compare(dropdot(ATl), dropdot(BTl));
       ANum < BNum   -> lt;
       ANum > BNum   -> gt
    end.

%% a.b.c and a.b.d match, but a.b.c and a.d.e don't. If
%% versions do not match that pattern, just compare them.
version_minor_equivalent(A, B) ->
    {ok, RE} = re:compile("^(\\d+\\.\\d+)(\\.\\d+)\$"),
    Opts = [{capture, all_but_first, list}],
    case {re:run(A, RE, Opts), re:run(B, RE, Opts)} of
        {{match, [A1|_]}, {match, [B1|_]}} -> A1 =:= B1;
        _                                  -> A =:= B
    end.

dropdot(A) -> lists:dropwhile(fun (X) -> X =:= $. end, A).

dict_cons(Key, Value, Dict) ->
    dict:update(Key, fun (List) -> [Value | List] end, [Value], Dict).

orddict_cons(Key, Value, Dict) ->
    orddict:update(Key, fun (List) -> [Value | List] end, [Value], Dict).

gb_trees_cons(Key, Value, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Values} -> gb_trees:update(Key, [Value | Values], Tree);
        none            -> gb_trees:insert(Key, [Value], Tree)
    end.

gb_trees_fold(Fun, Acc, Tree) ->
    gb_trees_fold1(Fun, Acc, gb_trees:next(gb_trees:iterator(Tree))).

gb_trees_fold1(_Fun, Acc, none) ->
    Acc;
gb_trees_fold1(Fun, Acc, {Key, Val, It}) ->
    gb_trees_fold1(Fun, Fun(Key, Val, Acc), gb_trees:next(It)).

gb_trees_foreach(Fun, Tree) ->
    gb_trees_fold(fun (Key, Val, Acc) -> Fun(Key, Val), Acc end, ok, Tree).

module_attributes(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', {undef, [{Module, module_info, _} | _]}} ->
            io:format("WARNING: module ~p not found, so not scanned for boot steps.~n",
                      [Module]),
            [];
        {'EXIT', Reason} ->
            exit(Reason);
        V ->
            V
    end.

all_module_attributes(Name) ->
    Targets =
        lists:usort(
          lists:append(
            [[{App, Module} || Module <- Modules] ||
                {App, _, _}   <- application:loaded_applications(),
                {ok, Modules} <- [application:get_key(App, modules)]])),
    lists:foldl(
      fun ({App, Module}, Acc) ->
              case lists:append([Atts || {N, Atts} <- module_attributes(Module),
                                         N =:= Name]) of
                  []   -> Acc;
                  Atts -> [{App, Module, Atts} | Acc]
              end
      end, [], Targets).

build_acyclic_graph(VertexFun, EdgeFun, Graph) ->
    G = digraph:new([acyclic]),
    try
        [case digraph:vertex(G, Vertex) of
             false -> digraph:add_vertex(G, Vertex, Label);
             _     -> ok = throw({graph_error, {vertex, duplicate, Vertex}})
         end || GraphElem       <- Graph,
                {Vertex, Label} <- VertexFun(GraphElem)],
        [case digraph:add_edge(G, From, To) of
             {error, E} -> throw({graph_error, {edge, E, From, To}});
             _          -> ok
         end || GraphElem  <- Graph,
                {From, To} <- EdgeFun(GraphElem)],
        {ok, G}
    catch {graph_error, Reason} ->
            true = digraph:delete(G),
            {error, Reason}
    end.

const(X) -> fun () -> X end.

%% Format IPv4-mapped IPv6 addresses as IPv4, since they're what we see
%% when IPv6 is enabled but not used (i.e. 99% of the time).
ntoa({0,0,0,0,0,16#ffff,AB,CD}) ->
    inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256});
ntoa(IP) ->
    inet_parse:ntoa(IP).

ntoab(IP) ->
    Str = ntoa(IP),
    case string:str(Str, ":") of
        0 -> Str;
        _ -> "[" ++ Str ++ "]"
    end.

%% We try to avoid reconnecting to down nodes here; this is used in a
%% loop in rabbit_amqqueue:on_node_down/1 and any delays we incur
%% would be bad news.
%%
%% See also rabbit_mnesia:is_process_alive/1 which also requires the
%% process be in the same running cluster as us (i.e. not partitioned
%% or some random node).
is_process_alive(Pid) ->
    Node = node(Pid),
    lists:member(Node, [node() | nodes()]) andalso
        rpc:call(Node, erlang, is_process_alive, [Pid]) =:= true.

pget(K, P) -> proplists:get_value(K, P).
pget(K, P, D) -> proplists:get_value(K, P, D).

pget_or_die(K, P) ->
    case proplists:get_value(K, P) of
        undefined -> exit({error, key_missing, K});
        V         -> V
    end.

%% property merge 
pmerge(Key, Val, List) ->
      case proplists:is_defined(Key, List) of
              true -> List;
              _    -> [{Key, Val} | List]
      end.

%% proplists merge
plmerge(P1, P2) ->
    dict:to_list(dict:merge(fun(_, V, _) ->
                                V 
                            end, 
                            dict:from_list(P1), 
                            dict:from_list(P2))).

pset(Key, Value, List) -> [{Key, Value} | proplists:delete(Key, List)].

format_message_queue(_Opt, MQ) ->
    Len = priority_queue:len(MQ),
    {Len,
     case Len > 100 of
         false -> priority_queue:to_list(MQ);
         true  -> {summary,
                   orddict:to_list(
                     lists:foldl(
                       fun ({P, V}, Counts) ->
                               orddict:update_counter(
                                 {P, format_message_queue_entry(V)}, 1, Counts)
                       end, orddict:new(), priority_queue:to_list(MQ)))}
     end}.

format_message_queue_entry(V) when is_atom(V) ->
    V;
format_message_queue_entry(V) when is_tuple(V) ->
    list_to_tuple([format_message_queue_entry(E) || E <- tuple_to_list(V)]);
format_message_queue_entry(_V) ->
    '_'.

append_rpc_all_nodes(Nodes, M, F, A) ->
    {ResL, _} = rpc:multicall(Nodes, M, F, A),
    lists:append([case Res of
                      {badrpc, _} -> [];
                      _           -> Res
                  end || Res <- ResL]).

os_cmd(Command) ->
    case os:type() of
        {win32, _} ->
            %% Clink workaround; see
            %% http://code.google.com/p/clink/issues/detail?id=141
            os:cmd(" " ++ Command);
        _ ->
            %% Don't just return "/bin/sh: <cmd>: not found" if not found
            Exec = hd(string:tokens(Command, " ")),
            case os:find_executable(Exec) of
                false -> throw({command_not_found, Exec});
                _     -> os:cmd(Command)
            end
    end.

is_os_process_alive(Pid) ->
    with_os([{unix, fun () ->
                            run_ps(Pid) =:= 0
                    end},
             {win32, fun () ->
                             Cmd = "tasklist /nh /fi \"pid eq " ++ Pid ++ "\" ",
                             Res = os_cmd(Cmd ++ "2>&1"),
                             case re:run(Res, "erl\\.exe", [{capture, none}]) of
                                 match -> true;
                                 _     -> false
                             end
                     end}]).

with_os(Handlers) ->
    {OsFamily, _} = os:type(),
    case proplists:get_value(OsFamily, Handlers) of
        undefined -> throw({unsupported_os, OsFamily});
        Handler   -> Handler()
    end.

run_ps(Pid) ->
    Port = erlang:open_port({spawn, "ps -p " ++ Pid},
                            [exit_status, {line, 16384},
                             use_stdio, stderr_to_stdout]),
    exit_loop(Port).

exit_loop(Port) ->
    receive
        {Port, {exit_status, Rc}} -> Rc;
        {Port, _}                 -> exit_loop(Port)
    end.

gb_sets_difference(S1, S2) ->
    gb_sets:fold(fun gb_sets:delete_any/2, S1, S2).

version() ->
    {ok, VSN} = application:get_key(rabbit, vsn),
    VSN.

%% See http://www.erlang.org/doc/system_principles/versions.html
otp_release() ->
    File = filename:join([code:root_dir(), "releases",
                          erlang:system_info(otp_release), "OTP_VERSION"]),
    case file:read_file(File) of
        {ok, VerBin} ->
            %% 17.0 or later, we need the file for the minor version
            string:strip(binary_to_list(VerBin), both, $\n);
        {error, _} ->
            %% R16B03 or earlier (no file, otp_release is correct)
            %% or we couldn't read the file (so this is best we can do)
            erlang:system_info(otp_release)
    end.

%% application:which_applications(infinity) is dangerous, since it can
%% cause deadlocks on shutdown. So we have to use a timeout variant,
%% but w/o creating spurious timeout errors.
which_applications() ->
    try
        application:which_applications()
    catch
        exit:{timeout, _} -> []
    end.

sequence_error([T])                      -> T;
sequence_error([{error, _} = Error | _]) -> Error;
sequence_error([_ | Rest])               -> sequence_error(Rest).

json_encode(Term) ->
    try
        {ok, mochijson2:encode(Term)}
    catch
        exit:{json_encode, E} ->
            {error, E}
    end.

json_decode(Term) ->
    try
        {ok, mochijson2:decode(Term)}
    catch
        %% Sadly `mochijson2:decode/1' does not offer a nice way to catch
        %% decoding errors...
        error:_ -> error
    end.

json_to_term({struct, L}) ->
    [{K, json_to_term(V)} || {K, V} <- L];
json_to_term(L) when is_list(L) ->
    [json_to_term(I) || I <- L];
json_to_term(V) when is_binary(V) orelse is_number(V) orelse V =:= null orelse
                     V =:= true orelse V =:= false ->
    V.

%% This has the flaw that empty lists will never be JSON objects, so use with
%% care.
term_to_json([{_, _}|_] = L) ->
    {struct, [{K, term_to_json(V)} || {K, V} <- L]};
term_to_json(L) when is_list(L) ->
    [term_to_json(I) || I <- L];
term_to_json(V) when is_binary(V) orelse is_number(V) orelse V =:= null orelse
                     V =:= true orelse V =:= false ->
    V.

check_expiry(N) when N < 0                 -> {error, {value_negative, N}};
check_expiry(_N)                           -> ok.

base64url(In) ->
    lists:reverse(lists:foldl(fun ($\+, Acc) -> [$\- | Acc];
                                  ($\/, Acc) -> [$\_ | Acc];
                                  ($\=, Acc) -> Acc;
                                  (Chr, Acc) -> [Chr | Acc]
                              end, [], base64:encode_to_string(In))).

%% Ideally, you'd want Fun to run every IdealInterval. but you don't
%% want it to take more than MaxRatio of IdealInterval. So if it takes
%% more then you want to run it less often. So we time how long it
%% takes to run, and then suggest how long you should wait before
%% running it again with a user specified max interval. Times are in millis.
interval_operation({M, F, A}, MaxRatio, MaxInterval, IdealInterval, LastInterval) ->
    {Micros, Res} = timer:tc(M, F, A),
    {Res, case {Micros > 1000 * (MaxRatio * IdealInterval),
                Micros > 1000 * (MaxRatio * LastInterval)} of
              {true,  true}  -> lists:min([MaxInterval,
                                           round(LastInterval * 1.5)]);
              {true,  false} -> LastInterval;
              {false, false} -> lists:max([IdealInterval,
                                           round(LastInterval / 1.5)])
          end}.

ensure_timer(State, Idx, After, Msg) ->
    case element(Idx, State) of
        undefined -> TRef = send_after(After, self(), Msg),
                     setelement(Idx, State, TRef);
        _         -> State
    end.

stop_timer(State, Idx) ->
    case element(Idx, State) of
        undefined -> State;
        TRef      -> cancel_timer(TRef),
                     setelement(Idx, State, undefined)
    end.

%% timer:send_after/3 goes through a single timer process but allows
%% long delays. erlang:send_after/3 does not have a bottleneck but
%% only allows max 2^32-1 millis.
-define(MAX_ERLANG_SEND_AFTER, 4294967295).
send_after(Millis, Pid, Msg) when Millis > ?MAX_ERLANG_SEND_AFTER ->
    {ok, Ref} = timer:send_after(Millis, Pid, Msg),
    {timer, Ref};
send_after(Millis, Pid, Msg) ->
    {erlang, erlang:send_after(Millis, Pid, Msg)}.

cancel_timer({erlang, Ref}) -> erlang:cancel_timer(Ref),
                               ok;
cancel_timer({timer, Ref})  -> {ok, cancel} = timer:cancel(Ref),
                               ok.

store_proc_name(Type, ProcName) -> store_proc_name({Type, ProcName}).
store_proc_name(TypeProcName)   -> put(process_name, TypeProcName).

%% application:get_env/3 is only available in R16B01 or later.
get_env(Application, Key, Def) ->
    case application:get_env(Application, Key) of
        {ok, Val} -> Val;
        undefined -> Def
    end.

moving_average(_Time, _HalfLife, Next, undefined) ->
    Next;
%% We want the Weight to decrease as Time goes up (since Weight is the
%% weight for the current sample, not the new one), so that the moving
%% average decays at the same speed regardless of how long the time is
%% between samplings. So we want Weight = math:exp(Something), where
%% Something turns out to be negative.
%%
%% We want to determine Something here in terms of the Time taken
%% since the last measurement, and a HalfLife. So we want Weight =
%% math:exp(Time * Constant / HalfLife). What should Constant be? We
%% want Weight to be 0.5 when Time = HalfLife.
%%
%% Plug those numbers in and you get 0.5 = math:exp(Constant). Take
%% the log of each side and you get math:log(0.5) = Constant.
moving_average(Time,  HalfLife,  Next, Current) ->
    Weight = math:exp(Time * math:log(0.5) / HalfLife),
    Next * (1 - Weight) + Current * Weight.

%% -------------------------------------------------------------------------
%% Begin copypasta from gen_server2.erl

get_parent() ->
    case get('$ancestors') of
        [Parent | _] when is_pid (Parent) -> Parent;
        [Parent | _] when is_atom(Parent) -> name_to_pid(Parent);
        _ -> exit(process_was_not_started_by_proc_lib)
    end.

name_to_pid(Name) ->
    case whereis(Name) of
        undefined -> case whereis_name(Name) of
                         undefined -> exit(could_not_find_registerd_name);
                         Pid       -> Pid
                     end;
        Pid       -> Pid
    end.

whereis_name(Name) ->
    case ets:lookup(global_names, Name) of
        [{_Name, Pid, _Method, _RPid, _Ref}] ->
            if node(Pid) == node() -> case erlang:is_process_alive(Pid) of
                                          true  -> Pid;
                                          false -> undefined
                                      end;
               true                -> Pid
            end;
        [] -> undefined
    end.

%% End copypasta from gen_server2.erl
%% -------------------------------------------------------------------------
