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
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2012 VMware, Inc.  All rights reserved.
%%

-module(rabbit_misc).
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-export([method_record_type/1, polite_pause/0, polite_pause/1]).
-export([die/1, frame_error/2, amqp_error/4,
         protocol_error/3, protocol_error/4, protocol_error/1]).
-export([not_found/1, assert_args_equivalence/4]).
-export([dirty_read/1]).
-export([table_lookup/2, set_table_value/4]).
-export([r/3, r/2, r_arg/4, rs/1]).
-export([enable_cover/0, report_cover/0]).
-export([enable_cover/1, report_cover/1]).
-export([start_cover/1]).
-export([confirm_to_sender/2]).
-export([throw_on_error/2, with_exit_handler/2, filter_exit_map/2]).
-export([is_abnormal_termination/1]).
-export([with_user/2, with_user_and_vhost/3]).
-export([execute_mnesia_transaction/1]).
-export([execute_mnesia_transaction/2]).
-export([execute_mnesia_tx_with_tail/1]).
-export([ensure_ok/2]).
-export([tcp_name/3]).
-export([upmap/2, map_in_order/2]).
-export([table_filter/3]).
-export([dirty_read_all/1, dirty_foreach_key/2, dirty_dump_log/1]).
-export([format/2, format_stderr/2, with_local_io/1, local_info_msg/2]).
-export([start_applications/1, stop_applications/1]).
-export([unfold/2, ceil/1, queue_fold/3]).
-export([sort_field_table/1]).
-export([pid_to_string/1, string_to_pid/1]).
-export([version_compare/2, version_compare/3]).
-export([dict_cons/3, orddict_cons/3, gb_trees_cons/3,
         gb_trees_set_insert/3]).
-export([gb_trees_fold/3, gb_trees_foreach/2]).
-export([get_options/2]).
-export([all_module_attributes/1, build_acyclic_graph/3]).
-export([now_ms/0]).
-export([const_ok/0, const/1]).
-export([ntoa/1, ntoab/1]).
-export([is_process_alive/1]).
-export([pget/2, pget/3, pget_or_die/2]).
-export([format_message_queue/2]).
-export([append_rpc_all_nodes/4]).
-export([multi_call/2]).
-export([quit/1]).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-export_type([resource_name/0, thunk/1]).

-type(ok_or_error() :: rabbit_types:ok_or_error(any())).
-type(thunk(T) :: fun(() -> T)).
-type(resource_name() :: binary()).
-type(optdef() :: {flag, string()} | {option, string(), any()}).
-type(channel_or_connection_exit()
      :: rabbit_types:channel_exit() | rabbit_types:connection_exit()).
-type(digraph_label() :: term()).
-type(graph_vertex_fun() ::
        fun ((atom(), [term()]) -> [{digraph:vertex(), digraph_label()}])).
-type(graph_edge_fun() ::
        fun ((atom(), [term()]) -> [{digraph:vertex(), digraph:vertex()}])).

-spec(method_record_type/1 :: (rabbit_framing:amqp_method_record())
                              -> rabbit_framing:amqp_method_name()).
-spec(polite_pause/0 :: () -> 'done').
-spec(polite_pause/1 :: (non_neg_integer()) -> 'done').
-spec(die/1 ::
        (rabbit_framing:amqp_exception()) -> channel_or_connection_exit()).
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
-spec(assert_args_equivalence/4 :: (rabbit_framing:amqp_table(),
                                    rabbit_framing:amqp_table(),
                                    rabbit_types:r(any()), [binary()]) ->
                                        'ok' | rabbit_types:connection_exit()).
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
         rabbit_framing:amqp_table(), binary())
        -> undefined | rabbit_types:r(K)
               when is_subtype(K, atom())).
-spec(rs/1 :: (rabbit_types:r(atom())) -> string()).
-spec(enable_cover/0 :: () -> ok_or_error()).
-spec(start_cover/1 :: ([{string(), string()} | string()]) -> 'ok').
-spec(report_cover/0 :: () -> 'ok').
-spec(enable_cover/1 :: ([file:filename() | atom()]) -> ok_or_error()).
-spec(report_cover/1 :: ([file:filename() | atom()]) -> 'ok').
-spec(throw_on_error/2 ::
        (atom(), thunk(rabbit_types:error(any()) | {ok, A} | A)) -> A).
-spec(with_exit_handler/2 :: (thunk(A), thunk(A)) -> A).
-spec(filter_exit_map/2 :: (fun ((A) -> B), [A]) -> [B]).
-spec(is_abnormal_termination/1 :: (any()) -> boolean()).
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
-spec(upmap/2 :: (fun ((A) -> B), [A]) -> [B]).
-spec(map_in_order/2 :: (fun ((A) -> B), [A]) -> [B]).
-spec(table_filter/3:: (fun ((A) -> boolean()), fun ((A, boolean()) -> 'ok'),
                                                    atom()) -> [A]).
-spec(dirty_read_all/1 :: (atom()) -> [any()]).
-spec(dirty_foreach_key/2 :: (fun ((any()) -> any()), atom())
                             -> 'ok' | 'aborted').
-spec(dirty_dump_log/1 :: (file:filename()) -> ok_or_error()).
-spec(format/2 :: (string(), [any()]) -> string()).
-spec(format_stderr/2 :: (string(), [any()]) -> 'ok').
-spec(with_local_io/1 :: (fun (() -> A)) -> A).
-spec(local_info_msg/2 :: (string(), [any()]) -> 'ok').
-spec(start_applications/1 :: ([atom()]) -> 'ok').
-spec(stop_applications/1 :: ([atom()]) -> 'ok').
-spec(unfold/2  :: (fun ((A) -> ({'true', B, A} | 'false')), A) -> {[B], A}).
-spec(ceil/1 :: (number()) -> integer()).
-spec(queue_fold/3 :: (fun ((any(), B) -> B), B, queue()) -> B).
-spec(sort_field_table/1 ::
        (rabbit_framing:amqp_table()) -> rabbit_framing:amqp_table()).
-spec(pid_to_string/1 :: (pid()) -> string()).
-spec(string_to_pid/1 :: (string()) -> pid()).
-spec(version_compare/2 :: (string(), string()) -> 'lt' | 'eq' | 'gt').
-spec(version_compare/3 ::
        (string(), string(), ('lt' | 'lte' | 'eq' | 'gte' | 'gt'))
        -> boolean()).
-spec(dict_cons/3 :: (any(), any(), dict()) -> dict()).
-spec(orddict_cons/3 :: (any(), any(), orddict:orddict()) -> orddict:orddict()).
-spec(gb_trees_cons/3 :: (any(), any(), gb_tree()) -> gb_tree()).
-spec(gb_trees_set_insert/3 :: (any(), any(), gb_tree()) -> gb_tree()).
-spec(gb_trees_fold/3 :: (fun ((any(), any(), A) -> A), A, gb_tree()) -> A).
-spec(gb_trees_foreach/2 ::
        (fun ((any(), any()) -> any()), gb_tree()) -> 'ok').
-spec(get_options/2 :: ([optdef()], [string()])
                       -> {[string()], [{string(), any()}]}).
-spec(all_module_attributes/1 :: (atom()) -> [{atom(), [term()]}]).
-spec(build_acyclic_graph/3 ::
        (graph_vertex_fun(), graph_edge_fun(), [{atom(), [term()]}])
        -> rabbit_types:ok_or_error2(digraph(),
                                     {'vertex', 'duplicate', digraph:vertex()} |
                                     {'edge', ({bad_vertex, digraph:vertex()} |
                                               {bad_edge, [digraph:vertex()]}),
                                      digraph:vertex(), digraph:vertex()})).
-spec(now_ms/0 :: () -> non_neg_integer()).
-spec(const_ok/0 :: () -> 'ok').
-spec(const/1 :: (A) -> thunk(A)).
-spec(ntoa/1 :: (inet:ip_address()) -> string()).
-spec(ntoab/1 :: (inet:ip_address()) -> string()).
-spec(is_process_alive/1 :: (pid()) -> boolean()).
-spec(pget/2 :: (term(), [term()]) -> term()).
-spec(pget/3 :: (term(), [term()], term()) -> term()).
-spec(pget_or_die/2 :: (term(), [term()]) -> term() | no_return()).
-spec(format_message_queue/2 :: (any(), priority_queue:q()) -> term()).
-spec(append_rpc_all_nodes/4 :: ([node()], atom(), atom(), [any()]) -> [any()]).
-spec(multi_call/2 ::
        ([pid()], any()) -> {[{pid(), any()}], [{pid(), any()}]}).
-spec(quit/1 :: (integer() | string()) -> no_return()).

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

type_class(byte)      -> int;
type_class(short)     -> int;
type_class(signedint) -> int;
type_class(long)      -> int;
type_class(decimal)   -> int;
type_class(float)     -> float;
type_class(double)    -> float;
type_class(Other)     -> Other.

assert_args_equivalence(Orig, New, Name, Keys) ->
    [assert_args_equivalence1(Orig, New, Name, Key) || Key <- Keys],
    ok.

assert_args_equivalence1(Orig, New, Name, Key) ->
    {Orig1, New1} = {table_lookup(Orig, Key), table_lookup(New, Key)},
    FailureFun = fun () ->
                     protocol_error(precondition_failed, "inequivalent arg '~s'"
                                    "for ~s: received ~s but current is ~s",
                                    [Key, rs(Name), val(New1), val(Orig1)])
                 end,
    case {Orig1, New1} of
        {Same, Same} ->
            ok;
        {{OrigType, OrigVal}, {NewType, NewVal}} ->
            case type_class(OrigType) == type_class(NewType) andalso
                 OrigVal == NewVal of
                 true  -> ok;
                 false -> FailureFun()
            end;
        {_, _} ->
            FailureFun()
    end.

val(undefined) ->
    "none";
val({Type, Value}) ->
    ValFmt = case is_binary(Value) of
                 true  -> "~s";
                 false -> "~w"
             end,
    format("the value '" ++ ValFmt ++ "' of type '~s'", [Value, Type]).

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

r(#resource{virtual_host = VHostPath}, Kind, Name)
  when is_binary(Name) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = Name};
r(VHostPath, Kind, Name) when is_binary(Name) andalso is_binary(VHostPath) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = Name}.

r(VHostPath, Kind) when is_binary(VHostPath) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = '_'}.

r_arg(#resource{virtual_host = VHostPath}, Kind, Table, Key) ->
    r_arg(VHostPath, Kind, Table, Key);
r_arg(VHostPath, Kind, Table, Key) ->
    case table_lookup(Table, Key) of
        {longstr, NameBin} -> r(VHostPath, Kind, NameBin);
        undefined          -> undefined
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
        exit:{R, _} when R =:= noproc; R =:= nodedown;
                         R =:= normal; R =:= shutdown ->
            Handler();
        exit:{{R, _}, _} when R =:= nodedown; R =:= shutdown ->
            Handler()
    end.

filter_exit_map(F, L) ->
    Ref = make_ref(),
    lists:filter(fun (R) -> R =/= Ref end,
                 [with_exit_handler(
                    fun () -> Ref end,
                    fun () -> F(I) end) || I <- L]).

is_abnormal_termination(Reason)
  when Reason =:= noproc; Reason =:= noconnection;
       Reason =:= normal; Reason =:= shutdown -> false;
is_abnormal_termination({shutdown, _})        -> false;
is_abnormal_termination(_)                    -> true.

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
                                    true  -> Res;
                                    false -> {sync, Res}
                                end;
                       true  -> mnesia:sync_transaction(TxFun)
                   end
           end) of
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

%% Execute Fun using the IO system of the local node (i.e. the node on
%% which the code is executing).
with_local_io(Fun) ->
    GL = group_leader(),
    group_leader(whereis(user), self()),
    try
        Fun()
    after
        group_leader(GL, self())
    end.

%% Log an info message on the local node using the standard logger.
%% Use this if rabbit isn't running and the call didn't originate on
%% the local node (e.g. rabbitmqctl calls).
local_info_msg(Format, Args) ->
    with_local_io(fun () -> error_logger:info_msg(Format, Args) end).

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];
                        {error, {SkipError, _}} -> Acc;
                        {error, Reason} ->
                            lists:foreach(Undo, Acc),
                            throw({error, {ErrorTag, App, Reason}})
                    end
            end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1,
                        fun application:stop/1,
                        already_started,
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        fun application:start/1,
                        not_started,
                        cannot_stop_application,
                        Apps).

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
    %% see http://erlang.org/doc/apps/erts/erl_ext_dist.html (8.10 and
    %% 8.7)
    <<131,103,100,NodeLen:16,NodeBin:NodeLen/binary,Id:32,Ser:32,Cre:8>>
        = term_to_binary(Pid),
    Node = binary_to_term(<<131,100,NodeLen:16,NodeBin:NodeLen/binary>>),
    format("<~w.~B.~B.~B>", [Node, Cre, Id, Ser]).

%% inverse of above
string_to_pid(Str) ->
    Err = {error, {invalid_pid_syntax, Str}},
    %% The \ before the trailing $ is only there to keep emacs
    %% font-lock from getting confused.
    case re:run(Str, "^<(.*)\\.(\\d+)\\.(\\d+)\\.(\\d+)>\$",
                [{capture,all_but_first,list}]) of
        {match, [NodeStr, CreStr, IdStr, SerStr]} ->
            %% the NodeStr atom might be quoted, so we have to parse
            %% it rather than doing a simple list_to_atom
            NodeAtom = case erl_scan:string(NodeStr) of
                           {ok, [{atom, _, X}], _} -> X;
                           {error, _, _} -> throw(Err)
                       end,
            <<131,NodeEnc/binary>> = term_to_binary(NodeAtom),
            [Cre, Id, Ser] = lists:map(fun list_to_integer/1,
                                       [CreStr, IdStr, SerStr]),
            binary_to_term(<<131,103,NodeEnc/binary,Id:32,Ser:32,Cre:8>>);
        nomatch ->
            throw(Err)
    end.

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

gb_trees_set_insert(Key, Value, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Values} ->
            Values1 = gb_sets:insert(Value, Values),
            gb_trees:update(Key, Values1, Tree);
        none ->
            gb_trees:insert(Key, gb_sets:singleton(Value), Tree)
    end.

gb_trees_fold(Fun, Acc, Tree) ->
    gb_trees_fold1(Fun, Acc, gb_trees:next(gb_trees:iterator(Tree))).

gb_trees_fold1(_Fun, Acc, none) ->
    Acc;
gb_trees_fold1(Fun, Acc, {Key, Val, It}) ->
    gb_trees_fold1(Fun, Fun(Key, Val, Acc), gb_trees:next(It)).

gb_trees_foreach(Fun, Tree) ->
    gb_trees_fold(fun (Key, Val, Acc) -> Fun(Key, Val), Acc end, ok, Tree).

%% Separate flags and options from arguments.
%% get_options([{flag, "-q"}, {option, "-p", "/"}],
%%             ["set_permissions","-p","/","guest",
%%              "-q",".*",".*",".*"])
%% == {["set_permissions","guest",".*",".*",".*"],
%%     [{"-q",true},{"-p","/"}]}
get_options(Defs, As) ->
    lists:foldl(fun(Def, {AsIn, RsIn}) ->
                        {K, {AsOut, V}} =
                            case Def of
                                {flag, Key} ->
                                    {Key, get_flag(Key, AsIn)};
                                {option, Key, Default} ->
                                    {Key, get_option(Key, Default, AsIn)}
                            end,
                        {AsOut, [{K, V} | RsIn]}
                end, {As, []}, Defs).

get_option(K, _Default, [K, V | As]) ->
    {As, V};
get_option(K, Default, [Nk | As]) ->
    {As1, V} = get_option(K, Default, As),
    {[Nk | As1], V};
get_option(_, Default, As) ->
    {As, Default}.

get_flag(K, [K | As]) ->
    {As, true};
get_flag(K, [Nk | As]) ->
    {As1, V} = get_flag(K, As),
    {[Nk | As1], V};
get_flag(_, []) ->
    {[], false}.

now_ms() ->
    timer:now_diff(now(), {0,0,0}) div 1000.

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
    Modules =
        lists:usort(
          lists:append(
            [Modules || {App, _, _}   <- application:loaded_applications(),
                        {ok, Modules} <- [application:get_key(App, modules)]])),
    lists:foldl(
      fun (Module, Acc) ->
              case lists:append([Atts || {N, Atts} <- module_attributes(Module),
                                         N =:= Name]) of
                  []   -> Acc;
                  Atts -> [{Module, Atts} | Acc]
              end
      end, [], Modules).


build_acyclic_graph(VertexFun, EdgeFun, Graph) ->
    G = digraph:new([acyclic]),
    try
        [case digraph:vertex(G, Vertex) of
             false -> digraph:add_vertex(G, Vertex, Label);
             _     -> ok = throw({graph_error, {vertex, duplicate, Vertex}})
         end || {Module, Atts}  <- Graph,
                {Vertex, Label} <- VertexFun(Module, Atts)],
        [case digraph:add_edge(G, From, To) of
             {error, E} -> throw({graph_error, {edge, E, From, To}});
             _          -> ok
         end || {Module, Atts} <- Graph,
                {From, To}     <- EdgeFun(Module, Atts)],
        {ok, G}
    catch {graph_error, Reason} ->
            true = digraph:delete(G),
            {error, Reason}
    end.

const_ok() -> ok.
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

is_process_alive(Pid) when node(Pid) =:= node() ->
    erlang:is_process_alive(Pid);
is_process_alive(Pid) ->
    case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
        true -> true;
        _    -> false
    end.

pget(K, P) -> proplists:get_value(K, P).
pget(K, P, D) -> proplists:get_value(K, P, D).

pget_or_die(K, P) ->
    case proplists:get_value(K, P) of
        undefined -> exit({error, key_missing, K});
        V         -> V
    end.

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

%% A simplified version of gen_server:multi_call/2 with a sane
%% API. This is not in gen_server2 as there is no useful
%% infrastructure there to share.
multi_call(Pids, Req) ->
    MonitorPids = [start_multi_call(Pid, Req) || Pid <- Pids],
    receive_multi_call(MonitorPids, [], []).

start_multi_call(Pid, Req) when is_pid(Pid) ->
    Mref = erlang:monitor(process, Pid),
    Pid ! {'$gen_call', {self(), Mref}, Req},
    {Mref, Pid}.

receive_multi_call([], Good, Bad) ->
    {lists:reverse(Good), lists:reverse(Bad)};
receive_multi_call([{Mref, Pid} | MonitorPids], Good, Bad) ->
    receive
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            receive_multi_call(MonitorPids, [{Pid, Reply} | Good], Bad);
        {'DOWN', Mref, _, _, noconnection} ->
            receive_multi_call(MonitorPids, Good, [{Pid, nodedown} | Bad]);
        {'DOWN', Mref, _, _, Reason} ->
            receive_multi_call(MonitorPids, Good, [{Pid, Reason} | Bad])
    end.

%% the slower shutdown on windows required to flush stdout
quit(Status) ->
    case os:type() of
        {unix,  _} -> halt(Status);
        {win32, _} -> init:stop(Status)
    end.
