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
%% Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_amqqueue_process).
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-behaviour(gen_server2).

-define(SYNC_INTERVAL,                 200). %% milliseconds
-define(RAM_DURATION_UPDATE_INTERVAL, 5000).
-define(CONSUMER_BIAS_RATIO,           1.1). %% i.e. consume 10% faster

-export([start_link/1, info_keys/0]).

-export([init_with_backing_queue_state/7]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, handle_pre_hibernate/1, prioritise_call/4,
         prioritise_cast/3, prioritise_info/3, format_message_queue/2]).

%% Queue's state
-record(q, {q,
            exclusive_consumer,
            has_had_consumers,
            backing_queue,
            backing_queue_state,
            consumers,
            expires,
            sync_timer_ref,
            rate_timer_ref,
            expiry_timer_ref,
            stats_timer,
            msg_id_to_channel,
            ttl,
            ttl_timer_ref,
            ttl_timer_expiry,
            senders,
            dlx,
            dlx_routing_key,
            max_length,
            args_policy_version,
            status
           }).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-spec(start_link/1 ::
        (rabbit_types:amqqueue()) -> rabbit_types:ok_pid_or_error()).
-spec(info_keys/0 :: () -> rabbit_types:info_keys()).
-spec(init_with_backing_queue_state/7 ::
        (rabbit_types:amqqueue(), atom(), tuple(), any(),
         [rabbit_types:delivery()], pmon:pmon(), dict:dict()) -> #q{}).

-endif.

%%----------------------------------------------------------------------------

-define(STATISTICS_KEYS,
        [name,
         policy,
         exclusive_consumer_pid,
         exclusive_consumer_tag,
         messages_ready,
         messages_unacknowledged,
         messages,
         consumers,
         consumer_utilisation,
         memory,
         slave_pids,
         synchronised_slave_pids,
         backing_queue_status,
         state
        ]).

-define(CREATION_EVENT_KEYS,
        [name,
         durable,
         auto_delete,
         arguments,
         owner_pid
        ]).

-define(INFO_KEYS, [pid | ?CREATION_EVENT_KEYS ++ ?STATISTICS_KEYS -- [name]]).

%%----------------------------------------------------------------------------

start_link(Q) -> gen_server2:start_link(?MODULE, Q, []).

info_keys() -> ?INFO_KEYS.

%%----------------------------------------------------------------------------

init(Q) ->
    process_flag(trap_exit, true),
    ?store_proc_name(Q#amqqueue.name),
    {ok, init_state(Q#amqqueue{pid = self()}), hibernate,
     {backoff, ?HIBERNATE_AFTER_MIN, ?HIBERNATE_AFTER_MIN, ?DESIRED_HIBERNATE}}.

init_with_backing_queue_state(Q = #amqqueue{exclusive_owner = Owner}, BQ, BQS,
                              RateTRef, Deliveries, Senders, MTC) ->
    case Owner of
        none -> ok;
        _    -> erlang:monitor(process, Owner)
    end,
    State = init_state(Q),
    State1 = State#q{backing_queue       = BQ,
                     backing_queue_state = BQS,
                     rate_timer_ref      = RateTRef,
                     senders             = Senders,
                     msg_id_to_channel   = MTC},
    State2 = process_args_policy(State1),
    State3 = lists:foldl(fun (Delivery, StateN) ->
                                 deliver_or_enqueue(Delivery, true, StateN)
                         end, State2, Deliveries),
    notify_decorators(startup, State3),
    State3.

init_state(Q) ->
    State = #q{q                   = Q,
               exclusive_consumer  = none,
               has_had_consumers   = false,
               consumers           = rabbit_queue_consumers:new(),
               senders             = pmon:new(delegate),
               msg_id_to_channel   = gb_trees:empty(),
               status              = running,
               args_policy_version = 0},
    rabbit_event:init_stats_timer(State, #q.stats_timer).

terminate(shutdown = R,      State = #q{backing_queue = BQ}) ->
    terminate_shutdown(fun (BQS) -> BQ:terminate(R, BQS) end, State);
terminate({shutdown, missing_owner} = Reason, State) ->
    %% if the owner was missing then there will be no queue, so don't emit stats
    terminate_shutdown(terminate_delete(false, Reason, State), State);
terminate({shutdown, _} = R, State = #q{backing_queue = BQ}) ->
    terminate_shutdown(fun (BQS) -> BQ:terminate(R, BQS) end, State);
terminate(Reason,            State) ->
    terminate_shutdown(terminate_delete(true, Reason, State), State).

terminate_delete(EmitStats, Reason,
                 State = #q{q = #amqqueue{name          = QName},
                                          backing_queue = BQ}) ->
    fun (BQS) ->
        BQS1 = BQ:delete_and_terminate(Reason, BQS),
        if EmitStats -> rabbit_event:if_enabled(State, #q.stats_timer,
                                                fun() -> emit_stats(State) end);
           true      -> ok
        end,
        %% don't care if the internal delete doesn't return 'ok'.
        rabbit_amqqueue:internal_delete(QName),
        BQS1
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------

declare(Recover, From, State = #q{q                   = Q,
                                  backing_queue       = undefined,
                                  backing_queue_state = undefined}) ->
    {Recovery, TermsOrNew} = recovery_status(Recover),
    case rabbit_amqqueue:internal_declare(Q, Recovery /= new) of
        #amqqueue{} = Q1 ->
            case matches(Recovery, Q, Q1) of
                true ->
                    gen_server2:reply(From, {new, Q}),
                    ok = file_handle_cache:register_callback(
                           rabbit_amqqueue, set_maximum_since_use, [self()]),
                    ok = rabbit_memory_monitor:register(
                           self(), {rabbit_amqqueue,
                                    set_ram_duration_target, [self()]}),
                    BQ = backing_queue_module(Q1),
                    BQS = bq_init(BQ, Q, TermsOrNew),
                    recovery_barrier(Recovery),
                    State1 = process_args_policy(
                               State#q{backing_queue       = BQ,
                                       backing_queue_state = BQS}),
                    notify_decorators(startup, State),
                    rabbit_event:notify(queue_created,
                                        infos(?CREATION_EVENT_KEYS, State1)),
                    rabbit_event:if_enabled(State1, #q.stats_timer,
                                            fun() -> emit_stats(State1) end),
                    noreply(State1);
                false ->
                    {stop, normal, {existing, Q1}, State}
            end;
        Err ->
            {stop, normal, Err, State}
    end.

recovery_status(new)              -> {new,     new};
recovery_status({Recover, Terms}) -> {Recover, Terms}.

matches(new, Q1, Q2) ->
    %% i.e. not policy
    Q1#amqqueue.name            =:= Q2#amqqueue.name            andalso
    Q1#amqqueue.durable         =:= Q2#amqqueue.durable         andalso
    Q1#amqqueue.auto_delete     =:= Q2#amqqueue.auto_delete     andalso
    Q1#amqqueue.exclusive_owner =:= Q2#amqqueue.exclusive_owner andalso
    Q1#amqqueue.arguments       =:= Q2#amqqueue.arguments       andalso
    Q1#amqqueue.pid             =:= Q2#amqqueue.pid             andalso
    Q1#amqqueue.slave_pids      =:= Q2#amqqueue.slave_pids;
matches(_,  Q,   Q) -> true;
matches(_, _Q, _Q1) -> false.

maybe_notify_decorators(false, State) -> State;
maybe_notify_decorators(true,  State) -> notify_decorators(State), State.

notify_decorators(Event, State) -> decorator_callback(qname(State), Event, []).

notify_decorators(State = #q{consumers           = Consumers,
                             backing_queue       = BQ,
                             backing_queue_state = BQS}) ->
    P = rabbit_queue_consumers:max_active_priority(Consumers),
    decorator_callback(qname(State), consumer_state_changed,
                       [P, BQ:is_empty(BQS)]).

decorator_callback(QName, F, A) ->
    %% Look up again in case policy and hence decorators have changed
    case rabbit_amqqueue:lookup(QName) of
        {ok, Q = #amqqueue{decorators = Ds}} ->
            [ok = apply(M, F, [Q|A]) || M <- rabbit_queue_decorator:select(Ds)];
        {error, not_found} ->
            ok
    end.

bq_init(BQ, Q, Recover) ->
    Self = self(),
    BQ:init(Q, Recover,
            fun (Mod, Fun) ->
                    rabbit_amqqueue:run_backing_queue(Self, Mod, Fun)
            end).

recovery_barrier(new) ->
    ok;
recovery_barrier(BarrierPid) ->
    MRef = erlang:monitor(process, BarrierPid),
    receive
        {BarrierPid, go}              -> erlang:demonitor(MRef, [flush]);
        {'DOWN', MRef, process, _, _} -> ok
    end.

process_args_policy(State = #q{q                   = Q,
                               args_policy_version = N}) ->
      ArgsTable =
        [{<<"expires">>,                 fun res_min/2, fun init_exp/2},
         {<<"dead-letter-exchange">>,    fun res_arg/2, fun init_dlx/2},
         {<<"dead-letter-routing-key">>, fun res_arg/2, fun init_dlx_rkey/2},
         {<<"message-ttl">>,             fun res_min/2, fun init_ttl/2},
         {<<"max-length">>,              fun res_min/2, fun init_max_length/2}],
      drop_expired_msgs(
         lists:foldl(fun({Name, Resolve, Fun}, StateN) ->
                             Fun(args_policy_lookup(Name, Resolve, Q), StateN)
                     end, State#q{args_policy_version = N + 1}, ArgsTable)).

args_policy_lookup(Name, Resolve, Q = #amqqueue{arguments = Args}) ->
    AName = <<"x-", Name/binary>>,
    case {rabbit_policy:get(Name, Q), rabbit_misc:table_lookup(Args, AName)} of
        {undefined, undefined}       -> undefined;
        {undefined, {_Type, Val}}    -> Val;
        {Val,       undefined}       -> Val;
        {PolVal,    {_Type, ArgVal}} -> Resolve(PolVal, ArgVal)
    end.

res_arg(_PolVal, ArgVal) -> ArgVal.
res_min(PolVal, ArgVal)  -> erlang:min(PolVal, ArgVal).

%% In both these we init with the undefined variant first to stop any
%% existing timer, then start a new one which may fire after a
%% different time.
init_exp(undefined, State) -> stop_expiry_timer(State#q{expires = undefined});
init_exp(Expires,   State) -> State1 = init_exp(undefined, State),
                              ensure_expiry_timer(State1#q{expires = Expires}).

init_ttl(undefined, State) -> stop_ttl_timer(State#q{ttl = undefined});
init_ttl(TTL,       State) -> (init_ttl(undefined, State))#q{ttl = TTL}.

init_dlx(undefined, State) ->
    State#q{dlx = undefined};
init_dlx(DLX, State = #q{q = #amqqueue{name = QName}}) ->
    State#q{dlx = rabbit_misc:r(QName, exchange, DLX)}.

init_dlx_rkey(RoutingKey, State) -> State#q{dlx_routing_key = RoutingKey}.

init_max_length(MaxLen, State) ->
    {_Dropped, State1} = maybe_drop_head(State#q{max_length = MaxLen}),
    State1.

terminate_shutdown(Fun, State) ->
    State1 = #q{backing_queue_state = BQS, consumers = Consumers} =
        lists:foldl(fun (F, S) -> F(S) end, State,
                    [fun stop_sync_timer/1,
                     fun stop_rate_timer/1,
                     fun stop_expiry_timer/1,
                     fun stop_ttl_timer/1]),
    case BQS of
        undefined -> State1;
        _         -> ok = rabbit_memory_monitor:deregister(self()),
                     QName = qname(State),
                     notify_decorators(shutdown, State),
                     [emit_consumer_deleted(Ch, CTag, QName) ||
                         {Ch, CTag, _, _, _} <-
                             rabbit_queue_consumers:all(Consumers)],
                     State1#q{backing_queue_state = Fun(BQS)}
    end.

reply(Reply, NewState) ->
    {NewState1, Timeout} = next_state(NewState),
    {reply, Reply, ensure_stats_timer(ensure_rate_timer(NewState1)), Timeout}.

noreply(NewState) ->
    {NewState1, Timeout} = next_state(NewState),
    {noreply, ensure_stats_timer(ensure_rate_timer(NewState1)), Timeout}.

next_state(State = #q{backing_queue       = BQ,
                      backing_queue_state = BQS,
                      msg_id_to_channel   = MTC}) ->
    assert_invariant(State),
    {MsgIds, BQS1} = BQ:drain_confirmed(BQS),
    MTC1 = confirm_messages(MsgIds, MTC),
    State1 = State#q{backing_queue_state = BQS1, msg_id_to_channel = MTC1},
    case BQ:needs_timeout(BQS1) of
        false -> {stop_sync_timer(State1),   hibernate     };
        idle  -> {stop_sync_timer(State1),   ?SYNC_INTERVAL};
        timed -> {ensure_sync_timer(State1), 0             }
    end.

backing_queue_module(Q) ->
    case rabbit_mirror_queue_misc:is_mirrored(Q) of
        false -> {ok, BQM} = application:get_env(backing_queue_module),
                 BQM;
        true  -> rabbit_mirror_queue_master
    end.

ensure_sync_timer(State) ->
    rabbit_misc:ensure_timer(State, #q.sync_timer_ref,
                             ?SYNC_INTERVAL, sync_timeout).

stop_sync_timer(State) -> rabbit_misc:stop_timer(State, #q.sync_timer_ref).

ensure_rate_timer(State) ->
    rabbit_misc:ensure_timer(State, #q.rate_timer_ref,
                             ?RAM_DURATION_UPDATE_INTERVAL,
                             update_ram_duration).

stop_rate_timer(State) -> rabbit_misc:stop_timer(State, #q.rate_timer_ref).

%% We wish to expire only when there are no consumers *and* the expiry
%% hasn't been refreshed (by queue.declare or basic.get) for the
%% configured period.
ensure_expiry_timer(State = #q{expires = undefined}) ->
    State;
ensure_expiry_timer(State = #q{expires             = Expires,
                               args_policy_version = Version}) ->
    case is_unused(State) of
        true  -> NewState = stop_expiry_timer(State),
                 rabbit_misc:ensure_timer(NewState, #q.expiry_timer_ref,
                                          Expires, {maybe_expire, Version});
        false -> State
    end.

stop_expiry_timer(State) -> rabbit_misc:stop_timer(State, #q.expiry_timer_ref).

ensure_ttl_timer(undefined, State) ->
    State;
ensure_ttl_timer(Expiry, State = #q{ttl_timer_ref       = undefined,
                                    args_policy_version = Version}) ->
    After = (case Expiry - now_micros() of
                 V when V > 0 -> V + 999; %% always fire later
                 _            -> 0
             end) div 1000,
    TRef = erlang:send_after(After, self(), {drop_expired, Version}),
    State#q{ttl_timer_ref = TRef, ttl_timer_expiry = Expiry};
ensure_ttl_timer(Expiry, State = #q{ttl_timer_ref    = TRef,
                                    ttl_timer_expiry = TExpiry})
  when Expiry + 1000 < TExpiry ->
    case erlang:cancel_timer(TRef) of
        false -> State;
        _     -> ensure_ttl_timer(Expiry, State#q{ttl_timer_ref = undefined})
    end;
ensure_ttl_timer(_Expiry, State) ->
    State.

stop_ttl_timer(State) -> rabbit_misc:stop_timer(State, #q.ttl_timer_ref).

ensure_stats_timer(State) ->
    rabbit_event:ensure_stats_timer(State, #q.stats_timer, emit_stats).

assert_invariant(State = #q{consumers = Consumers}) ->
    true = (rabbit_queue_consumers:inactive(Consumers) orelse is_empty(State)).

is_empty(#q{backing_queue = BQ, backing_queue_state = BQS}) -> BQ:is_empty(BQS).

maybe_send_drained(WasEmpty, State) ->
    case (not WasEmpty) andalso is_empty(State) of
        true  -> notify_decorators(State),
                 rabbit_queue_consumers:send_drained();
        false -> ok
    end,
    State.

confirm_messages([], MTC) ->
    MTC;
confirm_messages(MsgIds, MTC) ->
    {CMs, MTC1} =
        lists:foldl(
          fun(MsgId, {CMs, MTC0}) ->
                  case gb_trees:lookup(MsgId, MTC0) of
                      {value, {SenderPid, MsgSeqNo}} ->
                          {rabbit_misc:gb_trees_cons(SenderPid,
                                                     MsgSeqNo, CMs),
                           gb_trees:delete(MsgId, MTC0)};
                      none ->
                          {CMs, MTC0}
                  end
          end, {gb_trees:empty(), MTC}, MsgIds),
    rabbit_misc:gb_trees_foreach(fun rabbit_misc:confirm_to_sender/2, CMs),
    MTC1.

send_or_record_confirm(#delivery{confirm    = false}, State) ->
    {never, State};
send_or_record_confirm(#delivery{confirm    = true,
                                 sender     = SenderPid,
                                 msg_seq_no = MsgSeqNo,
                                 message    = #basic_message {
                                   is_persistent = true,
                                   id            = MsgId}},
                       State = #q{q                 = #amqqueue{durable = true},
                                  msg_id_to_channel = MTC}) ->
    MTC1 = gb_trees:insert(MsgId, {SenderPid, MsgSeqNo}, MTC),
    {eventually, State#q{msg_id_to_channel = MTC1}};
send_or_record_confirm(#delivery{confirm    = true,
                                 sender     = SenderPid,
                                 msg_seq_no = MsgSeqNo}, State) ->
    rabbit_misc:confirm_to_sender(SenderPid, [MsgSeqNo]),
    {immediately, State}.

send_mandatory(#delivery{mandatory  = false}) ->
    ok;
send_mandatory(#delivery{mandatory  = true,
                         sender     = SenderPid,
                         msg_seq_no = MsgSeqNo}) ->
    gen_server2:cast(SenderPid, {mandatory_received, MsgSeqNo}).

discard(#delivery{confirm = Confirm,
                  sender  = SenderPid,
                  message = #basic_message{id = MsgId}}, BQ, BQS, MTC) ->
    MTC1 = case Confirm of
               true  -> confirm_messages([MsgId], MTC);
               false -> MTC
           end,
    BQS1 = BQ:discard(MsgId, SenderPid, BQS),
    {BQS1, MTC1}.

run_message_queue(State) -> run_message_queue(false, State).

run_message_queue(ActiveConsumersChanged, State) ->
    case is_empty(State) of
        true  -> maybe_notify_decorators(ActiveConsumersChanged, State);
        false -> case rabbit_queue_consumers:deliver(
                        fun(AckRequired) -> fetch(AckRequired, State) end,
                        qname(State), State#q.consumers) of
                     {delivered, ActiveConsumersChanged1, State1, Consumers} ->
                         run_message_queue(
                           ActiveConsumersChanged or ActiveConsumersChanged1,
                           State1#q{consumers = Consumers});
                     {undelivered, ActiveConsumersChanged1, Consumers} ->
                         maybe_notify_decorators(
                           ActiveConsumersChanged or ActiveConsumersChanged1,
                           State#q{consumers = Consumers})
                 end
    end.

attempt_delivery(Delivery = #delivery{sender = SenderPid, message = Message},
                 Props, Delivered, State = #q{backing_queue       = BQ,
                                              backing_queue_state = BQS,
                                              msg_id_to_channel   = MTC}) ->
    case rabbit_queue_consumers:deliver(
           fun (true)  -> true = BQ:is_empty(BQS),
                          {AckTag, BQS1} = BQ:publish_delivered(
                                             Message, Props, SenderPid, BQS),
                          {{Message, Delivered, AckTag}, {BQS1, MTC}};
               (false) -> {{Message, Delivered, undefined},
                           discard(Delivery, BQ, BQS, MTC)}
           end, qname(State), State#q.consumers) of
        {delivered, ActiveConsumersChanged, {BQS1, MTC1}, Consumers} ->
            {delivered,   maybe_notify_decorators(
                            ActiveConsumersChanged,
                            State#q{backing_queue_state = BQS1,
                                    msg_id_to_channel   = MTC1,
                                    consumers           = Consumers})};
        {undelivered, ActiveConsumersChanged, Consumers} ->
            {undelivered, maybe_notify_decorators(
                            ActiveConsumersChanged,
                            State#q{consumers = Consumers})}
    end.

deliver_or_enqueue(Delivery = #delivery{message = Message, sender = SenderPid},
                   Delivered, State = #q{backing_queue       = BQ,
                                         backing_queue_state = BQS}) ->
    send_mandatory(Delivery), %% must do this before confirms
    {Confirm, State1} = send_or_record_confirm(Delivery, State),
    Props = message_properties(Message, Confirm, State1),
    {IsDuplicate, BQS1} = BQ:is_duplicate(Message, BQS),
    State2 = State1#q{backing_queue_state = BQS1},
    case IsDuplicate orelse attempt_delivery(Delivery, Props, Delivered,
                                             State2) of
        true ->
            State2;
        {delivered, State3} ->
            State3;
        %% The next one is an optimisation
        {undelivered, State3 = #q{ttl = 0, dlx = undefined,
                                  backing_queue_state = BQS2,
                                  msg_id_to_channel   = MTC}} ->
            {BQS3, MTC1} = discard(Delivery, BQ, BQS2, MTC),
            State3#q{backing_queue_state = BQS3, msg_id_to_channel = MTC1};
        {undelivered, State3 = #q{backing_queue_state = BQS2}} ->
            BQS3 = BQ:publish(Message, Props, Delivered, SenderPid, BQS2),
            {Dropped, State4 = #q{backing_queue_state = BQS4}} =
                maybe_drop_head(State3#q{backing_queue_state = BQS3}),
            QLen = BQ:len(BQS4),
            %% optimisation: it would be perfectly safe to always
            %% invoke drop_expired_msgs here, but that is expensive so
            %% we only do that if a new message that might have an
            %% expiry ends up at the head of the queue. If the head
            %% remains unchanged, or if the newly published message
            %% has no expiry and becomes the head of the queue then
            %% the call is unnecessary.
            case {Dropped > 0, QLen =:= 1, Props#message_properties.expiry} of
                {false, false,         _} -> State4;
                {true,  true,  undefined} -> State4;
                {_,     _,             _} -> drop_expired_msgs(State4)
            end
    end.

maybe_drop_head(State = #q{max_length = undefined}) ->
    {0, State};
maybe_drop_head(State = #q{max_length          = MaxLen,
                           backing_queue       = BQ,
                           backing_queue_state = BQS}) ->
    case BQ:len(BQS) - MaxLen of
        Excess when Excess > 0 ->
            {Excess,
             with_dlx(
               State#q.dlx,
               fun (X) -> dead_letter_maxlen_msgs(X, Excess, State) end,
               fun () ->
                       {_, BQS1} = lists:foldl(fun (_, {_, BQS0}) ->
                                                       BQ:drop(false, BQS0)
                                               end, {ok, BQS},
                                               lists:seq(1, Excess)),
                       State#q{backing_queue_state = BQS1}
               end)};
        _ -> {0, State}
    end.

requeue_and_run(AckTags, State = #q{backing_queue       = BQ,
                                    backing_queue_state = BQS}) ->
    WasEmpty = BQ:is_empty(BQS),
    {_MsgIds, BQS1} = BQ:requeue(AckTags, BQS),
    {_Dropped, State1} = maybe_drop_head(State#q{backing_queue_state = BQS1}),
    run_message_queue(maybe_send_drained(WasEmpty, drop_expired_msgs(State1))).

fetch(AckRequired, State = #q{backing_queue       = BQ,
                              backing_queue_state = BQS}) ->
    {Result, BQS1} = BQ:fetch(AckRequired, BQS),
    State1 = drop_expired_msgs(State#q{backing_queue_state = BQS1}),
    {Result, maybe_send_drained(Result =:= empty, State1)}.

ack(AckTags, ChPid, State) ->
    subtract_acks(ChPid, AckTags, State,
                  fun (State1 = #q{backing_queue       = BQ,
                                   backing_queue_state = BQS}) ->
                          {_Guids, BQS1} = BQ:ack(AckTags, BQS),
                          State1#q{backing_queue_state = BQS1}
                  end).

requeue(AckTags, ChPid, State) ->
    subtract_acks(ChPid, AckTags, State,
                  fun (State1) -> requeue_and_run(AckTags, State1) end).

possibly_unblock(Update, ChPid, State = #q{consumers = Consumers}) ->
    case rabbit_queue_consumers:possibly_unblock(Update, ChPid, Consumers) of
        unchanged               -> State;
        {unblocked, Consumers1} -> State1 = State#q{consumers = Consumers1},
                                   run_message_queue(true, State1)
    end.

should_auto_delete(#q{q = #amqqueue{auto_delete = false}}) -> false;
should_auto_delete(#q{has_had_consumers = false}) -> false;
should_auto_delete(State) -> is_unused(State).

handle_ch_down(DownPid, State = #q{consumers          = Consumers,
                                   exclusive_consumer = Holder,
                                   senders            = Senders}) ->
    State1 = State#q{senders = case pmon:is_monitored(DownPid, Senders) of
                                   false -> Senders;
                                   true  -> credit_flow:peer_down(DownPid),
                                            pmon:demonitor(DownPid, Senders)
                               end},
    case rabbit_queue_consumers:erase_ch(DownPid, Consumers) of
        not_found ->
            {ok, State1};
        {ChAckTags, ChCTags, Consumers1} ->
            QName = qname(State1),
            [emit_consumer_deleted(DownPid, CTag, QName) || CTag <- ChCTags],
            Holder1 = case Holder of
                          {DownPid, _} -> none;
                          Other        -> Other
                      end,
            State2 = State1#q{consumers          = Consumers1,
                              exclusive_consumer = Holder1},
            notify_decorators(State2),
            case should_auto_delete(State2) of
                true  -> {stop, State2};
                false -> {ok, requeue_and_run(ChAckTags,
                                              ensure_expiry_timer(State2))}
            end
    end.

check_exclusive_access({_ChPid, _ConsumerTag}, _ExclusiveConsume, _State) ->
    in_use;
check_exclusive_access(none, false, _State) ->
    ok;
check_exclusive_access(none, true, State) ->
    case is_unused(State) of
        true  -> ok;
        false -> in_use
    end.

is_unused(_State) -> rabbit_queue_consumers:count() == 0.

maybe_send_reply(_ChPid, undefined) -> ok;
maybe_send_reply(ChPid, Msg) -> ok = rabbit_channel:send_command(ChPid, Msg).

qname(#q{q = #amqqueue{name = QName}}) -> QName.

backing_queue_timeout(State = #q{backing_queue       = BQ,
                                 backing_queue_state = BQS}) ->
    State#q{backing_queue_state = BQ:timeout(BQS)}.

subtract_acks(ChPid, AckTags, State = #q{consumers = Consumers}, Fun) ->
    case rabbit_queue_consumers:subtract_acks(ChPid, AckTags, Consumers) of
        not_found               -> State;
        unchanged               -> Fun(State);
        {unblocked, Consumers1} -> State1 = State#q{consumers = Consumers1},
                                   run_message_queue(true, Fun(State1))
    end.

message_properties(Message, Confirm, #q{ttl = TTL}) ->
    #message_properties{expiry           = calculate_msg_expiry(Message, TTL),
                        needs_confirming = Confirm == eventually}.

calculate_msg_expiry(#basic_message{content = Content}, TTL) ->
    #content{properties = Props} =
        rabbit_binary_parser:ensure_content_decoded(Content),
    %% We assert that the expiration must be valid - we check in the channel.
    {ok, MsgTTL} = rabbit_basic:parse_expiration(Props),
    case lists:min([TTL, MsgTTL]) of
        undefined -> undefined;
        T         -> now_micros() + T * 1000
    end.

%% Logically this function should invoke maybe_send_drained/2.
%% However, that is expensive. Since some frequent callers of
%% drop_expired_msgs/1, in particular deliver_or_enqueue/3, cannot
%% possibly cause the queue to become empty, we push the
%% responsibility to the callers. So be cautious when adding new ones.
drop_expired_msgs(State) ->
    case is_empty(State) of
        true  -> State;
        false -> drop_expired_msgs(now_micros(), State)
    end.

drop_expired_msgs(Now, State = #q{backing_queue_state = BQS,
                                  backing_queue       = BQ }) ->
    ExpirePred = fun (#message_properties{expiry = Exp}) -> Now >= Exp end,
    {Props, State1} =
        with_dlx(
          State#q.dlx,
          fun (X) -> dead_letter_expired_msgs(ExpirePred, X, State) end,
          fun () -> {Next, BQS1} = BQ:dropwhile(ExpirePred, BQS),
                    {Next, State#q{backing_queue_state = BQS1}} end),
    ensure_ttl_timer(case Props of
                         undefined                         -> undefined;
                         #message_properties{expiry = Exp} -> Exp
                     end, State1).

with_dlx(undefined, _With,  Without) -> Without();
with_dlx(DLX,        With,  Without) -> case rabbit_exchange:lookup(DLX) of
                                            {ok, X}            -> With(X);
                                            {error, not_found} -> Without()
                                        end.

dead_letter_expired_msgs(ExpirePred, X, State = #q{backing_queue = BQ}) ->
    dead_letter_msgs(fun (DLFun, Acc, BQS1) ->
                             BQ:fetchwhile(ExpirePred, DLFun, Acc, BQS1)
                     end, expired, X, State).

dead_letter_rejected_msgs(AckTags, X,  State = #q{backing_queue = BQ}) ->
    {ok, State1} =
        dead_letter_msgs(
          fun (DLFun, Acc, BQS) ->
                  {Acc1, BQS1} = BQ:ackfold(DLFun, Acc, BQS, AckTags),
                  {ok, Acc1, BQS1}
          end, rejected, X, State),
    State1.

dead_letter_maxlen_msgs(X, Excess, State = #q{backing_queue = BQ}) ->
    {ok, State1} =
        dead_letter_msgs(
          fun (DLFun, Acc, BQS) ->
                  lists:foldl(fun (_, {ok, Acc0, BQS0}) ->
                                      {{Msg, _, AckTag}, BQS1} =
                                        BQ:fetch(true, BQS0),
                                      {ok, DLFun(Msg, AckTag, Acc0), BQS1}
                              end, {ok, Acc, BQS}, lists:seq(1, Excess))
          end, maxlen, X, State),
    State1.

dead_letter_msgs(Fun, Reason, X, State = #q{dlx_routing_key     = RK,
                                            backing_queue_state = BQS,
                                            backing_queue       = BQ}) ->
    QName = qname(State),
    {Res, Acks1, BQS1} =
        Fun(fun (Msg, AckTag, Acks) ->
                    rabbit_dead_letter:publish(Msg, Reason, X, RK, QName),
                    [AckTag | Acks]
            end, [], BQS),
    {_Guids, BQS2} = BQ:ack(Acks1, BQS1),
    {Res, State#q{backing_queue_state = BQS2}}.

stop(State) -> stop(noreply, State).

stop(noreply, State) -> {stop, normal, State};
stop(Reply,   State) -> {stop, normal, Reply, State}.

now_micros() -> timer:now_diff(now(), {0,0,0}).

infos(Items, State) -> [{Item, i(Item, State)} || Item <- Items].

i(name,        #q{q = #amqqueue{name        = Name}})       -> Name;
i(durable,     #q{q = #amqqueue{durable     = Durable}})    -> Durable;
i(auto_delete, #q{q = #amqqueue{auto_delete = AutoDelete}}) -> AutoDelete;
i(arguments,   #q{q = #amqqueue{arguments   = Arguments}})  -> Arguments;
i(pid, _) ->
    self();
i(owner_pid, #q{q = #amqqueue{exclusive_owner = none}}) ->
    '';
i(owner_pid, #q{q = #amqqueue{exclusive_owner = ExclusiveOwner}}) ->
    ExclusiveOwner;
i(policy,    #q{q = Q}) ->
    case rabbit_policy:name(Q) of
        none   -> '';
        Policy -> Policy
    end;
i(exclusive_consumer_pid, #q{exclusive_consumer = none}) ->
    '';
i(exclusive_consumer_pid, #q{exclusive_consumer = {ChPid, _ConsumerTag}}) ->
    ChPid;
i(exclusive_consumer_tag, #q{exclusive_consumer = none}) ->
    '';
i(exclusive_consumer_tag, #q{exclusive_consumer = {_ChPid, ConsumerTag}}) ->
    ConsumerTag;
i(messages_ready, #q{backing_queue_state = BQS, backing_queue = BQ}) ->
    BQ:len(BQS);
i(messages_unacknowledged, _) ->
    rabbit_queue_consumers:unacknowledged_message_count();
i(messages, State) ->
    lists:sum([i(Item, State) || Item <- [messages_ready,
                                          messages_unacknowledged]]);
i(consumers, _) ->
    rabbit_queue_consumers:count();
i(consumer_utilisation, #q{consumers = Consumers}) ->
    case rabbit_queue_consumers:count() of
        0 -> '';
        _ -> rabbit_queue_consumers:utilisation(Consumers)
    end;
i(memory, _) ->
    {memory, M} = process_info(self(), memory),
    M;
i(slave_pids, #q{q = #amqqueue{name = Name}}) ->
    {ok, Q = #amqqueue{slave_pids = SPids}} =
        rabbit_amqqueue:lookup(Name),
    case rabbit_mirror_queue_misc:is_mirrored(Q) of
        false -> '';
        true  -> SPids
    end;
i(synchronised_slave_pids, #q{q = #amqqueue{name = Name}}) ->
    {ok, Q = #amqqueue{sync_slave_pids = SSPids}} =
        rabbit_amqqueue:lookup(Name),
    case rabbit_mirror_queue_misc:is_mirrored(Q) of
        false -> '';
        true  -> SSPids
    end;
i(state, #q{status = running}) -> credit_flow:state();
i(state, #q{status = State})   -> State;
i(backing_queue_status, #q{backing_queue_state = BQS, backing_queue = BQ}) ->
    BQ:status(BQS);
i(Item, _) ->
    throw({bad_argument, Item}).

emit_stats(State) ->
    emit_stats(State, []).

emit_stats(State, Extra) ->
    ExtraKs = [K || {K, _} <- Extra],
    Infos = [{K, V} || {K, V} <- infos(?STATISTICS_KEYS, State),
                       not lists:member(K, ExtraKs)],
    rabbit_event:notify(queue_stats, Extra ++ Infos).

emit_consumer_created(ChPid, CTag, Exclusive, AckRequired, QName,
                      PrefetchCount, Args, Ref) ->
    rabbit_event:notify(consumer_created,
                        [{consumer_tag,   CTag},
                         {exclusive,      Exclusive},
                         {ack_required,   AckRequired},
                         {channel,        ChPid},
                         {queue,          QName},
                         {prefetch_count, PrefetchCount},
                         {arguments,      Args}],
                        Ref).

emit_consumer_deleted(ChPid, ConsumerTag, QName) ->
    rabbit_event:notify(consumer_deleted,
                        [{consumer_tag, ConsumerTag},
                         {channel,      ChPid},
                         {queue,        QName}]).

%%----------------------------------------------------------------------------

prioritise_call(Msg, _From, _Len, State) ->
    case Msg of
        info                                       -> 9;
        {info, _Items}                             -> 9;
        consumers                                  -> 9;
        stat                                       -> 7;
        {basic_consume, _, _, _, _, _, _, _, _, _} -> consumer_bias(State);
        {basic_cancel, _, _, _}                    -> consumer_bias(State);
        _                                          -> 0
    end.

prioritise_cast(Msg, _Len, State) ->
    case Msg of
        delete_immediately                   -> 8;
        {set_ram_duration_target, _Duration} -> 8;
        {set_maximum_since_use, _Age}        -> 8;
        {run_backing_queue, _Mod, _Fun}      -> 6;
        {ack, _AckTags, _ChPid}              -> 3; %% [1]
        {resume, _ChPid}                     -> 2;
        {notify_sent, _ChPid, _Credit}       -> consumer_bias(State);
        _                                    -> 0
    end.

%% [1] It should be safe to always prioritise ack / resume since they
%% will be rate limited by how fast consumers receive messages -
%% i.e. by notify_sent. We prioritise ack and resume to discourage
%% starvation caused by prioritising notify_sent. We don't vary their
%% prioritiy since acks should stay in order (some parts of the queue
%% stack are optimised for that) and to make things easier to reason
%% about. Finally, we prioritise ack over resume since it should
%% always reduce memory use.

consumer_bias(#q{backing_queue = BQ, backing_queue_state = BQS}) ->
    case BQ:msg_rates(BQS) of
        {0.0,          _} -> 0;
        {Ingress, Egress} when Egress / Ingress < ?CONSUMER_BIAS_RATIO -> 1;
        {_,            _} -> 0
    end.

prioritise_info(Msg, _Len, #q{q = #amqqueue{exclusive_owner = DownPid}}) ->
    case Msg of
        {'DOWN', _, process, DownPid, _}     -> 8;
        update_ram_duration                  -> 8;
        {maybe_expire, _Version}             -> 8;
        {drop_expired, _Version}             -> 8;
        emit_stats                           -> 7;
        sync_timeout                         -> 6;
        _                                    -> 0
    end.

handle_call({init, Recover}, From,
            State = #q{q = #amqqueue{exclusive_owner = none}}) ->
    declare(Recover, From, State);

%% You used to be able to declare an exclusive durable queue. Sadly we
%% need to still tidy up after that case, there could be the remnants
%% of one left over from an upgrade. So that's why we don't enforce
%% Recover = new here.
handle_call({init, Recover}, From,
            State = #q{q = #amqqueue{exclusive_owner = Owner}}) ->
    case rabbit_misc:is_process_alive(Owner) of
        true  -> erlang:monitor(process, Owner),
                 declare(Recover, From, State);
        false -> #q{backing_queue       = undefined,
                    backing_queue_state = undefined,
                    q                   = Q} = State,
                 gen_server2:reply(From, {owner_died, Q}),
                 BQ = backing_queue_module(Q),
                 {_, Terms} = recovery_status(Recover),
                 BQS = bq_init(BQ, Q, Terms),
                 %% Rely on terminate to delete the queue.
                 {stop, {shutdown, missing_owner},
                  State#q{backing_queue = BQ, backing_queue_state = BQS}}
    end;

handle_call(info, _From, State) ->
    reply(infos(?INFO_KEYS, State), State);

handle_call({info, Items}, _From, State) ->
    try
        reply({ok, infos(Items, State)}, State)
    catch Error -> reply({error, Error}, State)
    end;

handle_call(consumers, _From, State = #q{consumers = Consumers}) ->
    reply(rabbit_queue_consumers:all(Consumers), State);

handle_call({notify_down, ChPid}, _From, State) ->
    %% we want to do this synchronously, so that auto_deleted queues
    %% are no longer visible by the time we send a response to the
    %% client.  The queue is ultimately deleted in terminate/2; if we
    %% return stop with a reply, terminate/2 will be called by
    %% gen_server2 *before* the reply is sent.
    case handle_ch_down(ChPid, State) of
        {ok, State1}   -> reply(ok, State1);
        {stop, State1} -> stop(ok, State1)
    end;

handle_call({basic_get, ChPid, NoAck, LimiterPid}, _From,
            State = #q{q = #amqqueue{name = QName}}) ->
    AckRequired = not NoAck,
    State1 = ensure_expiry_timer(State),
    case fetch(AckRequired, State1) of
        {empty, State2} ->
            reply(empty, State2);
        {{Message, IsDelivered, AckTag},
         #q{backing_queue = BQ, backing_queue_state = BQS} = State2} ->
            case AckRequired of
                true  -> ok = rabbit_queue_consumers:record_ack(
                                ChPid, LimiterPid, AckTag);
                false -> ok
            end,
            Msg = {QName, self(), AckTag, IsDelivered, Message},
            reply({ok, BQ:len(BQS), Msg}, State2)
    end;

handle_call({basic_consume, NoAck, ChPid, LimiterPid, LimiterActive,
             PrefetchCount, ConsumerTag, ExclusiveConsume, Args, OkMsg},
            _From, State = #q{consumers          = Consumers,
                              exclusive_consumer = Holder}) ->
    case check_exclusive_access(Holder, ExclusiveConsume, State) of
        in_use -> reply({error, exclusive_consume_unavailable}, State);
        ok     -> Consumers1 = rabbit_queue_consumers:add(
                                 ChPid, ConsumerTag, NoAck,
                                 LimiterPid, LimiterActive,
                                 PrefetchCount, Args, is_empty(State),
                                 Consumers),
                  ExclusiveConsumer =
                      if ExclusiveConsume -> {ChPid, ConsumerTag};
                         true             -> Holder
                      end,
                  State1 = State#q{consumers          = Consumers1,
                                   has_had_consumers  = true,
                                   exclusive_consumer = ExclusiveConsumer},
                  ok = maybe_send_reply(ChPid, OkMsg),
                  emit_consumer_created(ChPid, ConsumerTag, ExclusiveConsume,
                                        not NoAck, qname(State1),
                                        PrefetchCount, Args, none),
                  notify_decorators(State1),
                  reply(ok, run_message_queue(State1))
    end;

handle_call({basic_cancel, ChPid, ConsumerTag, OkMsg}, _From,
            State = #q{consumers          = Consumers,
                       exclusive_consumer = Holder}) ->
    ok = maybe_send_reply(ChPid, OkMsg),
    case rabbit_queue_consumers:remove(ChPid, ConsumerTag, Consumers) of
        not_found ->
            reply(ok, State);
        Consumers1 ->
            Holder1 = case Holder of
                          {ChPid, ConsumerTag} -> none;
                          _                    -> Holder
                      end,
            State1 = State#q{consumers          = Consumers1,
                             exclusive_consumer = Holder1},
            emit_consumer_deleted(ChPid, ConsumerTag, qname(State1)),
            notify_decorators(State1),
            case should_auto_delete(State1) of
                false -> reply(ok, ensure_expiry_timer(State1));
                true  -> stop(ok, State1)
            end
    end;

handle_call(stat, _From, State) ->
    State1 = #q{backing_queue = BQ, backing_queue_state = BQS} =
        ensure_expiry_timer(State),
    reply({ok, BQ:len(BQS), rabbit_queue_consumers:count()}, State1);

handle_call({delete, IfUnused, IfEmpty}, _From,
            State = #q{backing_queue_state = BQS, backing_queue = BQ}) ->
    IsEmpty  = BQ:is_empty(BQS),
    IsUnused = is_unused(State),
    if
        IfEmpty  and not(IsEmpty)  -> reply({error, not_empty}, State);
        IfUnused and not(IsUnused) -> reply({error,    in_use}, State);
        true                       -> stop({ok, BQ:len(BQS)}, State)
    end;

handle_call(purge, _From, State = #q{backing_queue       = BQ,
                                     backing_queue_state = BQS}) ->
    {Count, BQS1} = BQ:purge(BQS),
    State1 = State#q{backing_queue_state = BQS1},
    reply({ok, Count}, maybe_send_drained(Count =:= 0, State1));

handle_call({requeue, AckTags, ChPid}, From, State) ->
    gen_server2:reply(From, ok),
    noreply(requeue(AckTags, ChPid, State));

handle_call(sync_mirrors, _From,
            State = #q{backing_queue       = rabbit_mirror_queue_master,
                       backing_queue_state = BQS}) ->
    S = fun(BQSN) -> State#q{backing_queue_state = BQSN} end,
    HandleInfo = fun (Status) ->
                         receive {'$gen_call', From, {info, Items}} ->
                                 Infos = infos(Items, State#q{status = Status}),
                                 gen_server2:reply(From, {ok, Infos})
                         after 0 ->
                                 ok
                         end
                 end,
    EmitStats = fun (Status) ->
                        rabbit_event:if_enabled(
                          State, #q.stats_timer,
                          fun() -> emit_stats(State#q{status = Status}) end)
                end,
    case rabbit_mirror_queue_master:sync_mirrors(HandleInfo, EmitStats, BQS) of
        {ok, BQS1}           -> reply(ok, S(BQS1));
        {stop, Reason, BQS1} -> {stop, Reason, S(BQS1)}
    end;

handle_call(sync_mirrors, _From, State) ->
    reply({error, not_mirrored}, State);

%% By definition if we get this message here we do not have to do anything.
handle_call(cancel_sync_mirrors, _From, State) ->
    reply({ok, not_syncing}, State).

handle_cast({run_backing_queue, Mod, Fun},
            State = #q{backing_queue = BQ, backing_queue_state = BQS}) ->
    noreply(State#q{backing_queue_state = BQ:invoke(Mod, Fun, BQS)});

handle_cast({deliver, Delivery = #delivery{sender = Sender}, Delivered, Flow},
            State = #q{senders = Senders}) ->
    Senders1 = case Flow of
                   flow   -> credit_flow:ack(Sender),
                             pmon:monitor(Sender, Senders);
                   noflow -> Senders
               end,
    State1 = State#q{senders = Senders1},
    noreply(deliver_or_enqueue(Delivery, Delivered, State1));

handle_cast({ack, AckTags, ChPid}, State) ->
    noreply(ack(AckTags, ChPid, State));

handle_cast({reject, true,  AckTags, ChPid}, State) ->
    noreply(requeue(AckTags, ChPid, State));

handle_cast({reject, false, AckTags, ChPid}, State) ->
    noreply(with_dlx(
              State#q.dlx,
              fun (X) -> subtract_acks(ChPid, AckTags, State,
                                       fun (State1) ->
                                               dead_letter_rejected_msgs(
                                                 AckTags, X, State1)
                                       end) end,
              fun () -> ack(AckTags, ChPid, State) end));

handle_cast(delete_immediately, State) ->
    stop(State);

handle_cast({resume, ChPid}, State) ->
    noreply(possibly_unblock(rabbit_queue_consumers:resume_fun(),
                             ChPid, State));

handle_cast({notify_sent, ChPid, Credit}, State) ->
    noreply(possibly_unblock(rabbit_queue_consumers:notify_sent_fun(Credit),
                             ChPid, State));

handle_cast({activate_limit, ChPid}, State) ->
    noreply(possibly_unblock(rabbit_queue_consumers:activate_limit_fun(),
                             ChPid, State));

handle_cast({set_ram_duration_target, Duration},
            State = #q{backing_queue = BQ, backing_queue_state = BQS}) ->
    BQS1 = BQ:set_ram_duration_target(Duration, BQS),
    noreply(State#q{backing_queue_state = BQS1});

handle_cast({set_maximum_since_use, Age}, State) ->
    ok = file_handle_cache:set_maximum_since_use(Age),
    noreply(State);

handle_cast(start_mirroring, State = #q{backing_queue       = BQ,
                                        backing_queue_state = BQS}) ->
    %% lookup again to get policy for init_with_existing_bq
    {ok, Q} = rabbit_amqqueue:lookup(qname(State)),
    true = BQ =/= rabbit_mirror_queue_master, %% assertion
    BQ1 = rabbit_mirror_queue_master,
    BQS1 = BQ1:init_with_existing_bq(Q, BQ, BQS),
    noreply(State#q{backing_queue       = BQ1,
                    backing_queue_state = BQS1});

handle_cast(stop_mirroring, State = #q{backing_queue       = BQ,
                                       backing_queue_state = BQS}) ->
    BQ = rabbit_mirror_queue_master, %% assertion
    {BQ1, BQS1} = BQ:stop_mirroring(BQS),
    noreply(State#q{backing_queue       = BQ1,
                    backing_queue_state = BQS1});

handle_cast({credit, ChPid, CTag, Credit, Drain},
            State = #q{consumers           = Consumers,
                       backing_queue       = BQ,
                       backing_queue_state = BQS}) ->
    Len = BQ:len(BQS),
    rabbit_channel:send_credit_reply(ChPid, Len),
    noreply(
      case rabbit_queue_consumers:credit(Len == 0, Credit, Drain, ChPid, CTag,
                                         Consumers) of
          unchanged               -> State;
          {unblocked, Consumers1} -> State1 = State#q{consumers = Consumers1},
                                     run_message_queue(true, State1)
      end);

handle_cast({force_event_refresh, Ref},
            State = #q{consumers          = Consumers,
                       exclusive_consumer = Exclusive}) ->
    rabbit_event:notify(queue_created, infos(?CREATION_EVENT_KEYS, State), Ref),
    QName = qname(State),
    AllConsumers = rabbit_queue_consumers:all(Consumers),
    case Exclusive of
        none       -> [emit_consumer_created(
                         Ch, CTag, false, AckRequired, QName, Prefetch,
                         Args, Ref) ||
                          {Ch, CTag, AckRequired, Prefetch, Args}
                              <- AllConsumers];
        {Ch, CTag} -> [{Ch, CTag, AckRequired, Prefetch, Args}] = AllConsumers,
                      emit_consumer_created(
                        Ch, CTag, true, AckRequired, QName, Prefetch, Args, Ref)
    end,
    noreply(State);

handle_cast(notify_decorators, State) ->
    notify_decorators(State),
    noreply(State);

handle_cast(policy_changed, State = #q{q = #amqqueue{name = Name}}) ->
    %% We depend on the #q.q field being up to date at least WRT
    %% policy (but not slave pids) in various places, so when it
    %% changes we go and read it from Mnesia again.
    %%
    %% This also has the side effect of waking us up so we emit a
    %% stats event - so event consumers see the changed policy.
    {ok, Q} = rabbit_amqqueue:lookup(Name),
    noreply(process_args_policy(State#q{q = Q})).

handle_info({maybe_expire, Vsn}, State = #q{args_policy_version = Vsn}) ->
    case is_unused(State) of
        true  -> stop(State);
        false -> noreply(State#q{expiry_timer_ref = undefined})
    end;

handle_info({maybe_expire, _Vsn}, State) ->
    noreply(State);

handle_info({drop_expired, Vsn}, State = #q{args_policy_version = Vsn}) ->
    WasEmpty = is_empty(State),
    State1 = drop_expired_msgs(State#q{ttl_timer_ref = undefined}),
    noreply(maybe_send_drained(WasEmpty, State1));

handle_info({drop_expired, _Vsn}, State) ->
    noreply(State);

handle_info(emit_stats, State) ->
    emit_stats(State),
    %% Don't call noreply/1, we don't want to set timers
    {State1, Timeout} = next_state(rabbit_event:reset_stats_timer(
                                     State, #q.stats_timer)),
    {noreply, State1, Timeout};

handle_info({'DOWN', _MonitorRef, process, DownPid, _Reason},
            State = #q{q = #amqqueue{exclusive_owner = DownPid}}) ->
    %% Exclusively owned queues must disappear with their owner.  In
    %% the case of clean shutdown we delete the queue synchronously in
    %% the reader - although not required by the spec this seems to
    %% match what people expect (see bug 21824). However we need this
    %% monitor-and-async- delete in case the connection goes away
    %% unexpectedly.
    stop(State);

handle_info({'DOWN', _MonitorRef, process, DownPid, _Reason}, State) ->
    case handle_ch_down(DownPid, State) of
        {ok, State1}   -> noreply(State1);
        {stop, State1} -> stop(State1)
    end;

handle_info(update_ram_duration, State = #q{backing_queue = BQ,
                                            backing_queue_state = BQS}) ->
    {RamDuration, BQS1} = BQ:ram_duration(BQS),
    DesiredDuration =
        rabbit_memory_monitor:report_ram_duration(self(), RamDuration),
    BQS2 = BQ:set_ram_duration_target(DesiredDuration, BQS1),
    %% Don't call noreply/1, we don't want to set timers
    {State1, Timeout} = next_state(State#q{rate_timer_ref      = undefined,
                                           backing_queue_state = BQS2}),
    {noreply, State1, Timeout};

handle_info(sync_timeout, State) ->
    noreply(backing_queue_timeout(State#q{sync_timer_ref = undefined}));

handle_info(timeout, State) ->
    noreply(backing_queue_timeout(State));

handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};

handle_info({bump_credit, Msg}, State = #q{backing_queue       = BQ,
                                           backing_queue_state = BQS}) ->
    credit_flow:handle_bump_msg(Msg),
    noreply(State#q{backing_queue_state = BQ:resume(BQS)});

handle_info(Info, State) ->
    {stop, {unhandled_info, Info}, State}.

handle_pre_hibernate(State = #q{backing_queue_state = undefined}) ->
    {hibernate, State};
handle_pre_hibernate(State = #q{backing_queue = BQ,
                                backing_queue_state = BQS}) ->
    {RamDuration, BQS1} = BQ:ram_duration(BQS),
    DesiredDuration =
        rabbit_memory_monitor:report_ram_duration(self(), RamDuration),
    BQS2 = BQ:set_ram_duration_target(DesiredDuration, BQS1),
    BQS3 = BQ:handle_pre_hibernate(BQS2),
    rabbit_event:if_enabled(
      State, #q.stats_timer,
      fun () -> emit_stats(State, [{idle_since,           now()},
                                   {consumer_utilisation, ''}]) end),
    State1 = rabbit_event:stop_stats_timer(State#q{backing_queue_state = BQS3},
                                           #q.stats_timer),
    {hibernate, stop_rate_timer(State1)}.

format_message_queue(Opt, MQ) -> rabbit_misc:format_message_queue(Opt, MQ).
