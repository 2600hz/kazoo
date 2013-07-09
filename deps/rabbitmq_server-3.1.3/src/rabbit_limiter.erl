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
%% Copyright (c) 2007-2013 VMware, Inc.  All rights reserved.
%%

%% The purpose of the limiter is to stem the flow of messages from
%% queues to channels, in order to act upon various protocol-level
%% flow control mechanisms, specifically AMQP 0-9-1's basic.qos
%% prefetch_count and channel.flow, and AMQP 1.0's link (aka consumer)
%% credit mechanism.
%%
%% Each channel has an associated limiter process, created with
%% start_link/1, which it passes to queues on consumer creation with
%% rabbit_amqqueue:basic_consume/9, and rabbit_amqqueue:basic_get/4.
%% The latter isn't strictly necessary, since basic.get is not
%% subject to limiting, but it means that whenever a queue knows about
%% a channel, it also knows about its limiter, which is less fiddly.
%%
%% The limiter process holds state that is, in effect, shared between
%% the channel and all queues from which the channel is
%% consuming. Essentially all these queues are competing for access to
%% a single, limited resource - the ability to deliver messages via
%% the channel - and it is the job of the limiter process to mediate
%% that access.
%%
%% The limiter process is separate from the channel process for two
%% reasons: separation of concerns, and efficiency. Channels can get
%% very busy, particularly if they are also dealing with publishes.
%% With a separate limiter process all the aforementioned access
%% mediation can take place without touching the channel.
%%
%% For efficiency, both the channel and the queues keep some local
%% state, initialised from the limiter pid with new/1 and client/1,
%% respectively. In particular this allows them to avoid any
%% interaction with the limiter process when it is 'inactive', i.e. no
%% protocol-level flow control is taking place.
%%
%% This optimisation does come at the cost of some complexity though:
%% when a limiter becomes active, the channel needs to inform all its
%% consumer queues of this change in status. It does this by invoking
%% rabbit_amqqueue:activate_limit_all/2. Note that there is no inverse
%% transition, i.e. once a queue has been told about an active
%% limiter, it is not subsequently told when that limiter becomes
%% inactive. In practice it is rare for that to happen, though we
%% could optimise this case in the future.
%%
%% In addition, the consumer credit bookkeeping is local to queues, so
%% it is not necessary to store information about it in the limiter
%% process. But for abstraction we hide it from the queue behind the
%% limiter API, and it therefore becomes part of the queue local
%% state.
%%
%% The interactions with the limiter are as follows:
%%
%% 1. Channels tell the limiter about basic.qos prefetch counts -
%%    that's what the limit_prefetch/3, unlimit_prefetch/1,
%%    is_prefetch_limited/1, get_prefetch_limit/1 API functions are
%%    about - and channel.flow blocking - that's what block/1,
%%    unblock/1 and is_blocked/1 are for. They also tell the limiter
%%    queue state (via the queue) about consumer credit changes -
%%    that's what credit/4 is for.
%%
%% 2. Queues also tell the limiter queue state about the queue
%%    becoming empty (via drained/1) and consumers leaving (via
%%    forget_consumer/2).
%%
%% 3. Queues register with the limiter - this happens as part of
%%    activate/1.
%%
%% 4. The limiter process maintains an internal counter of 'messages
%%    sent but not yet acknowledged', called the 'volume'.
%%
%% 5. Queues ask the limiter for permission (with can_send/3) whenever
%%    they want to deliver a message to a channel. The limiter checks
%%    whether a) the channel isn't blocked by channel.flow, b) the
%%    volume has not yet reached the prefetch limit, and c) whether
%%    the consumer has enough credit. If so it increments the volume
%%    and tells the queue to proceed. Otherwise it marks the queue as
%%    requiring notification (see below) and tells the queue not to
%%    proceed.
%%
%% 6. A queue that has been told to proceed (by the return value of
%%    can_send/3) sends the message to the channel. Conversely, a
%%    queue that has been told not to proceed, will not attempt to
%%    deliver that message, or any future messages, to the
%%    channel. This is accomplished by can_send/3 capturing the
%%    outcome in the local state, where it can be accessed with
%%    is_suspended/1.
%%
%% 7. When a channel receives an ack it tells the limiter (via ack/2)
%%    how many messages were ack'ed. The limiter process decrements
%%    the volume and if it falls below the prefetch_count then it
%%    notifies (through rabbit_amqqueue:resume/2) all the queues
%%    requiring notification, i.e. all those that had a can_send/3
%%    request denied.
%%
%% 8. Upon receipt of such a notification, queues resume delivery to
%%    the channel, i.e. they will once again start asking limiter, as
%%    described in (5).
%%
%% 9. When a queue has no more consumers associated with a particular
%%    channel, it deactivates use of the limiter with deactivate/1,
%%    which alters the local state such that no further interactions
%%    with the limiter process take place until a subsequent
%%    activate/1.

-module(rabbit_limiter).

-behaviour(gen_server2).

-export([start_link/0]).
%% channel API
-export([new/1, limit_prefetch/3, unlimit_prefetch/1, block/1, unblock/1,
         is_prefetch_limited/1, is_blocked/1, is_active/1,
         get_prefetch_limit/1, ack/2, pid/1]).
%% queue API
-export([client/1, activate/1, can_send/3, resume/1, deactivate/1,
         is_suspended/1, is_consumer_blocked/2, credit/4, drained/1,
         forget_consumer/2]).
%% callbacks
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, prioritise_call/4]).

%%----------------------------------------------------------------------------

-record(lstate, {pid, prefetch_limited, blocked}).
-record(qstate, {pid, state, credits}).

-ifdef(use_specs).

-type(lstate() :: #lstate{pid              :: pid(),
                          prefetch_limited :: boolean(),
                          blocked          :: boolean()}).
-type(qstate() :: #qstate{pid :: pid(),
                          state :: 'dormant' | 'active' | 'suspended'}).

-spec(start_link/0 :: () -> rabbit_types:ok_pid_or_error()).
-spec(new/1 :: (pid()) -> lstate()).

-spec(limit_prefetch/3      :: (lstate(), non_neg_integer(), non_neg_integer())
                               -> lstate()).
-spec(unlimit_prefetch/1    :: (lstate()) -> lstate()).
-spec(block/1               :: (lstate()) -> lstate()).
-spec(unblock/1             :: (lstate()) -> lstate()).
-spec(is_prefetch_limited/1 :: (lstate()) -> boolean()).
-spec(is_blocked/1          :: (lstate()) -> boolean()).
-spec(is_active/1           :: (lstate()) -> boolean()).
-spec(get_prefetch_limit/1  :: (lstate()) -> non_neg_integer()).
-spec(ack/2                 :: (lstate(), non_neg_integer()) -> 'ok').
-spec(pid/1                 :: (lstate()) -> pid()).

-spec(client/1       :: (pid()) -> qstate()).
-spec(activate/1     :: (qstate()) -> qstate()).
-spec(can_send/3     :: (qstate(), boolean(), rabbit_types:ctag()) ->
                             {'continue' | 'suspend', qstate()}).
-spec(resume/1       :: (qstate()) -> qstate()).
-spec(deactivate/1   :: (qstate()) -> qstate()).
-spec(is_suspended/1 :: (qstate()) -> boolean()).
-spec(is_consumer_blocked/2 :: (qstate(), rabbit_types:ctag()) -> boolean()).
-spec(credit/4 :: (qstate(), rabbit_types:ctag(), non_neg_integer(), boolean())
                  -> qstate()).
-spec(drained/1 :: (qstate())
                   -> {[{rabbit_types:ctag(), non_neg_integer()}], qstate()}).
-spec(forget_consumer/2 :: (qstate(), rabbit_types:ctag()) -> qstate()).

-endif.

%%----------------------------------------------------------------------------

-record(lim, {prefetch_count = 0,
              ch_pid,
              blocked = false,
              queues = orddict:new(), % QPid -> {MonitorRef, Notify}
              volume = 0}).
%% 'Notify' is a boolean that indicates whether a queue should be
%% notified of a change in the limit or volume that may allow it to
%% deliver more messages via the limiter's channel.

-record(credit, {credit = 0, drain = false}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

start_link() -> gen_server2:start_link(?MODULE, [], []).

new(Pid) ->
    %% this a 'call' to ensure that it is invoked at most once.
    ok = gen_server:call(Pid, {new, self()}),
    #lstate{pid = Pid, prefetch_limited = false, blocked = false}.

limit_prefetch(L, PrefetchCount, UnackedCount) when PrefetchCount > 0 ->
    ok = gen_server:call(L#lstate.pid,
                         {limit_prefetch, PrefetchCount, UnackedCount}),
    L#lstate{prefetch_limited = true}.

unlimit_prefetch(L) ->
    ok = gen_server:call(L#lstate.pid, unlimit_prefetch),
    L#lstate{prefetch_limited = false}.

block(L) ->
    ok = gen_server:call(L#lstate.pid, block),
    L#lstate{blocked = true}.

unblock(L) ->
    ok = gen_server:call(L#lstate.pid, unblock),
    L#lstate{blocked = false}.

is_prefetch_limited(#lstate{prefetch_limited = Limited}) -> Limited.

is_blocked(#lstate{blocked = Blocked}) -> Blocked.

is_active(L) -> is_prefetch_limited(L) orelse is_blocked(L).

get_prefetch_limit(#lstate{prefetch_limited = false}) -> 0;
get_prefetch_limit(L) -> gen_server:call(L#lstate.pid, get_prefetch_limit).

ack(#lstate{prefetch_limited = false}, _AckCount) -> ok;
ack(L, AckCount) -> gen_server:cast(L#lstate.pid, {ack, AckCount}).

pid(#lstate{pid = Pid}) -> Pid.

client(Pid) -> #qstate{pid = Pid, state = dormant, credits = gb_trees:empty()}.

activate(L = #qstate{state = dormant}) ->
    ok = gen_server:cast(L#qstate.pid, {register, self()}),
    L#qstate{state = active};
activate(L) -> L.

can_send(L = #qstate{pid = Pid, state = State, credits = Credits},
         AckRequired, CTag) ->
    case is_consumer_blocked(L, CTag) of
        false -> case (State =/= active orelse
                       safe_call(Pid, {can_send, self(), AckRequired}, true)) of
                     true  -> {continue, L#qstate{
                                credits = record_send_q(CTag, Credits)}};
                     false -> {suspend, L#qstate{state = suspended}}
                 end;
        true  -> {suspend, L}
    end.

safe_call(Pid, Msg, ExitValue) ->
    rabbit_misc:with_exit_handler(
      fun () -> ExitValue end,
      fun () -> gen_server2:call(Pid, Msg, infinity) end).

resume(L = #qstate{state = suspended}) ->
    L#qstate{state = active};
resume(L) -> L.

deactivate(L = #qstate{state = dormant}) -> L;
deactivate(L) ->
    ok = gen_server:cast(L#qstate.pid, {unregister, self()}),
    L#qstate{state = dormant}.

is_suspended(#qstate{state = suspended}) -> true;
is_suspended(#qstate{})                  -> false.

is_consumer_blocked(#qstate{credits = Credits}, CTag) ->
    case gb_trees:lookup(CTag, Credits) of
        {value, #credit{credit = C}} when C > 0 -> false;
        {value, #credit{}}                      -> true;
        none                                    -> false
    end.

credit(Limiter = #qstate{credits = Credits}, CTag, Credit, Drain) ->
    Limiter#qstate{credits = update_credit(CTag, Credit, Drain, Credits)}.

drained(Limiter = #qstate{credits = Credits}) ->
    {CTagCredits, Credits2} =
        rabbit_misc:gb_trees_fold(
          fun (CTag,  #credit{credit = C,  drain = true},  {Acc, Creds0}) ->
                  {[{CTag, C} | Acc], update_credit(CTag, 0, false, Creds0)};
              (_CTag, #credit{credit = _C, drain = false}, {Acc, Creds0}) ->
                  {Acc, Creds0}
          end, {[], Credits}, Credits),
    {CTagCredits, Limiter#qstate{credits = Credits2}}.

forget_consumer(Limiter = #qstate{credits = Credits}, CTag) ->
    Limiter#qstate{credits = gb_trees:delete_any(CTag, Credits)}.

%%----------------------------------------------------------------------------
%% Queue-local code
%%----------------------------------------------------------------------------

%% We want to do all the AMQP 1.0-ish link level credit calculations
%% in the queue (to do them elsewhere introduces a ton of
%% races). However, it's a big chunk of code that is conceptually very
%% linked to the limiter concept. So we get the queue to hold a bit of
%% state for us (#qstate.credits), and maintain a fiction that the
%% limiter is making the decisions...

record_send_q(CTag, Credits) ->
    case gb_trees:lookup(CTag, Credits) of
        {value, #credit{credit = Credit, drain = Drain}} ->
            update_credit(CTag, Credit - 1, Drain, Credits);
        none ->
            Credits
    end.

update_credit(CTag, Credit, Drain, Credits) ->
    %% Using up all credit implies no need to send a 'drained' event
    Drain1 = Drain andalso Credit > 0,
    gb_trees:enter(CTag, #credit{credit = Credit, drain = Drain1}, Credits).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

init([]) -> {ok, #lim{}}.

prioritise_call(get_prefetch_limit, _From, _Len, _State) -> 9;
prioritise_call(_Msg,               _From, _Len, _State) -> 0.

handle_call({new, ChPid}, _From, State = #lim{ch_pid = undefined}) ->
    {reply, ok, State#lim{ch_pid = ChPid}};

handle_call({limit_prefetch, PrefetchCount, UnackedCount}, _From,
            State = #lim{prefetch_count = 0}) ->
    {reply, ok, maybe_notify(State, State#lim{prefetch_count = PrefetchCount,
                                              volume         = UnackedCount})};
handle_call({limit_prefetch, PrefetchCount, _UnackedCount}, _From, State) ->
    {reply, ok, maybe_notify(State, State#lim{prefetch_count = PrefetchCount})};

handle_call(unlimit_prefetch, _From, State) ->
    {reply, ok, maybe_notify(State, State#lim{prefetch_count = 0,
                                              volume         = 0})};

handle_call(block, _From, State) ->
    {reply, ok, State#lim{blocked = true}};

handle_call(unblock, _From, State) ->
    {reply, ok, maybe_notify(State, State#lim{blocked = false})};

handle_call(get_prefetch_limit, _From,
            State = #lim{prefetch_count = PrefetchCount}) ->
    {reply, PrefetchCount, State};

handle_call({can_send, QPid, _AckRequired}, _From,
            State = #lim{blocked = true}) ->
    {reply, false, limit_queue(QPid, State)};
handle_call({can_send, QPid, AckRequired}, _From,
            State = #lim{volume = Volume}) ->
    case prefetch_limit_reached(State) of
        true  -> {reply, false, limit_queue(QPid, State)};
        false -> {reply, true,  State#lim{volume = if AckRequired -> Volume + 1;
                                                      true        -> Volume
                                                   end}}
    end.

handle_cast({ack, Count}, State = #lim{volume = Volume}) ->
    NewVolume = if Volume == 0 -> 0;
                   true        -> Volume - Count
                end,
    {noreply, maybe_notify(State, State#lim{volume = NewVolume})};

handle_cast({register, QPid}, State) ->
    {noreply, remember_queue(QPid, State)};

handle_cast({unregister, QPid}, State) ->
    {noreply, forget_queue(QPid, State)}.

handle_info({'DOWN', _MonitorRef, _Type, QPid, _Info}, State) ->
    {noreply, forget_queue(QPid, State)}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% Internal plumbing
%%----------------------------------------------------------------------------

maybe_notify(OldState, NewState) ->
    case (prefetch_limit_reached(OldState) orelse blocked(OldState)) andalso
        not (prefetch_limit_reached(NewState) orelse blocked(NewState)) of
        true  -> notify_queues(NewState);
        false -> NewState
    end.

prefetch_limit_reached(#lim{prefetch_count = Limit, volume = Volume}) ->
    Limit =/= 0 andalso Volume >= Limit.

blocked(#lim{blocked = Blocked}) -> Blocked.

remember_queue(QPid, State = #lim{queues = Queues}) ->
    case orddict:is_key(QPid, Queues) of
        false -> MRef = erlang:monitor(process, QPid),
                 State#lim{queues = orddict:store(QPid, {MRef, false}, Queues)};
        true  -> State
    end.

forget_queue(QPid, State = #lim{queues = Queues}) ->
    case orddict:find(QPid, Queues) of
        {ok, {MRef, _}} -> true = erlang:demonitor(MRef),
                           State#lim{queues = orddict:erase(QPid, Queues)};
        error           -> State
    end.

limit_queue(QPid, State = #lim{queues = Queues}) ->
    UpdateFun = fun ({MRef, _}) -> {MRef, true} end,
    State#lim{queues = orddict:update(QPid, UpdateFun, Queues)}.

notify_queues(State = #lim{ch_pid = ChPid, queues = Queues}) ->
    {QList, NewQueues} =
        orddict:fold(fun (_QPid, {_, false}, Acc) -> Acc;
                         (QPid, {MRef, true}, {L, D}) ->
                             {[QPid | L], orddict:store(QPid, {MRef, false}, D)}
                     end, {[], Queues}, Queues),
    case length(QList) of
        0 -> ok;
        1 -> ok = rabbit_amqqueue:resume(hd(QList), ChPid); %% common case
        L ->
            %% We randomly vary the position of queues in the list,
            %% thus ensuring that each queue has an equal chance of
            %% being notified first.
            {L1, L2} = lists:split(random:uniform(L), QList),
            [[ok = rabbit_amqqueue:resume(Q, ChPid) || Q <- L3]
             || L3 <- [L2, L1]],
            ok
    end,
    State#lim{queues = NewQueues}.
