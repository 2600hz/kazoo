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
%% The Original Code is RabbitMQ Federation.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_federation_queue).

-rabbit_boot_step({?MODULE,
                   [{description, "federation queue decorator"},
                    {mfa, {rabbit_registry, register,
                           [queue_decorator, <<"federation">>, ?MODULE]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_federation.hrl").

-behaviour(rabbit_queue_decorator).

-export([startup/1, shutdown/1, policy_changed/2, active_for/1,
         consumer_state_changed/3]).
-export([policy_changed_local/2]).

-import(rabbit_misc, [pget/2]).

%%----------------------------------------------------------------------------

startup(Q) ->
    case active_for(Q) of
        true  -> rabbit_federation_queue_link_sup_sup:start_child(Q);
        false -> ok
    end,
    ok.

shutdown(Q = #amqqueue{name = QName}) ->
    case active_for(Q) of
        true  -> rabbit_federation_queue_link_sup_sup:stop_child(Q),
                 rabbit_federation_status:remove_exchange_or_queue(QName);
        false -> ok
    end,
    ok.

policy_changed(Q1 = #amqqueue{name = QName}, Q2) ->
    case rabbit_amqqueue:lookup(QName) of
        {ok, #amqqueue{pid = QPid}} ->
            rpc:call(node(QPid), rabbit_federation_queue,
                     policy_changed_local, [Q1, Q2]);
        {error, not_found} ->
            ok
    end.

policy_changed_local(Q1, Q2) ->
    shutdown(Q1),
    startup(Q2).

active_for(Q = #amqqueue{arguments = Args}) ->
    case rabbit_misc:table_lookup(Args, <<"x-internal-purpose">>) of
        {longstr, _} -> false; %% [0]
        _            -> rabbit_federation_upstream:federate(Q)
    end.
%% [0] Currently the only "internal purpose" is federation, but I
%% suspect if we introduce another one it will also be for something
%% that doesn't want to be federated.

%% We need to reconsider whether we need to run or pause every time
%% the consumer state changes in the queue. But why can the state
%% change?
%%
%% consumer blocked   | We may have no more active consumers, and thus need to
%%                    | pause
%%                    |
%% consumer unblocked | We don't care
%%                    |
%% queue empty        | The queue has become empty therefore we need to run to
%%                    | get more messages
%%                    |
%% basic consume      | We don't care
%%                    |
%% basic cancel       | We may have no more active consumers, and thus need to
%%                    | pause
%%                    |
%% refresh            | We asked for it (we have started a new link after
%%                    | failover and need something to prod us into action
%%                    | (or not)).
%%
%% In the cases where we don't care it's not prohibitively expensive
%% for us to be here anyway, so never mind.
%%
%% Note that there is no "queue became non-empty" state change - that's
%% because of the queue invariant. If the queue transitions from empty to
%% non-empty then it must have no active consumers - in which case it stays
%% the same from our POV.

consumer_state_changed(#amqqueue{name = QName}, MaxActivePriority, IsEmpty) ->
    case IsEmpty andalso active_unfederated(MaxActivePriority) of
        true  -> rabbit_federation_queue_link:run(QName);
        false -> rabbit_federation_queue_link:pause(QName)
    end,
    ok.

active_unfederated(empty)         -> false;
active_unfederated(P) when P >= 0 -> true;
active_unfederated(_P)            -> false.
