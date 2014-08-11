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
-module(rabbit_ha_test_producer).

-export([await_response/1, start/5, create/5]).

-include_lib("amqp_client/include/amqp_client.hrl").

await_response(ProducerPid) ->
    error_logger:info_msg("waiting for producer pid ~p~n", [ProducerPid]),
    case receive {ProducerPid, Response} -> Response end of
        ok                -> ok;
        {error, _} = Else -> exit(Else);
        Else              -> exit({weird_response, Else})
    end.

create(Channel, Queue, TestPid, Confirm, MsgsToSend) ->
    ProducerPid = spawn_link(?MODULE, start, [Channel, Queue, TestPid,
                                              Confirm, MsgsToSend]),
    receive
        {ProducerPid, started} -> ProducerPid
    end.

start(Channel, Queue, TestPid, Confirm, MsgsToSend) ->
    ConfirmState =
        case Confirm of
            true  -> amqp_channel:register_confirm_handler(Channel, self()),
                     #'confirm.select_ok'{} =
                         amqp_channel:call(Channel, #'confirm.select'{}),
                     gb_trees:empty();
            false -> none
        end,
    TestPid ! {self(), started},
    error_logger:info_msg("publishing ~w msgs on ~p~n", [MsgsToSend, Channel]),
    producer(Channel, Queue, TestPid, ConfirmState, MsgsToSend).

%%
%% Private API
%%

producer(_Channel, _Queue, TestPid, none, 0) ->
    TestPid ! {self(), ok};
producer(Channel, _Queue, TestPid, ConfirmState, 0) ->
    error_logger:info_msg("awaiting confirms on channel ~p~n", [Channel]),
    Msg = case drain_confirms(no_nacks, ConfirmState) of
              no_nacks    -> ok;
              nacks       -> {error, received_nacks};
              {Nacks, CS} -> {error, {missing_confirms, Nacks,
                                      lists:sort(gb_trees:keys(CS))}}
          end,
    TestPid ! {self(), Msg};

producer(Channel, Queue, TestPid, ConfirmState, MsgsToSend) ->
    Method = #'basic.publish'{exchange    = <<"">>,
                              routing_key = Queue,
                              mandatory   = false,
                              immediate   = false},

    ConfirmState1 = maybe_record_confirm(ConfirmState, Channel, MsgsToSend),

    amqp_channel:call(Channel, Method,
                      #amqp_msg{props = #'P_basic'{delivery_mode = 2},
                                payload = list_to_binary(
                                            integer_to_list(MsgsToSend))}),

    producer(Channel, Queue, TestPid, ConfirmState1, MsgsToSend - 1).

maybe_record_confirm(none, _, _) ->
    none;
maybe_record_confirm(ConfirmState, Channel, MsgsToSend) ->
    SeqNo = amqp_channel:next_publish_seqno(Channel),
    gb_trees:insert(SeqNo, MsgsToSend, ConfirmState).

drain_confirms(Nacks, ConfirmState) ->
    case gb_trees:is_empty(ConfirmState) of
        true  -> Nacks;
        false -> receive
                     #'basic.ack'{delivery_tag = DeliveryTag,
                                  multiple     = IsMulti} ->
                         drain_confirms(Nacks,
                                        delete_confirms(DeliveryTag, IsMulti,
                                                        ConfirmState));
                     #'basic.nack'{delivery_tag = DeliveryTag,
                                   multiple     = IsMulti} ->
                         drain_confirms(nacks,
                                        delete_confirms(DeliveryTag, IsMulti,
                                                        ConfirmState))
                 after
                     60000 -> {Nacks, ConfirmState}
                 end
    end.

delete_confirms(DeliveryTag, false, ConfirmState) ->
    gb_trees:delete(DeliveryTag, ConfirmState);
delete_confirms(DeliveryTag, true, ConfirmState) ->
    multi_confirm(DeliveryTag, ConfirmState).

multi_confirm(DeliveryTag, ConfirmState) ->
    case gb_trees:is_empty(ConfirmState) of
        true  -> ConfirmState;
        false -> {Key, _, ConfirmState1} = gb_trees:take_smallest(ConfirmState),
                 case Key =< DeliveryTag of
                     true  -> multi_confirm(DeliveryTag, ConfirmState1);
                     false -> ConfirmState
                 end
    end.
