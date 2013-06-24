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

-module(rabbit_exchange_type).

-ifdef(use_specs).

-type(tx() :: 'transaction' | 'none').
-type(serial() :: pos_integer() | tx()).

-callback description() -> [proplists:property()].

%% Should Rabbit ensure that all binding events that are
%% delivered to an individual exchange can be serialised? (they
%% might still be delivered out of order, but there'll be a
%% serial number).
-callback serialise_events() -> boolean().

%% The no_return is there so that we can have an "invalid" exchange
%% type (see rabbit_exchange_type_invalid).
-callback route(rabbit_types:exchange(), rabbit_types:delivery()) ->
    rabbit_router:match_result().

%% called BEFORE declaration, to check args etc; may exit with #amqp_error{}
-callback validate(rabbit_types:exchange()) -> 'ok'.

%% called after declaration and recovery
-callback create(tx(), rabbit_types:exchange()) -> 'ok'.

%% called after exchange (auto)deletion.
-callback delete(tx(), rabbit_types:exchange(), [rabbit_types:binding()]) ->
    'ok'.

%% called after a binding has been added or recovered
-callback add_binding(serial(), rabbit_types:exchange(),
                      rabbit_types:binding()) -> 'ok'.

%% called after bindings have been deleted.
-callback remove_bindings(serial(), rabbit_types:exchange(),
                          [rabbit_types:binding()]) -> 'ok'.

%% called when comparing exchanges for equivalence - should return ok or
%% exit with #amqp_error{}
-callback assert_args_equivalence(rabbit_types:exchange(),
                                  rabbit_framing:amqp_table()) ->
    'ok' | rabbit_types:connection_exit().

%% called when the policy attached to this exchange changes.
-callback policy_changed(serial(), rabbit_types:exchange(),
                         rabbit_types:exchange()) -> 'ok'.

-else.

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{description, 0}, {serialise_events, 0}, {route, 2}, {validate, 1},
     {create, 2}, {delete, 3}, {add_binding, 3}, {remove_bindings, 3},
     {assert_args_equivalence, 2}, {policy_changed, 3}];
behaviour_info(_Other) ->
    undefined.

-endif.
