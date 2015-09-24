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

-module(rabbit_mqtt_util).

-include("rabbit_mqtt.hrl").

-compile(export_all).

subcription_queue_name(ClientId) ->
    Base = "mqtt-subscription-" ++ ClientId ++ "qos",
    {list_to_binary(Base ++ "0"), list_to_binary(Base ++ "1")}.

%% amqp mqtt descr
%% *    +    match one topic level
%% #    #    match multiple topic levels
%% .    /    topic level separator
mqtt2amqp(Topic) ->
    erlang:iolist_to_binary(
      re:replace(re:replace(Topic, "/", ".", [global]),
                 "[\+]", "*", [global])).

amqp2mqtt(Topic) ->
    erlang:iolist_to_binary(
      re:replace(re:replace(Topic, "[\*]", "+", [global]),
                 "[\.]", "/", [global])).

gen_client_id() ->
    lists:nthtail(1, rabbit_guid:string(rabbit_guid:gen_secure(), [])).

env(Key) ->
    case application:get_env(rabbitmq_mqtt, Key) of
        {ok, Val} -> coerce_env_value(Key, Val);
        undefined -> undefined
    end.

coerce_env_value(default_pass, Val) -> to_binary(Val);
coerce_env_value(default_user, Val) -> to_binary(Val);
coerce_env_value(exchange, Val)     -> to_binary(Val);
coerce_env_value(vhost, Val)        -> to_binary(Val);
coerce_env_value(_, Val)            -> Val.

to_binary(Val) when is_list(Val) -> list_to_binary(Val);
to_binary(Val)                   -> Val.

table_lookup(undefined, _Key) ->
    undefined;
table_lookup(Table, Key) ->
    rabbit_misc:table_lookup(Table, Key).
