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
%% Copyright (c) 2011-2015 Pivotal Software, Inc.  All rights reserved.
%%

-define(HEADER_ACCEPT_VERSION, "accept-version").
-define(HEADER_ACK, "ack").
-define(HEADER_AMQP_MESSAGE_ID, "amqp-message-id").
-define(HEADER_APP_ID, "app-id").
-define(HEADER_CONTENT_ENCODING, "content-encoding").
-define(HEADER_CONTENT_LENGTH, "content-length").
-define(HEADER_CONTENT_TYPE, "content-type").
-define(HEADER_CORRELATION_ID, "correlation-id").
-define(HEADER_DESTINATION, "destination").
-define(HEADER_EXPIRATION, "expiration").
-define(HEADER_HEART_BEAT, "heart-beat").
-define(HEADER_HOST, "host").
-define(HEADER_ID, "id").
-define(HEADER_LOGIN, "login").
-define(HEADER_MESSAGE_ID, "message-id").
-define(HEADER_PASSCODE, "passcode").
-define(HEADER_PERSISTENT, "persistent").
-define(HEADER_PREFETCH_COUNT, "prefetch-count").
-define(HEADER_PRIORITY, "priority").
-define(HEADER_RECEIPT, "receipt").
-define(HEADER_REDELIVERED, "redelivered").
-define(HEADER_REPLY_TO, "reply-to").
-define(HEADER_SERVER, "server").
-define(HEADER_SESSION, "session").
-define(HEADER_SUBSCRIPTION, "subscription").
-define(HEADER_TIMESTAMP, "timestamp").
-define(HEADER_TRANSACTION, "transaction").
-define(HEADER_TYPE, "type").
-define(HEADER_USER_ID, "user-id").
-define(HEADER_VERSION, "version").
-define(HEADER_X_DEAD_LETTER_EXCHANGE, "x-dead-letter-exchange").
-define(HEADER_X_DEAD_LETTER_ROUTING_KEY, "x-dead-letter-routing-key").
-define(HEADER_X_EXPIRES, "x-expires").
-define(HEADER_X_MAX_LENGTH, "x-max-length").
-define(HEADER_X_MAX_LENGTH_BYTES, "x-max-length-bytes").
-define(HEADER_X_MAX_PRIORITY, "x-max-priority").
-define(HEADER_X_MESSAGE_TTL, "x-message-ttl").

-define(MESSAGE_ID_SEPARATOR, "@@").

-define(HEADERS_NOT_ON_SEND, [?HEADER_MESSAGE_ID]).

-define(TEMP_QUEUE_ID_PREFIX, "/temp-queue/").

-define(HEADER_ARGUMENTS, [
                           ?HEADER_X_DEAD_LETTER_EXCHANGE,
                           ?HEADER_X_DEAD_LETTER_ROUTING_KEY,
                           ?HEADER_X_EXPIRES,
                           ?HEADER_X_MAX_LENGTH,
                           ?HEADER_X_MAX_LENGTH_BYTES,
                           ?HEADER_X_MAX_PRIORITY,
                           ?HEADER_X_MESSAGE_TTL
                          ]).
