%%% @doc Main module for apns4erl API. Use this one from your own applications.
%%%
%%% Copyright 2017 Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(inaka_apns).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% API
-export([ start/0
        , stop/0
        , connect/1
        , connect/2
        , close_connection/1
        , push_notification/3
        , push_notification/4
        , push_notification_token/4
        , push_notification_token/5
        , default_headers/0
        , generate_token/2
        , get_feedback/0
        , get_feedback/1
        ]).

-export_type([ json/0
             , device_id/0
             , response/0
             , token/0
             , headers/0
             , stream_id/0
             ]).

-type json()      :: #{binary() | atom() => binary() | json()}.
-type device_id() :: binary().
-type stream_id() :: integer().
-type response()  :: { integer()          % HTTP2 Code
                     , [term()]           % Response Headers
                     , [term()] | no_body % Response Body
                     } | {timeout, stream_id()}.
-type token()     :: binary().
-type headers()   :: #{ apns_id          => binary()
                      , apns_expiration  => binary()
                      , apns_priority    => binary()
                      , apns_topic       => binary()
                      , apns_collapse_id => binary()
                      , apns_auth_token  => binary()
                      }.
-type feedback()  :: inaka_apns_feedback:feedback().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(inaka_apns),
    ok.

%% @doc Stops the Application
-spec stop() -> ok.
stop() ->
    ok = application:stop(inaka_apns),
    ok.

%% @doc Connects to APNs service with Provider Certificate or Token
-spec connect( inaka_apns_connection:type(), inaka_apns_connection:name()) ->
                     {ok, pid()} | {error, timeout}.
connect(Type, ConnectionName) ->
    DefaultConnection = inaka_apns_connection:default_connection(Type, ConnectionName),
    connect(DefaultConnection).

%% @doc Connects to APNs service
-spec connect(inaka_apns_connection:connection()) -> {ok, pid()} | {error, timeout}.
connect(Connection) ->
    inaka_apns_sup:create_connection(Connection).

%% @doc Closes the connection with APNs service.
-spec close_connection(inaka_apns_connection:name() | pid()) -> ok.
close_connection(ConnectionId) ->
    inaka_apns_connection:close_connection(ConnectionId).

%% @doc Push notification to APNs. It will use the headers provided on the
%%      environment variables.
-spec push_notification( inaka_apns_connection:name() | pid()
                       , device_id()
                       , json()
                       ) -> response() | {error, not_connection_owner}.
push_notification(ConnectionId, DeviceId, JSONMap) ->
    Headers = default_headers(),
    push_notification(ConnectionId, DeviceId, JSONMap, Headers).

%% @doc Push notification to certificate APNs Connection.
-spec push_notification( inaka_apns_connection:name() | pid()
                       , device_id()
                       , json()
                       , headers()
                       ) -> response() | {error, not_connection_owner}.
push_notification(ConnectionId, DeviceId, JSONMap, Headers) ->
    Notification = jsx:encode(JSONMap),
    inaka_apns_connection:push_notification( ConnectionId
                                           , DeviceId
                                           , Notification
                                           , Headers
                                           ).

%% @doc Push notification to APNs with authentication token. It will use the
%%      headers provided on the environment variables.
-spec push_notification_token( inaka_apns_connection:name() | pid()
                             , token()
                             , device_id()
                             , json()
                             ) -> response() | {error, not_connection_owner}.
push_notification_token(ConnectionId, Token, DeviceId, JSONMap) ->
    Headers = default_headers(),
    push_notification_token(ConnectionId, Token, DeviceId, JSONMap, Headers).

%% @doc Push notification to authentication token APNs Connection.
-spec push_notification_token( inaka_apns_connection:name() | pid()
                             , token()
                             , device_id()
                             , json()
                             , headers()
                             ) -> response() | {error, not_connection_owner}.
push_notification_token(ConnectionId, Token, DeviceId, JSONMap, Headers) ->
    Notification = jsx:encode(JSONMap),
    inaka_apns_connection:push_notification( ConnectionId
                                           , Token
                                           , DeviceId
                                           , Notification
                                           , Headers
                                           ).

-spec generate_token(binary(), binary()) -> token().
generate_token(TeamId, KeyId) ->
    Algorithm = <<"ES256">>,
    Header = jsx:encode([ {alg, Algorithm}
                        , {typ, <<"JWT">>}
                        , {kid, KeyId}
                        ]),
    Payload = jsx:encode([ {iss, TeamId}
                         , {iat, inaka_apns_utils:epoch()}
                         ]),
    HeaderEncoded = base64url:encode(Header),
    PayloadEncoded = base64url:encode(Payload),
    DataEncoded = <<HeaderEncoded/binary, $., PayloadEncoded/binary>>,
    Signature = inaka_apns_utils:sign(DataEncoded),
    <<DataEncoded/binary, $., Signature/binary>>.

%% @doc Get the default headers from environment variables.
-spec default_headers() -> headers().
default_headers() ->
    Headers = [ apns_id
              , apns_expiration
              , apns_priority
              , apns_topic
              , apns_collapse_id
              ],

    default_headers(Headers, #{}).

%% Requests for feedback to APNs. This requires Provider Certificate.
-spec get_feedback() -> [feedback()] | {error, term()} | timeout.
get_feedback() ->
    {ok, Host} = application:get_env(inaka_apns, feedback_host),
    {ok, Port} = application:get_env(inaka_apns, feedback_port),
    {ok, Certfile} = application:get_env(inaka_apns, certfile),
    Keyfile = application:get_env(inaka_apns, keyfile, undefined),
    {ok, Timeout} = application:get_env(inaka_apns, timeout),
    Config = #{ host     => Host
              , port     => Port
              , certfile => Certfile
              , keyfile  => Keyfile
              , timeout  => Timeout
              },
    get_feedback(Config).

%% Requests for feedback to APNs. This requires Provider Certificate.
-spec get_feedback(inaka_apns_feedback:feedback_config()) -> [feedback()] | {error, term()} | timeout.
get_feedback(Config) ->
    inaka_apns_feedback:get_feedback(Config).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Build a headers() structure from environment variables.
-spec default_headers(list(), headers()) -> headers().
default_headers([], Headers) ->
    Headers;
default_headers([Key | Keys], Headers) ->
    case application:get_env(inaka_apns, Key) of
        {ok, undefined} ->
            default_headers(Keys, Headers);
        {ok, Value} ->
            NewHeaders = Headers#{Key => to_binary(Value)},
            default_headers(Keys, NewHeaders)
    end.

%% Convert to binary
to_binary(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
    Value.
