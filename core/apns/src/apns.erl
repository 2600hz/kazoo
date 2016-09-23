%% @doc Apple Push Notification Server for Erlang
-module(apns).
-author('Brujo Benavides <elbrujohalcon@inaka.net>').
-vsn('1.0.2').

-include("apns.hrl").
-include("localized.hrl").

-define(EPOCH, 62167219200).
-define(MAX_PAYLOAD, 256).

-behaviour(application).
-define(APP, ?MODULE).

-export([start/0, stop/0]).
-export([connect/0, connect/1, connect/2, connect/3, disconnect/1]).
-export([send_badge/3, send_message/2, send_message/3, send_message/4,
         send_message/5, send_message/6, send_message/7, send_message/8]).
-export([send_content_available/2, send_content_available/3]).
-export([estimate_available_bytes/1]).
-export([message_id/0, expiry/1, timestamp/1]).
-export([default_connection/0]).
-export([start/2, stop/1]).

-type status() :: no_errors
                | processing_error
                | missing_token
                | missing_topic
                | missing_payload
                | missing_token_size
                | missing_topic_size
                | missing_payload_size
                | invalid_token
                | unknown.
-export_type([status/0]).

-type connection() :: #apns_connection{}.
-export_type([connection/0]).

-type conn_id() :: atom() | pid().
-export_type([conn_id/0]).

-type apns_str() :: binary() | string().
-type alert() :: apns_str() | #loc_alert{}.
-export_type([alert/0]).

-type msg() :: #apns_msg{}.
-export_type([msg/0]).

%% @doc Starts the application
-spec start() -> 'ok'.
start() ->
    _ = application:ensure_all_started(?APP),
    'ok'.

%% @doc Stops the application
-spec stop() -> ok.
stop() ->
    ok.

%% @doc Opens an unnamed connection using the default parameters
-spec connect() -> {ok, pid()} | {error, Reason::any()}.
connect() ->
    connect(default_connection()).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, any()) ->
                   {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    apns_sup:start_link().

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.

%% @doc Opens an unnamed connection using the given feedback or error function
%%      or using the given connection() parameters
%%      or the name and default configuration if a name is given
-spec connect(atom() | string() | fun((string()) -> any()) | connection()) ->
                     {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
connect(Name) when is_atom(Name) ->
    connect(Name, default_connection());
connect(#apns_connection{} = Connection) ->
    apns_sup:start_connection(Connection);
connect(Fun) when is_function(Fun, 1) ->
    connect((default_connection())#apns_connection{feedback_fun = Fun});
connect(Fun) when is_function(Fun, 2) ->
    connect((default_connection())#apns_connection{error_fun = Fun}).

%% @doc Opens an connection named after the atom()
%%      using the given feedback or error function
%%      or using the given connection() parameters
-spec connect(atom(), string() | fun((string()) -> any()) | connection()) ->
                     {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
connect(Name, #apns_connection{} = Connection) ->
    apns_sup:start_connection(Name, Connection);
connect(Name, Fun) when is_function(Fun, 1) ->
    connect(Name, (default_connection())#apns_connection{feedback_fun = Fun});
connect(Name, Fun) when is_function(Fun, 2) ->
    connect(Name, (default_connection())#apns_connection{error_fun = Fun}).

%% @doc Opens an connection named after the atom()
%%      using the given feedback and error functions
-spec connect(atom(), fun((binary(), status()) -> stop | any()), fun((string()) -> any())) ->
                     {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::_}.
connect(Name, ErrorFun, FeedbackFun) ->
    connect(
      Name, (default_connection())#apns_connection{error_fun    = ErrorFun,
                                                   feedback_fun = FeedbackFun}).

%% @doc Closes an open connection
-spec disconnect(conn_id()) -> ok.
disconnect(ConnId) ->
    apns_connection:stop(ConnId).

%% @doc Sends a message to Apple
-spec send_message(conn_id(), msg()) -> ok.
send_message(ConnId, Msg) ->
    apns_connection:send_message(ConnId, Msg).

%% @doc Sends a message to Apple with content_available: 1
-spec send_content_available(conn_id(), string()) -> ok.
send_content_available(ConnId, DeviceToken) ->
    send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                   content_available = true,
                                   priority = 5}).

%% @doc Sends a message to Apple with content_available: 1 and an alert
-spec send_content_available(conn_id(), string(), string()) -> ok.
send_content_available(ConnId, DeviceToken, Alert) ->
    send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                   content_available = true,
                                   alert = Alert}).

%% @doc Sends a message to Apple with just a badge
-spec send_badge(conn_id(), string(), integer()) -> ok.
send_badge(ConnId, DeviceToken, Badge) ->
    send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                   badge = Badge}).

%% @doc Sends a message to Apple with just an alert
-spec send_message(conn_id(), string(), alert()) -> ok.
send_message(ConnId, DeviceToken, Alert) ->
    send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                   alert = Alert}).

%% @doc Sends a message to Apple with an alert and a badge
-spec send_message(
        conn_id(), Token::string(), Alert::alert(), Badge::integer()) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge) ->
    send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                   badge = Badge,
                                   alert = Alert}).

%% @doc Sends a full message to Apple
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(),
                   Sound::apns_str()) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge, Sound) ->
    send_message(ConnId, #apns_msg{alert = Alert,
                                   badge = Badge,
                                   sound = Sound,
                                   device_token = DeviceToken}).

%% @doc Predicts the number of bytes left in a message for additional data.
-spec estimate_available_bytes(msg()) -> integer().
estimate_available_bytes(#apns_msg{} = Msg) ->
    Payload = apns_connection:build_payload(Msg),
    ?MAX_PAYLOAD - erlang:size(Payload).

%% @doc Sends a full message to Apple (complete with expiry)
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(),
                   Sound::apns_str(), Expiry::non_neg_integer()) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge, Sound, Expiry) ->
    send_message(ConnId, #apns_msg{alert = Alert,
                                   badge = Badge,
                                   sound = Sound,
                                   expiry= Expiry,
                                   device_token = DeviceToken}).

%% @doc Sends a full message to Apple with expiry and extra arguments
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(),
                   Sound::apns_str(), Expiry::non_neg_integer(),
                   ExtraArgs::kz_proplist()) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs) ->
    send_message(ConnId, #apns_msg{alert = Alert,
                                   badge = Badge,
                                   sound = Sound,
                                   extra = ExtraArgs,
                                   expiry= Expiry,
                                   device_token = DeviceToken}).

%% @doc Sends a full message to Apple with id, expiry and extra arguments
-spec send_message(
        conn_id(), binary(), string(), alert(), integer(), apns_str(),
        non_neg_integer(), kz_proplist()) -> ok.
send_message(
  ConnId, MsgId, DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs) ->
    send_message(ConnId, #apns_msg{id     = MsgId,
                                   alert  = Alert,
                                   badge  = Badge,
                                   sound  = Sound,
                                   extra  = ExtraArgs,
                                   expiry = Expiry,
                                   device_token = DeviceToken}).

%% @doc  Generates an "unique" and valid message Id
-spec message_id() -> binary().
message_id() ->
    {_, _, MicroSecs} = kz_util:now(),
    Secs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    First = Secs rem 65536,
    Last = MicroSecs rem 65536,
    <<First:2/unsigned-integer-unit:8, Last:2/unsigned-integer-unit:8>>.

%% @doc  Generates a valid expiry value for messages.
%%       If called with <code>none</code> as the parameter, it will return a
%%       <code>no-expire</code> value.
%%       If called with a datetime as the parameter, it will convert it to a
%%       valid expiry value.
%%       If called with an integer, it will add that many seconds to current
%%       time and return a valid expiry value for that date.
-spec expiry(
        none | {{1970..9999, 1..12, 1..31}, {0..24, 0..60, 0..60}} | non_neg_integer()) ->
                    non_neg_integer().
expiry(none) -> 0;
expiry(Secs) when is_integer(Secs) ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time())
        - ?EPOCH + Secs;
expiry(Date) ->
    calendar:datetime_to_gregorian_seconds(Date) - ?EPOCH.

-spec timestamp(pos_integer()) -> calendar:datetime().
timestamp(Secs) ->
    calendar:gregorian_seconds_to_datetime(Secs + ?EPOCH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_env(K, Def) ->
    case application:get_env(apns, K) of
        {ok, V} -> V;
        _ -> Def
    end.

-spec default_connection() -> connection().
default_connection() ->
    DefaultConn = #apns_connection{},
    DefaultConn#apns_connection
        { apple_host =
              get_env(apple_host, DefaultConn#apns_connection.apple_host)
        , apple_port =
              get_env(apple_port, DefaultConn#apns_connection.apple_port)
        , key_file =
              get_env(key_file, DefaultConn#apns_connection.key_file)
        , cert_file =
              get_env(cert_file, DefaultConn#apns_connection.cert_file)
        , cert_password =
              get_env(cert_password, DefaultConn#apns_connection.cert_password)
        , timeout =
              get_env(timeout, DefaultConn#apns_connection.timeout)
        , error_fun =
              case get_env(error_fun, DefaultConn#apns_connection.error_fun) of
                  {M, F} -> fun(I, S) -> M:F(I, S) end;
                  Other -> Other
              end
        , feedback_timeout =
              get_env(feedback_timeout, DefaultConn#apns_connection.feedback_timeout)
        , feedback_fun =
              case get_env(feedback_fun, DefaultConn#apns_connection.feedback_fun) of
                  {M, F} -> fun(T) -> M:F(T) end;
                  Other -> Other
              end
        , feedback_host =
              get_env(feedback_host, DefaultConn#apns_connection.feedback_host)
        , feedback_port =
              get_env(feedback_port, DefaultConn#apns_connection.feedback_port)
        }.
