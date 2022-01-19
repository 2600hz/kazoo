%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc Test the directories API endpoint
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_blackhole).

-export([seq/0
        ,seq_api/0
        ,seq_amqp_disconnect/0
        ,seq_count_bindings/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-define(PORT, kapps_config:get_integer(<<"blackhole">>, <<"port">>, 5555)).

-spec seq() -> 'ok'.
seq() ->
    _ = [seq_ping()
        ,seq_max_conn()
        ,seq_api()
        ,seq_count_bindings()
         %% not running this during CI as it causes error.log CRASH REPORT
         %% when the AMQP connection is torn down
         %%,seq_amqp_disconnect()
        ],
    'ok'.

seq_ping() ->
    Model = initial_state(),
    #{'auth_token' := AuthToken} = API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    _AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    {'ok', WSConn} = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("connected to websocket: ~p", [WSConn]),

    PingReqId = kz_binary:rand_hex(4),
    Ping = kz_json:from_list([{<<"request_id">>, PingReqId}
                             ,{<<"action">>, <<"ping">>}
                             ,{<<"auth_token">>, AuthToken}
                             ]),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(Ping)),
    {'json', ReplyJObj} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("pong: ~p", [ReplyJObj]),
    PingReqId = kz_json:get_ne_binary_value(<<"request_id">>, ReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, ReplyJObj),

    timer:sleep(2 * ?MILLISECONDS_IN_SECOND),

    _Send = pqc_ws_client:send(WSConn, kz_json:encode(Ping)),
    {'json', Reply2JObj} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("pong2: ~p", [Reply2JObj]),

    pqc_ws_client:close(WSConn),

    cleanup(API),
    lager:info("FINISHED PING SEQ").

seq_max_conn() ->
    Model = initial_state(),
    #{'auth_token' := AuthToken} = API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    _AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    PrevMaxConn = kapps_config:get_integer(<<"blackhole">>, <<"max_connections_per_ip">>),
    _ = kapps_config:set_default(<<"blackhole">>, <<"max_connections_per_ip">>, 1),

    {'ok', WSConn} = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("connected to websocket: ~p", [WSConn]),

    PingReqId = kz_binary:rand_hex(4),
    Ping = kz_json:from_list([{<<"request_id">>, PingReqId}
                             ,{<<"action">>, <<"ping">>}
                             ,{<<"auth_token">>, AuthToken}
                             ]),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(Ping)),
    {'json', ReplyJObj} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("pong: ~p", [ReplyJObj]),
    PingReqId = kz_json:get_ne_binary_value(<<"request_id">>, ReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, ReplyJObj),

    {'error', {'ws_upgrade_failed', {429, _WSHeaders}}} = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("failed to connect a second time"),

    pqc_ws_client:close(WSConn),

    _ = kapps_config:set_default(<<"blackhole">>, <<"max_connections_per_ip">>, PrevMaxConn),

    cleanup(API),
    lager:info("FINISHED MAX_CONN SEQ").

-spec seq_api() -> 'ok'.
seq_api() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    AvailableBindings = pqc_cb_websockets:available(API),
    lager:info("available: ~s", [AvailableBindings]),
    'true' = ([] =/= kz_json:is_json_object([<<"data">>, <<"call">>], kz_json:decode(AvailableBindings))),

    test_empty_active_connections(API, AccountId),

    {'ok', WSConn} = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("connected to websocket: ~p", [WSConn]),

    %% test pinging the websocket
    _ = test_ws_ping(API, WSConn),

    Binding = <<"object.*.user">>,

    %% test using the API to list ws connections
    {SocketId, BindReq} = test_ws_api_listing(API, WSConn, AccountId, Binding),

    %% test receiving events over the ws
    _ = test_crud_user_events(WSConn, API, AccountId),

    %% test receiving call events over the ws
    _ = test_channel_create(WSConn, API, AccountId),

    %% test unbinding for events
    _ = test_ws_unbind(API, WSConn, AccountId, SocketId, BindReq, Binding),

    pqc_ws_client:close(WSConn),

    test_empty_active_connections(API, AccountId),

    cleanup(API),
    lager:info("FINISHED API SEQ").

-spec seq_amqp_disconnect() -> 'ok'.
seq_amqp_disconnect() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    AvailableBindings = pqc_cb_websockets:available(API),
    lager:info("available: ~s", [AvailableBindings]),
    'true' = ([] =/= kz_json:is_json_object([<<"data">>, <<"call">>], kz_json:decode(AvailableBindings))),

    test_empty_active_connections(API, AccountId),

    {'ok', WSConn} = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("connected to websocket: ~p", [WSConn]),

    %% test pinging the websocket
    _ = test_ws_ping(API, WSConn),

    Binding = <<"object.*.user">>,

    %% test using the API to list ws connections
    {SocketId, BindReq} = test_ws_api_listing(API, WSConn, AccountId, Binding),
    lager:info("socket ~p bind ~p", [SocketId, BindReq]),

    %% test receiving events over the ws
    _ = test_crud_user_events(WSConn, API, AccountId),

    %% test receiving call events over the ws
    _ = test_channel_create(WSConn, API, AccountId),

    _ = disconnect_reconnect_amqp(),

    %% test receiving events over the ws
    _ = test_crud_user_events(WSConn, API, AccountId),

    %% test receiving call events over the ws
    _ = test_channel_create(WSConn, API, AccountId),

    pqc_ws_client:close(WSConn),

    test_empty_active_connections(API, AccountId),

    cleanup(API),
    lager:info("FINISHED AMQP DISCONNECT").

%%------------------------------------------------------------------------------
%% @doc Test that bindings remain after subscriber unbind
%%

%% Bug: When a subscriber unbinds for a call event, blackhole can
%% erroneously unbind the AMQP queue for that call event, causing
%% missed events for other subscribers.

%% Context: Blackhole tracks how many subscribers are bound for a
%% particular binding. For call events, the Binding is typically the
%% {hook, AccountId, ChannelEvent} tuple. The counter converts this to
%% base64-encoded term_to_binary({hook, AccountId, ChannelEvent})
%% which means two different accounts for the same call event will
%% register as different counter Keys.

%% When the subscriber unbinds or disconnects, the Key is decremented
%% to 0 and kz_hooks is called to deregister the
%% binding. kz_hooks_util then discards the AccountId from the tuple
%% and unbinds the call event. Any other subscribers for that channel
%% event will now miss the events until a new subscriber binds for the
%% channel event again.

%% The Fix: blackhole_listener should just use the call event as the
%% Key instead of the converted tuple. Additional consideration should
%% be made to count bindings for `call.*.*` which match any Event or
%% AccountId allowed for that subscriber.

%% @end
%%------------------------------------------------------------------------------
-spec seq_count_bindings() -> 'ok'.
seq_count_bindings() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    AvailableBindings = pqc_cb_websockets:available(API),
    lager:info("available: ~s", [AvailableBindings]),
    'true' = ([] =/= kz_json:is_json_object([<<"data">>, <<"call">>], kz_json:decode(AvailableBindings))),

    test_empty_active_connections(API, AccountId),

    {'ok', WSConn} = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("connected to websocket: ~p", [WSConn]),

    %% test pinging the websocket
    _ = test_ws_ping(API, WSConn),

    %% bind for CHANNEL_CREATE on account and auth-account
    bind_for_channel_events(WSConn, API, AccountId),
    bind_for_channel_events(WSConn, API, pqc_cb_api:auth_account_id(API)),

    %% emit the call event on the account and receive it
    CallId = emit_channel_event(AccountId),
    _ = recv_channel_event(WSConn, CallId, AccountId),

    %% unbind for CHANNEL_CREATE on auth-account
    _ = unbind_for_channel_events(WSConn, API, pqc_cb_api:auth_account_id(API)),

    %% emit the call event on the account and should still receive it
    SecondCallId = emit_channel_event(AccountId),
    recv_channel_event(WSConn, SecondCallId, AccountId),

    pqc_ws_client:close(WSConn),

    test_empty_active_connections(API, AccountId),

    cleanup(API),
    lager:info("FINISHED COUNT BINDINGS").

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    _ = init_system(),
    API = pqc_cb_api:authenticate(),
    pqc_kazoo_model:new(API).

init_system() ->
    TestId = kz_binary:rand_hex(5),
    kz_util:put_callid(TestId),

    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar', 'blackhole']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_websockets']
        ],

    blackhole_listener:flush(),

    lager:info("INIT FINISHED").

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() -> 'ok'.

test_ws_ping(#{auth_token := AuthToken}, WSConn) ->
    PingReqId = kz_binary:rand_hex(4),
    Ping = kz_json:from_list([{<<"request_id">>, PingReqId}
                             ,{<<"action">>, <<"ping">>}
                             ,{<<"auth_token">>, AuthToken}
                             ]),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(Ping)),
    {'json', ReplyJObj} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("pong: ~p", [ReplyJObj]),
    PingReqId = kz_json:get_ne_binary_value(<<"request_id">>, ReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, ReplyJObj).

test_ws_api_listing(#{auth_token := AuthToken}=API, WSConn, AccountId, Binding) ->
    WithSocket = pqc_cb_websockets:summary(API, AccountId),
    lager:info("with socket: ~s", [WithSocket]),
    [SocketDetails] = kz_json:get_list_value(<<"data">>, kz_json:decode(WithSocket)),
    [] = kz_json:get_list_value(<<"bindings">>, SocketDetails),
    SocketId = kz_json:get_ne_binary_value(<<"websocket_session_id">>, SocketDetails),

    BindReqId = kz_binary:rand_hex(4),
    BindReq = kz_json:from_list([{<<"action">>, <<"subscribe">>}
                                ,{<<"auth_token">>, AuthToken}
                                ,{<<"request_id">>, BindReqId}
                                ,{<<"data">>
                                 ,kz_json:from_list([{<<"account_id">>, AccountId}
                                                    ,{<<"binding">>, Binding}
                                                    ])
                                 }
                                ]),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(BindReq)),
    {'json', BindReplyJObj} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("bind reply: ~p", [BindReplyJObj]),
    <<"reply">> = kz_json:get_ne_binary_value(<<"action">>, BindReplyJObj),
    BindReqId = kz_json:get_ne_binary_value(<<"request_id">>, BindReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, BindReplyJObj),
    [Binding] = kz_json:get_list_value([<<"data">>, <<"subscribed">>], BindReplyJObj),

    DetailsResp = pqc_cb_websockets:details(API, AccountId, SocketId),
    lager:info("details: ~s", [DetailsResp]),
    DetailsJObj = kz_json:decode(DetailsResp),
    [Binding] = kz_json:get_list_value([<<"data">>, <<"bindings">>], DetailsJObj),
    <<"127.0.0.1">> = kz_json:get_ne_binary_value([<<"data">>, <<"source">>], DetailsJObj),
    SocketId = kz_json:get_ne_binary_value([<<"data">>, <<"websocket_session_id">>], DetailsJObj),

    {SocketId, BindReq}.

test_crud_user_events(WSConn, API, AccountId) ->
    UserDoc = pqc_cb_users:user_doc(),
    Create = pqc_cb_users:create(API, AccountId, UserDoc),
    lager:info("created user ~s", [Create]),
    UserId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(Create)),

    {'json', CreateEvent} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("create event: ~p", [CreateEvent]),

    <<"event">> = kz_json:get_ne_binary_value(<<"action">>, CreateEvent),
    <<"object.*.user">> = kz_json:get_ne_binary_value(<<"subscribed_key">>, CreateEvent),
    <<"doc_created">> = kz_json:get_ne_binary_value(<<"name">>, CreateEvent),

    CreateJObj = kz_json:get_json_value(<<"data">>, CreateEvent),
    <<"user">> = kz_json:get_ne_binary_value(<<"type">>, CreateJObj),
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, CreateJObj),
    UserId = kz_json:get_ne_binary_value(<<"id">>, CreateJObj),

    Delete = pqc_cb_users:delete(API, AccountId, UserId),
    lager:info("deleted user ~s", [Delete]),

    {'json', DeleteEvent} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("delete event: ~p", [DeleteEvent]),

    <<"event">> = kz_json:get_ne_binary_value(<<"action">>, DeleteEvent),
    <<"object.*.user">> = kz_json:get_ne_binary_value(<<"subscribed_key">>, DeleteEvent),
    <<"doc_deleted">> = kz_json:get_ne_binary_value(<<"name">>, DeleteEvent),

    DeleteJObj = kz_json:get_json_value(<<"data">>, DeleteEvent),
    <<"user">> = kz_json:get_ne_binary_value(<<"type">>, DeleteJObj),
    'true' = kz_json:is_true(<<"is_soft_deleted">>, DeleteJObj),
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, DeleteJObj),
    UserId = kz_json:get_ne_binary_value(<<"id">>, DeleteJObj).

test_ws_unbind(API, WSConn, AccountId, SocketId, BindReq, Binding) ->
    UnbindReqId = kz_binary:rand_hex(4),
    UnbindReq = kz_json:set_values([{<<"action">>, <<"unsubscribe">>}
                                   ,{<<"request_id">>, UnbindReqId}
                                   ]
                                  ,BindReq
                                  ),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(UnbindReq)),
    {'json', UnbindReplyJObj} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("unbind reply: ~p", [UnbindReplyJObj]),
    <<"reply">> = kz_json:get_ne_binary_value(<<"action">>, UnbindReplyJObj),
    UnbindReqId = kz_json:get_ne_binary_value(<<"request_id">>, UnbindReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, UnbindReplyJObj),
    [Binding] = kz_json:get_list_value([<<"data">>, <<"unsubscribed">>], UnbindReplyJObj),
    [] = kz_json:get_list_value([<<"data">>, <<"subscribed">>], UnbindReplyJObj),

    NoDetailsResp = pqc_cb_websockets:details(API, AccountId, SocketId),
    lager:info("no details: ~s", [NoDetailsResp]),
    NoDetailsJObj = kz_json:decode(NoDetailsResp),
    [] = kz_json:get_list_value([<<"data">>, <<"bindings">>], NoDetailsJObj),
    <<"127.0.0.1">> = kz_json:get_ne_binary_value([<<"data">>, <<"source">>], NoDetailsJObj),
    SocketId = kz_json:get_ne_binary_value([<<"data">>, <<"websocket_session_id">>], NoDetailsJObj).

test_empty_active_connections(API, AccountId) ->
    EmptySockets = pqc_cb_websockets:summary(API, AccountId),
    lager:info("empty: ~s", [EmptySockets]),
    [] = kz_json:get_list_value(<<"data">>, kz_json:decode(EmptySockets)).

test_channel_create(WSConn, API, AccountId) ->
    bind_for_channel_events(WSConn, API, AccountId),

    CallId = emit_channel_event(AccountId),

    recv_channel_event(WSConn, CallId, AccountId),

    unbind_for_channel_events(WSConn, API, AccountId).

recv_channel_event(WSConn, CallId, AccountId) ->
    {'json', CallEvent} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("call event: ~p", [CallEvent]),
    CallId = kz_json:get_ne_binary_value([<<"data">>, <<"call_id">>], CallEvent),
    AccountId = kz_json:get_ne_binary_value([<<"data">>, <<"custom_channel_vars">>, <<"account_id">>], CallEvent),
    <<"CHANNEL_CREATE">> = kz_json:get_ne_binary_value([<<"name">>], CallEvent),
    <<"event">> = kz_json:get_ne_binary_value([<<"action">>], CallEvent),
    'ok'.

unbind_for_channel_events(WSConn, #{auth_token := AuthToken}, AccountId) ->
    UnbindReqId = kz_binary:rand_hex(4),
    UnbindReq = kz_json:from_list([{<<"action">>, <<"unsubscribe">>}
                                  ,{<<"auth_token">>, AuthToken}
                                  ,{<<"request_id">>, UnbindReqId}
                                  ,{<<"data">>
                                   ,kz_json:from_list([{<<"account_id">>, AccountId}
                                                      ,{<<"binding">>, (Binding = <<"call.CHANNEL_CREATE.*">>)}
                                                      ])
                                   }
                                  ]),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(UnbindReq)),

    {'json', UnbindReplyJObj} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("unbind reply: ~p", [UnbindReplyJObj]),
    <<"reply">> = kz_json:get_ne_binary_value(<<"action">>, UnbindReplyJObj),
    UnbindReqId = kz_json:get_ne_binary_value(<<"request_id">>, UnbindReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, UnbindReplyJObj),
    [Binding] = kz_json:get_list_value([<<"data">>, <<"unsubscribed">>], UnbindReplyJObj),
    'ok'.

bind_for_channel_events(WSConn, #{auth_token := AuthToken}, AccountId) ->
    BindReqId = kz_binary:rand_hex(4),
    BindReq = kz_json:from_list([{<<"action">>, <<"subscribe">>}
                                ,{<<"auth_token">>, AuthToken}
                                ,{<<"request_id">>, BindReqId}
                                ,{<<"data">>
                                 ,kz_json:from_list([{<<"account_id">>, AccountId}
                                                    ,{<<"binding">>, (Binding = <<"call.CHANNEL_CREATE.*">>)}
                                                    ])
                                 }
                                ]),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(BindReq)),
    {'json', BindReplyJObj} = pqc_ws_client:recv(WSConn, 2 * ?MILLISECONDS_IN_SECOND),
    lager:info("bind reply: ~p", [BindReplyJObj]),
    <<"reply">> = kz_json:get_ne_binary_value(<<"action">>, BindReplyJObj),
    BindReqId = kz_json:get_ne_binary_value(<<"request_id">>, BindReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, BindReplyJObj),
    Bindings = kz_json:get_list_value([<<"data">>, <<"subscribed">>], BindReplyJObj),
    'true' = lists:member(Binding, Bindings).

emit_channel_event(AccountId) ->
    CallId = kz_binary:rand_hex(4),
    CallProps = [{<<"Event-Name">>, <<"CHANNEL_CREATE">>}
                ,{<<"Call-ID">>, CallId}
                ,{<<"variable_sip_req_uri">>, <<"request@domain.com">>}
                ,{<<"variable_sip_to_uri">>, <<"to@domain.com">>}
                ,{<<"variable_sip_from_uri">>, <<"from@domain.com">>}
                ,{<<"Call-Direction">>, <<"inbound">>}
                ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Account-ID">>, AccountId}])}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ],
    lager:info("publishing call event ~s", [CallId]),
    ecallmgr_call_events:publish_event(CallProps),
    CallId.

disconnect_reconnect_amqp() ->
    _Disconnected = [disconnect(Connection) || Connection <- kz_amqp_connections:connections()],
    lager:info("disconnected: ~p", [_Disconnected]),
    timer:sleep(100),
    kz_amqp_connections:wait_for_available(5 * ?MILLISECONDS_IN_SECOND),
    lager:info("connection reconnected"),
    _ = blackhole_listener:wait_until_consuming(5 * ?MILLISECONDS_IN_SECOND),
    _ = kz_hooks_listener:wait_until_consuming(5 * ?MILLISECONDS_IN_SECOND),
    _ = kz_hooks_shared_listener:wait_until_consuming(5 * ?MILLISECONDS_IN_SECOND),
    lager:info("channel and bindings should be available").

disconnect(#kz_amqp_connections{connection=KZConn}) ->
    #kz_amqp_connection{connection=AMQPConn} = kz_amqp_connection:get_connection(KZConn),
    lager:info("exiting amqp conn ~p", [AMQPConn]),
    exit(AMQPConn, 'heartbeat_timeout').
