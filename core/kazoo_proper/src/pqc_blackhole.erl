%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Test the directories API endpoint
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_blackhole).

-export([seq/0
        ,seq_api/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-define(PORT, kapps_config:get_integer(<<"blackhole">>, <<"port">>, 5555)).

-spec seq() -> 'ok'.
seq() ->
    _ = [seq_ping()
        ,seq_max_conn()
        ,seq_api()
        ],
    'ok'.

seq_ping() ->
    Model = initial_state(),
    #{'auth_token' := AuthToken} = API = pqc_kazoo_model:api(Model),

    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    lager:info("created account: ~s", [AccountResp]),

    _AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),

    WSConn = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("connected to websocket: ~p", [WSConn]),

    PingReqId = kz_binary:rand_hex(4),
    Ping = kz_json:from_list([{<<"request_id">>, PingReqId}
                             ,{<<"action">>, <<"ping">>}
                             ,{<<"auth_token">>, AuthToken}
                             ]),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(Ping)),
    {'json', ReplyJObj} = pqc_ws_client:recv(WSConn, 1000),
    lager:info("pong: ~p", [ReplyJObj]),
    PingReqId = kz_json:get_ne_binary_value(<<"request_id">>, ReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, ReplyJObj),

    timer:sleep(1000),

    _Send = pqc_ws_client:send(WSConn, kz_json:encode(Ping)),
    {'json', Reply2JObj} = pqc_ws_client:recv(WSConn, 1000),
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

    WSConn = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("connected to websocket: ~p", [WSConn]),

    PingReqId = kz_binary:rand_hex(4),
    Ping = kz_json:from_list([{<<"request_id">>, PingReqId}
                             ,{<<"action">>, <<"ping">>}
                             ,{<<"auth_token">>, AuthToken}
                             ]),
    _Send = pqc_ws_client:send(WSConn, kz_json:encode(Ping)),
    {'json', ReplyJObj} = pqc_ws_client:recv(WSConn, 1000),
    lager:info("pong: ~p", [ReplyJObj]),
    PingReqId = kz_json:get_ne_binary_value(<<"request_id">>, ReplyJObj),
    <<"success">> = kz_json:get_ne_binary_value(<<"status">>, ReplyJObj),

    {'ws_upgrade_failed', {429, _Headers}} = pqc_ws_client:connect("localhost", ?PORT),
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

    WSConn = pqc_ws_client:connect("localhost", ?PORT),
    lager:info("connected to websocket: ~p", [WSConn]),

    %% test pinging the websocket
    _ = test_ws_ping(API, WSConn),

    Binding = <<"object.*.user">>,

    %% test using the API to list ws connections
    {SocketId, BindReq} = test_ws_api_listing(API, WSConn, AccountId, Binding),

    %% test receiving events over the ws
    _ = test_crud_user_events(WSConn, API, AccountId),

    %% test unbinding for events
    test_ws_unbind(API, WSConn, AccountId, SocketId, BindReq, Binding),

    pqc_ws_client:close(WSConn),

    test_empty_active_connections(API, AccountId),

    cleanup(API),
    lager:info("FINISHED API SEQ").

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
    {'json', ReplyJObj} = pqc_ws_client:recv(WSConn, 1000),
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
    {'json', BindReplyJObj} = pqc_ws_client:recv(WSConn, 1000),
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

    {'json', CreateEvent} = pqc_ws_client:recv(WSConn, 1000),
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

    {'json', DeleteEvent} = pqc_ws_client:recv(WSConn, 1000),
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
    {'json', UnbindReplyJObj} = pqc_ws_client:recv(WSConn, 1000),
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
