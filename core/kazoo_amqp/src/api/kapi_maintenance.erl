-module(kapi_maintenance).

-export([req/1, req_v/1
        ,resp/1, resp_v/1

        ,bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0

        ,publish_req/1, publish_req/2
        ,publish_resp/2, publish_resp/3
        ]).

-export([refresh_database/1, refresh_database/2, refresh_database/3
        ,refresh_views/1, refresh_views/2, refresh_views/3
        ]).

-export([restrict_to_db/1
        ,restrict_to_classification/1
        ,restrict_to_views_db/1
        ,restrict_to_views_classification/1
        ]).

-export([req_action/1
        ,req_database/1
        ,req_classification/1
        ]).

-export_type([req/0, resp/0]).

-type req() :: api_terms().
-type resp() :: api_terms().

-include_lib("amqp_util.hrl").

-define(EVENT_CAT, <<"maintenance">>).
-define(KEY_ACTION, <<"Action">>).
-define(KEY_CLASSIFICATION, <<"Classification">>).
-define(KEY_DATABASE, <<"Database">>).

-define(REFRESH_DB, <<"refresh_database">>).
-define(REFRESH_VIEWS, <<"refresh_views">>).

-define(REQ_HEADERS, [?KEY_ACTION]).
-define(OPTIONAL_REQ_HEADERS, [?KEY_CLASSIFICATION
                              ,?KEY_DATABASE
                              ]).
-define(REQ_VALUES, [{<<"Event-Name">>, <<"req">>}
                    ,{<<"Event-Category">>, ?EVENT_CAT}
                    ,{?KEY_ACTION, [?REFRESH_DB
                                   ,?REFRESH_VIEWS
                                   ]}
                    ]).
-define(REQ_TYPES, [{?KEY_CLASSIFICATION, fun(C) -> is_binary(C)
                                                        orelse is_atom(C)
                                          end
                    }
                   ,{?KEY_DATABASE, fun is_binary/1}
                   ]).

-spec req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REQ_HEADERS, ?OPTIONAL_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for maintenance request"}
    end;
req(JObj) ->
    req(kz_json:to_proplist(JObj)).

-spec req_v(api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REQ_HEADERS, ?REQ_VALUES, ?REQ_TYPES);
req_v(JObj) ->
    req_v(kz_json:to_proplist(JObj)).

-define(RESP_HEADERS, [<<"Code">>]).
-define(OPTIONAL_RESP_HEADERS, [<<"Message">>]).
-define(RESP_VALUES, [{<<"Event-Name">>, <<"resp">>}
                     ,{<<"Event-Category">>, ?EVENT_CAT}
                     ]).
-define(RESP_TYPES, [{<<"Message">>, fun is_binary/1}
                    ,{<<"Code">>, fun is_integer/1}
                    ]).

-spec resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESP_HEADERS, ?OPTIONAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for maintenance respuest"}
    end;
resp(JObj) ->
    resp(kz_json:to_proplist(JObj)).

-spec resp_v(api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESP_HEADERS, ?RESP_VALUES, ?RESP_TYPES);
resp_v(JObj) ->
    resp_v(kz_json:to_proplist(JObj)).

-define(REFRESH_DB_DB(Db), {'refresh_db', 'db', Db}).
-define(REFRESH_DB_TYPE(Type), {'refresh_db', 'type', Type}).
-define(REFRESH_VIEWS_DB(Db), {'refresh_views', 'db', Db}).
-define(REFRESH_VIEWS_TYPE(Type), {'refresh_views', 'type', Type}).

-type db_type() :: kz_datamgr:db_classification() | ne_binary().
-type binding() :: ?REFRESH_DB_DB(ne_binary()) |
                   ?REFRESH_DB_TYPE(db_type()) |
                   ?REFRESH_VIEWS_DB(ne_binary()) |
                   ?REFRESH_VIEWS_TYPE(db_type()).
-type bind_prop() :: {'restrict_to', [binding()]}.
-type binds() :: [bind_prop()].

-spec restrict_to_db(ne_binary()) -> ?REFRESH_DB_DB(ne_binary()).
restrict_to_db(<<_/binary>>=Db) ->
    ?REFRESH_DB_DB(Db).

-spec restrict_to_classification(db_type()) -> ?REFRESH_DB_TYPE(db_type()).
restrict_to_classification(Classification) ->
    ?REFRESH_DB_TYPE(Classification).

-spec restrict_to_views_db(ne_binary()) -> ?REFRESH_VIEWS_DB(ne_binary()).
restrict_to_views_db(Db) ->
    ?REFRESH_VIEWS_DB(Db).

-spec restrict_to_views_classification(db_type()) -> ?REFRESH_VIEWS_TYPE(db_type()).
restrict_to_views_classification(Classification) ->
    ?REFRESH_VIEWS_TYPE(Classification).

-spec refresh_routing_db(ne_binary(), ne_binary()) -> ne_binary().
refresh_routing_db(RefreshWhat, <<_/binary>>=Db) ->
    refresh_routing(RefreshWhat, kz_datamgr:db_classification(Db), Db).

-spec refresh_routing_classification(ne_binary(), db_type()) -> ne_binary().
refresh_routing_classification(RefreshWhat, Classification) ->
    refresh_routing(RefreshWhat, Classification, <<"*">>).

-spec refresh_routing(ne_binary(), db_type(), ne_binary()) -> ne_binary().
refresh_routing(RefreshWhat, Classification, Database) ->
    kz_binary:join([?EVENT_CAT
                   ,RefreshWhat
                   ,amqp_util:encode(kz_term:to_binary(Classification))
                   ,amqp_util:encode(Database)
                   ]
                  ,$.
                  ).

-spec bind_q(ne_binary(), binds()) -> 'ok'.
-spec bind_to_q(ne_binary(), 'undefined' | [binding()], kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).

bind_to_q(Queue, 'undefined', Props) ->
    bind_to_q(Queue, [?REFRESH_DB_TYPE(<<"*">>)], Props);
bind_to_q(Queue, [?REFRESH_DB_DB(Db)|Rest], Props) ->
    amqp_util:bind_q_to_sysconf(Queue, refresh_routing_db(?REFRESH_DB, Db)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [?REFRESH_DB_TYPE(Classification)|Rest], Props) ->
    amqp_util:bind_q_to_sysconf(Queue, refresh_routing_classification(?REFRESH_DB, Classification)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [?REFRESH_VIEWS_DB(Db)|Rest], Props) ->
    amqp_util:bind_q_to_sysconf(Queue, refresh_routing_db(?REFRESH_VIEWS, Db)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [?REFRESH_VIEWS_TYPE(Classification)|Rest], Props) ->
    amqp_util:bind_q_to_sysconf(Queue, refresh_routing_classification(?REFRESH_VIEWS, Classification)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(_Queue, [], _Props) -> 'ok'.

-spec unbind_q(ne_binary(), binds()) -> 'ok'.
-spec unbind_from_q(ne_binary(), 'undefined' | [binding()], kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_from_q(Queue, props:get_value('restrict_to', Props), Props).

unbind_from_q(Queue, 'undefined', Props) ->
    unbind_from_q(Queue, [?REFRESH_DB_TYPE(<<"*">>)], Props);
unbind_from_q(Queue, [?REFRESH_DB_DB(Db)|Rest], Props) ->
    amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_db(?REFRESH_DB, Db)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [?REFRESH_DB_TYPE(Classification)|Rest], Props) ->
    amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_classification(?REFRESH_DB, Classification)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [?REFRESH_VIEWS_DB(Db)|Rest], Props) ->
    amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_db(?REFRESH_VIEWS, Db)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [?REFRESH_VIEWS_TYPE(Classification)|Rest], Props) ->
    amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_classification(?REFRESH_VIEWS, Classification)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(_Queue, [], _Props) -> 'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:sysconf_exchange().

-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?REQ_VALUES, fun req/1),
    amqp_util:sysconf_publish(routing_key(Req), Payload, ContentType).

-spec publish_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(TargetQ, JObj) ->
    publish_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?RESP_VALUES, fun resp/1),
    amqp_util:targeted_publish(TargetQ, Payload, ContentType).

routing_key(Req) when is_list(Req) ->
    routing_key(Req, fun props:get_value/2);
routing_key(Req) ->
    routing_key(Req, fun kz_json:get_value/2).

routing_key(Req, Get) ->
    routing_key(Req, Get, Get(?KEY_ACTION, Req)).

routing_key(Req, Get, ?REFRESH_DB) ->
    refresh_routing(?REFRESH_DB
                   ,Get(?KEY_CLASSIFICATION, Req)
                   ,Get(?KEY_DATABASE, Req)
                   );
routing_key(Req, Get, ?REFRESH_VIEWS) ->
    refresh_routing(?REFRESH_VIEWS
                   ,Get(?KEY_CLASSIFICATION, Req)
                   ,Get(?KEY_DATABASE, Req)
                   ).


-spec refresh_database(ne_binary()) ->
                              kz_amqp_worker:request_return().
-spec refresh_database(ne_binary(), pid() | kz_datamgr:db_classification()) ->
                              kz_amqp_worker:request_return().
-spec refresh_database(ne_binary(), pid(), kz_datamgr:db_classification()) ->
                              kz_amqp_worker:request_return().
refresh_database(Database) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    Result = refresh_database(Database, Worker, kz_datamgr:db_classification(Database)),
    kz_amqp_worker:checkin_worker(Worker),
    Result.

refresh_database(Database, Worker) when is_pid(Worker) ->
    refresh_database(Database, Worker, kz_datamgr:db_classification(Database));
refresh_database(Database, Classification) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    refresh_database(Database, Worker, Classification).

refresh_database(Database, Worker, Classification) ->
    OldCallId = kz_util:get_callid(),
    MsgId = msg_id(Database),
    kz_util:put_callid(MsgId),

    Req = [{?KEY_ACTION, <<"refresh_database">>}
          ,{?KEY_CLASSIFICATION, Classification}
          ,{?KEY_DATABASE, Database}
          ,{?KEY_MSG_ID, MsgId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun ?MODULE:publish_req/1, Worker),

    Resp = wait_for_response(MsgId, 10 * ?MILLISECONDS_IN_SECOND),
    kz_util:put_callid(OldCallId),
    Resp.

-spec msg_id(ne_binary()) -> ne_binary().
msg_id(Database) ->
    kz_binary:join([<<"refresh">>, Database, kz_binary:rand_hex(8)], $-).

-spec refresh_views(ne_binary()) ->
                           kz_amqp_worker:request_return().
-spec refresh_views(ne_binary(), pid() | kz_datamgr:db_classification()) ->
                           kz_amqp_worker:request_return().
-spec refresh_views(ne_binary(), pid(), kz_datamgr:db_classification()) ->
                           kz_amqp_worker:request_return().
refresh_views(Database) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    Result = refresh_views(Database, Worker, kz_datamgr:db_classification(Database)),
    kz_amqp_worker:checkin_worker(Worker),
    Result.

refresh_views(Database, Worker) when is_pid(Worker) ->
    refresh_views(Database, Worker, kz_datamgr:db_classification(Database));
refresh_views(Database, Classification) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    Result = refresh_views(Database, Worker, Classification),
    kz_amqp_worker:checkin_worker(Worker),
    Result.

refresh_views(Database, Worker, Classification) ->
    OldCallId = kz_util:get_callid(),
    MsgId = msg_id(Database),
    kz_util:put_callid(MsgId),

    Req = [{?KEY_ACTION, <<"refresh_views">>}
          ,{?KEY_CLASSIFICATION, Classification}
          ,{?KEY_DATABASE, Database}
          ,{?KEY_MSG_ID, MsgId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    kz_amqp_worker:cast(Req, fun ?MODULE:publish_req/1, Worker),

    Resp = wait_for_response(MsgId, 30 * ?MILLISECONDS_IN_SECOND),
    kz_util:put_callid(OldCallId),
    Resp.

-spec wait_for_response(ne_binary(), kz_timeout()) ->
                               kz_amqp_worker:request_return().
-spec wait_for_response(ne_binary(), kz_timeout(), kz_json:objects()) ->
                               kz_amqp_worker:request_return().
wait_for_response(MsgId, Timeout) ->
    wait_for_response(MsgId, Timeout, []).

wait_for_response(_MsgId, Timeout, []) when Timeout =< 0 ->
    lager:debug("timed out waiting for responses"),
    {'error', 'timeout'};
wait_for_response(MsgId, Timeout, Resps) when Timeout =< 0 ->
    clear_mailbox(MsgId, Resps);
wait_for_response(MsgId, Timeout, Resps) ->
    Start = os:timestamp(),
    receive
        {'amqp_msg', JObj} ->
            Left = kz_time:decr_timeout(Timeout, Start),
            case ?MODULE:resp_v(JObj)
                andalso MsgId =:= kz_api:msg_id(JObj)
            of
                'true' ->
                    clear_mailbox(MsgId, [JObj|Resps]);
                'false' ->
                    wait_for_response(MsgId, Left, Resps)
            end
    after
        Timeout -> wait_for_response(MsgId, 0, Resps)
    end.

-spec clear_mailbox(ne_binary(), kz_json:objects()) ->
                           {'ok', kz_json:objects()}.
clear_mailbox(MsgId, Resps) ->
    receive
        {'amqp_msg', JObj} ->
            case ?MODULE:resp_v(JObj)
                andalso MsgId =:= kz_api:msg_id(JObj)
            of
                'true' -> clear_mailbox(MsgId, [JObj|Resps]);
                'false' -> clear_mailbox(MsgId, Resps)
            end
    after 1000 ->
            {'ok', Resps}
    end.


-spec req_action(req()) -> api_ne_binary().
req_action(Req) ->
    kz_json:get_ne_binary_value(?KEY_ACTION, Req).

-spec req_database(req()) -> api_ne_binary().
req_database(Req) ->
    kz_json:get_ne_binary_value(?KEY_DATABASE, Req).

-spec req_classification(req()) -> api_ne_binary().
req_classification(Req) ->
    kz_json:get_ne_binary_value(?KEY_CLASSIFICATION, Req).
