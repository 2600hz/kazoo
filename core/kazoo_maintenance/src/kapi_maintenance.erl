%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
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
        ,clean_services/1, clean_services/2
        ]).

-export([restrict_to_db/1
        ,restrict_to_classification/1
        ,restrict_to_views_db/1
        ,restrict_to_views_classification/1
        ,restrict_to_clean_services/1
        ]).

-export([req_action/1
        ,req_database/1
        ,req_classification/1
        ]).

-export_type([req/0, resp/0]).

-type req() :: kz_term:api_terms().
-type resp() :: kz_term:api_terms().

-include_lib("kazoo_amqp/src/kz_amqp_util.hrl").

-define(EVENT_CAT, <<"maintenance">>).
-define(KEY_ACTION, <<"Action">>).
-define(KEY_CLASSIFICATION, <<"Classification">>).
-define(KEY_DATABASE, <<"Database">>).

-define(REFRESH_DB, <<"refresh_database">>).
-define(REFRESH_VIEWS, <<"refresh_views">>).
-define(CLEAN_SERVICES, <<"clean_services">>).

-define(REQ_HEADERS, [?KEY_ACTION]).
-define(OPTIONAL_REQ_HEADERS, [?KEY_CLASSIFICATION
                              ,?KEY_DATABASE
                              ]).
-define(REQ_VALUES, [{<<"Event-Name">>, <<"req">>}
                    ,{<<"Event-Category">>, ?EVENT_CAT}
                    ,{?KEY_ACTION, [?REFRESH_DB
                                   ,?REFRESH_VIEWS
                                   ,?CLEAN_SERVICES
                                   ]}
                    ]).
-define(REQ_TYPES, [{?KEY_CLASSIFICATION, fun(C) -> is_binary(C)
                                                        orelse is_atom(C)
                                          end
                    }
                   ,{?KEY_DATABASE, fun is_binary/1}
                   ]).

-spec req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REQ_HEADERS, ?OPTIONAL_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for maintenance request"}
    end;
req(JObj) ->
    req(kz_json:to_proplist(JObj)).

-spec req_v(kz_term:api_terms()) -> boolean().
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

-spec resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RESP_HEADERS, ?OPTIONAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for maintenance respuest"}
    end;
resp(JObj) ->
    resp(kz_json:to_proplist(JObj)).

-spec resp_v(kz_term:api_terms()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RESP_HEADERS, ?RESP_VALUES, ?RESP_TYPES);
resp_v(JObj) ->
    resp_v(kz_json:to_proplist(JObj)).

-define(REFRESH_DB_DB(Db), {'refresh_db', 'db', Db}).
-define(REFRESH_DB_TYPE(Type), {'refresh_db', 'type', Type}).
-define(REFRESH_VIEWS_DB(Db), {'refresh_views', 'db', Db}).
-define(REFRESH_VIEWS_TYPE(Type), {'refresh_views', 'type', Type}).
-define(CLEAN_SERVICES_ID(AccountId), {'clean_services', AccountId}).

-type db_type() :: kz_datamgr:db_classification() | kz_term:ne_binary().
-type binding() :: ?REFRESH_DB_DB(kz_term:ne_binary()) |
                   ?REFRESH_DB_TYPE(db_type()) |
                   ?REFRESH_VIEWS_DB(kz_term:ne_binary()) |
                   ?REFRESH_VIEWS_TYPE(db_type()) |
                   ?CLEAN_SERVICES_ID(kz_term:ne_binary()).
-type bind_prop() :: {'restrict_to', [binding()]}.
-type binds() :: [bind_prop()].

-spec restrict_to_db(kz_term:ne_binary()) -> ?REFRESH_DB_DB(kz_term:ne_binary()).
restrict_to_db(<<_/binary>>=Db) ->
    ?REFRESH_DB_DB(Db).

-spec restrict_to_classification(db_type()) -> ?REFRESH_DB_TYPE(db_type()).
restrict_to_classification(Classification) ->
    ?REFRESH_DB_TYPE(Classification).

-spec restrict_to_views_db(kz_term:ne_binary()) -> ?REFRESH_VIEWS_DB(kz_term:ne_binary()).
restrict_to_views_db(Db) ->
    ?REFRESH_VIEWS_DB(Db).

-spec restrict_to_views_classification(db_type()) -> ?REFRESH_VIEWS_TYPE(db_type()).
restrict_to_views_classification(Classification) ->
    ?REFRESH_VIEWS_TYPE(Classification).

-spec restrict_to_clean_services(kz_term:ne_binary()) -> ?CLEAN_SERVICES_ID(kz_term:ne_binary()).
restrict_to_clean_services(AccountId) ->
    ?CLEAN_SERVICES_ID(AccountId).

-spec refresh_routing_db(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
refresh_routing_db(RefreshWhat, <<_/binary>>=Db) ->
    refresh_routing(RefreshWhat, kz_datamgr:db_classification(Db), Db).

-spec refresh_routing_classification(kz_term:ne_binary(), db_type()) -> kz_term:ne_binary().
refresh_routing_classification(RefreshWhat, Classification) ->
    refresh_routing(RefreshWhat, Classification, <<"*">>).

refresh_routing_clean_services(AccountId) ->
    refresh_routing(?CLEAN_SERVICES, 'account', AccountId).

-spec refresh_routing(kz_term:ne_binary(), db_type(), kz_term:ne_binary()) -> kz_term:ne_binary().
refresh_routing(RefreshWhat, Classification, Database) ->
    kz_binary:join([?EVENT_CAT
                   ,RefreshWhat
                   ,kz_amqp_util:encode(kz_term:to_binary(Classification))
                   ,kz_amqp_util:encode(Database)
                   ]
                  ,$.
                  ).

-spec bind_q(kz_term:ne_binary(), binds()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).

-spec bind_to_q(kz_term:ne_binary(), 'undefined' | [binding()], kz_term:proplist()) -> 'ok'.
bind_to_q(Queue, 'undefined', Props) ->
    bind_to_q(Queue, [?REFRESH_DB_TYPE(<<"*">>)], Props);
bind_to_q(Queue, [?REFRESH_DB_DB(Db)|Rest], Props) ->
    kz_amqp_util:bind_q_to_sysconf(Queue, refresh_routing_db(?REFRESH_DB, Db)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [?REFRESH_DB_TYPE(Classification)|Rest], Props) ->
    kz_amqp_util:bind_q_to_sysconf(Queue, refresh_routing_classification(?REFRESH_DB, Classification)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [?REFRESH_VIEWS_DB(Db)|Rest], Props) ->
    kz_amqp_util:bind_q_to_sysconf(Queue, refresh_routing_db(?REFRESH_VIEWS, Db)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [?REFRESH_VIEWS_TYPE(Classification)|Rest], Props) ->
    kz_amqp_util:bind_q_to_sysconf(Queue, refresh_routing_classification(?REFRESH_VIEWS, Classification)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [?CLEAN_SERVICES_ID(AccountId)|Rest], Props) ->
    kz_amqp_util:bind_q_to_sysconf(Queue, refresh_routing_clean_services(AccountId)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(_Queue, [], _Props) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), binds()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_from_q(Queue, props:get_value('restrict_to', Props), Props).

-spec unbind_from_q(kz_term:ne_binary(), 'undefined' | [binding()], kz_term:proplist()) -> 'ok'.
unbind_from_q(Queue, 'undefined', Props) ->
    unbind_from_q(Queue, [?REFRESH_DB_TYPE(<<"*">>)], Props);
unbind_from_q(Queue, [?REFRESH_DB_DB(Db)|Rest], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_db(?REFRESH_DB, Db)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [?REFRESH_DB_TYPE(Classification)|Rest], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_classification(?REFRESH_DB, Classification)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [?REFRESH_VIEWS_DB(Db)|Rest], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_db(?REFRESH_VIEWS, Db)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [?REFRESH_VIEWS_TYPE(Classification)|Rest], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_classification(?REFRESH_VIEWS, Classification)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [?CLEAN_SERVICES_ID(AccountId)|Rest], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_clean_services(AccountId)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(_Queue, [], _Props) -> 'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:sysconf_exchange().

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?REQ_VALUES, fun req/1),
    kz_amqp_util:sysconf_publish(routing_key(Req), Payload, ContentType).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_resp(TargetQ, JObj) ->
    publish_resp(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_resp(TargetQ, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?RESP_VALUES, fun resp/1),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

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


-spec refresh_database(kz_term:ne_binary()) ->
                              kz_amqp_worker:request_return().
refresh_database(Database) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    Result = refresh_database(Database, Worker, kz_datamgr:db_classification(Database)),
    kz_amqp_worker:checkin_worker(Worker),
    Result.

-spec refresh_database(kz_term:ne_binary(), pid() | kz_datamgr:db_classification()) ->
                              kz_amqp_worker:request_return().
refresh_database(Database, Worker) when is_pid(Worker) ->
    refresh_database(Database, Worker, kz_datamgr:db_classification(Database));
refresh_database(Database, Classification) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    refresh_database(Database, Worker, Classification).

-spec refresh_database(kz_term:ne_binary(), pid(), kz_datamgr:db_classification()) ->
                              kz_amqp_worker:request_return().
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
    'ok' = kz_amqp_worker:cast(Req, fun ?MODULE:publish_req/1, Worker),

    Resp = wait_for_response(MsgId, 10 * ?MILLISECONDS_IN_SECOND),
    kz_util:put_callid(OldCallId),
    Resp.

-spec msg_id(kz_term:ne_binary()) -> kz_term:ne_binary().
msg_id(Database) ->
    kz_binary:join([<<"refresh">>, Database, kz_binary:rand_hex(8)], $-).

-spec refresh_views(kz_term:ne_binary()) ->
                           kz_amqp_worker:request_return().
refresh_views(Database) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    Result = refresh_views(Database, Worker, kz_datamgr:db_classification(Database)),
    kz_amqp_worker:checkin_worker(Worker),
    Result.

-spec refresh_views(kz_term:ne_binary(), pid() | kz_datamgr:db_classification()) ->
                           kz_amqp_worker:request_return().
refresh_views(Database, Worker) when is_pid(Worker) ->
    refresh_views(Database, Worker, kz_datamgr:db_classification(Database));
refresh_views(Database, Classification) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    Result = refresh_views(Database, Worker, Classification),
    kz_amqp_worker:checkin_worker(Worker),
    Result.

-spec refresh_views(kz_term:ne_binary(), pid(), kz_datamgr:db_classification()) ->
                           kz_amqp_worker:request_return().
refresh_views(Database, Worker, Classification) ->
    OldCallId = kz_util:get_callid(),
    MsgId = msg_id(Database),
    kz_util:put_callid(MsgId),

    Req = [{?KEY_ACTION, ?REFRESH_VIEWS}
          ,{?KEY_CLASSIFICATION, Classification}
          ,{?KEY_DATABASE, Database}
          ,{?KEY_MSG_ID, MsgId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    'ok' = kz_amqp_worker:cast(Req, fun ?MODULE:publish_req/1, Worker),

    Resp = wait_for_response(MsgId, 30 * ?MILLISECONDS_IN_SECOND),
    kz_util:put_callid(OldCallId),
    Resp.

-spec clean_services(kz_term:ne_binary()) -> kz_amqp_worker:request_return().
clean_services(AccountId) ->
    {'ok', Worker} = kz_amqp_worker:checkout_worker(),
    kz_amqp_worker:relay_to(Worker, self()),
    Result = clean_services(AccountId, Worker),
    kz_amqp_worker:checkin_worker(Worker),
    Result.

-spec clean_services(kz_term:ne_binary(), pid()) -> kz_amqp_worker:request_return().
clean_services(AccountId, Worker) ->
    OldCallId = kz_util:get_callid(),
    MsgId = kz_binary:join([<<"clean_services">>, AccountId, kz_binary:rand_hex(8)], $-),
    kz_util:put_callid(MsgId),

    Req = [{?KEY_ACTION, ?CLEAN_SERVICES}
          ,{?KEY_CLASSIFICATION, <<"account">>}
          ,{?KEY_DATABASE, AccountId}
          ,{?KEY_MSG_ID, MsgId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    _ = kz_amqp_worker:cast(Req, fun ?MODULE:publish_req/1, Worker),

    Resp = wait_for_response(MsgId, 30 * ?MILLISECONDS_IN_SECOND),
    kz_util:put_callid(OldCallId),
    Resp.

-spec wait_for_response(kz_term:ne_binary(), timeout()) ->
                               kz_amqp_worker:request_return().
wait_for_response(MsgId, Timeout) ->
    wait_for_response(MsgId, Timeout, []).

-spec wait_for_response(kz_term:ne_binary(), timeout(), kz_json:objects()) ->
                               kz_amqp_worker:request_return().
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

-spec clear_mailbox(kz_term:ne_binary(), kz_json:objects()) ->
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


-spec req_action(req()) -> kz_term:api_ne_binary().
req_action(Req) ->
    kz_json:get_ne_binary_value(?KEY_ACTION, Req).

-spec req_database(req()) -> kz_term:api_ne_binary().
req_database(Req) ->
    kz_json:get_ne_binary_value(?KEY_DATABASE, Req).

-spec req_classification(req()) -> kz_term:api_ne_binary().
req_classification(Req) ->
    kz_json:get_ne_binary_value(?KEY_CLASSIFICATION, Req).
