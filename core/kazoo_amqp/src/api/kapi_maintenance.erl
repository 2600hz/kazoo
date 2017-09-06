-module(kapi_maintenance).

-export([req/1, req_v/1
        ,resp/1, resp_v/1

        ,bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0

        ,publish_req/1, publish_req/2
        ,publish_resp/2, publish_resp/3
        ]).

-export([restrict_to_db/1
        ,restrict_to_classification/1
        ,restrict_to_views_db/1
        ,restrict_to_views_classification/1
        ]).

-export_type([req/0, resp/0]).

-type req() :: api_terms().
-type resp() :: api_terms().

-include_lib("amqp_util.hrl").

-define(REFRESH_DB, <<"refresh_database">>).
-define(REFRESH_VIEWS, <<"refresh_views">>).

-define(REQ_HEADERS, [<<"Action">>]).
-define(OPTIONAL_REQ_HEADERS, [<<"Classification">>
                              ,<<"Database">>
                              ]).
-define(REQ_VALUES, [{<<"Event-Name">>, <<"req">>}
                    ,{<<"Event-Category">>, <<"maintenance">>}
                    ,{<<"Action">>, [?REFRESH_DB
                                    ,?REFRESH_VIEWS
                                    ]}
                    ]).
-define(REQ_TYPES, [{<<"Classification">>, fun is_binary/1}
                   ,{<<"Database">>, fun is_binary/1}
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
                     ,{<<"Event-Category">>, <<"maintenance">>}
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

-type db_type() :: kz_datamgr:db_classification() | ne_binary().
-type binding() :: {'refresh_db', 'db', ne_binary()} |
                   {'refresh_db', 'type', db_type()} |
                   {'refresh_views', 'db', ne_binary()} |
                   {'refresh_views', 'type', db_type()}.
-type bind_prop() :: {'restrict_to', [binding()]}.
-type binds() :: [bind_prop()].

-spec restrict_to_db(ne_binary()) -> {'refresh_db', 'db', ne_binary()}.
restrict_to_db(<<_/binary>>=Db) ->
    {'refresh_db', 'db', Db}.

-spec restrict_to_classification(db_type()) -> {'refresh_db', 'type', db_type()}.
restrict_to_classification(Classification) ->
    {'refresh_db', 'type', Classification}.

-spec restrict_to_views_db(ne_binary()) -> {'refresh_views', 'db', ne_binary()}.
restrict_to_views_db(Db) ->
    {'refresh_views', 'db', Db}.

-spec restrict_to_views_classification(db_type()) -> {'refresh_views', 'type', db_type()}.
restrict_to_views_classification(Classification) ->
    {'refresh_views', 'type', Classification}.


-spec refresh_routing_db(ne_binary(), ne_binary()) -> ne_binary().
refresh_routing_db(RefreshWhat, <<_/binary>>=Db) ->
    refresh_routing(RefreshWhat, kz_datamgr:db_classification(Db), Db).

-spec refresh_routing_classification(ne_binary(), db_type()) -> ne_binary().
refresh_routing_classification(RefreshWhat, Classification) ->
    refresh_routing(RefreshWhat, Classification, <<"*">>).

-spec refresh_routing(ne_binary(), db_type(), ne_binary()) -> ne_binary().
refresh_routing(RefreshWhat, Classification, Database) ->
    kz_binary:join([<<"maintenance">>
                   ,RefreshWhat
                   ,amqp_util:encode(kz_term:to_binary(Classification))
                   ,amqp_util:encode(Database)
                   ]
                  ,$.
                  ).

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
-spec bind_to_q(ne_binary(), binds(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).

bind_to_q(Queue, 'undefined', Props) ->
    bind_to_q(Queue, [{'refresh_db', 'type', <<"*">>}], Props);
bind_to_q(Queue, [{'refresh_db', 'db', Db}|Rest], Props) ->
    amqp_util:bind_q_to_sysconf(Queue, refresh_routing_db(?REFRESH_DB, Db)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [{'refresh_db', 'type', Classification}|Rest], Props) ->
    amqp_util:bind_q_to_sysconf(Queue, refresh_routing_classification(?REFRESH_DB, Classification)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [{'refresh_views', 'db', Db}|Rest], Props) ->
    amqp_util:bind_q_to_sysconf(Queue, refresh_routing_db(?REFRESH_VIEWS, Db)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(Queue, [{'refresh_views', 'type', Classification}|Rest], Props) ->
    amqp_util:bind_q_to_sysconf(Queue, refresh_routing_classification(?REFRESH_VIEWS, Classification)),
    bind_to_q(Queue, Rest, Props);
bind_to_q(_Queue, [], _Props) -> 'ok'.

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
-spec unbind_from_q(ne_binary(), binds(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_from_q(Queue, props:get_value('restrict_to', Props), Props).

unbind_from_q(Queue, 'undefined', Props) ->
    unbind_from_q(Queue, [{'refresh', 'db', <<"*">>}], Props);
unbind_from_q(Queue, [{'refresh', 'db', Db}|Rest], Props) ->
    amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_db(?REFRESH_DB, Db)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [{'refresh', 'type', Classification}|Rest], Props) ->
    amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_classification(?REFRESH_DB, Classification)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [{'refresh_views', 'db', Db}|Rest], Props) ->
    amqp_util:unbind_q_from_sysconf(Queue, refresh_routing_db(?REFRESH_VIEWS, Db)),
    unbind_from_q(Queue, Rest, Props);
unbind_from_q(Queue, [{'refresh_views', 'type', Classification}|Rest], Props) ->
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
    routing_key(Req, Get, Get(<<"Action">>, Req)).

routing_key(Req, Get, ?REFRESH_DB) ->
    refresh_routing(?REFRESH_DB
                   ,Get(<<"Classification">>, Req)
                   ,Get(<<"Database">>, Req)
                   );
routing_key(Req, Get, ?REFRESH_VIEWS) ->
    refresh_routing(?REFRESH_VIEWS
                   ,Get(<<"Classification">>, Req)
                   ,Get(<<"Database">>, Req)
                   ).
