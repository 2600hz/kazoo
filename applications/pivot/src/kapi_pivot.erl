%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%% Pivot API
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapi_pivot).

-include("pivot.hrl").

-export([req/1, req_v/1
        ,bind_q/2
        ,unbind_q/2
        ,declare_exchanges/0
        ,publish_req/1, publish_req/2
        ]).


-export([failed/1, failed_v/1
        ,publish_failed/2
        ]).

-define(KEY_PIVOT_REQ, <<"pivot.req">>).

-define(PIVOT_REQ_HEADERS, [<<"Call">>, <<"Voice-URI">>]).
-define(OPTIONAL_PIVOT_REQ_HEADERS, [<<"CDR-URI">>
                                    ,<<"Request-Format">>
                                    ,<<"HTTP-Method">>
                                    ,<<"Debug">>
                                    ]).
-define(PIVOT_REQ_VALUES, [{<<"Event-Category">>,<<"dialplan">>}
                          ,{<<"Event-Name">>, <<"pivot_req">>}
                          ]).
-define(PIVOT_REQ_TYPES, [{<<"Call">>, fun kz_json:is_json_object/1}
                         ,{<<"Debug">>, fun kz_util:is_boolean/1}
                         ]).

-define(PIVOT_FAILED_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_PIVOT_FAILED_HEADERS, []).
-define(PIVOT_FAILED_VALUES, [{<<"Event-Category">>,<<"pivot">>}
                             ,{<<"Event-Name">>, <<"failed">>}
                             ]).
-define(PIVOT_FAILED_TYPES, []).

-spec req(api_terms()) -> {'ok', iolist()} |
                          {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'false' -> {'error', "Proplist failed validation for pivot_req"};
        'true' -> kz_api:build_message(Prop, ?PIVOT_REQ_HEADERS, ?OPTIONAL_PIVOT_REQ_HEADERS)
    end;
req(JObj) ->
    req(kz_json:to_proplist(JObj)).

-spec req_v(api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PIVOT_REQ_HEADERS, ?PIVOT_REQ_VALUES, ?PIVOT_REQ_TYPES);
req_v(JObj) ->
    req_v(kz_json:to_proplist(JObj)).

-spec failed(api_terms()) -> {'ok', iolist()} |
                             {'error', string()}.
failed(Prop) when is_list(Prop) ->
    case failed_v(Prop) of
        'false' -> {'error', "Proplist failed validation for pivot_failed"};
        'true' -> kz_api:build_message(Prop, ?PIVOT_FAILED_HEADERS, ?OPTIONAL_PIVOT_FAILED_HEADERS)
    end;
failed(JObj) ->
    failed(kz_json:to_proplist(JObj)).

-spec failed_v(api_terms()) -> boolean().
failed_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PIVOT_FAILED_HEADERS, ?PIVOT_FAILED_VALUES, ?PIVOT_FAILED_TYPES);
failed_v(JObj) ->
    failed_v(kz_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), kz_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    amqp_util:bind_q_to_callmgr(Queue, get_pivot_req_routing(Realm)).

-spec unbind_q(ne_binary(), kz_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    amqp_util:unbind_q_from_callmgr(Queue, get_pivot_req_routing(Realm)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange().

get_pivot_req_routing(Realm) when is_binary(Realm) ->
    list_to_binary([?KEY_PIVOT_REQ, ".", amqp_util:encode(Realm)]);
get_pivot_req_routing(Api) ->
    get_pivot_req_routing(get_from_realm(Api)).

-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PIVOT_REQ_VALUES, fun req/1),
    amqp_util:callmgr_publish(Payload, ContentType, get_pivot_req_routing(Req)).


-spec publish_failed(ne_binary(), ne_binary()) -> 'ok'.
publish_failed(Target, JObj) ->
    {'ok', Payload} = kz_api:prepare_api_payload(JObj, ?PIVOT_FAILED_VALUES, fun failed/1),
    amqp_util:targeted_publish(Target, Payload).

-spec get_from_realm(api_terms()) -> ne_binary().
get_from_realm(Prop) when is_list(Prop) ->
    kz_json:get_value(<<"From-Realm">>, props:get_value(<<"Call">>, Prop));
get_from_realm(JObj) ->
    kz_json:get_value([<<"Call">>, <<"From-Realm">>], JObj).
