%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_queue).

-export([new_member/1, new_member_v/1]).
-export([result/1, result_v/1]).

-export([listener_queue_name/0, get_queue_id/1]).

-export([bind_q/2, unbind_q/2]).

-export([publish_new_member/1, publish_new_member/2, publish_new_member/3]).
-export([publish_result/2, publish_result/3]).

-include_lib("wh_api.hrl").

-define(LISTENER_QUEUE_NAME, <<"queue.listener">>).
-define(NEW_MEMBER_ROUTING_KEY, <<"queue.new_member">>).

-define(NEW_MEMBER_HEADERS, [<<"Call">>, <<"Call-ID">>, <<"Queue-ID">>]).
-define(OPTIONAL_NEW_MEMBER_HEADERS, [<<"Queue">>]).
-define(NEW_MEMBER_VALUES, [{<<"Event-Category">>, <<"queue">>}
                            ,{<<"Event-Name">>, <<"new_member">>}
                           ]).
-define(NEW_MEMBER_TYPES, []).

-define(RESULT_HEADERS, [<<"Call-ID">>, <<"Result">>]).
-define(OPTIONAL_RESULT_HEADERS, [<<"Queue">>]).
-define(RESULT_VALUES, [{<<"Event-Category">>, <<"queue">>}
                        ,{<<"Event-Name">>, <<"result">>}
                       ]).
-define(RESULT_TYPES, []).

-spec new_member/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
new_member(Prop) when is_list(Prop) ->
    case new_member_v(Prop) of
        true -> wh_api:build_message(Prop, ?NEW_MEMBER_HEADERS, ?OPTIONAL_NEW_MEMBER_HEADERS);
        false -> {error, "Proplist failed validation for new_member"}
    end;
new_member(JObj) ->
    new_member(wh_json:to_proplist(JObj)).

-spec new_member_v/1 :: (api_terms()) -> boolean().
new_member_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?NEW_MEMBER_HEADERS, ?NEW_MEMBER_VALUES, ?NEW_MEMBER_TYPES);
new_member_v(JObj) ->
    new_member_v(wh_json:to_proplist(JObj)).


-spec result/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
result(Prop) when is_list(Prop) ->
    case result_v(Prop) of
        true -> wh_api:build_message(Prop, ?RESULT_HEADERS, ?OPTIONAL_RESULT_HEADERS);
        false -> {error, "Proplist failed validation for result"}
    end;
result(JObj) ->
    result(wh_json:to_proplist(JObj)).

-spec result_v/1 :: (api_terms()) -> boolean().
result_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RESULT_HEADERS, ?RESULT_VALUES, ?RESULT_TYPES);
result_v(JObj) ->
    result_v(wh_json:to_proplist(JObj)).


get_queue_id(Prop) when is_list(Prop) ->
    case props:get_value(<<"Queue-ID">>, Prop) of
        undefined ->
            wh_json:get_value(<<"_id">>, props:get_value(<<"Queue">>, Prop));
        QID -> QID
    end;
get_queue_id(JObj) ->
    case wh_json:get_value(<<"Queue-ID">>, JObj) of
        undefined ->
            wh_json:get_value([<<"Queue">>, <<"_id">>], JObj);
        QID -> QID
    end.

get_account_db(Prop) when is_list(Prop) ->
    wh_json:get_value(<<"Account-DB">>, props:get_value(<<"Call">>, Prop));
get_account_db(JObj) ->
    wh_json:get_value([<<"Call">>, <<"Account-DB">>], JObj).

listener_queue_name() ->
    ?LISTENER_QUEUE_NAME.

bind_q(Queue, Props) ->
    AcctDb = props:get_value(account_db, Props, <<"*">>),
    QID = props:get_value(queue_id, Props, <<"*">>),

    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Queue, new_member_routing(AcctDb, QID)).

unbind_q(Queue, Props) ->
    AcctDb = props:get_value(account_db, Props, <<"*">>),
    QID = props:get_value(queue_id, Props, <<"*">>),

    amqp_util:unbind_q_from_callmgr(Queue, new_member_routing(AcctDb, QID)).

publish_new_member(JObj) ->
    publish_new_member(JObj, ?DEFAULT_CONTENT_TYPE, get_account_db(JObj), get_queue_id(JObj)).
publish_new_member(API, ContentType) when is_binary(ContentType) ->
    publish_new_member(API, ContentType, get_account_db(API), get_queue_id(API));
publish_new_member(API, ConnTimeout) when is_integer(ConnTimeout) ->
    publish_new_member(API, ?DEFAULT_CONTENT_TYPE, get_account_db(API), get_queue_id(API), ConnTimeout).

publish_new_member(API, ContentType, ConnTimeout) when is_integer(ConnTimeout) ->
    publish_new_member(API, ContentType, get_account_db(API), get_queue_id(API), ConnTimeout).

publish_new_member(API, ContentType, AcctDb, QID) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?NEW_MEMBER_VALUES, fun ?MODULE:new_member/1),
    amqp_util:callmgr_publish(Payload, ContentType, new_member_routing(AcctDb, QID)).
publish_new_member(API, ContentType, AcctDb, QID, ConnTimeout) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?NEW_MEMBER_VALUES, fun ?MODULE:new_member/1),
    amqp_util:callmgr_publish(Payload, ContentType, new_member_routing(AcctDb, QID), [{expiration, ConnTimeout}]).

-spec publish_result/2 :: (ne_binary(), api_terms()) -> 'ok'.
-spec publish_result/3 :: (ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_result(Queue, JObj) ->
    publish_result(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_result(Queue, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?RESULT_VALUES, fun ?MODULE:result/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

new_member_routing(AcctDb, QID) ->
    list_to_binary([?NEW_MEMBER_ROUTING_KEY, ".", AcctDb, ".", QID]).
