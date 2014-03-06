%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_caller10).

-export([contest_handler_request/1, contest_handler_request_v/1
         ,contest_handler_response/1, contest_handler_response_v/1

         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0

         ,publish_contest_handler_request/1, publish_contest_handler_request/2
         ,publish_contest_handler_response/1, publish_contest_handler_response/2

         ,account_id/1
         ,contest_id/1
         ,handling_app/1
         ,vote/1
        ]).

-include("caller10.hrl").

-define(CONTEST_HANDLER_REQUEST_HEADERS, [<<"Account-ID">>, <<"Contest-ID">>]).
-define(OPTIONAL_CONTEST_HANDLER_REQUEST_HEADERS, []).
-define(CONTEST_HANDLER_REQUEST_VALUES, [{<<"Event-Category">>, <<"caller10">>}
                                         ,{<<"Event-Name">>, <<"contest_handler_request">>}
                                        ]).
-define(CONTEST_HANDLER_REQUEST_TYPES, [{<<"Contest-ID">>, fun erlang:is_binary/1}
                                        ,{<<"Account-ID">>, fun erlang:is_binary/1}
                                       ]).
-define(CONTEST_HANDLER_REQUEST_BINDING(AccountId), <<"caller10.contest.handler_req.", AccountId/binary>>).

-spec contest_handler_request(api_terms()) ->
                                     {'ok', iolist()} |
                                     {'error', string()}.
contest_handler_request(Prop) when is_list(Prop) ->
    case contest_handler_request_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CONTEST_HANDLER_REQUEST_HEADERS, ?OPTIONAL_CONTEST_HANDLER_REQUEST_HEADERS);
        'false' -> {'error', "Proplist failed validation for contest_handler_request"}
    end;
contest_handler_request(JObj) -> contest_handler_request(wh_json:to_proplist(JObj)).

-spec contest_handler_request_v(api_terms()) -> boolean().
contest_handler_request_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONTEST_HANDLER_REQUEST_HEADERS, ?CONTEST_HANDLER_REQUEST_VALUES, ?CONTEST_HANDLER_REQUEST_TYPES);
contest_handler_request_v(JObj) ->
    contest_handler_request_v(wh_json:to_proplist(JObj)).

-define(CONTEST_HANDLER_RESPONSE_HEADERS, [<<"Account-ID">>, <<"Contest-ID">>
                                           ,<<"Handling-App">>, <<"Handling-Vote">>
                                          ]).
-define(OPTIONAL_CONTEST_HANDLER_RESPONSE_HEADERS, []).
-define(CONTEST_HANDLER_RESPONSE_VALUES, [{<<"Event-Category">>, <<"caller10">>}
                                          ,{<<"Event-Name">>, <<"contest_handler_response">>}
                                         ]).
-define(CONTEST_HANDLER_RESPONSE_TYPES, [{<<"Contest-ID">>, fun erlang:is_binary/1}
                                         ,{<<"Account-ID">>, fun erlang:is_binary/1}
                                        ]).
-define(CONTEST_HANDLER_RESPONSE_BINDING(AccountId), <<"caller10.contest.handler_resp.", AccountId/binary>>).

-spec contest_handler_response(api_terms()) ->
                                     {'ok', iolist()} |
                                     {'error', string()}.
contest_handler_response(Prop) when is_list(Prop) ->
    case contest_handler_response_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CONTEST_HANDLER_RESPONSE_HEADERS, ?OPTIONAL_CONTEST_HANDLER_RESPONSE_HEADERS);
        'false' -> {'error', "Proplist failed validation for contest_handler_response"}
    end;
contest_handler_response(JObj) -> contest_handler_response(wh_json:to_proplist(JObj)).

-spec contest_handler_response_v(api_terms()) -> boolean().
contest_handler_response_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONTEST_HANDLER_RESPONSE_HEADERS, ?CONTEST_HANDLER_RESPONSE_VALUES, ?CONTEST_HANDLER_RESPONSE_TYPES);
contest_handler_response_v(JObj) ->
    contest_handler_response_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    amqp_util:bind_q_to_whapps(Queue, ?CONTEST_HANDLER_REQUEST_BINDING(AccountId)),
    amqp_util:bind_q_to_whapps(Queue, ?CONTEST_HANDLER_RESPONSE_BINDING(AccountId)).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    amqp_util:unbind_q_from_whapps(Queue, ?CONTEST_HANDLER_REQUEST_BINDING(AccountId)),
    amqp_util:unbind_q_from_whapps(Queue, ?CONTEST_HANDLER_RESPONSE_BINDING(AccountId)).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:whapps_exchange().

-spec publish_contest_handler_request(api_terms()) -> 'ok'.
-spec publish_contest_handler_request(api_terms(), ne_binary()) -> 'ok'.
publish_contest_handler_request(API) ->
    publish_contest_handler_request(API, ?DEFAULT_CONTENT_TYPE).

publish_contest_handler_request(API, ContentType) ->
    AccountId = account_id(API),
    {'ok', Payload} = wh_api:prepare_api_payload(API
                                                 ,?CONTEST_HANDLER_REQUEST_VALUES
                                                 ,fun ?MODULE:contest_handler_request/1
                                                ),
    amqp_util:whapps_publish(?CONTEST_HANDLER_REQUEST_BINDING(AccountId), Payload, ContentType).

-spec publish_contest_handler_response(api_terms()) -> 'ok'.
-spec publish_contest_handler_response(api_terms(), ne_binary()) -> 'ok'.
publish_contest_handler_response(API) ->
    publish_contest_handler_response(API, ?DEFAULT_CONTENT_TYPE).

publish_contest_handler_response(API, ContentType) ->
    AccountId = account_id(API),
    {'ok', Payload} = wh_api:prepare_api_payload(API
                                                 ,?CONTEST_HANDLER_RESPONSE_VALUES
                                                 ,fun ?MODULE:contest_handler_response/1
                                                ),
    amqp_util:whapps_publish(?CONTEST_HANDLER_RESPONSE_BINDING(AccountId), Payload, ContentType).


-spec account_id(api_terms()) -> api_binary().
account_id(Props) when is_list(Props) ->
    props:get_value(<<"Account-ID">>, Props);
account_id(JObj) ->
    wh_json:get_value(<<"Account-ID">>, JObj).

-spec contest_id(api_terms()) -> api_binary().
contest_id(Props) when is_list(Props) ->
    props:get_value(<<"Contest-ID">>, Props);
contest_id(JObj) ->
    wh_json:get_value(<<"Contest-ID">>, JObj).

-spec handling_app(api_terms()) -> api_binary().
handling_app(Props) when is_list(Props) ->
    props:get_value(<<"Handling-App">>, Props);
handling_app(JObj) ->
    wh_json:get_value(<<"Handling-App">>, JObj).

-spec vote(api_terms()) -> api_binary().
vote(Props) when is_list(Props) ->
    props:get_integer_value(<<"Handling-Vote">>, Props);
vote(JObj) ->
    wh_json:get_integer_value(<<"Handling-Vote">>, JObj).
