%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Handles authorization requests, responses, queue bindings
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(wapi_authz).

-export([authz_req/1, authz_req_v/1
         ,authz_resp/1, authz_resp_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_authz_req/1, publish_authz_req/2
         ,publish_authz_resp/2, publish_authz_resp/3
         ,broadcast_authz_resp/1, broadcast_authz_resp/2
         ,maybe_determine_account_id/1, from_jobj/1
        ]).

-compile([{parse_transform, lager_transform}]).

-include_lib("whistle/include/wh_api.hrl").

-record(request, {account_id :: api_binary()
    ,account_billing :: api_binary()
    ,account_authorized = 'false' :: boolean()
    ,reseller_id :: api_binary()
    ,reseller_billing :: api_binary()
    ,reseller_authorized = 'false' :: boolean()
    ,soft_limit = 'false' :: boolean()
    ,call_id :: api_binary()
    ,call_direction :: api_binary()
    ,other_leg_call_id :: api_binary()
    ,sip_to :: api_binary()
    ,sip_from :: api_binary()
    ,sip_request :: api_binary()
    ,message_id :: api_binary()
    ,server_id :: api_binary()
    ,node :: api_binary()
    ,classification :: api_binary()
    ,number :: api_binary()
    ,billing_seconds = 0 :: non_neg_integer()
    ,answered_time = 0 :: non_neg_integer()
    ,timestamp = 0 :: gregorian_seconds()
    ,request_jobj = wh_json:new() :: wh_json:object()
}).
-opaque request() :: #request{}.

-define(EVENT_CATEGORY, <<"authz">>).
-define(KEY_AUTHZ_REQ, <<"authz.authorize">>).
-define(KEY_AUTHZ_BROADCAST, <<"authz.authorize.broadcast">>).
-define(WH_AUTHZ_LISTENER_QUEUE, <<"wh_authz_listener">>). % TODO: find all these values and use only this definition

-define(WAPI_AUTHZ_PROCESSED, <<"Wapi-Authz-Processed">>).
-define(WAPI_AUTHZ_RESPONSE_QUEUE, <<"Response-Queue">>).

%% Authorization Requests
-define(AUTHZ_REQ_HEADERS, [<<"To">>, <<"From">>, <<"Request">>
                            ,<<"Call-ID">>, <<"Call-Direction">>
                            ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                           ]).
-define(OPTIONAL_AUTHZ_REQ_HEADERS, [<<"Custom-Channel-Vars">>, <<"Switch-Hostname">>
                                     ,<<"Other-Leg-Call-ID">>, ?WAPI_AUTHZ_PROCESSED
                                     ,?WAPI_AUTHZ_RESPONSE_QUEUE, <<"Custom-Auth-Vars">>
                                    ]).

-define(AUTHZ_REQ_VALUES_MULTITARGET, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                                       ,{<<"Event-Name">>, <<"authz.broadcast.req">>}
                                      ]).
-define(AUTHZ_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, <<"authz_req">>}
                           ,{?WAPI_AUTHZ_RESPONSE_QUEUE, ?WH_AUTHZ_LISTENER_QUEUE}
                          ]).

-define(AUTHZ_REQ_TYPES, [{<<"To">>, fun is_binary/1}
                          ,{<<"From">>, fun is_binary/1}
                          ,{<<"Call-ID">>, fun is_binary/1}
                          ,{<<"Account-ID">>, fun is_binary/1}
                          ,{<<"Caller-ID-Name">>, fun is_binary/1}
                          ,{<<"Caller-ID-Number">>, fun is_binary/1}
                          ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                          ,{?WAPI_AUTHZ_PROCESSED, fun is_binary/1}
                          ,{?WAPI_AUTHZ_RESPONSE_QUEUE, fun is_binary/1}
                          ,{<<"Usage">>, fun wh_json:is_json_object/1}
                         ]).

%% Authorization Responses
-define(AUTHZ_RESP_HEADERS, [<<"Call-ID">>, <<"Is-Authorized">>]).
-define(OPTIONAL_AUTHZ_RESP_HEADERS, [<<"Account-ID">>, <<"Account-Billing">>
                                      ,<<"Reseller-ID">>, <<"Reseller-Billing">>
                                      ,<<"Custom-Channel-Vars">>, <<"Call-Direction">>
                                      ,<<"Soft-Limit">>, <<"Other-Leg-Call-ID">>
                                      ,<<"Custom-Auth-Vars">>, <<"Final-Authz-Response">>
                                     ]).
-define(AUTHZ_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                            ,{<<"Event-Name">>, <<"authz.broadcast.resp">>}
                            ,{<<"Is-Authorized">>, [<<"true">>, <<"false">>]}
                            ,{<<"Global-Resource">>, [<<"true">>, <<"false">>]}
                           ]).
-define(AUTHZ_RESP_VALUES_ORIG, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                            ,{<<"Event-Name">>, <<"authz_resp">>}
                            ,{<<"Is-Authorized">>, [<<"true">>, <<"false">>]}
                            ,{<<"Global-Resource">>, [<<"true">>, <<"false">>]}
                            ,{<<"Final-Authz-Response">>, <<"true">>}
                           ]).
-define(AUTHZ_RESP_TYPES, [{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}]).

%%--------------------------------------------------------------------
%% @doc Authorization Request - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
update_event_fields(JObj) ->
    % update event props to correspond validation function
    Eq = {wh_json:get_value(?WAPI_AUTHZ_PROCESSED, JObj)
          ,wh_json:get_value(<<"Event-Category">>, JObj)
          ,wh_json:get_value(<<"Event-Name">>, JObj)},
    case Eq of
        {'undefined', <<"authz">>, <<"authz_req">>} ->
            lager:debug("AUTHZ_REQ_VALUES values should be replaced by AUTHZ_REQ_VALUES_MULTITARGET"),
            wh_json:set_values(?AUTHZ_REQ_VALUES_MULTITARGET, JObj);
        {<<"true">>, <<"authz">>, <<"authz.broadcast.req">>} ->
            lager:debug("AUTHZ_REQ_VALUES_MULTITARGET values should be replaced by AUTHZ_REQ_VALUES"),
            wh_json:set_values(?AUTHZ_REQ_VALUES, JObj);
        _ ->
            lager:debug("passthrough values. Eq is ~p", [Eq]),
            JObj
    end.

-spec authz_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
authz_req(Prop) when is_list(Prop) ->
    % mark as processed
    case authz_req_v(Prop) of
        'true' ->
            Prop1 = props:set_value(?WAPI_AUTHZ_PROCESSED, <<"true">>, Prop),
            wh_api:build_message(Prop1, ?AUTHZ_REQ_HEADERS, ?OPTIONAL_AUTHZ_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_req"}
    end;
authz_req(JObj) ->
    authz_req(wh_json:to_proplist(JObj)).

-spec authz_req_v(api_terms()) -> boolean().
authz_req_v(Prop) when is_list(Prop) ->
    AuthzReqValues = case props:get_value(?WAPI_AUTHZ_PROCESSED, Prop) of
                         'undefined' -> ?AUTHZ_REQ_VALUES_MULTITARGET;
                         <<"true">> -> ?AUTHZ_REQ_VALUES
                     end,
    wh_api:validate(Prop, ?AUTHZ_REQ_HEADERS, AuthzReqValues, ?AUTHZ_REQ_TYPES);
authz_req_v(JObj) ->
    authz_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Authorization Response - see wiki
%% Takes proplist, creates JSON iolist or error
%% @end
%%--------------------------------------------------------------------
-spec authz_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
authz_resp(Prop) when is_list(Prop) ->
    case authz_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?AUTHZ_RESP_HEADERS, ?OPTIONAL_AUTHZ_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for authz_resp"}
    end;
authz_resp(JObj) ->
    authz_resp(wh_json:to_proplist(JObj)).

-spec authz_resp_v(api_terms()) -> boolean().
authz_resp_v(Prop) when is_list(Prop) ->
    case props:get_value(<<"Final-Authz-Response">>, Prop) of
        'undefined' ->
            wh_api:validate(Prop, ?AUTHZ_RESP_HEADERS, ?AUTHZ_RESP_VALUES, ?AUTHZ_RESP_TYPES);
        <<"true">> ->
            wh_api:validate(Prop, ?AUTHZ_RESP_HEADERS, ?AUTHZ_RESP_VALUES_ORIG, ?AUTHZ_RESP_TYPES)
    end;
authz_resp_v(JObj) ->
    authz_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authz gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

bind_to_q(Q, 'undefined') ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, <<"authz.*">>);
bind_to_q(Q, ['authorize'|T]) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_REQ),
    bind_to_q(Q, T);
bind_to_q(Q, ['broadcast'|T]) ->
    'ok' = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_BROADCAST),
    bind_to_q(Q, T);
bind_to_q(_Q, []) -> 'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q_from(Q, props:get_value('restrict_to', Props)).

unbind_q_from(Q, 'undefined') ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, <<"authz.*">>);
unbind_q_from(Q, ['authorize'|T]) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_REQ),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['broadcast'|T]) ->
    'ok' = amqp_util:unbind_q_from_callmgr(Q, ?KEY_AUTHZ_BROADCAST),
    bind_to_q(Q, T);
unbind_q_from(_Q, []) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange(),
    amqp_util:targeted_exchange().

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_authz_req(api_terms()) -> 'ok'.
-spec publish_authz_req(api_terms(), ne_binary()) -> 'ok'.
publish_authz_req(JObj) ->
    publish_authz_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_authz_req(Req, ContentType) when is_list(Req) ->
    JObj = wh_json:set_values(Req, wh_json:new()),
    publish_authz_req(JObj, ContentType);
publish_authz_req(Req, ContentType) ->
    AuthzReqValues = case wh_json:get_value(?WAPI_AUTHZ_PROCESSED, Req) of
                         'undefined' -> ?AUTHZ_REQ_VALUES_MULTITARGET;
                         <<"true">> -> ?AUTHZ_REQ_VALUES
                     end,
    Req1 = update_event_fields(Req),
    Req2 = case wh_json:get_value(<<"Custom-Auth-Vars">>, Req1) of
               'undefined' -> wh_json:set_value(<<"Custom-Auth-Vars">>, Req1, Req1);
               _ -> Req1
           end,
    {'ok', Payload} = wh_api:prepare_api_payload(Req2, AuthzReqValues, fun ?MODULE:authz_req/1),
    case wh_json:get_value(?WAPI_AUTHZ_PROCESSED, Req) of
        'undefined' ->
            lager:debug("Authz enabled for this account and the request isn't processed yet by wapi_authz"),
            lager:debug("Published to wh_authz_listener"),
            amqp_util:targeted_publish(<<"wh_authz_listener">>, Payload, ContentType);
        <<"true">> ->
            lager:debug("Authz enabled for this account and the request already processed by wapi_authz"),
            lager:debug("Published to callmgr"),
            amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_REQ)
    end.

-spec publish_authz_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_authz_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_authz_resp(Queue, JObj) ->
    publish_authz_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_authz_resp(Queue, Resp, ContentType) ->
    case wh_json:get_value(<<"Final-Authz-Response">>, Resp) of
        'undefined' ->
            {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun ?MODULE:authz_resp/1),
            amqp_util:targeted_publish(Queue, Payload, ContentType);
        <<"true">> ->
            {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES_ORIG, fun ?MODULE:authz_resp/1),
            amqp_util:targeted_publish(Queue, Payload, ContentType)
    end.

-spec broadcast_authz_resp(api_terms()) -> 'ok'.
-spec broadcast_authz_resp(api_terms(), ne_binary()) -> 'ok'.
broadcast_authz_resp(JObj) ->
    broadcast_authz_resp(JObj, ?DEFAULT_CONTENT_TYPE).
broadcast_authz_resp(Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?AUTHZ_RESP_VALUES, fun ?MODULE:authz_resp/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_AUTHZ_BROADCAST).


-spec account_id(request()) -> api_binary().
account_id(#request{account_id=AccountId}) -> AccountId.

-spec number(request()) -> ne_binary().
number(#request{number=Number}) -> Number.

maybe_determine_account_id(Request) ->
    lager:debug("Determine Account ID"),
    case account_id(Request) of
        'undefined' ->
            lager:debug("Account ID not found in request. Continue..."),
            determine_account_id(Request);
        AccountId ->
            lager:debug("Account ID found in request"),
            {'ok', AccountId}
    end.

determine_account_id(Request) ->
    lager:debug("Find Account ID"),
    Number = number(Request),
    case wh_number_manager:lookup_account_by_number(Number) of
        {'ok', AccountId, _Props} ->
            lager:debug("Found by number"),
            {'ok', AccountId};
        {'error', {'account_disabled', AccountId}} = Error ->
            lager:debug("account ~s is disabled", [AccountId]),
            Error;
        {'error', _R} = Error ->
            lager:debug("unable to determine account id for ~s: ~p", [Number, _R]),
            Error
    end.

-spec from_jobj(wh_json:object()) -> request().
from_jobj(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),

    Request = wh_json:get_value(<<"Request">>, JObj),
    [Num|_] = binary:split(Request, <<"@">>),
    Number = request_number(Num, CCVs),

    #request{account_id = wh_json:get_ne_value(<<"Account-ID">>, CCVs)
        ,account_billing = wh_json:get_ne_value(<<"Account-Billing">>, CCVs, <<"limits_enforced">>)
        ,reseller_id = wh_json:get_ne_value(<<"Reseller-ID">>, CCVs)
        ,reseller_billing = wh_json:get_ne_value(<<"Reseller-Billing">>, CCVs, <<"limits_enforced">>)
        ,call_id = wh_json:get_ne_value(<<"Call-ID">>, JObj)
        ,call_direction = wh_json:get_value(<<"Call-Direction">>, JObj)
        ,other_leg_call_id = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj)
        ,sip_to = wh_json:get_ne_value(<<"To">>, JObj)
        ,sip_from = wh_json:get_ne_value(<<"From">>, JObj)
        ,sip_request = Request
        ,message_id = wh_api:msg_id(JObj)
        ,server_id = wh_api:server_id(JObj)
        ,node = wh_api:node(JObj)
        ,billing_seconds = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj, 0)
        ,answered_time = wh_json:get_integer_value(<<"Answered-Seconds">>, JObj, 0)
        ,timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj, wh_util:current_tstamp())
        ,classification = wnm_util:classify_number(Number)
        ,number = Number
        ,request_jobj = JObj
    }.

-spec request_number(ne_binary(), wh_json:object()) -> ne_binary().
request_number(Number, CCVs) ->
    case wh_json:get_value(<<"Original-Number">>, CCVs) of
        'undefined' -> Number;
        Original ->
            lager:debug("using original number ~s instead of ~s", [Original, Number]),
            Original
    end.
