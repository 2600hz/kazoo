%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_fax).

-export([req/1, req_v/1
         ,bind_q/2, unbind_q/2
         ,publish_req/1, publish_req/2
        ]).

-include("fax.hrl").

-define(FAX_REQ_HEADERS, [<<"Call">>, <<"Action">>]).
-define(OPTIONAL_FAX_REQ_HEADERS, [<<"Owner-ID">>]).
-define(FAX_REQ_VALUES, [{<<"Event-Category">>,<<"dialplan">>}
                         ,{<<"Event-Name">>, <<"fax_req">>}
                         ,{<<"Action">>, [<<"receive">>, <<"transmit">>]}
                        ]).
-define(FAX_REQ_TYPES, [{<<"Call">>, fun wh_json:is_json_object/1}]).

req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        false -> {error, "Proplist failed validation for fax_req"};
        true -> wh_api:build_message(Prop, ?FAX_REQ_HEADERS, ?OPTIONAL_FAX_REQ_HEADERS)
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FAX_REQ_HEADERS, ?FAX_REQ_VALUES, ?FAX_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

bind_q(Q, _Prop) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, fax_routing_key()).

unbind_q(Q, _Prop) ->
    amqp_util:unbind_q_from_callmgr(Q, fax_routing_key()).

publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Api, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Api, ?FAX_REQ_VALUES, fun req/1),
    amqp_util:callmgr_publish(Payload, ContentType, fax_routing_key()).

fax_routing_key() ->
    <<"fax.req">>.
