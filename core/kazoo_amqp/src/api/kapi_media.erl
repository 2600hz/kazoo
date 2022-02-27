%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc Media requests, responses, and errors.
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_media).

-compile({no_auto_import, [error/1]}).

-export([req/1, req_v/1]).
-export([resp/1, resp_v/1]).
-export([error/1, error_v/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_req/1, publish_req/2]).
-export([publish_resp/2, publish_resp/3]).
-export([publish_error/2, publish_error/3]).

-include_lib("kz_amqp_util.hrl").

%% Media Request - when streaming is needed
-define(MEDIA_REQ_ROUTING_KEY, <<"media_req">>).
-define(MEDIA_REQ_HEADERS, [<<"Media-Name">>]).
-define(OPTIONAL_MEDIA_REQ_HEADERS, [<<"Stream-Type">>, <<"Call-ID">>
                                         %% TTS-related flags
                                    ,<<"Text">>, <<"Voice">>, <<"Language">>, <<"Format">>
                                    ,<<"Account-ID">>, <<"Protocol">>, <<"Engine">>
                                    ]).
-define(MEDIA_REQ_VALUES, [{<<"Event-Category">>, <<"media">>}
                          ,{<<"Event-Name">>, <<"media_req">>}
                          ,{<<"Stream-Type">>, [<<"new">>, <<"extant">>]}
                          ,{<<"Voice">>, [<<"male">>, <<"female">>]}
                          ,{<<"Format">>, [<<"mp3">>, <<"wav">>]}
                          ,{<<"Protocol">>, [<<"http">>, <<"https">>, <<"shout">>, <<"vlc">>]}
                          ]).
-define(MEDIA_REQ_TYPES, []).

%% Media Response
-define(MEDIA_RESP_HEADERS, [<<"Media-Name">>, <<"Stream-URL">>]).
-define(OPTIONAL_MEDIA_RESP_HEADERS, []).
-define(MEDIA_RESP_VALUES, [{<<"Event-Category">>, <<"media">>}
                           ,{<<"Event-Name">>, <<"media_resp">>}
                           ]).
-define(MEDIA_RESP_TYPES, [{<<"Stream-URL">>, fun(<<"shout://", _/binary>>) -> 'true';
                                                 (<<"http://", _/binary>>) -> 'true';
                                                 (<<"vlc://", _/binary>>) -> 'true';
                                                 (_) -> 'false'
                                              end}
                          ]).

%% Media Error
-define(MEDIA_ERROR_HEADERS, [<<"Media-Name">>, <<"Error-Code">>]).
-define(OPTIONAL_MEDIA_ERROR_HEADERS, [<<"Error-Msg">>]).
-define(MEDIA_ERROR_VALUES, [{<<"Event-Category">>, <<"media">>}
                            ,{<<"Event-Name">>, <<"media_error">>}
                            ,{<<"Error-Code">>, [<<"not_found">>, <<"no_data">>, <<"other">>]}
                            ]).
-define(MEDIA_ERROR_TYPES, []).

%%------------------------------------------------------------------------------
%% @doc Request media.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec req(kz_term:api_terms()) ->
          {'ok', iolist()} |
          {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MEDIA_REQ_HEADERS, ?OPTIONAL_MEDIA_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for media_req"}
    end;
req(JObj) -> req(kz_json:to_proplist(JObj)).

-spec req_v(kz_term:api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MEDIA_REQ_HEADERS, ?MEDIA_REQ_VALUES, ?MEDIA_REQ_TYPES);
req_v(JObj) -> req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Response with media.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec resp(kz_json:object() | kz_term:proplist()) ->
          {'ok', iolist()} |
          {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MEDIA_RESP_HEADERS, ?OPTIONAL_MEDIA_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for media_resp"}
    end;
resp(JObj) -> resp(kz_json:to_proplist(JObj)).

-spec resp_v(kz_term:proplist() | kz_json:object()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MEDIA_RESP_HEADERS, ?MEDIA_RESP_VALUES, ?MEDIA_RESP_TYPES);
resp_v(JObj) -> resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Media error.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec error(kz_term:proplist() | kz_json:object()) ->
          {'ok', iolist()} |
          {'error', string()}.
error(Prop) when is_list(Prop) ->
    case error_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MEDIA_ERROR_HEADERS, ?OPTIONAL_MEDIA_ERROR_HEADERS);
        'false' -> {'error', "Proplist failed validation for media_error"}
    end;
error(JObj) -> error(kz_json:to_proplist(JObj)).

-spec error_v(kz_term:proplist() | kz_json:object()) -> boolean().
error_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MEDIA_ERROR_HEADERS, ?MEDIA_ERROR_VALUES, ?MEDIA_ERROR_TYPES);
error_v(JObj) -> error_v(kz_json:to_proplist(JObj)).

-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    kz_amqp_util:bind_q_to_kapps(Queue, ?MEDIA_REQ_ROUTING_KEY).

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    kz_amqp_util:unbind_q_from_kapps(Queue, ?MEDIA_REQ_ROUTING_KEY).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

-spec publish_req(kz_term:api_terms()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?MEDIA_REQ_VALUES, fun req/1),
    kz_amqp_util:kapps_publish(?MEDIA_REQ_ROUTING_KEY, Payload, ContentType).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?MEDIA_RESP_VALUES, fun resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_error(Queue, JObj) ->
    publish_error(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_error(Queue, Error, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Error, ?MEDIA_ERROR_VALUES, fun error/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).
