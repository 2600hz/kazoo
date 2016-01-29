%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Media requests, responses, and errors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wapi_media).

-compile({no_auto_import, [error/1]}).

-export([req/1, req_v/1]).
-export([resp/1, resp_v/1]).
-export([error/1, error_v/1]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_req/1, publish_req/2]).
-export([publish_resp/2, publish_resp/3]).
-export([publish_error/2, publish_error/3]).

-include_lib("whistle/include/wh_api.hrl").

%% Media Request - when streaming is needed
-define(MEDIA_REQ_ROUTING_KEY, <<"media_req">>).
-define(MEDIA_REQ_HEADERS, [<<"Media-Name">>]).
-define(OPTIONAL_MEDIA_REQ_HEADERS, [<<"Stream-Type">>, <<"Call-ID">>
                                         %% TTS-related flags
                                     ,<<"Voice">>, <<"Language">>, <<"Format">>
                                     ,<<"Account-ID">>, <<"Protocol">>, <<"Engine">>
                                    ]).
-define(MEDIA_REQ_VALUES, [{<<"Event-Category">>, <<"media">>}
                           ,{<<"Event-Name">>, <<"media_req">>}
                           ,{<<"Stream-Type">>, [<<"new">>, <<"extant">>, 'new', 'extant']}
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

%%--------------------------------------------------------------------
%% @doc Request media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req(wh_json:object() | wh_proplist()) ->
                 {'ok', iolist()} |
                 {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?MEDIA_REQ_HEADERS, ?OPTIONAL_MEDIA_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for media_req"}
    end;
req(JObj) -> req(wh_json:to_proplist(JObj)).

-spec req_v(wh_json:object() | wh_proplist()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEDIA_REQ_HEADERS, ?MEDIA_REQ_VALUES, ?MEDIA_REQ_TYPES);
req_v(JObj) -> req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Response with media - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec resp(wh_json:object() | wh_proplist()) ->
                  {'ok', iolist()} |
                  {'error', string()}.
resp(Prop) when is_list(Prop) ->
    case resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?MEDIA_RESP_HEADERS, ?OPTIONAL_MEDIA_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for media_resp"}
    end;
resp(JObj) -> resp(wh_json:to_proplist(JObj)).

-spec resp_v(wh_proplist() | wh_json:object()) -> boolean().
resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEDIA_RESP_HEADERS, ?MEDIA_RESP_VALUES, ?MEDIA_RESP_TYPES);
resp_v(JObj) -> resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Media error - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec error(wh_proplist() | wh_json:object()) ->
                   {'ok', iolist()} |
                   {'error', string()}.
error(Prop) when is_list(Prop) ->
    case error_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?MEDIA_ERROR_HEADERS, ?OPTIONAL_MEDIA_ERROR_HEADERS);
        'false' -> {'error', "Proplist failed validation for media_error"}
    end;
error(JObj) -> error(wh_json:to_proplist(JObj)).

-spec error_v(wh_proplist() | wh_json:object()) -> boolean().
error_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MEDIA_ERROR_HEADERS, ?MEDIA_ERROR_VALUES, ?MEDIA_ERROR_TYPES);
error_v(JObj) -> error_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    amqp_util:bind_q_to_whapps(Queue, ?MEDIA_REQ_ROUTING_KEY).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, _Props) ->
    amqp_util:unbind_q_from_whapps(Queue, ?MEDIA_REQ_ROUTING_KEY).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:whapps_exchange().

-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), ne_binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?MEDIA_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:whapps_publish(?MEDIA_REQ_ROUTING_KEY, Payload, ContentType).

-spec publish_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(Queue, JObj) ->
    publish_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?MEDIA_RESP_VALUES, fun ?MODULE:resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

-spec publish_error(ne_binary(), api_terms()) -> 'ok'.
-spec publish_error(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_error(Queue, JObj) ->
    publish_error(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_error(Queue, Error, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Error, ?MEDIA_ERROR_VALUES, fun ?MODULE:error/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).
