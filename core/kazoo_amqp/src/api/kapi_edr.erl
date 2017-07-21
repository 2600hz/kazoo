%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(kapi_edr).

-export([build_msg/1, validate/1]).
-export([bind_q/1, bind_q/2, unbind_q/1]).
-export([declare_exchanges/0]).
-export([publish/1, publish/2]).

-include_lib("amqp_util.hrl").

-define(EDR_REQ_HEADERS, [<<"Timestamp">>, <<"Tags">>]).
-define(OPTIONAL_EDR_HEADERS, []).
-define(EDR_REQ_VALUES, []).
-define(EDR_REQ_TYPES, [{<<"Tags">>, fun kz_json:is_json_object/1}]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_msg(api_terms()) -> {'ok', iolist()} | {'error', string()}.
build_msg(Prop) when is_list(Prop) ->
    case validate(Prop) of
        'true' -> kz_api:build_message(Prop, ?EDR_REQ_HEADERS, ?OPTIONAL_EDR_HEADERS);
        'false' -> {error, "Proplist failed validation for edr_build_msg"}
    end;
build_msg(JObj) ->
    build_msg(kz_json:to_proplist(JObj)).

-spec validate(api_terms()) -> boolean().
validate(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?EDR_REQ_HEADERS, ?EDR_REQ_VALUES, ?EDR_REQ_TYPES);
validate(JObj) ->
    validate(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% bind to a queue to the edr exchange and events
%% @end
%%--------------------------------------------------------------------
-spec bind_q(binary()) -> 'ok'.
-spec bind_q(binary(), kz_proplist()) -> 'ok'.
bind_q(Queue) -> bind_q(Queue, []).
bind_q(Queue, _Props) ->
    amqp_util:bind_q_to_edr(Queue, ?EXCHANGE_EDR).

%%--------------------------------------------------------------------
%% @doc
%% unbind to a queue to the edr exchange and events
%% @end
%%--------------------------------------------------------------------
-spec unbind_q(binary()) -> 'ok'.
unbind_q(Queue) ->
    amqp_util:unbind_q_from_edr(Queue).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:edr_exchange().

%%--------------------------------------------------------------------
%% @doc
%% prepare and publish an edr
%% @end
%%--------------------------------------------------------------------
-spec publish(api_terms()) -> 'ok'.
-spec publish(api_terms(), ne_binary()) -> 'ok'.
publish(JObj) ->
    publish(JObj, ?DEFAULT_CONTENT_TYPE).
publish(Req, ContentType) ->
    {ok, Payload} = kz_api:prepare_api_payload(Req, [], fun ?MODULE:build_msg/1),
    amqp_util:edr_publish(?EXCHANGE_EDR, Payload, ContentType).
