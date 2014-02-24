%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wapi_nodes).

-export([advertise/1, advertise_v/1]).

-export([bind_q/2, unbind_q/1]).
-export([declare_exchanges/0]).

-export([publish_advertise/1, publish_advertise/2]).

-include_lib("whistle/include/wh_api.hrl").

%% Advertise message
-define(ADVERTISE_HEADERS, [<<"Expires">>]).
-define(OPTIONAL_ADVERTISE_HEADERS, [<<"Media-Servers">>
                                     ,<<"WhApps">>, <<"Used-Memory">>, <<"Processes">>
                                     ,<<"Ports">>, <<"Version">>, <<"Channels">>
                                     ,<<"Registrations">>, <<"Zone">>
                                    ]).
-define(ADVERTISE_VALUES, [{<<"Event-Category">>, <<"nodes">>}
                           ,{<<"Event-Name">>, <<"advertise">>}
                          ]).
-define(ADVERTISE_TYPES, []).

%%--------------------------------------------------------------------
%% @doc Request asr - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec advertise(api_terms()) -> {'ok', iolist()} | {'error', string()}.
advertise(Prop) when is_list(Prop) ->
    case advertise_v(Prop) of
        true -> wh_api:build_message(Prop, ?ADVERTISE_HEADERS, ?OPTIONAL_ADVERTISE_HEADERS);
        false -> {error, "Proplist failed validation for advertise"}
    end;
advertise(JObj) ->
    advertise(wh_json:to_proplist(JObj)).

-spec advertise_v(api_terms()) -> boolean().
advertise_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?ADVERTISE_HEADERS, ?ADVERTISE_VALUES, ?ADVERTISE_TYPES);
advertise_v(JObj) ->
    advertise_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% bind to a queue to the asr exchange and events
%% @end
%%--------------------------------------------------------------------
-spec bind_q(binary(), proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    amqp_util:bind_q_to_nodes(Queue).

%%--------------------------------------------------------------------
%% @doc
%% unbind to a queue to the asr exchange and events
%% @end
%%--------------------------------------------------------------------
-spec unbind_q(binary()) -> 'ok'.
unbind_q(Queue) ->
    amqp_util:unbind_q_from_nodes(Queue).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:nodes_exchange().

%%--------------------------------------------------------------------
%% @doc
%% prepare and publish a nodes advertise message
%% @end
%%--------------------------------------------------------------------
-spec publish_advertise(api_terms()) -> 'ok'.
-spec publish_advertise(api_terms(), ne_binary()) -> 'ok'.
publish_advertise(JObj) ->
    publish_advertise(JObj, ?DEFAULT_CONTENT_TYPE).
publish_advertise(Advertise, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Advertise, ?ADVERTISE_VALUES, fun ?MODULE:advertise/1),
    amqp_util:nodes_publish(Payload, ContentType).
