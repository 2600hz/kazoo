%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_nodes).

-export([advertise/1, advertise_v/1]).

-export([bind_q/2, unbind_q/1]).
-export([declare_exchanges/0]).

-export([publish_advertise/1, publish_advertise/2]).

-include_lib("kz_amqp_util.hrl").

%% Advertise message
-define(ADVERTISE_HEADERS, [<<"Expires">>]).
-define(OPTIONAL_ADVERTISE_HEADERS, [<<"Channels">>
                                    ,<<"Conferences">>
                                    ,<<"Globals">>
                                    ,<<"Media-Servers">>
                                    ,<<"Node-Info">>
                                    ,<<"Ports">>
                                    ,<<"Processes">>
                                    ,<<"Registrations">>
                                    ,<<"Used-Memory">>
                                    ,<<"Version">>
                                    ,<<"WhApps">>
                                    ,<<"Zone">>
                                    ,<<"md5">>
                                    ]).
-define(ADVERTISE_VALUES, [{<<"Event-Category">>, <<"nodes">>}
                          ,{<<"Event-Name">>, <<"advertise">>}
                          ]).
-define(ADVERTISE_TYPES, []).

%%------------------------------------------------------------------------------
%% @doc Nodes advertise.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec advertise(kz_term:api_terms()) -> {'ok', iolist()} |
          {'error', string()}.
advertise(Prop) when is_list(Prop) ->
    case advertise_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?ADVERTISE_HEADERS, ?OPTIONAL_ADVERTISE_HEADERS);
        'false' -> {'error', "Proplist failed validation for advertise"}
    end;
advertise(JObj) ->
    advertise(kz_json:to_proplist(JObj)).

-spec advertise_v(kz_term:api_terms()) -> boolean().
advertise_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?ADVERTISE_HEADERS, ?ADVERTISE_VALUES, ?ADVERTISE_TYPES);
advertise_v(JObj) ->
    advertise_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Bind to a queue to this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, _Props) ->
    kz_amqp_util:bind_q_to_nodes(Queue).

%%------------------------------------------------------------------------------
%% @doc Unbind to a queue to this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q(binary()) -> 'ok'.
unbind_q(Queue) ->
    kz_amqp_util:unbind_q_from_nodes(Queue).

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:nodes_exchange().

%%------------------------------------------------------------------------------
%% @doc Prepare and publish a nodes advertise message.
%% @end
%%------------------------------------------------------------------------------

-spec publish_advertise(kz_term:api_terms()) -> 'ok'.
publish_advertise(JObj) ->
    publish_advertise(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_advertise(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_advertise(Advertise, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Advertise, ?ADVERTISE_VALUES, fun advertise/1),
    kz_amqp_util:nodes_publish(Payload, ContentType).
