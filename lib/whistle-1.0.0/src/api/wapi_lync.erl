%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, UMojo
%%% @doc
%%% API for Lync related operations
%%% @end
%sdassd%%-------------------------------------------------------------------
-module(wapi_lync).

-export([broadcast_epid/1, broadcast_epid_v/1]).
-export([publish_epid/1, publish_epid/2]).
-export([bind_q/2, unbind_q/2]).
-include_lib("wh_api.hrl").

-define(LYNC_VALUES, [{<<"Event-Category">>, <<"lync">>}]).

%% lync whapp routing keys for responses to clients

-define(LYNC_BROADCAST_EPID_HEADERS, [<<"Epid">>, <<"Account-ID">>]).
-define(OPTIONAL_LYNC_BROADCAST_EPID_HEADERS, []).
-define(LYNC_BROADCAST_EPID_VALUES, [{<<"Event-Name">>, <<"broadcast_epid">>} | ?LYNC_VALUES]).

-define(LYNC_BROADCAST_EPID_TYPES, [{<<"Epid">>, fun is_binary/1}
                                    ,{<<"Account-ID">>, fun is_binary/1}
                                    ]).

%%--------------------------------------------------------------------
%% @doc
%% Broadcast an Epid/Account-ID couple 
%% @end
%%--------------------------------------------------------------------
-spec broadcast_epid/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
broadcast_epid(Prop) when is_list(Prop) ->
    case broadcast_epid_v(Prop) of
        true -> wh_api:build_message(Prop, ?LYNC_BROADCAST_EPID_HEADERS, ?OPTIONAL_LYNC_BROADCAST_EPID_HEADERS);
        false -> {error, "Proplist failed validation for lync broadcast epid"}
    end;
broadcast_epid(JObj) ->
    broadcast_epid(wh_json:to_proplist(JObj)).

-spec broadcast_epid_v/1 :: (api_terms()) -> boolean().
broadcast_epid_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?LYNC_BROADCAST_EPID_HEADERS, ?LYNC_BROADCAST_EPID_VALUES, ?LYNC_BROADCAST_EPID_TYPES);
broadcast_epid_v(JObj) ->
    broadcast_epid_v(wh_json:to_proplist(JObj)).


-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Q, _Prop) ->
    amqp_util:lync_exchange(),
    amqp_util:bind_q_to_lync(Q, <<"epid">>).

-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Q, _Prop) ->
    amqp_util:unbind_q_from_lync(Q, <<"epid">>).

%% broadcast an EPID/AccountID to subscribers
-spec publish_epid/1 :: (api_terms()) -> 'ok'.
-spec publish_epid/2 :: (api_terms(), binary()) -> 'ok'.
publish_epid(Api) ->
    publish_epid(Api, ?DEFAULT_CONTENT_TYPE).
publish_epid(Api, ContentType) ->   
    {ok, Payload} = wh_api:prepare_api_payload(Api, ?LYNC_VALUES, fun ?MODULE:broadcast_epid/1),
    amqp_util:lync_publish(<<"epid">>, Payload, ContentType).
