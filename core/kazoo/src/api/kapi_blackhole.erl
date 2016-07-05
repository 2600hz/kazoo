%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(kapi_blackhole).

-export([get_req/1, get_req_v/1
        ,get_resp/1, get_resp_v/1

        ,bind_q/2, unbind_q/2
        ,declare_exchanges/0

        ,publish_get_req/1, publish_get_req/2
        ,publish_get_resp/2, publish_get_resp/3
        ]).

-include_lib("kazoo/include/kz_api.hrl").


-define(BLACKHOLE_VALUES, [{<<"Event-Category">>, <<"blackhole">>}]).


%% blackhole whapp routing keys for responses to clients
-define(KEY_BLACKHOLE_GET_REQ, <<"blackhole.get">>).

%% Configuration Document Update
%% request to read
-define(BLACKHOLE_GET_REQ_HEADERS, []).
-define(OPTIONAL_BLACKHOLE_GET_REQ_HEADERS, [<<"Account-ID">>, <<"Socket-ID">>]).
-define(BLACKHOLE_GET_REQ_VALUES, [{<<"Event-Name">>, <<"get_req">>} | ?BLACKHOLE_VALUES]).

%% answer to a read request
-define(BLACKHOLE_GET_RESP_HEADERS, []).
-define(OPTIONAL_BLACKHOLE_GET_RESP_HEADERS, [<<"Data">>]).
-define(BLACKHOLE_GET_RESP_VALUES, [{<<"Event-Name">>, <<"get_resp">>} | ?BLACKHOLE_VALUES]).

-define(BLACKHOLE_TYPES, []).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_req(api_terms()) ->{'ok', iolist()} | {'error', string()}.
get_req(Prop) when is_list(Prop) ->
    case get_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?BLACKHOLE_GET_REQ_HEADERS, ?OPTIONAL_BLACKHOLE_GET_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for blackhole get_req"}
    end;
get_req(JObj) ->
    get_req(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_req_v(api_terms()) -> boolean().
get_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?BLACKHOLE_GET_REQ_HEADERS, ?BLACKHOLE_GET_REQ_VALUES, ?BLACKHOLE_TYPES);
get_req_v(JObj) ->
    get_req_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
get_resp(Prop) when is_list(Prop) ->
    case get_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?BLACKHOLE_GET_RESP_HEADERS, ?OPTIONAL_BLACKHOLE_GET_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for blackhole get_resp"}
    end;
get_resp(JObj) ->
    get_resp(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_resp_v(api_terms()) -> boolean().
get_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?BLACKHOLE_GET_RESP_HEADERS, ?BLACKHOLE_GET_RESP_VALUES, ?BLACKHOLE_TYPES);
get_resp_v(JObj) ->
    get_resp_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), proplist()) -> 'ok'.
bind_q(Q, Prop) ->
    add_bindings(Q, props:get_value('restrict_to', Prop)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unbind_q(ne_binary(), proplist()) -> 'ok'.
unbind_q(Q, Prop) ->
    rm_bindings(Q, props:get_value('restrict_to', Prop)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:sysconf_exchange().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_get_req(api_terms()) -> 'ok'.
-spec publish_get_req(api_terms(), ne_binary()) -> 'ok'.
publish_get_req(JObj) ->
    publish_get_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_get_req(Api, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?BLACKHOLE_GET_REQ_VALUES, fun ?MODULE:get_req/1),
    amqp_util:sysconf_publish(routing_key_get(), Payload, ContentType).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_get_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_get_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_get_resp(RespQ, JObj) ->
    publish_get_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_get_resp(RespQ, Api, ContentType) ->
    PrepareOptions = [{'formatter', fun ?MODULE:get_resp/1}
                     ,{'remove_recursive', 'false'}
                     ],
    {'ok', Payload} = kz_api:prepare_api_payload(Api, ?BLACKHOLE_GET_RESP_VALUES, PrepareOptions),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
add_bindings(Q, 'undefined') ->
    _ = amqp_util:bind_q_to_sysconf(Q, routing_key_get());
add_bindings(Q, ['get'|T]) ->
    _ = amqp_util:bind_q_to_sysconf(Q, routing_key_get()),
    add_bindings(Q, T);
add_bindings(Q, [_|T]) ->
    add_bindings(Q, T);
add_bindings(_, []) ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
rm_bindings(Q, 'undefined') ->
    _ = amqp_util:unbind_q_from_sysconf(Q, routing_key_get());
rm_bindings(Q, ['get'|T]) ->
    _ = amqp_util:unbind_q_from_sysconf(Q, routing_key_get()),
    rm_bindings(Q, T);
rm_bindings(Q, [_|T]) ->
    rm_bindings(Q, T);
rm_bindings(_, []) ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
routing_key_get() ->
    ?KEY_BLACKHOLE_GET_REQ.
