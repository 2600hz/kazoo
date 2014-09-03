%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(wapi_kamdb).

-include("kamdb.hrl").

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).
-export([ratelimits_req_v/1]).

-define(KAMDB_ROUTE, <<"kamdb">>).

-define(RATELIMITS_REQ_HEADERS, [<<"Entity">>]).
-define(OPTIONAL_RATELIMITS_REQ_HEADERS, [<<"With-Realm">>]).
-define(RATELIMITS_REQ_VALUES, [{<<"Event-Category">>, <<"rate_limit">>}
                                ,{<<"Event-Name">>, <<"query">>}
                               ]).
-define(RATELIMITS_REQ_TYPES, [{<<"With-Realm">>, fun(V) -> is_boolean(wh_util:to_boolean(V)) end}]).

-spec ratelimits_req_v(api_terms()) -> boolean().
ratelimits_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RATELIMITS_REQ_HEADERS, ?RATELIMITS_REQ_VALUES, ?RATELIMITS_REQ_TYPES);
ratelimits_req_v(JObj) -> ratelimits_req_v(wh_json:to_proplist(JObj)).

-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, _Props) ->
    amqp_util:bind_q_to_configuration(Q, ?KAMDB_ROUTE).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_configuration(Q, ?KAMDB_ROUTE).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:configuration_exchange().
