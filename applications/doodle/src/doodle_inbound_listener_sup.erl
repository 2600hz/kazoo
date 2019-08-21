%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_inbound_listener_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_inbound_listener/1]).

-include("doodle.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_TYPE('doodle_inbound_listener', 'temporary')]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_inbound_listener(amqp_listener_connection()) -> kz_types:startlink_ret().
start_inbound_listener(Connection) ->
    supervisor:start_child(?SERVER, [Connection]).

-spec start_listeners() -> 'ok'.
start_listeners() ->
    lists:foreach(fun start_inbound_listener/1, connections()).

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    R = supervisor:start_link({'local', ?SERVER}, ?MODULE, []),
    case R of
        {'ok', _} -> start_listeners();
        _Other -> lager:error("error starting inbound_listeners sup : ~p", [_Other])
    end,
    R.

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

-spec default_connection() -> amqp_listener_connection().
default_connection() ->
    #amqp_listener_connection{name = <<"default">>
                             ,broker = ?DEFAULT_BROKER
                             ,exchange = ?DEFAULT_EXCHANGE
                             ,type = ?DEFAULT_EXCHANGE_TYPE
                             ,queue = ?DEFAULT_QUEUE_NAME
                             ,options = []
                             }.

-spec connections() -> amqp_listener_connections().
connections() ->
    case kapps_config:get_json(?CONFIG_CAT, <<"connections">>) of
        'undefined' -> [default_connection()];
        JObj -> [default_connection()] ++ kz_json:foldl(fun connections_fold/3, [], JObj)
    end.

-spec connections_fold(kz_json:key(), kz_json:object(), amqp_listener_connections()) ->
                              amqp_listener_connections().
connections_fold(K, V, Acc) ->
    C = #amqp_listener_connection{name = K
                                 ,broker = kz_json:get_value(<<"broker">>, V)
                                 ,exchange = kz_json:get_value(<<"exchange">>, V)
                                 ,type = kz_json:get_value(<<"type">>, V)
                                 ,queue = kz_json:get_value(<<"queue">>, V)
                                 ,options = connection_options(kz_json:get_json_value(<<"options">>, V))
                                 },
    [C | Acc].

-spec connection_options(kz_term:api_object()) -> kz_term:proplist().
connection_options('undefined') ->
    connection_options(?DEFAULT_EXCHANGE_OPTIONS_JOBJ);
connection_options(JObj) ->
    [{kz_term:to_atom(K, 'true'), V}
     || {K, V} <- kz_json:to_proplist(JObj)
    ].
