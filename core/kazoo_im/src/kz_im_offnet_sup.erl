%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_im_offnet_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([worker/0]).

-export([init/1]).

-include("kazoo_im.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_TYPE('kz_im_offnet', 'temporary')]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Random Worker.
%% @end
%%------------------------------------------------------------------------------
-spec worker() -> {'error', 'no_connections'} | pid().
worker() ->
    Listeners = supervisor:which_children(?SERVER),
    case length(Listeners) of
        0 -> {'error', 'no_connections'};
        Size ->
            Selected = rand:uniform(Size),
            {_, Pid, _, _} = lists:nth(Selected, Listeners),
            Pid
    end.

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    R = supervisor:start_link({'local', ?SERVER}, ?MODULE, []),
    case R of
        {'ok', _} -> start_listeners();
        _Other -> lager:error("error starting offnet supervisor : ~p", [_Other])
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

%%==============================================================================
%% Private
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec connections() -> amqp_listener_connections().
connections() ->
    case kapps_config:get_json(?CONFIG_CAT, [<<"connector">>, <<"connections">>]) of
        'undefined' -> [];
        JObj -> kz_json:foldl(fun connections_fold/3, [], JObj)
    end.

-spec connections_fold(kz_json:key(), kz_json:object(), amqp_listener_connections()) ->
          amqp_listener_connections().
connections_fold(K, V, Acc) ->
    C = #amqp_listener_connection{name = K
                                 ,broker = kz_json:get_value(<<"broker">>, V)
                                 ,exchange = kz_json:get_value(<<"exchange">>, V)
                                 ,type = kz_json:get_value(<<"type">>, V)
                                 ,queue = kz_json:get_value(<<"queue">>, V)
                                 ,options = connection_options(kz_json:get_json_value(<<"options">>, V, kz_json:new()))
                                 },
    [C | Acc].

-spec connection_options(kz_json:object()) -> kz_term:proplist().
connection_options(JObj) ->
    [{kz_term:to_atom(K, 'true'), V}
     || {K, V} <- kz_json:to_proplist(JObj)
    ].

-spec start_external_listener(amqp_listener_connection()) -> kz_types:startlink_ret().
start_external_listener(Connection) ->
    supervisor:start_child(?SERVER, [Connection]).

-spec start_listeners() -> 'ok'.
start_listeners() ->
    lists:foreach(fun start_external_listener/1, connections()).
