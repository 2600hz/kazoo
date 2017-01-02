%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%  Pierre Fenoll
%%%-------------------------------------------------------------------
-module(stepswitch_cnam_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("stepswitch.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_NAME_ARGS('poolboy', ?STEPSWITCH_CNAM_POOL, [[{'name', {'local', ?STEPSWITCH_CNAM_POOL}}
                                                                        ,{'worker_module', 'stepswitch_cnam'}
                                                                        ,{'size', 10}
                                                                        ,{'max_overflow', 50}
                                                                        ,{'neg_resp_threshold', 1}
                                                                        ]
                                                                       ]
                                    )
                  ]).


%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
