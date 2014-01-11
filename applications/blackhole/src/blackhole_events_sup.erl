%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_events_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_listener/1
         ,get_listener/1
        ]).


-include("blackhole.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, []).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the listener
%% @end
%%--------------------------------------------------------------------
-spec start_listener(ne_binary()) -> startlink_ret().
start_listener(ConfId) ->
    case already_started(ConfId) of
        {'true', Pid} ->
            {'ok', Pid};
        {'false', 'not_found'} ->
        	ChildSpec = {ConfId, {'blackhole_events_listener', 'start_link', [ConfId]}
                         ,'temporary', 5000, 'worker', [?MODULE]
                        },
            supervisor:start_child(?MODULE, ChildSpec)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_listener(ne_binary()) -> startlink_ret() | 'not_found'.
get_listener(ConfId) ->
    Children = supervisor:which_children(?MODULE),
    lists:foldl(fun({Id, _, _, _}=Child, Acc) ->
                    case Id =:= ConfId of
                        'true' ->
                            Child;
                        'false' ->
                            Acc
                    end
                end
                ,'not_found'
                ,Children).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec already_started(ne_binary()) -> {boolean(), 'not_found' | pid()}.
already_started(ConfId) ->
    Children = supervisor:which_children(?MODULE),
    lists:foldl(fun({Id, Pid, _, _}, Acc) ->
                    case Id =:= ConfId of
                        'true' ->
                            {'true', Pid};
                        'false' ->
                            Acc
                    end
                end
                ,{'false', 'not_found'}
                ,Children).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
