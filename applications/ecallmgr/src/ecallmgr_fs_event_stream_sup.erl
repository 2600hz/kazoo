%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_event_stream_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-export([start_link/2]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), wh_proplist()) -> startlink_ret().
start_link(Node, Options) -> supervisor:start_link(?MODULE, [Node, Options]).

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
-spec init(list()) -> sup_init_ret().
init([Node, Props]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 6,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = create_children_spec(Node, Props),
io:format("~p~n", [Children]),
    {'ok', {SupFlags, Children}}.

create_children_spec(Node, Props) ->
    Children = create_event_children(Node, Props, []),
    create_custom_children(Node, Props, Children).

create_event_children(Node, _Props, Children) ->
    lists:foldr(fun(Event, Childs) ->
                        [{Event, {'ecallmgr_fs_event_stream', 'start_link', [Node, Event, 'undefined']}
                          ,'permanent', 6000, 'worker', [Event]}|Childs]
                end, Children, ?FS_EVENTS).

create_custom_children(Node, _Props, Children) ->
    lists:foldr(fun(Subclass, Childs) ->
                        [{Subclass, {'ecallmgr_fs_event_stream', 'start_link', [Node, 'CUSTOM', Subclass]}
                          ,'permanent', 6000, 'worker', [Subclass]}|Childs]
                end, Children, ?FS_CUSTOM_EVENTS).
