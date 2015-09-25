%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_event_stream_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-export([start_link/2
         ,add_child/2
        ]).
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
start_link(Node, Options) -> supervisor:start_link({'local', sup_name(Node)}
                                                   ,?MODULE
                                                   ,[Node, Options]
                                                  ).

sup_name(Node) ->
    Name = iolist_to_binary([wh_util:to_binary(?MODULE)
                             ,"_"
                             ,wh_util:to_binary(Node)
                            ]),
    wh_util:to_atom(Name, 'true').

-spec add_child(atom(), {'CUSTOM', atom()} | atom()) -> sup_startchild_ret().
add_child(Node, {'CUSTOM', Subclass}) ->
    supervisor:start_child(sup_name(Node), ?WORKER_NAME_ARGS('ecallmgr_fs_event_stream'
                                                             ,Subclass
                                                             ,[Node, 'CUSTOM', Subclass]
                                                            ));
add_child(Node, Evt) ->
    supervisor:start_child(sup_name(Node), ?WORKER_NAME_ARGS('ecallmgr_fs_event_stream'
                                                             ,Evt
                                                             ,[Node, Evt, 'undefined']
                                                            )).

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
    {'ok', {SupFlags, Children}}.

-spec create_children_spec(atom(), wh_proplist()) -> sup_child_specs().
create_children_spec(Node, Props) ->
    Children = create_event_children(Node, Props, []),
    create_custom_children(Node, Props, Children).

-spec create_event_children(atom(), any(), sup_child_specs()) -> sup_child_specs().
create_event_children(Node, _Props, Children) ->
    lists:foldr(fun(Event, Kids) ->
                        [?WORKER_NAME_ARGS('ecallmgr_fs_event_stream'
                                           ,Event
                                           ,[Node, Event, 'undefined']
                                          )
                         | Kids
                        ]
                end, Children, ?FS_EVENTS).

-spec create_custom_children(atom(), any(), sup_child_specs()) -> sup_child_specs().
create_custom_children(Node, _Props, Children) ->
    lists:foldr(fun(Subclass, Kids) ->
                        [?WORKER_NAME_ARGS('ecallmgr_fs_event_stream'
                                           ,Subclass
                                           ,[Node, 'CUSTOM', Subclass]
                                          )
                         | Kids
                        ]
                end, Children, ?FS_CUSTOM_EVENTS).
