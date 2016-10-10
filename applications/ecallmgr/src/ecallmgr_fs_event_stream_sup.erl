%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
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

-define(EVENTS, application:get_env(?APP, 'event_stream', ?FS_EVENTS)).
-define(CUSTOM_EVENTS, application:get_env(?APP, 'event_stream_custom', ?FS_CUSTOM_EVENTS)).

-define(CHILDREN, [event_child(Node, Event) || Event <- ?EVENTS]
        ++ [custom_child(Node, Subclass) || Subclass <- ?CUSTOM_EVENTS]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node, Options) ->
    supervisor:start_link({'local', sup_name(Node)}, ?MODULE, [Node, Options]).

sup_name(Node) ->
    Name = iolist_to_binary([kz_util:to_binary(?MODULE)
                            ,"_"
                            ,kz_util:to_binary(Node)
                            ]),
    kz_util:to_atom(Name, 'true').

-spec add_child(atom(), {'CUSTOM', atom()} | atom()) -> sup_startchild_ret().
add_child(Node, {'CUSTOM', Subclass}) ->
    supervisor:start_child(sup_name(Node), custom_child(Node, Subclass));
add_child(Node, Event) ->
    supervisor:start_child(sup_name(Node), event_child(Node, Event)).

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
init([Node, _Props]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 6,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.


-spec event_child(atom(), atom()) -> sup_child_spec().
event_child(Node, Event) ->
    ?WORKER_NAME_ARGS('ecallmgr_fs_event_stream', Event, [Node, Event, 'undefined']).

-spec custom_child(atom(), atom()) -> sup_child_spec().
custom_child(Node, Subclass) ->
    ?WORKER_NAME_ARGS('ecallmgr_fs_event_stream', Subclass, [Node, 'CUSTOM', Subclass]).

