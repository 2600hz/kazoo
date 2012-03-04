%%%-------------------------------------------------------------------
%%% @copyright (C) 2010 VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_mgr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start, [Args]}, permanent, 5000, Type, dynamic}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, _) ->
                      _ = supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id)
              end, ok, Kill),

    _ = [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Processes = [
                 ?CHILD(media_single_sup, supervisor)
                 ,?CHILD(media_continuous_suo, supervisor)
                 ,?CHILD(media_listener, worker)
                ], %% Put list of ?CHILD(media_mgr_server, worker) or ?CHILD(media_mgr_other_sup, supervisor)
    {ok, { {one_for_one, 10, 10}, Processes} }.
