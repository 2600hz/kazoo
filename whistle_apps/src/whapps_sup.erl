%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_app/1, restart_app/1, stop_app/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_app/1 :: (atom()) -> {'ok', pid() | 'undefined'} | {'ok', pid() | 'undefined', term()} | {'error', term()}.
start_app(App) ->
    supervisor:start_child(?MODULE, ?CHILD(App, supervisor)).

-spec restart_app/1 :: (atom()) -> {'ok', pid() | 'undefined'} | {'ok', pid() | 'undefined', term()} | {'error', term()}.
restart_app(App) ->
    ok = supervisor:terminate_child(?MODULE, App),
    supervisor:restart_child(?MODULE, App).

stop_app(App) ->
    _ = supervisor:terminate_child(?MODULE, App),
    supervisor:delete_child(?MODULE, App).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 120,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
