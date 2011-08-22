%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011 VoIP Inc
%%% @doc
%%% Supervisor for running conference service processes
%%% @end
%%% Created : 28 Jun 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conf_service_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

%% API
-export([start_link/0, start_service/2, start_service/3]).

%% Supervisor callbacks
-export([init/1]).

-include("conference.hrl").

-define(SERVER, ?MODULE).
-define(CHILD(Name, Mod, Args), {Name, {Mod, start_link, Args}, transient, 5000, worker, [Mod]}).

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
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_service/2 :: (ConfId, Conference) -> sup_startchild_ret() when
      ConfId :: binary(),
      Conference :: json_object().
start_service(ConfId, Conference) ->
    start_service(ConfId, Conference, undefined).

-spec start_service/3 :: (ConfId, Conference, Caller) -> sup_startchild_ret() when
      ConfId :: binary(),
      Conference :: json_object(),
      Caller :: undefined | json_object().
start_service(ConfId, Conference, Caller) ->
    _ = supervisor:delete_child(conf_service_sup, ConfId),
    supervisor:start_child(?SERVER, ?CHILD(ConfId, conf_service, [Conference, Caller])).

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
-spec init(Args) -> sup_init_ret() when
      Args :: [].
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.
