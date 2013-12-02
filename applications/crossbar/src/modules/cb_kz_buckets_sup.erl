%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Supervisor for Kazoo Token Bucket Servers
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_kz_buckets_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
         ,start_bucket/3
         ,stop_bucket/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).

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
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_bucket(pos_integer(), pos_integer(), kz_token_bucket:fill_rate_time()) -> any().
-spec stop_bucket(pid()) -> any().

start_bucket(MaxTokens, FillRate, FillTime) ->
    supervisor:start_child(?SERVER, [MaxTokens, FillRate, 'false', FillTime]).

stop_bucket(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

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
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?WORKER('kz_token_bucket')]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
