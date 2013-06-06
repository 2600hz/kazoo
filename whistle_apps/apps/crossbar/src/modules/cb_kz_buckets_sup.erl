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
         ,start_child/0
         ,stop_child/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("./src/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(MAX_TOKENS, whapps_config:get_integer(?CONFIG_CAT, <<"max_bucket_tokens">>, 100)).
-define(FILL_RATE, whapps_config:get_integer(?CONFIG_CAT, <<"tokens_fill_rate">>, 10)).

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

start_child() ->
    supervisor:start_child(?SERVER, [?MAX_TOKENS, ?FILL_RATE]).

stop_child(Pid) ->
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
