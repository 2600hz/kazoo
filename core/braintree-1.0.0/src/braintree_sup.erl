-module(braintree_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(any()) -> sup_init_ret().
init([]) ->
    {'ok', { {'one_for_one', 5, 10}, []} }.
