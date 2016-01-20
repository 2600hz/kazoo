-module(gcm_sup).
-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0, start_child/2]).

-export([init/1]).

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_child(atom(),string()) -> sup_startchild_ret().
start_child(Name, ApiKey) ->
    supervisor:start_child(?SERVER, [Name, ApiKey]).

-spec init(any()) -> sup_init_ret().
init([]) ->
    {'ok', {{simple_one_for_one, 5, 10}, [?WORKER_TYPE('gcm', 'transient')]}}.
