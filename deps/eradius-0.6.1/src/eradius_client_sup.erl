
-module(eradius_client_sup).

-behaviour(supervisor).

-export([start/0, init/1]).

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
