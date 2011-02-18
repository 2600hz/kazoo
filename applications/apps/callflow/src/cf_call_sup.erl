-module(cf_call_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_proc/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_proc(Args) -> supervisor:start_child(?SERVER, Args).


init([]) ->
    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild = {
       cf_call_handler,
       {cf_call_handler, start_link, []},
       Restart, Shutdown, Type, [cf_call_handler]
    },

    {ok, {{simple_one_for_one, 5, 10}, [AChild]}
}.
