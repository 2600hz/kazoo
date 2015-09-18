-module(gcm_sup).
-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(),string()) ->
       {'error',_} | {'ok',api_pid()} | {'ok',api_pid(),_}.
start_child(Name, ApiKey) ->
    supervisor:start_child(?MODULE, [Name, ApiKey]).

-spec init([]) -> {ok, {{supervisor:strategy(), 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(gcm, worker)]}}.

