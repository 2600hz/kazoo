-module(SKEL_sup).
-behavior(supervisor).

-export([start_link/0,init/1]).

start_link() ->
  supervisor:start_link(?MODULE,[]).

init(_Args) ->
  Restart = {one_for_all,5,1},
  Children = [
      %{Id,{M,F,A},permanent,brutal_kill,worker,[Module]}
    ],
  {ok,{Restart,Children}}.
