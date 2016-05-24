-module(kz_rel).
-export([start/0]).

start() ->
	{ok, Cwd} = file:get_cwd(),
	code:add_path(Cwd).
