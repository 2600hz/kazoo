#!/usr/bin/env escript

path_erts() ->
  ErtsDir = lists:concat([code:root_dir(), "/erts-", erlang:system_info(version)]),
  filename:join(ErtsDir, "include").

path_interface() ->
  code:lib_dir(erl_interface, include).

path() -> lists:concat(["-I ", path_erts(), " -I ", path_interface()]).

main(_) ->
  io:format(path()).
