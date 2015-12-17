-module(complex).
-compile(export_all).

header() ->
  "Colors".

item() ->
  A = dict:from_list([{name, "red"}, {current, true}, {url, "#Red"}]),
  B = dict:from_list([{name, "green"}, {current, false}, {url, "#Green"}]),
  C = dict:from_list([{name, "blue"}, {current, false}, {url, "#Blue"}]),
  [A, B, C].

link(Ctx) ->
  mustache:get(current, Ctx).

list() ->
  length(item()) =/= 0.

empty() ->
  length(item()) =:= 0.

%%---------------------------------------------------------------------------

start() ->
  code:add_patha(".."),
  Output = mustache:render(complex, "complex.mustache"),
  io:format(Output, []).