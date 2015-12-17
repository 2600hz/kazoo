-module(simple).
-compile(export_all).

name() ->
  "Tom".

value() ->
  10000.

taxed_value() ->
  value() - (value() * 0.4).

in_ca() ->
  true.

%%---------------------------------------------------------------------------

start() ->
  code:add_patha(".."),
  Ctx = dict:from_list([{name, "TPW"}]),
  Output = mustache:render(simple, "simple.mustache", Ctx),
  io:format(Output, []).