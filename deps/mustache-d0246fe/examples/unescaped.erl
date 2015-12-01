-module(unescaped).
-compile(export_all).

title() ->
  "Bear > Shark".

%%---------------------------------------------------------------------------

start() ->
  code:add_patha(".."),
  Output = mustache:render(unescaped, "unescaped.mustache"),
  io:format(Output, []).