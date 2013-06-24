%%% a simple example of the use of erlsom.
%%%
-module(erlsom_sax_example).

%% user interface
-export([run/0]).

run() ->
  case file:read_file(xml()) of
    {ok, Bin} ->
      {ok, _, _} = erlsom:parse_sax(Bin, ok, fun callback/2);
    Error ->
      Error
  end,
  ok.

callback(Event, State) ->
  io:format("~p\n", [Event]),
  State.

%% this is just to make it easier to test this little example
xml() -> filename:join([codeDir(), "sax_example.xml"]).
codeDir() -> filename:dirname(code:which(?MODULE)).
