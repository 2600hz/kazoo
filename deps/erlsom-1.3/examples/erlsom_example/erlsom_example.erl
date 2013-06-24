%%% a simple example of the use of erlsom.
%%%
-module(erlsom_example).

%% user interface
-export([run/0]).

%% define records
-record('in:arguments', {anyAttribs, values, precision}).
-record('out:resultType', {anyAttribs, result}).
-record('out:resultType-error', {anyAttribs, error}).
-record('out:resultType-okResult', {anyAttribs, value}).
-record('out:errorType', {anyAttribs, errorCode, errorDescription}).

run() ->
  %% compile xsd
  {ok, ModelIn} = erlsom:compile_xsd_file(example_in_xsd(), [{prefix, "in"}]),
  {ok, ModelOut} = erlsom:compile_xsd_file(example_out_xsd(),[{prefix, "out"}]),

  %% parse xml
  {ok, Input, _} = erlsom:scan_file(example_in_xml(), ModelIn),

  %% do something with the content
  case Input of
    #'in:arguments'{values = undefined} ->
      Error = #'out:errorType'{errorCode = "01", 
                               errorDescription = "No arguments provided"},
      Result = #'out:resultType-error'{error = Error};
    #'in:arguments'{values = List, precision = Precision} ->
      Result = #'out:resultType-okResult'{value = calcAverage(List, Precision)}
  end,
  
  %% generate xml.
  Response = #'out:resultType'{result=Result},
  XmlResult = erlsom:write(Response, ModelOut),
  io:format("Result: ~p~n", [XmlResult]),
  ok.

calcAverage(List, Precision) ->
  calcAverage(List, Precision, 0, 0).
calcAverage([], Precision, Acc, NrOfElements) ->
  lists:flatten(io_lib:format("~.*f", [Precision, Acc/NrOfElements]));
calcAverage([Head|Tail], Precision, Acc, NrOfElements) ->
  calcAverage(Tail, Precision, Acc + Head, NrOfElements + 1).

%% this is just to make it easier to test this little example
example_in_xsd() -> filename:join([codeDir(), "example_in.xsd"]).
example_out_xsd() -> filename:join([codeDir(), "example_out.xsd"]).
example_in_xml() -> filename:join([codeDir(), "example_in.xml"]).
codeDir() -> filename:dirname(code:which(?MODULE)).

