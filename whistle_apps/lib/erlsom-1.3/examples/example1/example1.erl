-module(example1).
-export([test_erlsom/1]).
-export([run/0]).
%% this example has 2 purposes:
%%
%% - It shows how easy Erlsom makes it for you to use an XML configuration file.
%%  The configuration file describes a set of 10 test cases, which are run by
%%  this example. The configuration file is described by "example1.xsd".
%%  Compiling this XSD and then parsing the configuration file ("example1.xml")
%%  gives you access to an Erlang structure of records that corresponds with the
%%  XML schema.
%%
%%- It shows how 11 different schemas (names "abb1.xsd" through "abb11.xsd") can
%%  describe the same XML document (named "abb.xml"), and it shows the output
%%  that results from running Erlsom on this file using these schema’s.  To run
%%  the example for XSD abb1.xsd, use the command  example1:test_erlsom("abb1").


%% example1.hrl contains the record definitions.
%% It was generated using erlsom:writeHrl
-include("example1.hrl").
run() ->
  test_erlsom("abb11").

test_erlsom(Test) ->
  XsdFile = filename:join([codeDir(), "example1.xsd"]),
  {ok, Model} = erlsom:compile_xsd_file(XsdFile),
  Xml = filename:join([codeDir(), "example1.xml"]),
  {ok, TestSuite, _} = erlsom:scan_file(Xml, Model),
  Cases = TestSuite#testConfig.'case',
  Dir = codeDir(),
  case findCase(Cases, Test) of
    {ok, Case} -> execute_case(Case, Dir);
    _Else -> io:format("Case not found\n", [])
  end.

execute_case(#'case'{name=Name, xsd=XSD, xml=XML}, Path) ->
  io:format("example: ~p\n", [Name]),
  XsdFile = filename:join([Path, XSD]),
  io:format("compiling xsd ~p...\n", [XsdFile]),
  Result = erlsom:compile_file(XsdFile, []),
  case Result of
    {error, Message} ->
      io:format("XSD error: ~p\n", [Message]);
    {ok, Model} ->
      Xml = filename:join([Path, XML]),
      erlsom:parse_file(Xml, Model)
  end.

findCase([], _Name) ->
  false;
findCase([Case = #'case'{name=Name}| _], Name) ->
  {ok, Case};
findCase([_| Tail], Name) ->
  findCase(Tail, Name).

%% this is just to make it easier to test this little example
codeDir() -> filename:dirname(code:which(?MODULE)).
