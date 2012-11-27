%% shows how you could (in theory, at least) apply xpath to the 
%% result of erlsom_complex_form.
-module(complex_form_example).
-include_lib("xmerl/include/xmerl.hrl").

-export([run/0]).

run() ->
  {ok, ParsedDoc, _} = erlsom_complex_form:scan_file(xml()),
  Result = xmerl_xpath:string("//myelement[. = 'x']/text()", ParsedDoc),
  io:format("result of Xpath query \"//myelement[. = 'x']/text()\"~n"),
  io:format("~p~n", [Result]).

%% this is just to make it easier to test this little example
xml() -> filename:join([codeDir(), "foo.xml"]).
codeDir() -> filename:dirname(code:which(?MODULE)).
