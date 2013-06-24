%%% a simple example of the use of erlsom.
%%%
-module(book_store).
-include("BookStore.hrl").

%% user interface
-export([run/0]).

run() ->
  %% compile xsd
  {ok, Model} = erlsom:compile_xsd_file(xsd()),
  %% parse xml
  {ok, #book_store{book=Books}, _} = erlsom:scan_file(xml(), Model),
  %% do something with the content
  lists:foreach(fun process_book/1, Books).

process_book(#book_type{'ISBN' = ISBN, title = Title, author = Author,
                        date = Date, publisher = Publisher}) ->
  io:format("Title    : ~s~n"
            "Author   : ~s~n"
            "Publisher: ~s~n"
            "ISBN     : ~s~n"
            "Date     : ~s~n~n", 
            [Title, Author, Publisher, ISBN, formatDate(Date)]).

formatDate(undefined) -> "<unknown>";
formatDate(Date) -> Date.

%% this is just to make it easier to test this little example
xsd() -> filename:join([codeDir(), "BookStore.xsd"]).
xml() -> filename:join([codeDir(), "BookStore.xml"]).
codeDir() -> filename:dirname(code:which(?MODULE)).
