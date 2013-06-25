-module(continuation_example).
%% Example to show how the Erlsom Sax parser can be used in combination 
%% with a 'continuation function'. This enables parsing of very big documents
%% in a sort of streaming mode.
%% 
%% When the sax parser reaches the end of a block of data, it calls the
%% continuation function. This should return the next block of data.
%%
%% the continuation function is a function that takes 2 arguments: Tail and 
%% State.
%%    - Tail is the (short) list of characters that could not yet be parsed 
%%      because it might be a special token or not. Since this still has to
%%      be parsed, it should be put in front of the next block of data.
%%    - State is information that is passed by the parser to the callback
%%      functions transparently. This can be used to keep track of the 
%%      location in the file etc.
%% The function returns {NewData, NewState}, where NewData is a list of 
%% characters/unicode code points, and NewState the new value for the State.

-export([run/0]).

%% 'chunk' is the number of characters that is read at a time. 
%% should be tuned for the best result. (109 is obviously not a good value, 
%% it should be bigger than that - try it out).
-define(chunk, 109).

run() ->
  F = fun count_books/2,   %% the callback function that handles the sax events
  G = fun continue_file/2, %% the callback function that returns the next 
                           %% chunk of data
  %% open file
  {ok, Handle} = file:open(xml(), [read, raw, binary]),
  Position = 0,
  CState = {Handle, Position, ?chunk},
  SaxCallbackState = undefined,
  %% erlsom:parse_sax() returns {ok, FinalState, TrailingBytes},
  %% where TrailingBytes is the rest of the input-document
  %% that follows after the last closing tag of the XML, and Result 
  %% is the value of the State after processing the last SAX event.
  {ok, Result, _TrailingBytes} = 
    erlsom:parse_sax(<<>>, SaxCallbackState, F, 
      [{continuation_function, G, CState}]),
  %% close file
  ok = file:close(Handle),

  %% Result is a list [{track_id, count}, ...]
  lists:foreach(fun({Date, Count}) -> 
                  io:format("Date: ~p - count: ~p~n", [Date, Count])
                end, Result),
  ok.

%% this is a continuation function that reads chunks of data 
%% from a file.
continue_file(Tail, {Handle, Offset, Chunk}) ->
  %% read the next chunk
  case file:pread(Handle, Offset, Chunk) of
    {ok, Data} ->
      {<<Tail/binary, Data/binary>>, {Handle, Offset + Chunk, Chunk}};
    eof ->
      {Tail, {Handle, Offset, Chunk}}
  end.

%% This function is specific for the example. It counts the number 
%% of books per year.
%%
%% The input is the sax-event and the state. 
%% The output is the new state.
%% 
%% The state consists of a stack that corresponds to 
%% the level in the XML, and an accumulator for the result: [{Date, Count}].
%% Additionally there is a field 'element_acc' which contains
%% an intermediate result while parsing character data,
%% because there can be more than 1 character event per element (in theory).
-record(state, {stack = [], acc = [], element_acc = ""}).
count_books(startDocument, _) ->
  #state{};
count_books({startElement, _, Tag, _, _}, #state{stack = Stack} = State) ->
  State#state{stack = [Tag | Stack]};
count_books({characters, Value}, 
            #state{stack = ["date", "book", "book_store"],
                   element_acc = ElementAcc} = State)->
  State#state{element_acc = ElementAcc ++ Value};
count_books({endElement, _, _, _}, 
            #state{stack = ["date" | Tail], 
                   acc = Acc, 
                   element_acc = Value} = State)->
  State#state{stack = Tail, acc = processBook(Acc, Value), element_acc = ""};
count_books({endElement, _, _, _}, #state{stack = [_ | Tail]} = State)->
  State#state{stack = Tail};
count_books(endDocument, #state{acc = Acc})->
  Acc;
count_books(_, S) -> S.

processBook(List, Date) ->
  case lists:keysearch(Date, 1, List) of
    false ->
      [{Date, 1} | List];
    {value, {_, Count}} ->
      lists:keyreplace(Date, 1, List, {Date, Count + 1})
  end.

%% this is just to make it easier to test this little example
xml() -> filename:join([codeDir(), "BookStore.xml"]).
codeDir() -> filename:dirname(code:which(?MODULE)).
