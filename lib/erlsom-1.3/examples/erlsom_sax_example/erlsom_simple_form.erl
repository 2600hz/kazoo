%%% translate XML to the 'simple form' as used by XMERL.
%%%
-module(erlsom_simple_form).

%% user interface
-export([simple_form/1]).
%% with options
-export([simple_form/2]).

-include_lib("erlsom/src/erlsom_sax.hrl").

-export([callback/2]).
-export([nameFun/3]).

-record(sState, {stack, nameFun, options}).

simple_form(File) ->
  simple_form(File, []).

simple_form(File, Options) ->
  case file:read_file(File) of
    {ok, Bin} ->
      erlsom:sax(binary_to_list(Bin), 
                 #sState{stack = [], nameFun = fun erlsom_simple_form:nameFun/3, options = Options},
                 fun erlsom_simple_form:callback/2);
    Error ->
      Error
  end.

callback(Event, State) ->

  %% debugState(State),
  %% debugEvent(Event),
  try
    case Event of
      startDocument -> 
         State;
      {startElement, _Uri, _LocalName, _Prefix, _Attributes} ->
         %% debug(Event),
         startElement(Event, State);
      {endElement, _Uri, _LocalName, _Prefix} ->
         endElement(Event, State);
      {characters, _Characters} ->
         characters(Event, State);
      {ignorableWhitespace, _Characters} -> State;
      {processingInstruction, _Target, _Data} ->  State;
      {startPrefixMapping, _Prefix, _URI} -> 
         State;
      {endPrefixMapping, _Prefix} ->
         State;
      endDocument -> 
         case State of 
           #sState{stack = [Root]} ->
	     %% debug(Result),
	     {ok, Root};
	   _Else ->
	     %% debug(State),
             throw({error, "unexpected end"})
         end;
      {error, Message} ->
         throw(Message);
      {'EXIT', Message} ->
         exit(Message)
    end
  catch
    error:Reason -> throwError(error, {Reason,erlang:get_stacktrace()}, Event, State);
    Class:Exception -> throwError(Class, Exception, Event, State)
  end.
  
%% Stack contains the tree that is growing as the elements come in.
%% [{root, [attributes], [element1, element2]},
%%  {element3, [attributes], [element3.1, element3.2]},
%%  {element3.3, [attributes], [element3.3.1]}] (but in reverse order...)

%% When a startElement event comes in, add a new element to the stack:
%% [{root, [attributes], [element1, element2]},
%%  {element3, [attributes], [element3.1, element3.2]},
%%  {element3.3, [attributes], [element3.3.1]},
%%  {element3.3.2, [attributes], []}]

%% When a textElement event comes in, insert it into the top element:
%% [{root, [attributes], [element1, element2]},
%%  {element3, [attributes], [element3.1, element3.2]},
%%  {element3.3, [attributes], [element3.3.1, element3.3.2]}]
%%  {element3.3, [attributes], [element3.3.1]},
%%  {element3.3.2, [attributes], [{#text, "the text"}]}]

%% When an endElement comes in, insert the top element of the stack in the 
%% layer below it (its parent):
%% [{root, [attributes], [element1, element2]},
%%  {element3, [attributes], [element3.1, element3.2]},
%%  {element3.3, [attributes], [element3.3.1, element3.3.2]}]

startElement({startElement, Uri, LocalName, Prefix, Attributes}, 
             State = #sState{stack = Stack, nameFun = NameFun}) ->
  Name = NameFun(LocalName, Uri, Prefix),
  State#sState{stack = [{Name, processAttributes(Attributes, State), []} | Stack]}.

endElement({endElement, _Uri, _LocalName, _Prefix},
           State = #sState{stack = [{Name, Attributes, Elements}]}) ->
  State#sState{stack = [{Name, Attributes, lists:reverse(Elements)}]};

endElement({endElement, _Uri, _LocalName, _Prefix},
           State) ->
  #sState{stack = [{Name, Attributes, Elements} | [{ParentName, ParentAttributes, ParentElements} | Tail]]} = State,
  State#sState{stack = [{ParentName, 
                         ParentAttributes, 
                         [{Name, Attributes, lists:reverse(Elements)} | ParentElements]} | Tail]}.

characters({characters, Characters},
           State = #sState{stack = [{Name, 
                                     Attributes, 
                                     [FirstBit | OtherElements]
                                    } | Tail]})
           when is_list(FirstBit) ->
  State#sState{stack = [{Name, Attributes, [FirstBit ++ Characters | OtherElements]} | Tail]};
characters({characters, Characters},
           State = #sState{stack = [{Name, Attributes, Elements} | Tail]}) ->
  State#sState{stack = [{Name, Attributes, [Characters | Elements]} | Tail]}.

processAttributes(Attributes, State) ->
  processAttributes(Attributes, State, []).
processAttributes([], _State, Acc) ->
  lists:reverse(Acc);
processAttributes([#attribute{localName=LocalName, uri=Uri, prefix = Prefix, value=Value} | Tail], 
                  State = #sState{nameFun = NameFun},
                  Acc) ->
  processAttributes(Tail, State, [{NameFun(LocalName, Uri, Prefix), Value} | Acc]).

nameFun(Name, [], _Prefix) ->
  Name;
nameFun(Name, Namespace, _Prefix) ->
  "{" ++ Namespace ++ "}" ++ Name.
  

throwError(Class, Exception, Event, 
           #sState{stack = Stack}) ->
%% "Error while parsing type " 
%% Take the ElementRecord at current state, and print the first element
  Message = [{exception, Exception},
             %% for each of the elements in ResultSoFar, 
             %% take the 'elementRecord' element and print the first element (the type).
             {stack, printStackTrace(Stack)},
             %% "Received: "
             {received, Event}],
  case Class of 
    'error' -> exit({error, Message});
    'throw' -> throw({error, Message});
    'exit' -> exit({error, Message})
  end;

throwError(Class, Exception, _Event, 
           _Something) ->
  case Class of 
    'error' -> exit({error, Exception});
    'throw' -> throw({error, Exception});
    'exit' -> exit({error, Exception})
  end.

printStackTrace(Stack) ->
  printStackTrace(Stack, []).
printStackTrace([], Acc) ->
  Acc;
printStackTrace([{Name, _, _} | Tail], Acc) ->
  printStackTrace(Tail, [{element, Name} | Acc]).
  
