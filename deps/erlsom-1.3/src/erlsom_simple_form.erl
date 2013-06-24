%%% Copyright (C) 2006 - 2008 Willem de Jong
%%%
%%% This file is part of Erlsom.
%%%
%%% Erlsom is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% Erlsom is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: w.a.de.jong@gmail.com

%%% translate XML to the 'simple form' as used by XMERL.
-module(erlsom_simple_form).

%% user interface
-export([scan/2]).
-export([callback/2]).

-include_lib("erlsom_sax.hrl").

-record(sState, {stack, nameFun, options}).

scan(Xml, Options) ->
  case lists:keysearch('nameFun', 1, Options) of
    {value, {_, Fun}} ->
      Options2 = lists:keydelete('nameFun', 1, Options);
    _ -> 
      Fun = fun nameFun/3,
      Options2 = Options
  end,
  erlsom:parse_sax(Xml, 
    #sState{stack = [], nameFun = Fun},
    fun callback/2, Options2).

%% It is also possible to call erlsom_simple_form:callback from within
%% another callback function (to parse a part of an xml document).
callback(Event, State) ->

  %% debugState(State),
  %% debugEvent(Event),
  try
    case Event of
      startDocument -> 
        case State of
          #sState{} ->
            State;
          %% could be more options in the future, but for now there
          %% is just 1
          [{name_function, NameFun}] ->
            #sState{stack = [], nameFun = NameFun, options = []};
          _ ->
            #sState{stack = [], nameFun = fun nameFun/3, options = []}
        end;
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
	    Root;
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
           State = #sState{stack = [{Name, 
                                     Attributes, 
                                     [FirstBit | OtherElements]
                                    } | Tail]})
           when is_binary(FirstBit) ->
  State#sState{stack = [{Name, Attributes, [<<FirstBit/binary, Characters/binary>> | OtherElements]} | Tail]};
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
  
