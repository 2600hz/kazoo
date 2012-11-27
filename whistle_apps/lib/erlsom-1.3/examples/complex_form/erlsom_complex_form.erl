%%% translate XML to the output format used by XMERL.
%%% The output is not complete: some fields in the XMERL output records
%%% are not populated. But is it enough to use the XPATH fucntions (at 
%%% least for the examples that I tried).
%%%
%%% Note: this hasn't been tested properly. See it as an example of how 
%%% the sax parser can be used.
%%%
-module(erlsom_complex_form).

%% user interface
-export([scan/1]).
-export([scan_file/1]).
%% with options
-export([scan/2]).
-export([scan_file/2]).

-include_lib("erlsom/src/erlsom_sax.hrl").

%% The record defintions below are copied from xmerl hrl files!
%% XML Element
%% content = [#xmlElement()|#xmlText()|#xmlPI()|#xmlComment()|#xmlDecl()]
-record(xmlElement,{
	  name,			% atom()
	  expanded_name = [],	% string() | {URI,Local} | {"xmlns",Local}
	  nsinfo = [],	        % {Prefix, Local} | []
	  namespace,
	  parents = [],		% [{atom(),integer()}]
	  pos,			% integer()
	  attributes = [],	% [#xmlAttribute()]
	  content = [],
	  language = "",	% string()
	  xmlbase="",           % string() XML Base path, for relative URI:s
	  elementdef=undeclared % atom(), one of [undeclared | prolog | external | element]
	 }).

%% plain text
%% IOlist = [char() | binary () | IOlist]
-record(xmlText,{
	  parents = [],	% [{atom(),integer()}]
	  pos,		% integer()
	  language = [],% inherits the element's language
	  value,	% IOlist()
	  type = text   % atom() one of (text|cdata)
	 }).

%% Attribute
-record(xmlAttribute,{
	  name,		   % atom()
	  expanded_name=[],% atom() | {string(),atom()}
	  nsinfo = [],	   % {Prefix, Local} | []
	  namespace = [],  % inherits the element's namespace
	  parents = [],	   % [{atom(),integer()}]
	  pos,		   % integer()
	  language = [],   % inherits the element's language
	  value,	   % IOlist() | atom() | integer()
	  normalized       % atom() one of (true | false)
	 }).

%% namespace record
-record(xmlNamespace,{
	  default = [],
	  nodes = []
	 }).


-record(sState, {stack = [], posStack = [],  options}).

scan_file(File) ->
  scan_file(File, []).

scan_file(File, Options) ->
  case file:read_file(File) of
    {ok, Bin} ->
      scan(Bin, Options);
    Error ->
      Error
  end.

scan(Xml) ->
  scan(Xml, []).

scan(Xml, Options) ->
  erlsom:parse_sax(Xml, 
    #sState{stack = []},
    fun callback/2, Options).


callback(Event, State) ->

  try
    case Event of
      startDocument -> 
        case State of
          #sState{} ->
            State;
          _ ->
            #sState{stack = [], options = []}
        end;
      {startElement, _Uri, _LocalName, _Prefix, _Attributes} ->
        startElement(Event, State);
      {endElement, _Uri, _LocalName, _Prefix} ->
        endElement(Event, State);
      {characters, _Characters} ->
        characters(Event, State);
      {ignorableWhitespace, Characters} -> 
        characters({characters, Characters}, State);
      {processingInstruction, _Target, _Data} ->  State;
      {startPrefixMapping, _Prefix, _URI} -> 
        State;
      {endPrefixMapping, _Prefix} ->
        State;
      endDocument -> 
        case State of 
          #sState{stack = [Root]} ->
	    Root;
	  _Else ->
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

%% Now with some added info that we need for the complex form:
%% Stack contains the tree that is growing as the elements come in.
%% [{root, SeqNo, [attributes], [element1, element2]},
%%  {element3, SeqNo, [attributes], [element3.1, element3.2]},
%%  {element3.3, SeqNo, [attributes], [element3.3.1]}] (but in reverse order...)


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
             State = #sState{stack = [], posStack = []}) ->
  Name = makeName(LocalName, Prefix),
  State#sState{stack = [#xmlElement{name = Name,
	                            expanded_name = makeExpandedName(Uri, LocalName),
                                    pos = 1,
                                    nsinfo = makeNsInfo(Prefix, LocalName),
                                    namespace = makeNs(Prefix, Uri, #xmlNamespace{}),
                                    parents = [],
                                    attributes = processAttributes(Attributes, State),
                                    content = []}],
               posStack = [0]};

startElement({startElement, Uri, LocalName, Prefix, Attributes}, 
             State = #sState{stack = [Parent | _], posStack = [Pos | _]}) ->
  Name = makeName(LocalName, Prefix),
  State#sState{stack = [#xmlElement{name = Name,
	                            expanded_name = makeExpandedName(Uri, LocalName),
                                    pos = Pos + 1,
                                    nsinfo = makeNsInfo(Prefix, LocalName),
                                    namespace = makeNs(Prefix, Uri, Parent#xmlElement.namespace),
                                    parents = getParentsFromStack(State#sState.stack, []),
                                    attributes = processAttributes(Attributes, State),
                                    content = []} | State#sState.stack],
               posStack = [0 | State#sState.posStack]}.

endElement({endElement, _Uri, _LocalName, _Prefix},
           State = #sState{stack = [#xmlElement{content = Content} = Top]}) ->
  State#sState{stack = [Top#xmlElement{content = lists:reverse(Content)}]};

endElement({endElement, _Uri, _LocalName, _Prefix},
           %%State) ->
  #sState{stack = [#xmlElement{content = ChildContent} = Child | 
                     [#xmlElement{content = ParentContent} = Parent | Tail]],
          posStack = [_NrOfChildEls | [NrOfElements | PosTail]]} = State) ->
  State#sState{stack = [Parent#xmlElement{content = [Child#xmlElement{content = lists:reverse(ChildContent)} | 
                                                     ParentContent]} | 
                        Tail],
               posStack = [NrOfElements + 1 | PosTail]}.

characters({characters, Characters},
           State = #sState{stack = [#xmlElement{content = [#xmlText{value = Text} = FirstPart | Rest]} = Element | Tail]}) ->
  State#sState{stack = [Element#xmlElement{content = [FirstPart#xmlText{value = Text ++ Characters} | Rest]} | Tail]};

characters({characters, Characters},
           State = #sState{stack = [#xmlElement{content = Content} = Element | Tail],
                           posStack = [NrOfElements | PosTail]}) ->
  State#sState{stack = [Element#xmlElement{content = [#xmlText{value = Characters,
                                                               parents = getParentsFromStack(State#sState.stack, []),
                                                               pos = NrOfElements + 1} | Content]} | Tail],
                                           posStack = [NrOfElements + 1 | PosTail]}.

getParentsFromStack([], Acc) ->
  Acc;
getParentsFromStack([#xmlElement{name = Name, pos = Pos} | Tail], Acc) ->
  getParentsFromStack(Tail, [{Name, Pos} | Acc]).

processAttributes(Attributes, State) ->
  processAttributes(Attributes, State, 1,  []).
processAttributes([], _State, _Count, Acc) ->
  lists:reverse(Acc);
processAttributes([#attribute{localName=LocalName, uri = Uri, prefix = Prefix, value=Value} | Tail], 
                  State, Count, Acc) ->
  processAttributes(Tail, State, Count + 1, [
    #xmlAttribute{
	  name = makeName(LocalName, Prefix), 
	  expanded_name = makeExpandedName(Uri, LocalName),
          nsinfo = makeNsInfo(Prefix, LocalName),
	  pos = Count,
	  value = Value
	 } | Acc]).

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
printStackTrace([#xmlElement{name = Name} | Tail], Acc) ->
  printStackTrace(Tail, [{element, Name} | Acc]).
  
makeName(Local, []) ->
  list_to_atom_or_not(Local);
makeName(Local, Prefix) ->
  list_to_atom_or_not(Prefix ++ ":" ++ Local).

makeNsInfo([], _) -> [];
makeNsInfo(Prefix, Local) -> {Prefix, Local}.

makeNs(_Prefix, [], Ns) ->
 Ns;
makeNs(Prefix, Uri, #xmlNamespace{nodes = Nodes} = ParentNs) ->
 ParentNs#xmlNamespace{nodes = Nodes ++ [{Prefix, list_to_atom_or_not(Uri)}]}.

% string() | {URI,Local} | {"xmlns",Local}
makeExpandedName([], Local) ->
  list_to_atom_or_not(Local);
makeExpandedName(Uri, Local) ->
  {list_to_atom_or_not(Uri), list_to_atom_or_not(Local)}.

list_to_atom_or_not(String) ->
  try list_to_atom(String)
  catch 
    _:_ -> String
  end.

