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

%%% ====================================================================
%%% An XML parser, using the SAX model.
%%% ====================================================================

-module(erlsom_sax).

-include_lib("erlsom_sax.hrl").

-export([parseDocument/3]).
-export([parseDocument/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Interface
%%
%%  parseDocument(Xml, State, EventFun)
%%  parseDocument(Xml, State, EventFun, Options)
%%
%%  Xml = A list of integers that correspond with the characters in an XML 
%%    document. Can be either 1 byte characters according to ISO ...,
%%    or integers that correspond to Unicode code points (see...).
%%
%%  State - a term() that is passed to the EventFun. 
%%
%%  Eventfun - a fun() that is called by the parser whenever it has parsed 
%%    a bit of the Xml input. The function is called by the parser according
%%    to the Sax specification (see [SAX]).
%%
%%    EventFun should accept the following arguments:
%%    - Event, a tuple that describes the event, see below.
%%    - State - a term()
%%
%%    EventFun should return State, a term() that wil be passed back to the next
%%    invocation of EventFun.
%%
%%  Options - a list of options. Currently the only option is 
%%    {continuation_function, CFunction}, where CFuntion is a fun() that 
%%    returns the next block of data.
%%    CFunction should be a function that takes 2 arguments: Tail and State.
%%    - Tail is the (short) list of characters that could not yet be parsed 
%%      because it might be a special token or not. Since this still has to
%%      be parsed, it should be put in front of the next block of data.
%%    - State is information that is passed by the parser to the callback
%%      functions transparently. This can be used to keep track of the 
%%      location in the file etc.
%%    CFunction returns {NewData, NewState}, where NewData is a list of 
%%    characters/unicode code points, and NewState the new value for the State.
%%
%%  Returns: State 
%%    (i.e.: the result of the last invocation of the callback function)
%%
%%  parseDocumentBinary(Xml, State, EventFun, Encoding)
%%  parseDocument(Xml, State, EventFun, Encoding, Options)
%%
%% Just like parseDocument, but working on a binary in stead of a list.
%% Encoding = the encoding of the binary (atom()). Supported values:
%% - 'utf-8'
%% - 'latin-1'
%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Events sent out by the SAX parser.
%%
%%  Based on org.xml.sax ContentHandler interface [SAX].
%%
%%  startDocument
%%
%%  endDocument 
%%    Will NOT be sent out in case of an error
%%
%%  {startPrefixMapping, Prefix, URI}
%%    Begin the scope of a prefix - URI namespace mapping
%%    Will be sent immediately before the corresponding startElement event.
%%
%%  {endPrefixMapping, Prefix}
%%    End the scope of a prefix - URI namespace mapping
%%    Will be sent immediately before the corresponding endElement event.
%% 
%%  {startElement, Uri, LocalName, Prefix, [Attributes]}
%%    Receive notification of the beginning of an element.
%%    There will be a corresponding endElement (even when the element is
%%    empty).
%%    All three name components will be provided.
%%
%%    [Attributes] is a list of attribute records, see sax.hrl.
%%    Namespace attributes (xmlns:*) will not be reported.
%%    There will be NO attribute values for defaulted attributes!
%%
%%    Providing 'Prefix'in stead of 'Qualified name' is probably not quite
%%    in line with the SAX spec, but it appears to be more convenient.
%%
%%  {endElement, Uri, LocalName, Prefix}
%%    Receive notification of the end of an element.
%%
%%  {characters, Characters}
%%    Receive notification  of character data.
%%    All character data will be in one chunk, except if there is a 
%%    CDATA section included inside a character section. In that case
%%    there will be separate events for the characters before the CDATA, the
%%    CDATA section and the characters following it (if any, of course).
%%    
%%  {ignorableWhitespace, Characters}
%%    If a character data section (as it would be reported by the 'characters'
%%    event, see above) consists ONLY of whitespace, it will be 
%%    reported as ignorableWhitespace.
%%
%%  {processingInstruction, Target, Data}
%%
%%  {error, Description}
%%  {internalError, Description}
%%
%%%%%%%%%%%%%%%%%%%%%%%%

parseDocument(Xml, UserState, Callback) ->
  parseDocument(Xml, UserState, Callback, []).

parseDocument(Xml, UserState, Callback, Options) when is_list(Xml) ->
  {CFun, CState} = getCFunction(Options),
  erlsom_sax_list:parse(Xml, 
    #erlsom_sax_state{callback = Callback, 
                      user_state = UserState,
                      continuation_state = CState,
                      continuation_fun = CFun,
                      output = getOutput(Options)});

parseDocument(Xml, UserState, Callback, Options) when is_binary(Xml) ->
  {CFun, CState} = getCFunction(Options),
  case getEncoding(Options) of
    undefined ->
      {Encoding, Xml2, CState2} = erlsom_lib:detectEncoding(Xml, CFun, CState),
      parseDocumentBinary(Encoding, Xml2, 
        #erlsom_sax_state{callback = Callback, 
                          user_state = UserState,
                          continuation_state = CState2,
                          continuation_fun = CFun,
                          output = getOutput(Options)});
    Encoding ->
      parseDocumentBinary(Encoding, Xml, 
        #erlsom_sax_state{callback = Callback, 
                          user_state = UserState,
                          continuation_state = CState,
                          continuation_fun = CFun,
                          output = getOutput(Options)})
  end.

parseDocumentBinary(Encoding, Xml, State) ->
  case Encoding of 
    'utf8' -> 
      erlsom_sax_utf8:parse(Xml, State);
    'utf16be' -> 
      erlsom_sax_utf16be:parse(Xml, State);
    'utf16le' -> 
      erlsom_sax_utf16le:parse(Xml, State);
    'latin-1' ->
      erlsom_sax_latin1:parse(Xml, State);
    'iso_8859_1' ->
      erlsom_sax_latin1:parse(Xml, State);
    'list' ->
      erlsom_sax_list:parse(Xml, State);
    _ ->
      throw({error, "Encoding not supported: " ++ atom_to_list(Encoding)})
  end.

getCFunction(Options) ->
  case lists:keysearch(continuation_function,1,Options) of
    {value, {_, F, S}} -> {F, S};
    false -> {fun(T, S) -> {T, S} end, undefined}
  end.

getOutput(Options) ->
  case lists:keysearch(output_encoding,1,Options) of
    {value, {_, Encoding}} -> Encoding;
    false -> list
  end.

getEncoding(Options) ->
  case lists:keysearch(encoding,1,Options) of
    {value, {_, F}} -> list_to_atom(F);
    false -> undefined
  end.
