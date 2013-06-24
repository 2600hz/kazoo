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
%%% A couple of support functions for Erlsom
%%% ====================================================================

-module(erlsom_lib).

-export([convertPCData/4,
         makeName/2, makeName/4, nameWithoutPrefix/1,
         makeAttrName/2, makeTypeName/4, makeTypeName/2,
         makeElementName/2, makeGroupName/2, makeTypeRef/2,
         makeTypeRefAtom/2, makeGroupRef/2, makeElementRef/2,
         makeAttrRef/2, makeTagFromRef/2,
         makeTag/2, makeTag/3,
         findPrefix/2, findPrefix2/2,
         translateType/1,
         minMax/1, multMinMax/2,
         tagNamespace/2,
         listLength/1,
         toUnicode/1, detect_encoding/1, detectEncoding/3,
         findFile/4, find_xsd/4, findType/6,
         readImportFile/1,
         newTree/0, addTreeElement/3, isAncestor/3, getAncestor/2,
         emptyListIfUndefined/1,
         searchBase/2,
         makeQname/1, localName/1, 
         getTargetNamespaceFromXsd/1,
         removePrefixes/1]).

-include_lib("erlsom_compile.hrl").
-include_lib("erlsom_sax.hrl").
-include_lib("erlsom.hrl").
-include_lib("erlsom_parse.hrl"). %% the record definitions

%% debug(Text) -> io:format("lib: ~p\n", [Text]).

%%debug(Text1, Text2) ->
  %%io:format("~p ~p\n", [Text1, Text2]).


%% Convert text to the indicated type.
convertPCData(Text, char, _Namespaces, _NamespaceMapping) when is_binary(Text) ->
  Text;
convertPCData(Text, Type, Namespaces, NamespaceMapping) when is_binary(Text) ->
  convertPCData(erlsom_ucs:decode_utf8(Text), Type, Namespaces, NamespaceMapping);
convertPCData(Text, Type, Namespaces, NamespaceMapping) ->
  case Type of
    char ->
      Text;
    atom ->
      list_to_atom(Text);
    ascii ->
      %% this is only used by the compiler. Names (which have to be converted to atoms later on
      %% in the process) have this type.
      try 
        list_to_atom(Text)
      catch
        _Class:Exception -> throw(Exception)
      end,
      Text;
    integer ->
      list_to_integer(Text);
    bool ->
      case Text of
        "true" -> true;
        "false" -> false;
        "0" -> false;
        "1" -> true;
        _ -> throw({error, "invalid value for boolean: " ++ Text})
      end;
    float ->
      list_to_float(Text);
    qname ->
      %% qname has form prefix:localname (or, if there is no prefix: localname)
      %% split the two parts, look up the prefix to find the uri, and put it into 
      %% a qname record {localname, URI, prefix} (URI = undefined if there was no prefix)
      {Prefix, LocalName} = splitOnColon(Text),
      case lists:keysearch(Prefix, 3, Namespaces) of
        {value, #ns{uri = URI}} ->
          %% this is namespace qualified - now see whether a mapping applies
          case lists:keysearch(URI, 2, NamespaceMapping) of
            {value, #ns{prefix = MappedPrefix}}  ->
              #qname{localPart = LocalName, uri = URI, prefix = Prefix, mappedPrefix = MappedPrefix};
            _Else ->
              #qname{localPart = LocalName, uri = URI, prefix = Prefix, mappedPrefix = Prefix}
          end;
        _Else ->
          if 
            Prefix == [] -> %% no prefix, no default namespace
              #qname{localPart = Text};
            Prefix == "xml"  -> %% by convention 
              #qname{localPart = LocalName, uri = "http://www.w3.org/XML/1998/namespace", 
                     prefix = Prefix, mappedPrefix = Prefix};
            true ->
              throw({error, "Invalid Qname: " ++ Text})
          end
      end
  end.

%% Tree is a data structure used to find out the relations between types.
%% It is actually a forest, in the sense that there doesn't have to be 
%% a unique root.
newTree() -> [].

addTreeElement(Child, Parent, Tree) ->
  [{Child, Parent} | Tree].

%% find out whether Ancestor is really an Ancestor of Element.
isAncestor(Element, Element, _Tree) -> true; %% added because of problem reported by Stu
isAncestor(Ancestor, Element, Tree) ->
  case lists:keysearch(Element, 1, Tree) of
    {value, {_, Ancestor}} -> true;
    {value, {_, Parent} = Elem} -> 
      %% remove the element, just to be sure that we don't end up in an
      %% endless loop.
      isAncestor(Ancestor, Parent, lists:delete(Elem, Tree));
    _ -> false
  end.
    
%% get the ancestor of this type (if any)
getAncestor(Element, Tree) ->
  case lists:keysearch(Element, 1, Tree) of
    {value, {_, Ancestor}} -> {value, Ancestor};
    _ -> false
  end.
    

minMax(undefined) ->
  1;
minMax("unbounded") ->
  unbound;
minMax(Integer) ->
  list_to_integer(Integer).

multMinMax(0, _) -> 0;
multMinMax(_, 0) -> 0;
multMinMax(unbound, _) -> unbound;
multMinMax(_, unbound) -> unbound;
multMinMax(A, B) -> A * B.

%% returns the URI that belongs to a tag.
%% tag is an atom 'ppp:llll' or 'llll' (ppp = prefix, llll  = local name)
%% namespaces is [#ns{prefix, uri}]
tagNamespace(Tag, Namespaces) ->
  tagNamespace(atom_to_list(Tag), [], Namespaces).

tagNamespace([$: | _Tail], Acc, Namespaces) ->
  case lists:keysearch(lists:reverse(Acc), #ns.prefix, Namespaces) of
    {value, #ns{uri = Uri}} ->
      Uri;
    _Other ->
      undefined
  end;
tagNamespace([], _Acc, _Namespaces) ->
  undefined;
tagNamespace([Char | Tail], Acc, Namespaces) ->
  tagNamespace(Tail, [Char | Acc], Namespaces).
  

nameWithoutPrefix(Name) ->
  nameWithoutPrefix(Name, []).

nameWithoutPrefix([$: | Tail], _Acc) ->
  Tail;
nameWithoutPrefix([Char | Tail], Acc) ->
  nameWithoutPrefix(Tail, [Char | Acc]);
nameWithoutPrefix([], Acc) ->
  lists:reverse(Acc).

translateType(String) ->
  case String of
    "integer" ->
       'integer';
    "int" ->
       'integer';
    "QName" ->
       'qname';
    "boolean" ->
       'bool';
    _Else ->
       'char' 
  end.


findPrefix(undefined, _Namespaces) ->
  [];
findPrefix(Namespace, Namespaces) ->
  case lists:keysearch(Namespace, #ns.uri, Namespaces) of
    {value, #ns{prefix = undefined}} ->
      "";
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":";
    _Else ->
      throw({error, "Namespace not found " ++ Namespace})
  end.

findPrefix2(undefined, _Namespaces) ->
  [];
findPrefix2(Namespace, Namespaces) ->
  case lists:keysearch(Namespace, #ns.uri, Namespaces) of
    {value, #ns{prefix = undefined}} ->
      "";
    {value, #ns{prefix = Prefix}} ->
      Prefix;
    _Else ->
      "P"
  end.

makeTypeName(Name, Prefix) ->
  TypePrefix = case get(erlsom_typePrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  Prefix ++ TypePrefix ++ Name.

makeGroupName(Name, Prefix) ->
  TypePrefix = case get(erlsom_groupPrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  Prefix ++ TypePrefix ++ Name.

makeElementName(Name, Prefix) ->
  %% TypePrefix = case get(erlsom_elementPrefix) of
                 %% undefined -> "";
                 %% Value -> Value
               %% end,
  Prefix ++ Name.

makeTypeName(NameInXsd, ElementFormDefault, Path, Prefix) ->
  TypePrefix = case get(erlsom_typePrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  makeName(NameInXsd, ElementFormDefault, TypePrefix ++ Path, Prefix).

makeName(NameInXsd, _ElementFormDefault = "qualified", Path, Prefix) ->
   Prefix ++ Path ++ NameInXsd;

makeName(NameInXsd, _ElementFormDefault, Path, _Prefix) ->
   Path ++ NameInXsd.

%% -record(schemaInfo, {targetNamespace, elementFormDefault, namespacePrefix, namespaces}).
makeName(NameInXsd, #schemaInfo{elementFormDefault="qualified", targetNamespace=TNS, namespaces=NS,
                                path=Path}) ->

  %% find the target namespace in NS,
  %% add the prefix and the path

  case NameInXsd of
    _ -> ok
  end,
  case lists:keysearch(TNS, 2, NS) of
    {value, #ns{prefix = undefined}} ->
      Path ++ NameInXsd;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ Path ++ NameInXsd;
    _Else ->
      if 
        TNS == undefined ->
          Path ++ NameInXsd;
        true ->
          throw({error, "Namespace not found " ++ NameInXsd})
      end
  end;

makeName(NameInXsd, #schemaInfo{targetNamespace=TNS, namespaces=NS, path=[]}) ->
  %% elementFormDefault = unqualified (or undefined)
  %% since Path = [], this is a global element,and we need to add the prefix.

  case lists:keysearch(TNS, 2, NS) of
    {value, #ns{prefix = undefined}} ->
      NameInXsd;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ NameInXsd;
    _Else ->
      if 
        TNS == undefined ->
          NameInXsd;
        true ->
          throw({error, "Namespace not found " ++ NameInXsd})
      end
  end;

makeName(NameInXsd, #schemaInfo{targetNamespace=TNS, namespaces=NS, path=Path}) ->
  %% elementFormDefault = unqualified (or undefined)
  %% since Path /= [], this is a local element,and we need to add the prefix and the path.

  case lists:keysearch(TNS, 2, NS) of
    {value, #ns{prefix = undefined}} ->
      NameInXsd;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ Path ++ NameInXsd;
    _Else ->
      if 
        TNS == undefined ->
          Path ++ NameInXsd;
        true ->
          throw({error, "Namespace not found " ++ NameInXsd})
      end
  end.


%% -record(schemaInfo, {targetNamespace, elementFormDefault, namespacePrefix, namespaces}).
makeAttrName(NameInXsd, _Info) ->
  NameInXsd.

makeTypeRefAtom(Qname, Namespaces) ->
  TypeRef = makeTypeRef(Qname, Namespaces),
  case TypeRef of
    {_, _} -> TypeRef;
    _ -> list_to_atom(TypeRef)
  end.

%% makeTypeRef creates a reference to a type. This can either be a type 
%% defined in the XSD (or an imorted XSD), or a predefined type (like 
%% xsd:string).
%% For the predefined types special codes are returned ({'#PCDATA', ...}).
%% input is a qname.
%% The output includes the prefix (as found in the Namespaces list), unless the 
%% type is not in a particular namespace.
%% If a special prefix was defined for types, this prefix is also added (after
%% the namespace prefix. So the result could be P:t#MyType, for example.
%% The 'type-prefix' is taken from a proces-variable (not very nice, sorry).

%% TODO: should return an atom (or {'PCDATA', ...})?
makeTypeRef(undefined, _) ->
  %% the 'ur-type': any type (and any attibute).
   {'#PCDATA', 'char'};
  
makeTypeRef(Qname = #qname{uri = NS, localPart = Local}, Namespaces) ->
  TypePrefix = case get(erlsom_typePrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  case NS of
    "http://www.w3.org/2001/XMLSchema" ->
      {'#PCDATA', translateType(Local)};
    _Else ->
      makeRef(Qname, Namespaces, TypePrefix)
  end.

makeElementRef(Qname, Namespaces) ->
  %% ElementPrefix = case get(erlsom_elementPrefix) of
                 %% undefined -> "";
                 %% Value -> Value
               %% end,
  makeRef(Qname, Namespaces, "").

makeGroupRef(Qname, Namespaces) ->
  GroupPrefix = case get(erlsom_groupPrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  makeRef(Qname, Namespaces, GroupPrefix).

makeAttrRef(QName, Namespaces) ->
  makeRef(QName, Namespaces, "").

makeTagFromRef(QName, Namespaces) ->
  makeRef(QName, Namespaces, "").

%% ExtraPrefix is the additional prefix to distinguish 
%% types, groups and elements
makeRef(#qname{uri = NS, localPart = Local}, Namespaces, ExtraPrefix) ->
  case lists:keysearch(NS, 2, Namespaces) of
    {value, #ns{prefix = undefined}} ->
      ExtraPrefix ++ Local;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ ExtraPrefix ++ Local;
    _ ->
      if 
        NS == undefined ->
          ExtraPrefix ++ Local;
        true ->
          case {NS, Local} of
            %% weird cases
            {"http://www.w3.org/XML/1998/namespace", "lang"} -> "xml:lang";
            {"http://www.w3.org/XML/1998/namespace", "space"} -> "xml:space";
            _ -> 
              throw({error, "Namespace not found " ++ NS})
          end
      end
  end.


makeTag(NameInXsd, Prefix, _ElementFormDefault = "qualified") ->
      Prefix ++ NameInXsd;
makeTag(NameInXsd, _Prefix, _ElementFormDefault) ->
      NameInXsd.

makeTag(NameInXsd, #schemaInfo{targetNamespace=undefined}) ->
  NameInXsd;

makeTag(NameInXsd, #schemaInfo{elementFormDefault="qualified", targetNamespace=TNS, namespaces=NS}) ->
  %% find the target namespace in NS,
  %% add the prefix.

  case lists:keysearch(TNS, 2, NS) of
    {value, #ns{prefix = undefined}} ->
      NameInXsd;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ NameInXsd;
    _Else ->
      if 
        TNS == undefined ->
          NameInXsd;
        true ->
          throw({error, "Namespace not found " ++ TNS})
      end
  end;

makeTag(NameInXsd, _SchemaInfo) ->
  %% defaultElementForm = unqualified, do not add the prefix (TODO: exept for global elements and types).
  NameInXsd.

listLength(undefined) -> 0;
listLength(List) -> length(List).

toUnicode(Bin) ->
  autodetect(Bin).

findType(TypeReference, Types, Attributes, TypeHierarchy, Namespaces, NamespaceMapping) ->
  case lists:keysearch(TypeReference, #type.nm, Types) of
    {value, Value} ->
      case findXsiType(Attributes) of
        {value, XsiType} ->
          findDerivedType(TypeReference, XsiType, Types, TypeHierarchy, Namespaces, NamespaceMapping);
        _ -> Value
      end;
    _Else ->
      throw({error, "Type not found: " ++ atom_to_list(TypeReference)})
  end.

findXsiType([]) -> false;
findXsiType([#attribute{localName= "type", 
                        uri = "http://www.w3.org/2001/XMLSchema-instance", 
                        value = Value}| _Tail]) -> {value, Value};
findXsiType([_| Tail]) -> 
  findXsiType(Tail).

findDerivedType(Type, XsiType, Types, TypeHierarchy, Namespaces, NamespaceMapping) ->
  #qname{localPart = LocalName, mappedPrefix = MappedPrefix} = 
    convertPCData(XsiType, qname, Namespaces, NamespaceMapping),
  XsiTypeMapped = list_to_atom(makeTypeName(LocalName, case MappedPrefix of undefined -> ""; _ -> MappedPrefix ++ ":" end)),
  case isAncestor(Type, XsiTypeMapped, TypeHierarchy) of
    false ->
      throw({error, "Type not found in hierarchy: " ++ atom_to_list(Type)});
    _ ->
      case lists:keysearch(XsiTypeMapped, #type.nm, Types) of
        {value, Value} ->
          Value;
        _ ->
          throw({error, "Derived type not found: " ++ atom_to_list(Type)})
      end
  end.
      
autodetect(Input) ->
  case detectEncoding(Input) of
    ucs4be ->
      xmerl_ucs:from_ucs4be(Input);
    ucs4le ->
      xmerl_ucs:from_ucs4le(Input);
    utf16be ->
      {Result, []} = erlsom_ucs:from_utf16be(Input),
      Result;
    utf16le ->
      {Result, []} = erlsom_ucs:from_utf16le(Input),
      Result;
    utf8 ->
      {Result, []} = erlsom_ucs:from_utf8(Input),
      Result;
    iso_8859_1 ->
      binary_to_list(Input)
  end.
    

%% CFun = {ContinuationFunction, ContinuationState} (or undefined)
detectEncoding(Xml, _CFun, CState) when is_list(Xml) ->
  {list, Xml, CState};
detectEncoding(Xml, CFun, CState) when is_binary(Xml) ->
  case Xml of 
    <<>> ->
      case CFun(Xml, CState) of 
        {<<>>, _} ->
          throw({error, "empty document"});
        {Xml2, State2} ->
          {detectEncoding(Xml2), Xml2, State2}
      end;
    _ -> 
      {detectEncoding(Xml), Xml, CState}
  end.

%%------------------------------------------------------------------------------
%% This was copied from xmerl_lib (by Ulf Wiger), but modified to work
%% on binaries in stead of lists. I also removed the option to specify
%% a character set - the function only looks at the first 2 or 4 bytes.
%% Finally, I changed it to not remove the byte order mark (since 
%% erlsom has no problem with the byte order mark).
%% 
%% Auto detect what kind of character set we are dealing with and transform
%% to Erlang integer Unicode format if found.
%% Appendix F, Page 56-57, XML 1.0 W3C Recommendation 6 October 2000
%% (http://www.w3.org/TR/REC-xml)
%% 00 00 00 3C ( "<" in UCS-4 big-endian)
%% 3C 00 00 00 ( "<" in UCS-4 little-endian)
%% FE FF (UTF-16 - big-endian Mark)
%% FF FE (UTF-16 - little-endian Mark)
%% 00 3C 00 3F ( "<?" in UTF-16 big-endian)
%% 3C 00 3F 00 ( "<?" in UTF-16 big-endian)
%% 3C 3F (7-bit,8-bit or mixed width encoding)
%% 4C 6F A7 94 (EBCDIC) - Not Implemented!!!!

%% Check byte-order mark and transform to Unicode, Erlang integer
%%% --- With byte-order mark
detect_encoding(List) 
  when is_list(List) ->
  detect_encoding(list_to_binary(List));
detect_encoding(Xml) ->
  {detectEncoding(Xml), Xml}.


%% This is the internal version
%% the version detect_encoding() is there for reasons of backward 
%% compatibility
detectEncoding(<<0,0,16#fe,16#ff, _Rest/binary>>) ->
  ucs4be;
detectEncoding(<<16#ff,16#fe,0,0, _Rest/binary>>) ->
  ucs4le;
detectEncoding(<<16#fe,16#ff, _Rest/binary>>) ->
  utf16be;
detectEncoding(<<16#ff,16#fe, _Rest/binary>>) ->
  utf16le;

detectEncoding(<<16#ef,16#bb,16#bf, _Rest/binary>>) ->
  utf8;

%%% --- Without byte-order mark
detectEncoding(<<0,0,0,16#3c, _Rest/binary>>) ->
  ucs4be;
detectEncoding(<<16#3c,0,0,0, _Rest/binary>>) ->
  ucs4le;

detectEncoding(<<0,16#3c,0,16#3f, _Rest/binary>>) ->
  utf16be;
detectEncoding(<<16#3c,0,16#3f,0, _Rest/binary>>) ->
  utf16le;

%% based on a suggestion by Tobbe:
detectEncoding(<<$<,$?,$x,$m,$l, _Rest/binary>> = Content) ->
  detect_encoding2(Content);

detectEncoding(_Input) ->
  %% debug("unknown encoding? Assume UTF-8"),
  utf8.

detect_encoding2(<<First100:100/binary, _Rest/binary>>) ->
  Variables = parse_prolog(binary_to_list(First100)),
  detect_encoding3(Variables);

detect_encoding2(Content) -> %% < 100 characters in the XML
  Variables = parse_prolog(binary_to_list(Content)),
  detect_encoding3(Variables).

detect_encoding3(Variables) ->
  case lists:keysearch("encoding", 1, Variables) of
    {value, {_, Encoding}} ->
      case encoding_type(Encoding) of
        'utf-8' -> 
          utf8;
        'iso-8859-1' -> 
          iso_8859_1;
        _ -> throw({error, "Encoding " ++ Encoding ++ " not supported"})
      end;
    _ -> 
      utf8
  end.

encoding_type(Cs) when is_list(Cs) -> 
   case to_lower(Cs) of
       "iso-8859-1" -> 'iso-8859-1';
       "iso_8859_1" -> 'iso-8859-1';
       "iso_8859-1" -> 'iso-8859-1';
       "iso8859-1"  -> 'iso-8859-1';
       "utf-8"      -> 'utf-8';
       "utf_8"      -> 'utf-8';
       _            -> false 
   end.

to_lower(Str) when is_list(Str)   -> [to_lower(C) || C <- Str];
to_lower(C) when C >= $A, C =< $Z -> C+($a-$A);
to_lower(C)                       -> C.
%% end of the code based on Tobbes code 

%% some code to parse the XML prolog.
%% returns a list of tuples {variable, value}
%% <?xml version = '1.0' encoding="utf-9" ?>
%% gives [{"version", "1.0"}, {"encoding", "utf-9"}]

parse_prolog("<?xml" ++ Tail) ->
  parse_variables(Tail, []);

parse_prolog(_) -> [].

parse_variables([], _Acc) ->
  %% error, but never mind
  [];

parse_variables("?>" ++ _, Acc) -> Acc;

parse_variables(T = [X | Tail], Acc) ->
  case typeOfMarkupChar(X) of
    namestartchar -> 
      parse_variable(T, [], Acc);
    whitespace -> 
      parse_variables(Tail, Acc);
    _ -> []
  end.

parse_variable([X| Tail], NameAcc, Acc) ->
  case typeOfNameChar(X) of
    namechar -> parse_variable(Tail, [X | NameAcc], Acc);
    whitespace -> parse_variable_is(Tail, lists:reverse(NameAcc), Acc);
    equalsign -> parse_to_quote(Tail, lists:reverse(NameAcc), Acc);
    _ -> []
  end.

parse_variable_is([X | Tail], Name, Acc) ->
  case typeOfNameChar(X) of
    whitespace -> parse_variable_is(Tail, Name, Acc);
    equalsign -> parse_to_quote(Tail, Name, Acc);
    _ -> []
  end.

parse_to_quote([X|Tail], Name, Acc) ->
  case typeOfMarkupChar(X) of
    whitespace -> parse_to_quote(Tail, Name, Acc);
    quote -> parse_value(Tail, [], Name, X, Acc);
    _ -> []
  end.

parse_value([Quote|Tail], ValueAcc, Name, Quote, Acc) ->
  parse_variables(Tail, [{Name, lists:reverse(ValueAcc)} | Acc]);
parse_value([X|Tail], ValueAcc, Name, Quote, Acc) ->
  parse_value(Tail, [X | ValueAcc], Name, Quote, Acc);
parse_value([], _ValueAcc, _Name, _Quote, _Acc) ->
  [].


findFile(Namespace, Location, IncludeFiles, IncludeDirs) ->
  case lists:keysearch(Namespace, 1, IncludeFiles) of
    {value, {_, Prefix, Schema = #schemaType{}}} ->
      {Schema, Prefix};
    {value, {_, Prefix, undefined}} ->
      {getFile(Location, IncludeDirs), Prefix};
    {value, {_, Prefix, FileName}} ->
      {getFile(FileName, IncludeDirs), Prefix};
    _ ->
      {getFile(Location, IncludeDirs), undefined}
  end.

getFile(Location, IncludeDirs) -> 
  case filelib:is_file(Location) of
    true -> 
      readImportFile(Location);
    _ -> 
      %% debug(IncludeDirs),
      findFileInDirs(Location, IncludeDirs)
  end.



findFileInDirs(undefined, []) ->
  throw({error, "Include file not found (undefined)"});
findFileInDirs(Location, []) ->
  throw({error, "Include file not found " ++ Location});

findFileInDirs(Location, [H | T]) ->
  Name = filename:join([H, Location]),
  case filelib:is_file(Name) of
    true -> 
      readImportFile(Name);
    _ -> 
      findFileInDirs(Location, T)
  end.

readImportFile(Name) ->
  case file:read_file(Name) of
    {ok, Bin} ->
      toUnicode(Bin);
    Error ->
      throw({error, 
            lists:flatten(io_lib:format("Error reading include file ~p - ~p", 
                                        [Name, Error]))})
  end.

%% Include_fun is a function that finds the files that are included or imported in
%% the XSD. It should be a function that takes 4 arguments: 
%%        - Namespace (from the XSD). This is a string or 'undefined'
%%        - SchemaLocation (from the XSD). This is a string or 'undefined'
%%        - Dir_list. This is the value of the Dir_list option if this option
%%          was passed to compile_xsd(); 'undefined' otherwise.
%%        - Inlcude_list. This is the value of the Include_list option if this
%%          option was passed to compile_xsd(); 'undefined' otherwise.
%%
%% Include_fun should return {XSD, Prefix}, where XSD is a XSD = string(), Prefix
%% = string or 'undefined', see above.
find_xsd(Namespace, Location, Dir_list, Include_list) ->
  case get_url(Location) of
    {ok, Body} ->
      Prefix = prefix(Namespace),
      {Body, Prefix};
    _ -> 
       erlsom_lib:findFile(Namespace, Location, Dir_list, Include_list)
  end.
 
prefix(Namespace) ->
  Tokens = string:tokens(Namespace, "/ "),
  string:substr(lists:last(Tokens), 1, 5).


%%% --------------------------------------------------------------------
%%% Get a file from an URL spec.
%%% function created by Tobbe, copied from yaws_soap_lib
%%% slightly modified
%%% --------------------------------------------------------------------
get_url("http://"++_ = URL) ->
    case httpc:request(URL) of
	{ok,{{_HTTP,200,_OK}, _Headers, Body}} -> 
	    case http_uri:parse(URL) of
		{_Method, _, _Host, _Port, _Path, _Qargs} ->
		    {ok, Body};
		_ -> 
		    {error, "failed to retrieve: "++URL}
	    end;
	_Error -> 
	    {error, "failed to retrieve: "++URL}
    end;
get_url(_) ->
    {error, "not an URL"}.
  
emptyListIfUndefined(undefined) -> [];
emptyListIfUndefined(List) -> List.

%% Note: initially there can be more than 1 element in the list with the same name (an element and a type).
%% We are looking for the type.
searchBase(_Name, []) ->
  not_found;
searchBase(Name, [H = #typeInfo{typeName = Name, typeType = TypeT} | _])
  when TypeT /= globalElementRefOnly ->
  {value, H};
searchBase(Name, [_H | T]) ->
  searchBase(Name, T).

%% Context can be:
%%  name, markup
%% Returns:
%%  whitespace, lessthan, morethan, etc, see below.
typeOfNameChar(Char) ->
  if 
    Char > 96 ->
      if 
        Char < 123 -> namechar;
	Char < 127 -> char;
	true -> illegal
      end;
    Char > 64 ->
      if 
        Char < 91 -> namechar;
        Char == 95 -> namechar;
	true -> char
      end;
    Char > 47 ->
      if
        Char < 59 -> namechar;
	Char == 61 -> equalsign;
	Char == 62 -> morethan;
	Char == 63 -> questionmark;
	true -> char
      end;
    Char > 32 ->
      if 
        Char < 40 -> char;
        Char < 45 -> illegal;
	Char == 47 -> slash;
	true -> namechar
      end;
    Char == 32 -> whitespace;
    Char == 9 -> whitespace;
    Char == 10 -> whitespace;
    Char == 13 -> whitespace;
    true -> illegal
  end.

%% Context can be:
%%  name, markup
%% Returns:
%%  whitespace, lessthan, morethan, etc, see below.
typeOfMarkupChar(Char) ->
  if 
    Char == 32 -> whitespace;
    Char > 96 ->
      if 
        Char < 123 -> namestartchar;
	Char < 127 -> char;
	true -> illegal
      end;
    Char > 64 ->
      if 
        Char < 91 -> namestartchar;
        Char == 95 -> namestartchar;
	true -> char
      end;
    Char > 40 ->
      if
        Char == 47 -> slash;
        Char < 60 -> char;
	Char == 60 -> lessthan;
	Char == 61 -> equalsign;
	Char == 62 -> morethan;
	true -> char
      end;
    Char > 32 ->
      if 
        Char == 39 -> quote;
        Char == 34 -> quote;
        Char == 38 -> ampersand;
	true -> char
      end;
    Char == 9 -> whitespace;
    Char == 10 -> whitespace;
    Char == 13 -> whitespace;
    true -> illegal
  end.

%%% hides the definition of #qname{}
makeQname(LocalName) ->
  #qname{localPart = LocalName}.

%%% hides the definition of #qname{}
localName(#qname{localPart = LocalName}) ->
  LocalName.

%%% hides the definition of #schemaType{}
getTargetNamespaceFromXsd(#schemaType{targetNamespace = TNS}) ->
  TNS.

%% this is a hack, see erlsom_compile
%% remove the type-prefix from the name
removePrefixes(Name) ->
  {Prefix, LocalName} = splitOnColon(Name),
  %% now remove the typePrefix (if it exists)
  TypePrefix = case get(erlsom_typePrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  {_, WithoutTypePrefix} = lists:split(length(TypePrefix), LocalName), 
  case Prefix of
    [] -> WithoutTypePrefix;
    _ -> Prefix ++ ":" ++ WithoutTypePrefix
  end.


  
%% returns {Prefix, LocalName}
splitOnColon(Text) ->
  PosOfColon = string:chr(Text, $:),
  if 
    PosOfColon == 0 ->
      {[], Text};
    true ->
      {string:substr(Text, 1, PosOfColon - 1), string:substr(Text, PosOfColon + 1)}
  end.
