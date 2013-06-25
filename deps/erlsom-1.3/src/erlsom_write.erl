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
%%% translates a data structure of a pre-defined form to an XML document.
%%% ====================================================================

%%% This is the companion of erlsom_parse, which performs the inverse operation.
%%% Both modules use the same 'model' that descibes the translation, see the
%%% introduction to erlsom_parse for the definition of this model.

-module(erlsom_write).
-export([write/2]).

-include_lib("erlsom_parse.hrl").
-include_lib("erlsom.hrl").

%% debug(Text) ->
  %% io:format("write: ~p\n", [Text]).


%% Returns the XML document. {ok, Document}
write(Struct, Model = #model{tps = Types}) ->

  %% start with _document type.
  case lists:keysearch('_document', 2, Types) of
    {value, #type{els = [Head | _Tail], mxd = Mixed}} ->
      CurrentValue = Struct,
      ResultWithThisElement = processElementValues([CurrentValue], Head, [], 0, Model, {[], 0}, Mixed),
      %% debug(ResultWithThisElement);
      {ok, lists:flatten(ResultWithThisElement)};
    _Else ->
      {error, "Model should have _document type"}
  end.

struct2xml(_Struct, [], ResultSoFar, _Model, _Namespaces, _Mixed) ->
  lists:flatten(lists:reverse(ResultSoFar));

%% Struct = {RecordType, value, ...}
%% Processes whatever is INSIDE the tags (= the values of the struct), one by one, from the first
struct2xml(Struct,
           _StructModel = [ModelForThisElement = #el{alts = Alternatives, mn = Min, mx = Max, nr = SequenceNr} |
                           NextElements],
           ResultSoFar, Model = #model{nss = Namespaces, th = Th}, DeclaredNamespaces,
           Mixed) ->

  %% We are dealing with element nr given by SequnceNr + 2
  %% (n+2 because the 1st is the type name and the 2nd is the list of 'anyAttributes')
  %% Which alternative has been selected follows from the value of this element
  CurrentValue = element(SequenceNr + 2, Struct),
  %% value = tuple => subtype
  %% value = list of chars (integers) => text
  %% value = list (not string) of:
  %%              tuples => subtypes, element has maxOccurs > 1 OR alternative has maxOccurs > 1
  %%              strings =>
  %%              lists => element has maxOccurs > 1 AND alternative has maxOccurs > 1
  %% value == undefined -> no value provided
  %% etc.

  %% debug(CurrentValue),

  if
    (Max == 1) and (Mixed /= true) ->
      case CurrentValue of
        undefined ->
          if
            Min > 0  -> throw({error, "No value provided for non-optional element", SequenceNr + 2, element(1, Struct)});
	    true -> true
          end,
          ResultForThisElement = [];
        [V1 | _] ->
          case V1 of
            _ when is_integer(V1) -> %% CurrentValue is a string
              ResultForThisElement = printValue(CurrentValue, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
            _ when is_tuple(V1) ->
              %% debug("alternative with MaxOccurs > 1"),
              ResultForThisElement = processAlternatives(CurrentValue, Alternatives, Model, DeclaredNamespaces, Th,
                                                         Mixed)
          end;
        #qname{} ->
          ResultForThisElement = printValue(CurrentValue, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
        _ when is_tuple(CurrentValue) ->
          %% debug("subtype"),
          ResultForThisElement =
                 processElementValues([CurrentValue], ModelForThisElement, [], 0, Model, DeclaredNamespaces, Mixed);
        _ when is_binary(CurrentValue) ->
          ResultForThisElement = printValue(CurrentValue, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
        _ ->
          %% debug("simple type"),
          ResultForThisElement = printValue(CurrentValue, Alternatives, Namespaces, DeclaredNamespaces, Mixed)
      end;
    true -> %% CurrentValue is a list, because Element has maxOccurs > 1.
      if
        is_list(CurrentValue); CurrentValue == undefined -> true;
        true -> throw({error, "value has to be a list"})
      end,
      if
        CurrentValue == undefined ->
          ResultForThisElement = [];
        true ->
          ResultForThisElement = processElementValues(CurrentValue, ModelForThisElement, [], 0, Model,
                                                      DeclaredNamespaces, Mixed)
      end
  end,

  %% process remaining elements
  struct2xml(Struct, NextElements, [ResultForThisElement | ResultSoFar], Model, DeclaredNamespaces, Mixed).


processElementValues([],
                     _ModelForThisElement = #el{mn = Min},
                     ResultSoFar, Counter, _Model, _DeclaredNamespaces,
                     _Mixed) ->
  if
    Counter < Min ->
      throw({error, "Not enough values provided"});
    true ->
      lists:flatten(lists:reverse(ResultSoFar))
  end;

%% ElemnentValues can be:
%%
%% FirstElement can be:
%% - a tuple
%%    - for a value
%% - a string (list)
%%    - for a value
%% - a list of tuples:
%%    - for a sequence (or all, no need to distinguish) where the element has more values
%%    - for a choice where the selected alternative has maxOccurs > 1
%% - a list of lists
%%    - for a choice with maxOccurs > 1 and an alternative with maxOccurs > 1
%%    - it could in theory be a list of lists of lists (etc.)? But not now, since choice in choice is
%%      not supported.
processElementValues([V1 | NextValues],
               ModelForThisElement = #el{alts = Alternatives, mx = Max},
               ResultSoFar, Counter, Model = #model{nss = Namespaces, th = Th}, DeclaredNamespaces, Mixed) ->

  %% debug("procesElementValues, counter = " ++ integer_to_list(Counter)),
  {Case, IncreaseCounter} =
    case V1 of
      [] -> %% "", string of 0 characters
        {listOfStrings, 1};
      _ when is_list(V1) ->
        V11 = hd(V1),
        case V11 of
          #qname{} ->
            throw({error, "wrong type in value"});
          _ when is_tuple(V11) ->
            {listOfTuples, 1};
          _ when is_integer(V11) ->
            if
              Mixed == true ->
                {mixed, 0};
              true ->
                %% debug("element w. MaxOccurs > 1; 1st value is a string"),
                {listOfStrings, 1}
              end;
          _ ->
            throw({error, "wrong type in value"})
        end;
      _ when is_binary(V1) ->
         if
           Mixed == true ->
             {mixed, 0};
           true ->
             %% debug("element w. MaxOccurs > 1; 1st value is a string"),
             {listOfStrings, 1}
         end;
      #qname{} ->
        %% debug("element w. MaxOccurs > 1, (1st value is a qname)."),
        {qname, 1};
      _ when is_tuple(V1) ->
        %% debug("element w. MaxOccurs > 1, (1st value is a subtype)"),
        {tuple, 1};
      _ ->
        %% debug("element w. MaxOccurs > 1, (1st value is a simple type)"),
        {simpleType, 1}
    end,
  if
    Counter + IncreaseCounter > Max ->    %% if Max == unbound the result of the test is false!
      throw({error, "Too many values provided"});
    true -> true
  end,
  ResultWithThisElement =
    case Case of
      listOfTuples ->
        ResultForThisElement = processAlternatives(V1, Alternatives, Model, DeclaredNamespaces, Th, Mixed),
        [ResultForThisElement | ResultSoFar];
      mixed ->
        [xmlString(V1) | ResultSoFar];
      listOfStrings ->
        ResultForThisElement = printValue(V1, Alternatives, Namespaces, DeclaredNamespaces, Mixed),
        [ResultForThisElement | ResultSoFar];
      qname ->
        ResultForThisElement = printValue(V1, Alternatives, Namespaces, DeclaredNamespaces, Mixed),
        [ResultForThisElement | ResultSoFar];
      tuple ->
        ResultForThisElement = processSubType(V1, Alternatives, Model, DeclaredNamespaces, Mixed),
        [ResultForThisElement | ResultSoFar];
      simpleType ->
        ResultForThisElement = printValue(V1, Alternatives, Namespaces, DeclaredNamespaces, Mixed),
        [ResultForThisElement | ResultSoFar]
    end,

  processElementValues(NextValues, ModelForThisElement, ResultWithThisElement, Counter + IncreaseCounter,
                       Model, DeclaredNamespaces, Mixed).

%% returns a string that represents the value
processSubType(Value, Alternatives, Model = #model{tps = Types, th = TypeHierarchy}, DeclaredNamespaces,
               Mixed) ->
  %% RecordType can be an instabtiated abstract type
  RecordType = element(1, Value),
  {Alternative, Abstract} = findAlternative(RecordType, Alternatives, TypeHierarchy),
  TypeRecord = findType(RecordType, Types),
  processAlternativeValue(Value, 1, Alternative, TypeRecord, Model, DeclaredNamespaces, Abstract, Mixed).

processAlternatives(Values = [Value | _], Alternatives, Model = #model{tps = Types},  DeclaredNamespaces, TypeHierarchy,
                    Mixed) ->
  %% See which alternative this is
  RecordType = element(1, Value),
  {Alternative, Abstract} = findAlternative(RecordType, Alternatives, TypeHierarchy),
  TypeRecord = findType(RecordType, Types),
  processAlternativeValues(Values, 0, Alternative, TypeRecord, Model, DeclaredNamespaces, Abstract, [], Mixed).

processAlternativeValues([], Count, #alt{mn = Min}, _Type, _Model, _Ns, _Abstract, Acc, _Mixed) ->
  if
    Count < Min -> throw({error, "not enough values"});
    true -> lists:reverse(Acc)
  end;

processAlternativeValues([V1 | Tail], Count, Alternative, Type, Model, Ns, Abstract, Acc, Mixed) ->
  processAlternativeValues(Tail, Count + 1, Alternative, Type, Model, Ns, Abstract,
      [processAlternativeValue(V1, Count, Alternative, Type, Model, Ns, Abstract, Mixed) | Acc], Mixed).

processAlternativeValue(Value, Count,
                        #alt{tag = Tag, rl = RealElement, mx = MaxAlt},
                        #type{els = Elements, atts = Attributes, typeName = Name, mxd = MixedChild},
                        Model = #model{nss = Namespaces},
                        DeclaredNamespaces,
                        Abstract,
                        MixedParent) ->

  Mixed =
    case MixedParent of
      true ->
        case RealElement of
          true -> MixedChild;
          simple -> MixedChild;
          _ -> MixedParent %% 'non-real' elements (groups) inherit this property
        end;
      _ -> MixedChild
    end,

  if
   Count > MaxAlt -> throw({error, "too many values"});
   true -> true
  end,

  if
    RealElement ->
      %% proces the attributes
      {AttributesString, NewDeclaredNamespaces} = processAttributes(Value, [], Attributes, Namespaces, DeclaredNamespaces),
      %% process anyAttributes
      %% for now we don't check whether 'anyAttributes' are allowed!
      {AnyAttributesString, DeclaredNamespaces2} = processAnyAttributes(element(2, Value), [],
                                                      Namespaces, NewDeclaredNamespaces),
      {AnyAttrPlusXsiTypeString, DeclaredNamespaces3} =
        case Abstract of
          false -> {AnyAttributesString, DeclaredNamespaces2};
          _ ->
            XsiType = " xsi:type=\"" ++ atom_to_list(Name) ++ "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
            {AnyAttributesString ++ XsiType, DeclaredNamespaces2}
     end,

      %% deal with namespaces (that is, see if they have to be declared here)
      TagAsText = atom_to_list(Tag),
      {NamespacesString, NewDeclaredNamespaces4} = processNamespaces(TagAsText, Namespaces, DeclaredNamespaces3),
      %% print startTag
      StartTag = "<" ++ TagAsText ++ NamespacesString ++ AttributesString ++ AnyAttrPlusXsiTypeString ++ ">";
    true ->
      StartTag = [],
      NewDeclaredNamespaces4 = DeclaredNamespaces
  end,

  if
    RealElement ->
      %% print end tag
      EndTag = "</" ++ atom_to_list(Tag) ++ ">";
    true ->
      EndTag = []
  end,

  %% process sub-elements - recursively!
  ResultForThisElement = struct2xml(Value, Elements, [], Model, NewDeclaredNamespaces4, Mixed),
  [StartTag | [ResultForThisElement | EndTag]].


findType(TypeName, Types) ->
  case lists:keysearch(TypeName, #type.nm, Types) of
    {value, Type} -> Type;
    _ -> throw({error, "Something wrong with the Model"})
  end.

findAlternative(RecordType, Alternatives, TypeHierarchy) ->
  findAlternative(RecordType, Alternatives, TypeHierarchy, false).

findAlternative(RecordType, Alternatives, TypeHierarchy, Abstract) ->
  case lists:keysearch(RecordType, #alt.tp, Alternatives) of
    {value, Alternative} -> {Alternative, Abstract};
    _ ->
        %% see whether an ancestor in the type hierarchy is among the alternatives
        case erlsom_lib:getAncestor(RecordType, TypeHierarchy) of
          {value, Ancestor} ->
            findAlternative(Ancestor, Alternatives, TypeHierarchy, true);
          _ ->
            throw({error, "Struct doesn't match model: recordtype not expected: " ++ atom_to_list(RecordType)})
        end
  end.


%% Attribute is a tuple {Name, SequenceNr, Optional, Type}
processAttributes(_Struct, ResultSoFar, [], _Namespaces, DeclaredNamespaces) ->
  {ResultSoFar, DeclaredNamespaces};

processAttributes(Struct, ResultSoFar, [#att{nm = Name,
                                             nr = SequenceNr,
                                             opt = Optional,
                                             tp = Type} | Rest], Namespaces, DeclaredNamespaces) ->
  NameAsString = atom_to_list(Name),
  {NamespacesString, NewDeclaredNamespaces} = processNamespaces(NameAsString, Namespaces, DeclaredNamespaces),
  AttributeValue = element(SequenceNr + 2, Struct),
  case AttributeValue of
    undefined ->
      if
        Optional ->
          processAttributes(Struct, ResultSoFar, Rest, Namespaces, DeclaredNamespaces);
	true ->
          throw({error, "No value provided for mandatory attribute " ++ atom_to_list(Name)})
      end;
    _Defined ->
      case Type of

        char ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = xmlString(AttributeValue);
        integer ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = try integer_to_list(AttributeValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type in attribute  " ++ atom_to_list(Name) ++ ", expected Integer"})
          end;
        bool ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = case AttributeValue of
                        true -> "true";
                        false -> "false";
                        _ -> throw({error, "Wrong Type in attribute  " ++ atom_to_list(Name) ++ ", expected boolean"})
                      end;
        float ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = try float_to_list(AttributeValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type in attribute  " ++ atom_to_list(Name) ++ ", expected Float"})
          end;
        atom ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = try atom_to_list(AttributeValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type in attribute  " ++ atom_to_list(Name) ++ ", expected Atom"})
          end;
        qname ->
          writeQnameAttValue(AttributeValue, NamespacesString, Namespaces, NewDeclaredNamespaces),
          {CharValue, NamespacesString2, DeclaredNamespaces2} =
             try writeQnameAttValue(AttributeValue, NamespacesString, Namespaces, NewDeclaredNamespaces)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type in attribute " ++ atom_to_list(Name) ++ ", expected qname"})
          end;
        _Else ->
          throw({error, "unknown type in model"}),
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = []
      end,
      ResultWithThisAttribute = ResultSoFar ++ NamespacesString2 ++ " " ++ NameAsString ++ "=\"" ++ CharValue ++ "\" ",
      processAttributes(Struct, ResultWithThisAttribute, Rest, Namespaces, DeclaredNamespaces2)
  end.

%% returns:
%% {AttributeValue, NamespacesString, NewDeclaredNamespaces}
%% -record(qname, {uri, localPart, prefix, mappedPrefix}).
writeQnameAttValue(#qname{uri = Uri, localPart = LP, mappedPrefix = MP}, NamespacesString, Namespaces,
                   DeclaredNamespaces = {NamespacesList, Counter}) ->
  case Uri of
    [] ->
      {LP, NamespacesString, DeclaredNamespaces};
    _ ->
      %% see whether the namespace has been defined. If not, then this has to be done.
      case lists:keysearch(Uri, 2, NamespacesList) of
        {value, {Prefix, _Uri}} -> %% already declared
          {Prefix ++ ":" ++ LP, NamespacesString, DeclaredNamespaces};
        _ ->
          %% see whether a prefix was specified
          case lists:keysearch(Uri, #ns.uri, Namespaces) of
            {value, #ns{prefix = Prefix2}} ->
              {Prefix2 ++ ":" ++ LP, NamespacesString ++ " xmlns:" ++  Prefix2 ++ "=\"" ++ Uri ++ "\"",
               {[{Prefix2, Uri} | NamespacesList], Counter}};
            _ ->
              {MP ++ ":" ++ LP, NamespacesString ++ " xmlns:" ++  MP ++ "=\"" ++ Uri ++ "\"",
               {[{MP, Uri} | NamespacesList], Counter}}
          end
      end
  end.


processAnyAttributes(undefined, Acc, _Namespaces, DeclaredNamespaces) ->
  {Acc, DeclaredNamespaces};
processAnyAttributes([], Acc, _Namespaces, DeclaredNamespaces) ->
  {Acc, DeclaredNamespaces};
processAnyAttributes([{{Name, Uri}, Value} | Tail], Acc, Namespaces, DeclaredNamespaces) ->
  case Uri of
    [] ->
      processAnyAttributes(Tail, Acc ++ " " ++ Name ++ "=\"" ++ decodeIfRequired(Value) ++ "\"",
        Namespaces, DeclaredNamespaces);
    _Other ->
     %% get prefix +, if relevant, NS declaration text
     {PrefixedName, DeclaredNamespaces2} = processAnyNamespaces(Name, Uri, Namespaces, DeclaredNamespaces),
     processAnyAttributes(Tail, Acc ++ " " ++ PrefixedName ++ "=\"" ++ decodeIfRequired(Value) ++ "\"",
       Namespaces, DeclaredNamespaces2)
  end.


%% see if the tag references a namespace. If so, see whether
%% this namespace has been declared.
%%
%% Namespaces is of the form {Prefix, URI}
%% DeclaredNamespaces = [Prefix]
%%
%% returns {NameSpacesString, NewDeclaredNamespaces}, where
%% NamespacesString is the declaration (if required), and
%% NewDeclaredNamespaces is the new list of declared
%% namespaces.
processNamespaces(Tag, Namespaces, DeclaredNamespaces = {NamespacesList, Counter}) ->
  %% look for ':' in the tag
  %% debug(Tag),
  %% debug(DeclaredNamespaces),
  Prefix = case string:tokens(Tag, ":") of
             [Pf, _LName] ->
               Pf;
             [_LName] ->
               undefined;
             _ ->
               throw({error, "Tag " ++ Tag ++ " is not of form [prefix:]localName"})
           end,

  %% declaredNamespaces = [{Prefix, Uri}]
  case lists:keysearch(Prefix, 1, NamespacesList) of
    {value, _} -> %% already declared
      {[], DeclaredNamespaces};
    _Else ->
      %% find prefix in Model
      case lists:keysearch(Prefix, 3, Namespaces) of
	{value, #ns{uri = Uri}} ->
	  Xmlns = case Prefix of
	           undefined -> " xmlns";
		   _ -> " xmlns:" ++ Prefix
		 end,
	  {Xmlns  ++ "=\"" ++ Uri ++ "\"", {[{Prefix, Uri} | NamespacesList], Counter}};
	_ ->
	  case Prefix of
	    undefined -> {[], DeclaredNamespaces};
	    "" -> {[], DeclaredNamespaces};
	    "xml" -> {[], DeclaredNamespaces};
	    _ -> throw({error, "Inconsistency in model: namespace is not declared - " ++ Prefix})
	  end
      end
  end.

%%processAnyNamespaces(Name, Uri, Namespaces, {NamespacesList, Counter} = DeclaredNamespaces ) ->
processAnyNamespaces(Name, Uri, Namespaces, {NamespacesList, Counter} = DeclaredNamespaces ) ->
  case lists:keysearch(Uri, 2, NamespacesList) of
    {value, {Prefix, _}} -> %% already declared
      {Prefix ++ ":" ++ Name, DeclaredNamespaces};
    _Else ->
      %% find Uri in Model
      case lists:keysearch(Uri, #ns.uri, Namespaces) of
        {value, #ns{prefix = ModelPrefix}} ->
          ThePrefix = ModelPrefix;
        _Else ->
          %% make up a prefix, using counter
          ThePrefix = "pre" ++ integer_to_list(Counter +1)
      end,
      {" xmlns:" ++  ThePrefix ++ "=\"" ++ Uri ++ "\" " ++ ThePrefix ++ ":" ++ Name ,
          {[{ThePrefix, Uri} | NamespacesList], Counter + 1}}
  end.

printValue(CurrentValue, Alternatives, Namespaces,
           DeclaredNamespaces = {NamespacesList, _Counter}, Mixed) ->
  case CurrentValue of
    #qname{localPart = LocalName, prefix = Prefix, uri = Uri} ->
      case lists:keysearch({'#PCDATA', qname}, #alt.tp, Alternatives) of
        {value, #alt{tag = Tag, rl = RealElement}} ->  %% Print Tags if RealElement
          %% see whether this namespace was already declared
          {PrintPrefix, NsDecl} = printPrefix(Uri, Prefix, NamespacesList, Namespaces),
          TextValue = case PrintPrefix of
                        undefined -> xmlString(LocalName);
                        _ -> xmlString(PrintPrefix ++ ":" ++ LocalName)
                      end,
	  printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces, NsDecl);
	_Else ->
          throw({error, "Type of value (qname) does not match model"})
      end;

    _B when is_list(CurrentValue); is_binary(CurrentValue) ->
      if
        Mixed == true ->
          xmlString(CurrentValue); %% Note: values for (non-mixed) elements have to be of the
                                   %% form {type, Value} within mixed types
        true ->
          case lists:keysearch({'#PCDATA', char}, #alt.tp, Alternatives) of
            {value, #alt{tag = Tag, rl = RealElement}} ->  %% Print Tags if RealElement
              TextValue = xmlString(CurrentValue),
	      printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces);
	    _Else ->
              throw({error, "Type of value (list) does not match model", CurrentValue})
          end
      end;

    _C when is_integer(CurrentValue) ->
      %% is an integer also a float?
      case lists:keysearch({'#PCDATA', integer}, #alt.tp, Alternatives) of
        {value, #alt{tag = Tag, rl = RealElement}} ->
          TextValue = try integer_to_list(CurrentValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type"})
          end,
	  printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces);
	_Else ->
          throw({error, "Type of value (integer) does not match model"})
      end;

    _D when is_float(CurrentValue) ->
      %% is an integer also a float?
      case lists:keysearch({'#PCDATA', float}, #alt.tp, Alternatives) of
        {value, #alt{tag = Tag, rl = RealElement}} ->
          TextValue = try float_to_list(CurrentValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type"})
          end,
	  printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces);
	_Else ->
          throw({error, "Type of value (float) does not match model"})
      end;

    _E when CurrentValue ==  true; CurrentValue == false ->
      %% is an integer also a float?
      case lists:keysearch({'#PCDATA', bool}, #alt.tp, Alternatives) of
        {value, #alt{tag = Tag, rl = RealElement}} ->
          TextValue = try atom_to_list(CurrentValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type"})
          end,
	  printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces);
	_Else ->
          throw({error, "Type of value (atom) does not match model"})
      end;

    _Else ->
      throw({error, "Type of value not valid for XML structure"})

  end.

printPrefix(undefined, Prefix, _NamespacesList, _Namespaces) ->
  {Prefix, []};

printPrefix(Uri, Prefix, NamespacesList, Namespaces) ->
  case lists:keysearch(Uri, 2, NamespacesList) of
    {value, {Prefix2, _Uri}} -> %% already declared
      {Prefix2, []};
    _ ->
      %% see whether a prefix was specified
      PrintedPrefix = case lists:keysearch(Uri, #ns.uri, Namespaces) of
                        {value, #ns{prefix = Prefix3}} ->
                          Prefix3;
                        _ ->
                          Prefix
                      end,
      Xmlns = case PrintedPrefix of
	        undefined -> " xmlns";
                _ -> " xmlns:" ++ Prefix
              end,
      {PrintedPrefix, Xmlns ++ "=\"" ++ Uri ++ "\""}
  end.

printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces, QnameNs) ->
  if
    RealElement ->
       TagAsText = atom_to_list(Tag),
       %% this function is only used in 'leaves' of the struct, so we don't need to store the
       %% new declared namespaces (since those would apply only to child-elements, of
       %% which there are none)
       {NamespacesString, _} = processNamespaces(TagAsText, Namespaces, DeclaredNamespaces),
       "<" ++ TagAsText ++ NamespacesString ++ QnameNs ++ ">" ++ TextValue ++ "</" ++ TagAsText ++ ">";
    true ->
       TextValue
  end.

printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces) ->
  printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces, []).

%% - fixed an error that Anton Fedorov pointed out.
%% TODO: Probably not the fastest way to do this
xmlString(String) when is_list(String) ->
  lists:map(fun(Char) -> escapeChar(Char) end, String);
xmlString(String) when is_binary(String) ->
  {String2, []} = erlsom_ucs:from_utf8(String),
  xmlString(String2).

escapeChar(38) -> "&amp;";
escapeChar(34) -> "&quot;";
escapeChar(60) -> "&lt;";
escapeChar(Char) -> Char.

decodeIfRequired(Text) when is_binary(Text) ->
  erlsom_ucs:decode_utf8(Text);
decodeIfRequired(Text) ->
  Text.
