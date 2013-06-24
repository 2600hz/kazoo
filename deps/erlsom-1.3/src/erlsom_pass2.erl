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
%%% A second (and 3rd..) pass translating XSD to model used by 
%%% erlsom_parse.
%%% ====================================================================

-module(erlsom_pass2).
-export([secondPass/2]).
-export([pass5/2]).
-export([pass0Types/2]).
-export([pass0Type/2]).
-export([pass0Elements/2]).
-export([pass0Alternatives/2]).
-export([pass0Alternatives/3]).
-import(erlsom_lib, [findType/6]). 
-include_lib("erlsom_compile.hrl").
-include_lib("erlsom.hrl").
-include_lib("erlsom_parse.hrl").
-define(MAX_PASSES, 7).

%% translate 'min' and 'max'
%% translate types
%% add sequence numbers and totals
%% add namespace element

%% typeInfo - the intermediate format.
%% global (true or false): we need to find out in the
%% end whether this type should be available as 'top level' element in the 
%% xml document.
%% -record(typeInfo, 
%%         {typeName, global, typeType, typeRef, elements, attributes}).
%% -record(elementInfo, {alternatives, min, max}).
%% -record(alternative, {tag, type, real, min, max}).
%% -record(attribute, {name, optional, type}).
%% -record(schemaInfo, {targetNamespace, elementFormDefault, namespacePrefix, namespaces, path=[]}).

%% debug(Text) -> io:format("pass2: ~p\n", [Text]).
%% debug(Text1, Text2) -> io:format("pass2: ~p - ~p\n", [Text1, Text2]).

%% debugTypes([]) -> true;
%% debugTypes([#type{nm = Name}| Tail]) -> 
  %% debug(Name),
  %% debugTypes(Tail).

secondPass(IntermediateStruct, 
           Info = #schemaInfo{namespaces=NS, 
                              targetNamespace = Tns}) ->
  Types0 = pass0(IntermediateStruct),
  {Types1, GlobalElements, TypeHierarchy} = translateTypes(Types0, 
                            [], [], Types0, Info, erlsom_lib:newTree()),
  %% Note: pass 4 is done before pass 3 (seems to be better).
  Types2 = pass3(Types1),
  Types3 = pass4(Types2, Info),
  DocType = make_Document(GlobalElements, [], Info),
  %% fiddle a bit more - replace refernces in the _document that point
  %% to unknown types by {#PCDATA, ...}, assuming that they point to 
  %% simple types that are no longer visible. Even if that assumption 
  %% would be wrong, it wouldn't have worked otherwise either, so it won't 
  %% break anything (but it fixes the error pointer out by Alexander Wingrad 
  %% for those cases).
  DocType2 = removeDeadRefsFromDoc(DocType, Types3), 
  DocType3 =  pass3Type(DocType2, Types2), %% this is a list
  Types5 = DocType3 ++ Types3,
  Types6 = pass5(Types5, Info),
  #model{tps = Types6, nss = NS, tns = Tns,
         th = TypeHierarchy}.

%% for substitution groups:
%% for each element X that is in a substitution group: 
%%   for each element (in any type) that has an alternative that refers to the head element
%%     add an alternative to that refers to X.
%%
%% procedure:
%% 
%% for each element S_E{name=Name, type = Type substitutionGroup = H}  (within a substitution group with head H)
%%   for each type T 
%%     for each element T_E
%%       if T_E contains an alternative that refers to S_E 
%%         add an alternative to T_E that refers to S_E:
%%         tag = Name,
%%         type = Type 
%%
pass0(Types) ->
  lists:foldl(fun pass0Types/2, Types, Types).

pass0Types(#typeInfo{substitutionGroup = Head}, Types)
  when Head == undefined ->
  Types;
pass0Types(TypeToReferTo = #typeInfo{}, Types) ->
  {NewTypes, _Acc} = lists:mapfoldl(fun pass0Type/2, TypeToReferTo, Types),
  NewTypes.

pass0Type(Type = #typeInfo{elements = Elements}, Acc)
  when Elements == undefined ->
  {Type, Acc};
pass0Type(Type = #typeInfo{elements = Elements}, TypeToReferTo) ->
  {NewElements, _Acc}  = lists:mapfoldl(fun pass0Elements/2, TypeToReferTo, Elements),
  {Type#typeInfo{elements = NewElements}, TypeToReferTo}.
  
pass0Elements(Element = #elementInfo{alternatives = Alternatives}, 
              TypeToReferTo = #typeInfo{}) ->
  {Element#elementInfo{alternatives = pass0Alternatives(Alternatives, 
                                                        TypeToReferTo)},
   TypeToReferTo}.

pass0Alternatives(Alternatives, TypeToReferTo) ->
  pass0Alternatives(Alternatives, TypeToReferTo, []).

pass0Alternatives([], _, Acc) ->
  Acc;
pass0Alternatives([Alternative = #alternative{type = Head} | Tail],
                  TypeToReferToPlusNs = #typeInfo{substitutionGroup = Head, typeName = Name, typeRef = Type}, 
                  Acc)
                  when Type /= undefined ->
  %% debug("add an element - ref only"),
  pass0Alternatives(Tail, TypeToReferToPlusNs, 
                    [Alternative | [Alternative#alternative{type = Type, tag = Name} | Acc]]);
pass0Alternatives([Alternative = #alternative{type = Head} | Tail],
                  TypeToReferToPlusNs = #typeInfo{substitutionGroup = Head, typeName = Name}, 
                  Acc) ->
  %% debug("add an element - type"),
  pass0Alternatives(Tail, TypeToReferToPlusNs, 
                    [Alternative | [Alternative#alternative{type = Name, tag = Name} | Acc]]);
pass0Alternatives([Head | Tail], TypeToReferTo, Acc) ->
  pass0Alternatives(Tail, TypeToReferTo, [Head | Acc]).
  
%% This also creates the type-hierarchy
translateTypes([Type | Tail], Acc, GlobalAcc, Types, Info = #schemaInfo{namespaces=NS}, TypeHierarchy) ->
  {TranslatedType, TypeOfType} = translateType(Type, Types, Info),
  {Th, TranslatedType2} = case Type#typeInfo.base of
                            undefined -> 
                              {TypeHierarchy, TranslatedType};
                            Base -> 
                              {erlsom_lib:addTreeElement(list_to_atom(Type#typeInfo.typeName), 
                                                         erlsom_lib:makeTypeRefAtom(Base, NS), 
                                                         TypeHierarchy), 
                               %% TranslatedType#type{typeName = list_to_atom(erlsom_lib:makeTagFromRef(Base, NS))}}
                               TranslatedType#type{typeName = 
                                 list_to_atom(erlsom_lib:removePrefixes(Type#typeInfo.typeName))}}
                          end,
  case TypeOfType of
    globalElement ->
      translateTypes(Tail, [TranslatedType2| Acc], [Type|GlobalAcc], Types, Info, Th);
    globalElementRefOnly ->
      translateTypes(Tail, Acc, [Type|GlobalAcc], Types, Info, Th);
    noType ->
      translateTypes(Tail, Acc, GlobalAcc, Types, Info, Th);
    _Else ->
      translateTypes(Tail, [TranslatedType2| Acc], GlobalAcc, Types, Info, Th)
  end;
translateTypes([], Acc, GlobalAcc, _Types, _Info, TypeHierarchy) ->
  {Acc, GlobalAcc, TypeHierarchy}.

%% {'_document', sequence, [{[{'xs:schema', 'schemaType', true, 1, 1}], 1, 1, 1}], [], 1}
make_Document([#typeInfo{typeName=Name, typeRef=Type}|Tail], Acc, Info)
  when Type /= undefined ->
  make_Document(Tail, [#alt{tag = list_to_atom(Name), 
                            tp = list_to_type(Type), rl = true, mn = 1, mx = 1}|Acc], Info);
make_Document([#typeInfo{typeName=Name}|Tail], Acc, Info) ->
  make_Document(Tail, [#alt{tag = list_to_atom(Name), 
                            tp = list_to_atom(Name), rl = true, mn = 1, mx = 1}|Acc], Info);
make_Document([], Acc, _Info) ->
  #type{nm = '_document', tp = sequence, 
        els = [#el{alts = Acc, mn = 1,  mx = 1, nr = 1}], 
        atts = [], 
        nr = 1}.

removeDeadRefsFromDoc(Type = #type{nm = '_document', els = [El = #el{alts = Alts}]}, Types) ->
  F = fun(Alt = #alt{tp = {'#PCDATA', _}}) -> Alt;
         (Alt = #alt{tp = AltType}) -> 
           case lists:keysearch(AltType, #type.nm, Types) of
             {value, _} -> Alt;
             _ -> Alt#alt{tp = list_to_type("##string")}
           end
      end,
  LivingAlts = [F(Alt) || Alt <- Alts],
  Type#type{els=[El#el{alts = LivingAlts}]};
removeDeadRefsFromDoc(Type, _Types) ->
  Type.


%% Each Type is of the form {TypeName, TypeType, Elements, Attributes, NrOfElements}.
%% - TypeName is an atom.
%% - TypeType is currently always 'sequence', meaning that the elements have to
%%   be received in the specified order. In the future it might be considered to
%%   add something like 'bag' or 'set'.
%% - Elements is a list of Element. These Elements correspond to the elements
%%   that together form the Type.
%% - Attributes is a list of Attribute. Each Attribute describes an attribute
%%   that may occur as part of the Type.
%% - NrOfElements is an integer that indicates the number of elements in the Type.

%% resolve 'extended' types: look up the base and add its elements and attributes
translateType(Type = #typeInfo{elements=Elemts, attributes=Attrs, extends = Base, anyAttr = AnyAttr, mixed = Mixed}, 
              Types, Info = #schemaInfo{namespaces=NS})
  when Base /= undefined ->
  case erlsom_lib:searchBase(erlsom_lib:makeTypeRef(Base, NS), Types) of
    {value, #typeInfo{elements = BaseEls, attributes = BaseAttrs, anyAttr = BaseAnyAttr, extends = Base2, mixed = Mixed2}} ->
      %% debug(Elemts),
      %% debug(BaseEls),
      %% debug(Attrs),
      %% debug(BaseAttrs),
      NewAnyAttr = if BaseAnyAttr == undefined -> AnyAttr; true -> BaseAnyAttr end,
      translateType(Type#typeInfo{elements = BaseEls ++ Elemts, %% TODO: will never be 'undefined'?
                                  attributes = BaseAttrs ++ Attrs,  
				  anyAttr = NewAnyAttr,
                                  mixed = case Mixed of undefined -> Mixed2; _ -> Mixed end,
                                  extends = Base2}, Types, Info);
    _Else ->
      throw({error, "Base type not found: " ++ erlsom_lib:makeTypeRef(Base, NS)})
  end;

%% resolve 'restricted' types: look up the base and add its attributes (actually: replace the 
%% take attributes in the derived type by those from the base type)
%% 
%% TODO: this needs some improvement - for example around anyAttributes
%% 
%% First a special hack for extension of the 'anyType'. In this
%% special case the attributes of the derived type should remain.
translateType(Type = #typeInfo{elements=Elemts, attributes=Attrs, 
                               restricts = {qname,"http://www.w3.org/2001/XMLSchema","anyType",_,_}, anyAttr = AnyAttr, mixed = Mixed}, 
              Types, Info = #schemaInfo{}) ->
  translateType(Type#typeInfo{elements = Elemts, 
                              attributes = Attrs,
			      anyAttr = AnyAttr,
                              mixed = Mixed,
                              extends = undefined,
		              restricts = undefined}, Types, Info);

%% resolve 'restricted' types: look up the base and add (actually:  replace) its attributes
translateType(Type = #typeInfo{elements=Elemts, restricts = Base, anyAttr = AnyAttr, mixed = Mixed}, 
              Types, Info = #schemaInfo{namespaces=NS})
  when Base /= undefined ->
  case erlsom_lib:searchBase(erlsom_lib:makeTypeRef(Base, NS), Types) of
    {value, #typeInfo{attributes = BaseAttrs, anyAttr = BaseAnyAttr, extends = Base2, 
                      restricts = Base3, mixed = Mixed2}} ->
      %% debug(Elemts),
      %% debug(BaseEls),
      %% debug(Attrs),
      %% debug(BaseAttrs),
      NewAnyAttr = if BaseAnyAttr == undefined -> AnyAttr; true -> BaseAnyAttr end,
      translateType(Type#typeInfo{elements = Elemts, 
                                  attributes = BaseAttrs,
				  anyAttr = NewAnyAttr,
                                  mixed = case Mixed of undefined -> Mixed2; _ -> Mixed end,
                                  extends = Base2,
				  restricts = Base3}, Types, Info);
    _Else ->
      %% debug(Base),
      throw({error, "Base type not found: " ++ erlsom_lib:makeTypeRef(Base, NS)})
  end;

%% this corresponds with simple types. They don't have to be included in the model,
%% since references will be replaced by {#PCDATA, ...} type.
translateType(#typeInfo{typeType = simpleType}, _Types, _Info) ->
  {[], noType};

translateType(#typeInfo{typeName=Name, typeRef=undefined,
                        typeType = TypeType,
                        elements=Elemts, attributes=Attrs,
			anyAttr = AnyAttrValue,
                        mixed = Mixed,
                        seqOrAll = SorA, min = Min, max = Max}, Types, Info) ->
  TypeName=list_to_atom(Name),
  %% 'anyAttribute' can be defined in the type, but also in an attribute group
  {Attributes, AnyAttr} = translateAttributes(Attrs, {[], undefined}, 1, Info, 1),
  AnyAttr2 = if 
               AnyAttr == undefined -> AnyAttrValue; 
               true -> AnyAttr
             end,
  NrOfElements = erlsom_lib:listLength(Elemts) + 
                 erlsom_lib:listLength(Attributes) +1,
  Elements = translateElements(Elemts, [], erlsom_lib:listLength(Attributes) +1, 
                               Types),
  %% case Mixed of
    %% true ->
      %% Elements2 = makeMixed(Elements, NrOfElements),
      %% NrOfElements2 = erlsom_lib:listLength(Elements2) + 
                      %% erlsom_lib:listLength(Attributes) + 1;
    %% _NotMixed ->
      %% Elements2 = Elements,
      %% NrOfElements2 = NrOfElements
  %% end,
  %% {#type{nm = TypeName, tp = seqOrAll(SorA), els = Elements2, atts = Attributes, nr = NrOfElements2,
  {#type{nm = TypeName, tp = seqOrAll(SorA), els = Elements, atts = Attributes, nr = NrOfElements,
         mn = Min, mx = Max, anyAttr = AnyAttr2, mxd = Mixed}, TypeType};

translateType(#typeInfo{typeName=Name, typeRef=Type,
                        typeType = TypeType,
                        elements=undefined, %% attributes=undefined,
                        seqOrAll = SorA,
			anyAttr = AnyAttrValue,
                        min = Min,
                        max = Max}, _Types, _Info) ->
  TypeName=list_to_atom(Name),
  {#type{nm = TypeName, tp = seqOrAll(SorA), 
         els = [#el{alts = [#alt{tag = TypeName, 
                                 tp = list_to_type(Type), 
                                 rl = true, 
                                 mn = 1, 
                                 mx = 1}], 
                    mn = 1, mx = 1, nr = 1}], 
         atts = [], nr = 2, mn = Min, mx = Max, anyAttr = AnyAttrValue}, TypeType}.

seqOrAll(all) -> all;
seqOrAll(_) -> sequence.

%% purpose of 'Count' is to make sure that we don't get into an infinite loop in case 
%% of circular refernces in attributeGroups
translateAttributes(_, _Acc, _SeqNr, _Info, 10) ->
  throw({error, "circular reference in attribute group?"});
translateAttributes(undefined, _Acc, _SeqNr, _Info, _Count) ->
  {[], undefined};
translateAttributes([Attribute | Tail], {Acc, AnyAttr}, SeqNr, Info, Count) ->
  %% 'Attribute' can also be a reference to an attribute group - which may include 'anyAttribute'
  {TranslatedAttributes, AnyAttr2} = translateAttribute(Attribute, SeqNr, Info, Count),
  AnyAttr3 = if AnyAttr2 == undefined -> AnyAttr; true -> AnyAttr2 end,
  %% 20100131: changed the order of the ++ term, to solve issue with attribute groups. See also below
  translateAttributes(Tail, {Acc ++ TranslatedAttributes, AnyAttr3}, SeqNr + length(TranslatedAttributes), Info, Count);
translateAttributes([], {Acc, AnyAttr}, _SeqNr, _Info, _Count) ->
  %% 20100131: removed the list:reverse, because the ++ above already puts them in the right order
  {Acc, AnyAttr}.

translateElements(undefined, _Acc, _SeqNr, _Types) ->
  [];
translateElements([Element | Tail], Acc, SeqNr, Types) ->
  %% debug(Element),
  translateElements(Tail, [translateElement(Element, SeqNr, Types) | Acc], SeqNr+1, Types);
translateElements([], Acc, _SeqNr, _Types) ->
  lists:reverse(Acc).

%% -record(elementInfo, {alternatives, min, max}).
%%
%% Each Element is of the form {Alternatives, MinOccurs, MaxOccurs,
%% SequenceNumber}.
%% 
%% - Alternatives is a list of Aternative. Quite often there will be only 1
%%   alternative. If there are several possibilities that may occur at this
%%   position in the type (a 'choice', in XSD terminology), they will be listed
%%   here.
%% - MinOccurs is the number of times this Element may occur. The value 0 means
%%   that the Element is optional.
%% - MaxOccurs is the maximum number of times the Element may occur. The value
%%   'unbound' indicates that there is no limit.
%% - SequenceNumber is the position of the Element in the Elements list. This
%%   value is redundant, it is only there because I thought it would be easier,
%%   and maybe more performant.

translateElement(#elementInfo{alternatives=Alternatives, min=Min, max=Max}, SeqNr, Types) ->
  #el{alts = translateAlternatives(Alternatives, [], Types), 
      mn = Min, 
      mx = Max, nr = SeqNr}.

translateAlternatives([Alternative | Tail], Acc, Types) ->
  translateAlternatives(Tail, [translateAlternative(Alternative, Types) | Acc], Types);
translateAlternatives([], Acc, _Types) ->
  lists:reverse(Acc).

%% -record(alternative, {tag, type, real, min, max}).
%% Each Alternative is of the form {Tag, TypeReference, RealElement, min, max}.
%% - Tag is generally the tag of the element. In case of a 'choice', the
%%   combination of the Tag and the next event determines which altenative is
%%   selected. Tag can also be '#text', in which case we expect a 'character'
%%   event.
%% - TypeReference is either a reference to a Type (a TypeName), or a tuple
%%   {#PCDATA, PredefinedType}. This corresponds roughly to the next state of the
%%   state machine. In otherwords: "If we receive a start tag A, look for the
%%   Alternative with Tag=A, and start parsing a type as indicated by
%%   TypeReference".
%% - RealElement can be 'true' or 'false'. If it is 'false', then the current
%%   event will be left in the input stream. If it is 'true', the current event
%%   will be removed from the stream.
translateAlternative(#alternative{tag=Tag, type=Type, real=Real, min=Min, max=Max, anyInfo = AnyInfo}, Types) ->
  %% if Type refers to an element in Types that is actually just a simple
  %% type (string, for example), then we don't put a reference, but the type
  %% itself.
  %%
  %% If Type refers to an element in Types that is a globelElement that is 
  %% just a reference to another type, then we put that second reference.
  case lists:keysearch(Type, #typeInfo.typeName, Types) of
    %% -record(typeInfo, {typeName, global, typeType, typeRef, elements, attributes}).
    {value, #typeInfo{typeRef="##string"}} ->
      %% debug("Tag " ++ Tag),
      #alt{tag = list_to_atom(Tag), tp = list_to_type("##string"), rl = Real, mn = Min, mx = Max, anyInfo = AnyInfo};
    {value, #typeInfo{typeType = globalElementRefOnly, typeRef=Ref, elements=undefined, attributes=[]}} ->
      %% (error reported by Alexander Wingard) this can also be a ref to a simple type (as above)
      Tp = case lists:keysearch(Ref, #typeInfo.typeName, Types) of
        {value, #typeInfo{typeRef="##string"}} ->
          list_to_type("##string");
        _ -> 
          list_to_type(Ref)
      end,
      #alt{tag = list_to_atom(Tag), tp = Tp, rl = Real, mn = Min, mx = Max, anyInfo = AnyInfo};
    %% If Type has only 1 element and this element has only 1 alternative with tag = '#text' and 'real'  = false
    %% *and* the element has no attributes, then there is no point in referring to that type.
    %% However, this remains on the TODO list for now.
    _Else ->
      #alt{tag = list_to_atom(Tag), tp = list_to_type(Type), rl = Real, mn = Min, mx = Max, anyInfo = AnyInfo}
  end.

%% pass3 replaces, for choices that have several alternatives, the elements with a 
%% sort of group (real = false, new type is introduced) (see example10)
%% This also happens for simple type elements of mixed types (because you can't distinguish
%% them from the mixed (text) elements otherwise).
pass3(Types) ->
  pass3(Types, [], Types).

pass3([Type|Tail], Acc, Types) ->
  pass3(Tail, pass3Type(Type, Types) ++ Acc, Types);
pass3([], Acc, _Types) ->
  lists:reverse(Acc).

pass3Type(#type{nm = TypeName, els = Elements, mxd = Mixed} = Rec, Types) ->
  {ThisTypesElements, NewTypes} = pass3Elements(Elements, TypeName, Types, Mixed),
  [Rec#type{els = ThisTypesElements} | NewTypes].

pass3Elements(Elements, TypeName, Types, Mixed) ->
  pass3Elements(Elements, TypeName, [], [], Types, Mixed).

pass3Elements([Element|Tail], TypeName, Acc, NewTypes, Types, Mixed) ->
  {TranslatedElement, Types2} = pass3Element(Element, TypeName, Types, Mixed),
  pass3Elements(Tail, TypeName, [TranslatedElement | Acc], Types2 ++ NewTypes, Types, Mixed);
pass3Elements([], _TypeName, Acc, NewTypes, _Types, _Mixed) ->
  {lists:reverse(Acc), NewTypes}.

pass3Element(#el{alts = Alternatives, mn = Min} = Rec, TypeName, Types, Mixed)->
  %% if one of the alternatives is optional, the whole choice is optional!
  {ThisElementsAlternatives, NewTypes, Optional} = pass3Alternatives(Alternatives, TypeName, Types, Mixed),
  if 
    Optional -> Min2 = 0;
    true -> Min2 = Min
  end,
  if 
    (length(Alternatives) /= 1) or (Mixed == true) ->
      %% TODO: this doesn't work correctly if this happens in a group that is referred from 
      %% within a mixed element (because we only find out that this group is also mixed at
      %% parse time, due to the 'inheritance' mechanism). 
      {Rec#el{alts = ThisElementsAlternatives, mn = Min2}, NewTypes};
    true ->
      {Rec#el{mn = Min2}, []}
  end.

pass3Alternatives(Alternatives, TypeName, Types, Mixed) ->
  pass3Alternatives(Alternatives, TypeName, [], [], false, Alternatives, Types, Mixed).

pass3Alternatives([Alternative | Tail], TypeName, Acc, NewTypes, Optional, Alternatives, Types, Mixed) ->
  {TranslatedAlternative, Types2} = pass3Alternative(Alternative, TypeName, Alternatives, Types, Mixed),
  Min = TranslatedAlternative#alt.mn,
  Optional2 = if 
                 Min == 0 -> true;
                 true -> Optional
              end,
  pass3Alternatives(Tail, TypeName, [TranslatedAlternative | Acc], Types2 ++ NewTypes, 
                    Optional2, Alternatives, Types, Mixed);
pass3Alternatives([], _TypeName, Acc, NewTypes, Optional, _Alternatives, _Types, _Mixed) ->
  {Acc, NewTypes, Optional}.

pass3Alternative(Alternative = #alt{tag = Name, rl = Real, tp = Type, mn = Mn}, TypeName, Alternatives, Types, _Mixed) ->
   if 
     Real == false ->
       {Alternative, []};
     true ->
       case Type of
         {'#PCDATA', _} -> 
           NewTypeName = 
              if 
                %% TODO: this probably doesn't make any sense. I guess 'any' types
                %% should be handled as above (return {Alternative, []}).
                Name == '#any' -> list_to_atom(atom_to_list(TypeName) ++ "-any");
                true -> 
                  case TypeName of
                    '_document' -> Name;
                    _ -> list_to_atom(atom_to_list(TypeName) ++ "-" ++ erlsom_lib:nameWithoutPrefix(atom_to_list(Name)))
                  end
              end,
           {Alternative#alt{tp = NewTypeName, rl = simple, mn = case Mn of 0 -> 0; _ -> 1 end}, 
             [#type{nm = NewTypeName, tp = sequence, 
                    els = [#el{alts = [Alternative#alt{mn = 1, mx = 1}], 
                               mn = 1, mx = 1, nr = 1}], 
                    atts = [], 
                    nr = 2}]};
         _NoSimpleType ->
           %% if the same type occurs in several alternatives, we need to do something similar
           case occursMoreThanOnce(Type, #alt.tp, Alternatives) of
             true ->
               if
                 %% for any, we don't care (no need to distinguish between 2 different kinds of any)
                 Name == '#any' -> {Alternative, []};
                 true -> 
                   NewTypeName = 
                     list_to_atom(atom_to_list(TypeName) ++ "-" ++ erlsom_lib:nameWithoutPrefix(atom_to_list(Name))),
                   case lists:keysearch(Type, #type.nm, Types) of
                     {value, Record} ->
                       {Alternative#alt{tp = NewTypeName, rl = true, mn = case Mn of 0 -> 0; _ -> 1 end}, 
                         [Record#type{nm = NewTypeName}]};
                     _Else ->
                       %% debugTypes(Types),
                       throw({error, "Type definition not found " ++ atom_to_list(Type)})
                   end
               end;
             _ ->
               {Alternative, []}
           end
       end
   end.

occursMoreThanOnce(Key, Pos, List) ->
  occursMoreThanOnce(Key, Pos, List, false).
occursMoreThanOnce(_Key, _Pos, [], _) ->
  false;
occursMoreThanOnce(Key, Pos, [H|Tail], OneFound) ->
  case element(Pos, H) of
    Key -> 
      case OneFound of
        true ->
          true;
        _ -> occursMoreThanOnce(Key, Pos, Tail, true)
      end;
    _ ->
      occursMoreThanOnce(Key, Pos, Tail, OneFound)
  end.


%% pass4 replaces the tags of group references (##TODO) with the first tag of the 
%% group definition.
%% It also takes into account the fact that, if all elements of the group are optional,
%% the whole group is optional (minOccurs = 0)
pass4(Types, Info) ->
  pass4(Types, ?MAX_PASSES, Info).

pass4(_Types, 0, _Info) ->
  throw({error, "Group or anonymous sequence nested too deep"});
pass4(Types, Count, Info) ->
  put(erlsom_pass4_ready, true),
  Result = pass4(Types, [], Types, Count, Info),
  case get(erlsom_pass4_ready) of 
    true -> 
      erase(erlsom_pass4_ready),
      Result;
    _Else ->
      pass4(Result, Count - 1, Info)
  end.

pass4([Type|Tail], Acc, Types, Count, Info) ->
  pass4(Tail, [pass4Type(Type, Types, Count, Info) | Acc], Types, Count, Info);
pass4([], Acc, _Types, _Count, _Info) ->
  lists:reverse(Acc).

pass4Type(Rec = #type{els = Elements}, Types, Count, Info) ->
  ThisTypesElements = pass4Elements(Elements, Types, Count, Info),
  Rec#type{els = ThisTypesElements}.

pass4Elements(Elements, Types, Count, Info) ->
  pass4Elements(Elements, [], Types, Count, Info).

pass4Elements([Element|Tail], Acc, Types, Count, Info) ->
  TranslatedElement = pass4Element(Element, Types, Count, Tail, Info),
  pass4Elements(Tail, [TranslatedElement | Acc], Types, Count, Info);
pass4Elements([], Acc, _Types, _Count, _Info) ->
  lists:reverse(Acc).

pass4Element(Element = #el{alts = Alternatives, mn = Min}, Types, Count, NextElements, Info)->
  {ThisElementsAlternatives, Optional} = pass4Alternatives(Alternatives, Types, Count, NextElements, Info),
  Min2 = if Optional == true -> 0; true -> Min end,
  UniqueAlternatives = lists:usort(ThisElementsAlternatives),
  Element#el{alts = UniqueAlternatives, mn = Min2}.

pass4Alternatives(Alternatives, Types, Count, NextElements, Info) ->
  pass4Alternatives(Alternatives, [], true, Types, Count, NextElements, Info).

%% returns {TranslatedAlternatives, Optional}
pass4Alternatives([Alternative | Tail], Acc, Optional, Types, Count, NextElements, Info) ->
  {TranslatedAlternatives, Optional2} = pass4Alternative(Alternative, Types, Count, NextElements, Info),
  Optional3 = if Optional2 == true -> Optional; true -> false end,
  pass4Alternatives(Tail, TranslatedAlternatives ++ Acc, Optional3, Types, Count, NextElements, Info);
pass4Alternatives([], Acc, Optional, _Types, _Count, _NextElements, _Info) ->
  {Acc, Optional}.

%% returns {TranslatedAlternatives, Optional}
%% Alternatives that have the tag '##TODO' have to be populated. These refer to groups, 'real' = false
pass4Alternative(Alternative = #alt{tag = Name, tp = Type}, Types, _Count, _NextElements, _Info) ->
  case Name of
    '##TODO' ->
      %% Search the type, and look for the first tag.
      %% debug_types(Types),
      case lists:keysearch(Type, #type.nm, Types) of
        {value, Record} ->
           getTagFromType(Alternative, Record#type.els);
        _Else ->
          %% debugTypes(Types),
          throw({error, "Group definition not found " ++ atom_to_list(Type)})
      end;
    _Else ->
      {[Alternative], false}
  end.

%% end of pass 4

%% Pass5 also deals with 'any' elements.
%% - add the 'nxt' values that signal the end of the 'any' bit
%% - add (concrete) alternatives to all 'any' alternatives.
%% added should be all alternatives from the '_document' type that meet the criteria wrt namespace
%% The criteria have to be kept as part of the 'any' alternative (in the final model), since we have
%% to be able to repeat this when a namespace is added to the model.
%%
%% it has to be done after the '_document' type has been created!
pass5(Types, Info) ->
  pass5(Types, [], Types, Info).

pass5([Type|Tail], Acc, Types, Info) ->
  pass5(Tail, [pass5Type(Type, Types, Info) | Acc], Types, Info);
pass5([], Acc, _Types, _Info) ->
  lists:reverse(Acc).

pass5Type(Rec = #type{nm = Name, els = Elements}, Types, Info = #schemaInfo{namespaces=NS}) ->
  %% this is a hack - Tns from #schemaInfo is not always correct when an xsd has been imported.
  Tns = erlsom_lib:tagNamespace(Name, NS),
  ThisTypesElements = pass5Elements(Elements, Types, Info, Tns),
  Rec#type{els = ThisTypesElements}.

pass5Elements(Elements, Types, Info, Tns) ->
  pass5Elements(Elements, [], Types, Info, Tns).

pass5Elements([Element|Tail], Acc, Types, Info, Tns) ->
  TranslatedElement = pass5Element(Element, Types, Tail, Info, Tns),
  pass5Elements(Tail, [TranslatedElement | Acc], Types, Info, Tns);
pass5Elements([], Acc, _Types, _Info, _Tns) ->
  lists:reverse(Acc).

pass5Element(Element = #el{alts = Alternatives}, Types, NextElements, Info, Tns)->
  ThisElementsAlternatives = pass5Alternatives(Alternatives, Types, NextElements, Info, Tns),
  Element#el{alts = ThisElementsAlternatives}.

pass5Alternatives(Alternatives, Types, NextElements, Info, Tns) ->
  pass5Alternatives(Alternatives, [], Types, NextElements, Info, Tns).

pass5Alternatives([Alternative | Tail], Acc, Types, NextElements, Info, Tns) ->
  TranslatedAlternatives = pass5Alternative(Alternative, Types, NextElements, Info, Tns),
  pass5Alternatives(Tail, TranslatedAlternatives ++ Acc, Types, NextElements, Info, Tns);
pass5Alternatives([], Acc, _Types, _NextElements, _Info, _Tns) ->
  Acc.

pass5Alternative(Alternative = #alt{tag = Name, anyInfo = AnyInfo}, Types, NextElements, Info, Tns) ->
  case Name of
    '#any' ->  
      %% #anyInfo{prCont = Pc}  = AnyInfo, 
      Pc = case AnyInfo of
             #anyInfo{} -> AnyInfo#anyInfo.prCont;
             undefined -> undefined
           end,

      case Pc of
        "lax" ->
          [Alternative#alt{nxt=getNextTags(NextElements)} | getDocumentAlternatives(AnyInfo, Types, Info, Tns)];
        %% undefined ->
          %% [Alternative#alt{nxt=getNextTags(NextElements), anyInfo = xxx}];
        _ ->
          [Alternative | getDocumentAlternatives(AnyInfo, Types, Info, Tns)]
      end;
    _Else ->
      [Alternative]
  end.

%% returns {TranslatedAlternatives, Optional}
getTagFromType(Alternative, [#el{mn = 0, alts = Alternatives} | Tail]) ->
  %% if Tail == [] -> debug("all alternatives are optional!"); true -> true end,
  {Alternatives2, Optional} = getTagFromType(Alternative, Tail),
  {getTagFromAlternatives(Alternative, Alternatives) ++ Alternatives2, Optional};
getTagFromType(Alternative, [#el{alts = Alternatives} | _Tail]) ->
  {getTagFromAlternatives(Alternative, Alternatives), false};
getTagFromType(_Alternative, []) ->
  {[], true}.

getTagFromAlternatives(Alternative, [#alt{tag = '##TODO'} | Tail])->
  put(erlsom_pass4_ready, false),
  [Alternative#alt{tag = '##TODO'} | getTagFromAlternatives(Alternative, Tail)];
getTagFromAlternatives(Alternative, [#alt{tag = Name, anyInfo = AnyInfo} | Tail])->
  [Alternative#alt{tag = Name, anyInfo = AnyInfo} | getTagFromAlternatives(Alternative, Tail)];
getTagFromAlternatives(_Alternative, [])->
  [].

%% Add the 'nxt' values that signal the end of the 'any' bit
%% Get the alternatives of the next element. IF the next element is optional, also get the alternatives 
%% of the element after that, etc.
getNextTags(NextElements) ->
  getNextTags(NextElements, []).

getNextTags([], Acc) ->
  Acc;
getNextTags([#el{alts = Alternatives, mn = 0} | Tail], Acc) ->
  getNextTags(Tail, getTags(Alternatives) ++ Acc);
getNextTags([#el{alts = Alternatives} | _Tail], Acc) ->
  getTags(Alternatives) ++ Acc.

getTags(Alternatives) ->
  getTags(Alternatives, []).

getTags([], Acc) ->
  Acc;
getTags([#alt{tag=Tag} | Tail], Acc) ->
  getTags(Tail, [Tag | Acc]).
  
  
%% get the alternatives from the '_document' type
%% -record(anyInfo, {prCont, ns}). %% for any elements
getDocumentAlternatives(AnyInfo, Types, Info = #schemaInfo{}, Tns) ->   
  #type{els = [#el{alts = Alternatives}]} = findType('_document', Types, [], undefined, undefined, undefined), %% only 1 element
  %% TODO: this is a weird hack, lets see whether it works
  getMatchingAlts(Alternatives, AnyInfo, Info, Tns).

getMatchingAlts(Alternatives, #anyInfo{ns = Namespaces}, Info, Tns) ->
  case Namespaces of 
    "##any" ->
      Alternatives;
    _  ->
      getMatchingAlts(Alternatives, [], Namespaces, Info, Tns)
  end.

getMatchingAlts([], Acc, _Namespaces, _Info, _Tns) ->
  Acc;
getMatchingAlts([Alt = #alt{tag=Tag} | Tail], Acc, Namespaces, Info = #schemaInfo{namespaces=NS}, Tns) ->
  Namespace = erlsom_lib:tagNamespace(Tag, NS),
  case Namespaces of
    "##local" -> 
      case Namespace of
        undefined -> 
          getMatchingAlts(Tail, add_alternative_no_dups(Alt, Acc), Namespaces, Info, Tns);
        _Else ->
          getMatchingAlts(Tail, Acc, Namespaces, Info, Tns)
      end;
    "##other" -> 
      case Namespace of
        undefined -> 
          getMatchingAlts(Tail, Acc, Namespaces, Info, Tns);
        %% TODO: '##other' doesn't work, because Tns isn't always correct: see soap-wsdl example
        %% problem arises with imported namespaces. In the case of wsdl (imported into soap)
        %% Tns is soap - so the code below (commented out) doesn't work correctly.
        Tns -> 
          getMatchingAlts(Tail, Acc, Namespaces, Info, Tns);
        _Else ->
          getMatchingAlts(Tail, add_alternative_no_dups(Alt, Acc), Namespaces, Info, Tns)
      end;
    _ ->
      case Namespace of
        Namespaces -> 
          getMatchingAlts(Tail, add_alternative_no_dups(Alt, Acc), Namespaces, Info, Tns);
        _ ->
          getMatchingAlts(Tail, Acc, Namespaces, Info, Tns)
      end
  end.

add_alternative_no_dups(Alt = #alt{tag = Tag}, List) ->
  case lists:keysearch(Tag, #alt.tag, List) of
    {value, _} ->
      List;
    _ ->
      [Alt | List]
  end.
          

list_to_type("##string") ->
  {'#PCDATA', 'char'};

list_to_type(Type = {'#PCDATA', _}) ->
  Type;

list_to_type(undefined) ->
  {'#PCDATA', 'char'};

list_to_type(List) ->
  list_to_atom(List).

%% -record(attribute, {name, optional, type}).
%% Attribute is a tuple {Name, SequenceNr, Optional, Type}
translateAttribute(#attrib{name=Name, optional=Optional, type = Type, ref = undefined}, 
                   SeqNo, _Info, _Count) ->
  {[#att{nm = list_to_atom(Name), nr = SeqNo, opt = trueFalse(Optional), tp = attributeType(Type)}], undefined};

translateAttribute(#attrib{ref = Ref, optional = Optional}, SeqNo, Info= #schemaInfo{namespaces=NS}, _Count) ->
  Name = erlsom_lib:makeAttrRef(Ref, NS),
  %% debug(Info#schemaInfo.atts),
  case lists:keysearch(Name, #attrib.name, Info#schemaInfo.atts) of
    {value, #attrib{name = Name, type=Type, optional=Optional2}} ->
      Optional3 = if Optional == undefined -> Optional2; true -> Optional end,
      {[#att{nm = list_to_atom(Name), nr = SeqNo, opt = trueFalse(Optional3), tp = attributeType(Type)}], undefined};
    _ ->
      if 
        Name == "xml:lang" ->
          {[#att{nm = list_to_atom(Name), nr = SeqNo, opt = trueFalse(Optional), tp = 'char'}], undefined};
        true ->
          throw({error, "Attribute not found: " ++ erlsom_lib:makeAttrRef(Ref, NS)})
      end
  end;

%% -record(attributeGroupRefType, {elInfo, ref}).
translateAttribute(#attributeGroupRefType{ref=Ref}, SeqNo, Info = #schemaInfo{namespaces=NS}, Count) ->
  %% look for atributeGroup
  %% -record(attGrp, {name, atts, anyAttr}).
  case lists:keysearch(erlsom_lib:makeAttrRef(Ref, NS), #attGrp.name, Info#schemaInfo.attGrps) of
    {value, #attGrp{atts = Attrs, anyAttr = AnyAttrValue}} ->
      %% translate recursively
      {Attributes, AnyAttr} = translateAttributes(Attrs, {[], undefined}, SeqNo, Info, Count + 1),
      AnyAttr2 = if 
                   AnyAttrValue == undefined -> AnyAttr; 
                   true -> AnyAttrValue
                 end,
      {Attributes, AnyAttr2};
    _Else -> 
      %% debug(erlsom_lib:makeAttrRef(Ref, NS)),
      %% debug(Info#schemaInfo.attGrps),
      %% debug(Ref),
      %% debug(NS),
      throw({error, "Attribute group not found: " ++ erlsom_lib:makeAttrRef(Ref, NS)})
  end.

attributeType(undefined) -> 'char';
attributeType(#qname{uri = NS, localPart = Local}) -> 
  case NS of
    "http://www.w3.org/2001/XMLSchema" ->
      erlsom_lib:translateType(Local);
    _Else ->
      'char'
  end.

%% add a #text alternative to every element.
%% note that this only makes sense for elements with maxOccurs > 1, but that is the 
%% most likely way to use 'mixed' anyway.


trueFalse(undefined) -> true; %% atributes are optional by default
trueFalse("required") -> false;
trueFalse("prohibited") -> true;  %% TODO: this is obviously not correct
trueFalse("optional") -> true.

