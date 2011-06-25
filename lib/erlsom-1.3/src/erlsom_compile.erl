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


%%% Version history:
%%% 22-04-2009: fixed bug in processing of import inside include - see
%%%             comment marked 20090422.

%%% ===================================================================
%%% Translates the XSD into the model used by erlsom_parse.
%%% ===================================================================

%% transform the xsd (in record form, as delivered by erlsom!) into
%% the model required by erlsom.
%%
%% - import (and parse) imported XSDs (adding them to the list of 
%%   types)
%% - perform some additional checks on the validity of the XSD
%% - add sequence numbers and element counts 
%% - add names to the types that include the prefix and the 'path'
%% - groups and 'anonymous sequences' have to be 'linked in' by 
%%   providing alternatives ('real' = false).
%% - 'anonymous' sequences have to get a (unique) name
%% - a top level element ('_document') has to be added to includes
%%   alternatives for all globally declared elements
%% - special treatment is necessary for choices where (some or all) 
%%   alternatives have the same type: otherwise it will not be possible
%%   to recognise from the result which alternative was selected. For 
%%   the moment this is only implented for 'text' alternatives.

%% Ideally it should also take care of:
%% - attribute groups
%% - 'extends'


%% The first pass puts all types and elements into lists. Locally 
%% defined types ('russian doll design') are given names that reflect 
%% the fact that they are local, in order to avoid name conflicts (and 
%% they are explicitly marked as local, to avoid including then in the 
%% '_model' element).

%% result of pass1:
%% 
%% types are distinguished from elements, because only elements can be
%% used as 'root' of the XML document.

-module(erlsom_compile).
-record(p1acc, {tps = [],       %% types
                attGrps = [],   %% attributeGroups
                atts = [],      %% global Attributes
                seqCnt = 0,     %% sequence counter, make unique names
                tns,            %% target namespace
                efd,            %% element form default
                afd,            %% attribute form default
                nsp,            %% namespacePrefix
                nss,            %% namespaces ([#namespace{}])
                includeFun,     %% function to find included XSDs
                includeDirs,    %% directories to look for XSDs
	        includeFiles,   %% tuples {Namespace, Location, Prefix}
                                %% or {Namespace, Schema, Prefix}, where 
                                %% Schema is a parsed XSD (used to parse WSDLs
                                %% with multiple XSDs).
                imported = [],  %% a list of imported namespaces, to prevent
                                %% getting into a loop when there are circular 
                                %% refernces.
                path = []}).    %% path


-export([compile/2]).
-export([compile_parsed_xsd/2]).
-export([compile_parsed_xsd/3]).

-include_lib("erlsom.hrl").
-include_lib("erlsom_parse.hrl").
-include_lib("erlsom_compile.hrl").

-import('erlsom_lib', [minMax/1]).

%% debug(Text) -> io:format("compile - ~p\n", [Text]).

%% debugTypes([]) ->
  %% ok;
%% debugTypes([Type = #typeInfo{typeName= Name}|T]) ->
  %% %% debug(Name),
  %% case Name of
    %% "P:speak.class" -> debug(Name), debug(Type#typeInfo.elements);
    %% "P:speak" -> debug(Name), debug(Type#typeInfo.elements);
    %% _ -> ok
  %% end,
  %% debugTypes(T).

compile(Xsd, Options) ->
  compile_internal(Xsd, Options, false).

compile_parsed_xsd(ParsedXsd, Options) ->
  compile_internal(ParsedXsd, Options, true).

%% This is slightly messy: xsd can either be a parsed xsd or not. If it isn't, then it still has
%% to be parsed. 
compile_internal(Xsd, Options, Parsed) ->
  Namespaces = case lists:keysearch('namespaces', 1, Options) of
                 {value, {_, Namesp}} -> 
                   lists:map(fun translateNs/1, Namesp);
                 _ -> []
               end,
  Prefix = case lists:keysearch('prefix', 1, Options) of
             {value, {_, Pf}} -> Pf;
             _ -> undefined
           end,
  TypePrefix = case lists:keysearch('type_prefix', 1, Options) of
             {value, {_, TPf}} -> TPf;
             _ -> ""
           end,
  GroupPrefix = case lists:keysearch('group_prefix', 1, Options) of
             {value, {_, GPf}} -> GPf;
             _ -> ""
           end,
  ElementPrefix = case lists:keysearch('element_prefix', 1, Options) of
             {value, {_, EPf}} -> EPf;
             _ -> ""
           end,
  IncludeFun = case lists:keysearch('include_fun', 1, Options) of
                 {value, {_, If}} -> If;
                 _ -> fun erlsom_lib:findFile/4
               end,
  IncludeDirs = case lists:keysearch('dir_list', 1, Options) of
                 {value, {_, Id}} -> Id;
                 _ -> ["."]
               end,
  IncludeFiles = case lists:keysearch('include_files', 1, Options) of
                   {value, {_, Files}} -> Files;
                   _ -> Namespaces %% the two options are mutually exlclusive
                 end,
  put(erlsom_typePrefix, TypePrefix),
  put(erlsom_groupPrefix, GroupPrefix),
  put(erlsom_elementPrefix, ElementPrefix),
  %% first we translate to an intermediate format, in a final pass we will add
  %% things like sequence numbers and element counts.
  Ns = [#ns{prefix="xsd", uri="http://www.w3.org/2001/XMLSchema"} | Namespaces],
  ParsingResult = case Parsed of
                    false -> erlsom_parseXsd:parseXsd(Xsd, Ns);
                    _ -> Xsd
                  end,
  case ParsingResult of
    {error, _Message} -> ParsingResult;
    ParsedXsd -> compile_parsed_xsd(ParsedXsd, Prefix, Ns, IncludeFun, IncludeDirs, IncludeFiles)
  end.


      
%% obsolete
compile_parsed_xsd(ParsedXsd, Prefix, Namespaces) ->
  compile_parsed_xsd(ParsedXsd, Prefix, Namespaces, fun erlsom_lib:findFile/4, ["."], []).

compile_parsed_xsd(ParsedXsd, Prefix, Namespaces, IncludeFun, IncludeDirs, IncludeFiles) ->
  %% InfoRecord will contain some information required along the way
  TargetNs = ParsedXsd#schemaType.targetNamespace,
  Acc = #p1acc{tns = TargetNs,
               includeFun = IncludeFun,
               includeDirs = IncludeDirs,
	       includeFiles = IncludeFiles,
	       efd = ParsedXsd#schemaType.elementFormDefault, 
	       afd = ParsedXsd#schemaType.attributeFormDefault, 
	       nsp = case TargetNs of
                       undefined -> "";
                       _ -> case Prefix of 
                         undefined -> ""; 
                         _ -> Prefix ++ ":" 
                       end
                     end,
	       nss = case TargetNs of
                       undefined -> Namespaces;
                       _ -> [#ns{prefix = Prefix, uri = TargetNs} | Namespaces]
                     end},
  case catch transform(ParsedXsd, Acc) of
    {error, Message} -> {error, Message};
    {'EXIT', Message} -> throw({'EXIT', Message});
    IntermediateResult -> 
      case catch erlsom_pass2:secondPass(IntermediateResult#p1acc.tps,
					 #schemaInfo{targetNamespace = IntermediateResult#p1acc.tns,
						     namespaces = IntermediateResult#p1acc.nss,
						     atts = IntermediateResult#p1acc.atts,
						     attGrps = IntermediateResult#p1acc.attGrps}) of
	 {error, Message} -> {error, Message};
         {'EXIT', Message} -> throw({'EXIT', Message});
	 FinalResult -> {ok, FinalResult}
      end
  end.

%% -record(schemaType, {targetNamespace, elementFormDefault, elements}).
transform(#schemaType{elements=Elements, imports=Impts}, 
          Acc = #p1acc{}) ->
  %% transorm element and types etc.
  %% in TransformedTypes the top-level types have to be recognisable somehow.
  Acc2 = processImports(Impts, Acc),
  transformTypes(Elements, Acc2).

processImports(undefined, Acc) ->
  Acc;
processImports([], Acc) ->
  Acc;
processImports([#annotationType{} | Tail], Acc) ->
  processImports(Tail, Acc);
%% processImports([#importType{schemaLocation="http://www.w3.org/2001/xml.xsd"} | Tail], Acc) ->
  %% %% no need to import this.
  %% processImports(Tail, Acc);
processImports([#importType{namespace="http://www.w3.org/XML/1998/namespace",
                            schemaLocation = undefined} | Tail], Acc) ->
  %% no need to import this.
  processImports(Tail, Acc);
processImports([Impt = #importType{namespace = Ns} | Tail], 
               Acc = #p1acc{includeDirs = Dirs,
                            includeFun = InclFun,
			    includeFiles = InclFiles,
                            nss = Namespaces,
                            imported = Imported}) ->
  case lists:member(Ns, Imported) of
    true ->
      %% only import a namespace once
      processImports(Tail, Acc);
    _ ->
      %% read file
      {Xsd, Prefix1} = InclFun(Ns,
                              Impt#importType.schemaLocation,
	                      InclFiles,
                              Dirs),
      Prefix = case Prefix1 of
                 undefined ->
                   erlsom_lib:findPrefix2(Ns, Namespaces);
                 _ -> Prefix1
               end,

      ParsedGrammar = case Xsd of
                        #schemaType{} -> 
                          Xsd;
                        _ -> 
                          erlsom_parseXsd:parseXsd(Xsd, Acc#p1acc.nss)
                      end,
      Acc2 = combineInfo(ParsedGrammar, Acc, Prefix),
      Acc2a = Acc2#p1acc{nsp = case Prefix of
                                 undefined -> "";
                                 "" -> "";
                                 _ -> Prefix ++ ":"
                               end,
                         imported= [Ns | Imported]},
      %% transform (recursively)
      Acc3 = transform(ParsedGrammar, Acc2a),
      %%% why copy everything again? Why not use Acc3?
      Acc4 = Acc#p1acc{tps = Acc3#p1acc.tps,
                       seqCnt = Acc3#p1acc.seqCnt,
                       attGrps = Acc3#p1acc.attGrps,
                       nss = Acc3#p1acc.nss,
                       atts = Acc3#p1acc.atts,
                       imported = Acc3#p1acc.imported},
      %% processImports(Tail, transform(ParsedGrammar, Acc3)).
      processImports(Tail, Acc4)
  end;
processImports([#includeType{schemaLocation = undefined} | Tail], Acc) ->
  processImports(Tail, Acc);
processImports([Impt = #includeType{} | Tail], Acc = #p1acc{includeDirs = Dirs,
                                                           includeFun = InclFun,
                                                           nsp = Prefix}) ->
  {Xsd, _Prefix} = InclFun(undefined,
                          Impt#includeType.schemaLocation,
			  [],
                          Dirs),
  %% debug("Parse imported file"),
  ParsedGrammar = erlsom_parseXsd:parseXsd(Xsd, Acc#p1acc.nss),
  % TODO: do we need 'combine info' for includes???
  % Acc2 = combineInfo(ParsedGrammar, Acc, Prefix),
  Acc2 = Acc,
  %% transform (recursively)
  Acc3 = transform(ParsedGrammar, Acc2),
  Acc4 = Acc#p1acc{tps = Acc3#p1acc.tps,
                   nsp = Prefix,
                   seqCnt = Acc3#p1acc.seqCnt,
                   attGrps = Acc3#p1acc.attGrps,
                   nss = Acc3#p1acc.nss, %% 20090422: added this line to make
                                         %% namespaces known. 
                   atts = Acc3#p1acc.atts},
  processImports(Tail, Acc4);
processImports([#redefineType{schemaLocation = Location,
                                     elements = Redefines} | Tail], 
               Acc = #p1acc{includeDirs = Dirs, includeFun = InclFun, nsp = Prefix, nss = Namespaces}) ->
  {Xsd, _Prefix} = InclFun(undefined,
                          Location,
			  [],
                          Dirs),
  %% debug("Parse imported/redefined file, location = " ++ Location),
  ParsedGrammar = erlsom_parseXsd:parseXsd(Xsd, Acc#p1acc.nss),
  Acc2 = Acc#p1acc{tps = [], attGrps = [], atts = []},
  %% transform (recursively)
  Acc3 = (#p1acc{tps = ImportedTypes} = transform(ParsedGrammar, Acc2)),
  #p1acc{tps = TypesToRedefine} = transformTypes(Redefines, Acc2),
  %% TODO : deal with attribute groups
  Types2 = replaceElements(ImportedTypes, ImportedTypes, TypesToRedefine, Namespaces),
  Acc4 = Acc#p1acc{tps = Acc#p1acc.tps ++ Types2,
                   nsp = Prefix,
                   seqCnt = Acc#p1acc.seqCnt,
                   attGrps = Acc#p1acc.attGrps ++ Acc3#p1acc.attGrps,
                   atts = Acc#p1acc.atts ++ Acc3#p1acc.atts},
  %% debugTypes(Acc4#p1acc.tps),
  processImports(Tail, Acc4).

replaceElements(ImportedTypes, Types, TypesToRedefine, Namespaces) ->
  %% returns the imported types with redefinitions applied
  replaceElements(ImportedTypes, Types, TypesToRedefine, [], Namespaces).

replaceElements(_ImportedTypes, [], _Elements, Acc, _Namespaces) ->
  Acc;
replaceElements(ImportedTypes, [Original = #typeInfo{typeName= Name, typeType = TypeT} | Tail], 
                TypesToRedefine, Acc, Namespaces)
  when TypeT /= globalElementRefOnly ->
  case lists:keysearch(Name, #typeInfo.typeName, TypesToRedefine) of
    {value, RedefinedType = #typeInfo{extends = undefined}} -> 
      replaceElements(ImportedTypes, Tail, TypesToRedefine, [RedefinedType | Acc], Namespaces);
    {value, Redefined = #typeInfo{base = Base, anyAttr = AnyAttr, elements = Elemts, attributes = Attrs}} -> 
      RedefinedType = case erlsom_lib:searchBase(erlsom_lib:makeTypeRef(Base, Namespaces), ImportedTypes) of
                        {value, #typeInfo{elements = BaseEls, attributes = BaseAttrs, 
                                          anyAttr = BaseAnyAttr, extends = Base2}} ->
                          NewAnyAttr = if BaseAnyAttr == undefined -> AnyAttr; true -> BaseAnyAttr end,
                          Redefined#typeInfo{elements = erlsom_lib:emptyListIfUndefined(BaseEls) ++ 
                                                        erlsom_lib:emptyListIfUndefined(Elemts), 
                                             attributes = BaseAttrs ++ Attrs,  
                                             anyAttr = NewAnyAttr,
                                             extends = Base2};
                        _Else ->
                          throw({error, "Base type not found: " ++ erlsom_lib:makeTypeRef(Base, Namespaces)})
                      end,
      replaceElements(ImportedTypes, Tail, TypesToRedefine, [RedefinedType | Acc], Namespaces);
    _ ->
      replaceElements(ImportedTypes, Tail, TypesToRedefine, [Original | Acc], Namespaces)
  end;
replaceElements(ImportedTypes, [Original = #typeInfo{} | Tail], 
                TypesToRedefine, Acc, Namespaces) ->
  replaceElements(ImportedTypes, Tail, TypesToRedefine, [Original | Acc], Namespaces).
    
%% Deals with targetNamespace and 'elementFormDefault' setting. 
%% information from this schema is combined with info from a potential 'parent' XSD. 
combineInfo(#schemaType{targetNamespace=Tns, elementFormDefault=Efd, 
                        attributeFormDefault = Afd}, 
            Acc = #p1acc{tns=TnsParent, efd = EfdParent, 
                         afd = AfdParent, nss = Namespaces},
            Prefix) ->
   Acc#p1acc{tns = if 
                      Tns == undefined -> TnsParent; 
                      true -> Tns
                   end,
             efd = if 
                      %% what is right???
                      %% Efd == undefined -> EfdParent;
                      true -> Efd
                    end,
             afd = if 
                      %% what is right???
                      %% Efd == undefined -> EfdParent;
                      true -> Afd
                    end,
             nss = if 
                      Tns == undefined -> Namespaces;
                      true -> 
                        [#ns{prefix=Prefix, uri=Tns} | Namespaces]
                   end}.

%% globalElementType
%% -record(globalElementType, {name, type, simpleOrComplex}).
transformTypes([#globalElementType{name=Name, type=undefined, simpleOrComplex=SorC,
                                   substitutionGroup = SubstGroup}| Tail],
                Acc = #p1acc{tps = ResultSoFar, nsp = Prefix, tns = Tns, nss = Nss}) ->
   SubstitutionGroup = case SubstGroup of 
                         undefined -> undefined;
                         _ -> erlsom_lib:makeElementRef(SubstGroup, Nss)
                       end,
   case SorC of 
      #localSimpleTypeType{} ->
        ThisType = #typeInfo{typeName= erlsom_lib:makeElementName(Name, Prefix), global = true
              ,         typeRef = "##string"
              ,         substitutionGroup = SubstitutionGroup
              ,         typeType = globalElementRefOnly},
         transformTypes(Tail, Acc#p1acc{tps = [ThisType | ResultSoFar]});
      #localComplexTypeType{} ->
        {Type, Acc2} = translateLocalComplexType(SorC, Acc#p1acc{path = Name ++ "/"}),
        ResultSoFar2 = Acc2#p1acc.tps,
        Type2 = Type#typeInfo{typeName= erlsom_lib:makeElementName(Name, Prefix), global = true, 
                              substitutionGroup = SubstitutionGroup,
                              typeType = globalElement},
        transformTypes(Tail, Acc2#p1acc{path = [], tps = [Type2 | ResultSoFar2]});
      undefined ->
        AnyInfo = #anyInfo{prCont = "lax", ns = "##any"}, 
        Element = #elementInfo{alternatives=[#alternative{tag="#any", type="any", real=true, anyInfo = AnyInfo}], 
                               min=1, max=1}, 
        ThisType = #typeInfo{typeName = erlsom_lib:makeElementName(Name, Prefix), global = true
                   ,         elements = [Element]
                   ,         substitutionGroup = SubstitutionGroup
                   ,         anyAttr = translateAnyAttrValue(default, Tns)
                   ,         typeType = globalElement},
        transformTypes(Tail, Acc#p1acc{tps = [ThisType | ResultSoFar]})
   end;

transformTypes(undefined, Acc) ->
  Acc;

transformTypes([#globalElementType{name=Name, type=Type, simpleOrComplex=undefined,
                                   substitutionGroup = SubstGroup}| Tail],
                Acc = #p1acc{tps = ResultSoFar, nsp = Prefix, nss = Nss}) ->
   ThisType = #typeInfo{typeName= erlsom_lib:makeElementName(Name, Prefix), global = true
              ,         typeRef = erlsom_lib:makeTypeRef(Type, Nss)
              ,         substitutionGroup = case SubstGroup of 
                                              undefined -> undefined;
                                              _ -> erlsom_lib:makeElementRef(SubstGroup, Nss)
                                            end
              ,         typeType = globalElementRefOnly},
   transformTypes(Tail, Acc#p1acc{tps = [ThisType | ResultSoFar]});

transformTypes([#globalComplexTypeType{name=Name, model=Model, attributes=Attributes, anyAttribute=AnyAttr, mixed = Mixed} |
                Tail],
                Acc = #p1acc{nsp = Prefix, tns = Tns}) ->
   Path = Name ++ "/",
   {Type, Acc2} = translateComplexTypeModel(Model, Acc#p1acc{path = Path}),
   TheAttributes = Type#typeInfo.attributes ++ translateAttributes(Attributes, Acc2),
   AnyAttrValue = if 
                     AnyAttr /= undefined -> translateAnyAttrValue(AnyAttr, Tns);
		     true -> Type#typeInfo.anyAttr
		  end,
   ResultSoFar = Acc2#p1acc.tps,
   Type2 = Type#typeInfo{typeName= erlsom_lib:makeTypeName(Name, Prefix), global = true, 
                         attributes=TheAttributes,
			 anyAttr = AnyAttrValue,
                         mixed = if Mixed -> Mixed; true -> Type#typeInfo.mixed end, 
                         typeType = type},
   transformTypes(Tail, Acc2#p1acc{path = [], tps = [Type2 | ResultSoFar]});

%%-record(globalSimpleTypeType, {name, annotation, model}).
%% TODO: better would be to avoid getting the 'any' stuff in the 
%% translation of the XSD alltogether.
%% transformTypes([#globalSimpleTypeType{name=Name, model=undefined}| Tail],
transformTypes([#globalSimpleTypeType{name=Name}| Tail],
                Acc = #p1acc{tps = ResultSoFar, nsp = Prefix}) ->
   Type = #typeInfo{typeName = erlsom_lib:makeTypeName(Name, Prefix), typeRef = "##string", global = false, 
                            typeType = simpleType},
   transformTypes(Tail, Acc#p1acc{tps = [Type | ResultSoFar]});

%%-record(groupDefType, {name, annotation, model}).
transformTypes([GroupDef = #groupDefType{}| Tail], Acc) ->
   {Type, Acc2} = transformGroupDef(GroupDef, Acc),
   ResultSoFar = Acc2#p1acc.tps,
   transformTypes(Tail, Acc2#p1acc{tps = [Type | ResultSoFar]});

%% -record(attributeGroupDefType, {id, name, annotation, attributes, anyAttribute}).
transformTypes([#attributeGroupDefType{name=Name, attributes=Atts, anyAttribute = AnyAttr}|Tail], 
                Acc = #p1acc{nsp = Prefix, tns = Tns}) ->
  %% parse the attributes
  AttGrp = #attGrp{name= Prefix ++ Name, atts = translateAttributes(Atts, Acc),
                   anyAttr = translateAnyAttrValue(AnyAttr, Tns)},
  ResultSoFar = Acc#p1acc.attGrps,
  transformTypes(Tail, Acc#p1acc{attGrps = [AttGrp | ResultSoFar]});

%% -record(globalAttributeType, {elInfo, name, type, use, model}).
transformTypes([#globalAttributeType{name=Name, type=Type, use = Use} |Tail], 
                Acc = #p1acc{nsp = Prefix}) ->
  Att = #attrib{name = Prefix ++ Name, type=Type, optional=Use},
  ResultSoFar = Acc#p1acc.atts,
  transformTypes(Tail, Acc#p1acc{atts = [Att | ResultSoFar]});

transformTypes([#annotationType{}|Tail], Acc) ->
  transformTypes(Tail, Acc);

transformTypes([], Acc = #p1acc{tps = Types}) ->
  Acc#p1acc{tps = lists:reverse(Types)}.

makeNewType(Name, Model, Acc = #p1acc{nsp = Prefix}) ->
   Path = Name ++ "/",
   {Type, Acc2} = translateComplexTypeModel(Model, Acc#p1acc{path = Path}),
   {Type#typeInfo{typeName= erlsom_lib:makeTypeName(Name, Prefix), global = false, typeType = type}, Acc2}.

%%-record(groupDefType, {name, annotation, model}).
transformGroupDef(#groupDefType{name=Name, model=Model}, 
                  Acc = #p1acc{nsp = Prefix}) ->
   Path = Name ++ "/",
   {Type, Acc2} = translateComplexTypeModel(Model, Acc#p1acc{path = Path}),
   {Type#typeInfo{typeName= erlsom_lib:makeGroupName(Name, Prefix), global = false, typeType = type}, Acc2}.


%% -record(localComplexTypeType, {annotation, model, attributes, anyAttribute}).
translateLocalComplexType(#localComplexTypeType{model=Model, attributes=Attributes, anyAttribute=AnyAttr, mixed = Mixed},
                          Acc = #p1acc{tns = Tns}) ->
  if 
    (Model == undefined) and (Mixed == true) ->
      Element = #elementInfo{alternatives=[#alternative{tag="#text", type="##string", real=false}], min=0, max=1}, 
      Type = #typeInfo{elements = [Element], seqOrAll = text}, 
      Acc2 = Acc;
    true ->
      {Type, Acc2} = translateComplexTypeModel(Model, Acc)
  end,
  TheAttributes = Type#typeInfo.attributes ++ translateAttributes(Attributes, Acc2),
  AnyAttrValue = case Type#typeInfo.anyAttr of 
                   undefined -> translateAnyAttrValue(AnyAttr, Tns);
                   Value -> Value %% in theory it could be defined both in the localComplexType and 
                                  %% in its 'model' - no idea what that would mean. For now assume 
                                  %% that the 'model' has the last word.
                 end,
  {Type#typeInfo{attributes = TheAttributes, global = false, anyAttr = AnyAttrValue, 
                 mixed = if Mixed -> Mixed; true -> Type#typeInfo.mixed end}, Acc2#p1acc{seqCnt = 0}}. 
   %% global = false???

%% -record(anyAttributeType, {elInfo, id, namespace, processContents, annotation}).
translateAnyAttrValue(undefined, _) ->
  undefined;
translateAnyAttrValue(default, _) ->
  %% TODO: set tns (and use it)
  #anyAttr{prCont = "lax", ns = "any"};
translateAnyAttrValue(#anyAttributeType{namespace = Ns, processContents = Pc}, Tns) ->
%% -record(anyAttr, {prCont, ns}).
  #anyAttr{prCont = Pc, ns = Ns, tns = Tns}.


%% -record(simpleContentType, {annotation, model}).
%% -record(extensionType, {annotation, attributes, anyAttribute}).
%% This is a bit weird - see it as a hook to do something else with the simpleContentType
translateSimpleContentTypeModel(#simpleContentType{model=#extensionType{attributes=Attributes, anyAttribute = AnyAttr}}, 
                                Acc = #p1acc{tns = Tns}) ->
   TheAttributes = translateAttributes(Attributes, Acc),
   AnyAttrValue = translateAnyAttrValue(AnyAttr, Tns),
   {TheAttributes, AnyAttrValue};
translateSimpleContentTypeModel(#simpleContentType{model=#restrictionType{attributes=Attributes, anyAttribute = AnyAttr}}, 
                                Acc = #p1acc{tns = Tns}) ->
   TheAttributes = translateAttributes(Attributes, Acc),
   AnyAttrValue = translateAnyAttrValue(AnyAttr, Tns),
   {TheAttributes, AnyAttrValue};
translateSimpleContentTypeModel(#simpleContentType{}, _Acc) ->
   {[], undefined}.

translateAttributes(undefined, _Acc) ->
  [];

translateAttributes(Attributes, Acc) ->
  translateAttributes(Attributes, Acc, []).

translateAttributes([Attribute | Tail], Acc, ResultSoFar) ->
  translateAttributes(Tail, Acc, [translateAttribute(Attribute, Acc) | ResultSoFar]);
 
translateAttributes([], _Acc, ResultSoFar) ->
  lists:reverse(ResultSoFar).

%% -record(localAttributeType, {name, type, use, model}).
translateAttribute(#localAttributeType{name=Name, type=Type, use=Use, ref=Ref,
                                       form=Form}, 
                   #p1acc{afd = Afd, nsp = Prefix}) ->
  Form2 = case Form of undefined -> Afd; _ -> Form end,
  #attrib{name = erlsom_lib:makeTag(Name, Prefix, Form2), type=Type, optional=Use, ref=Ref};

translateAttribute(GroupRef = #attributeGroupRefType{}, _Acc) ->
  GroupRef.

%% -record(sequenceType, {annotation, elements}).
%% returns {#typeInfo{}, #p1acc{}}
translateComplexTypeModel(undefined, Acc) ->
  {#typeInfo{elements = [], seqOrAll = sequence}, Acc};
translateComplexTypeModel(Sequence = #sequenceType{elements=Elements, minOccurs=Min, maxOccurs=Max},
                            %% Acc = #p1acc{seqCnt = Count, nsp = Prefix, path = Path, efd = Efd}) ->
                            Acc = #p1acc{seqCnt = Count, path = Path}) ->
  if 
    Min /= undefined; Max /= undefined ->
      %% create 1 element for the sequence (actuallly like a group), and a group-def-like type
      %% debug("sequence with mx > 0"),
      Count2 = Count + 1,
      Name = "SEQ" ++ integer_to_list(Count2),
      %% TypeName = erlsom_lib:makeTypeName(Name, Efd, Path, Prefix),
      {SeqType = #typeInfo{typeName = TN}, Acc2} = 
        makeNewType(Path ++ Name, Sequence#sequenceType{minOccurs = undefined, maxOccurs = undefined}, 
                                    Acc#p1acc{seqCnt = Count2}),
      Element = #elementInfo{alternatives=[#alternative{tag="##TODO", type= TN, real=false}], 
                     min=minMax(Min), max=minMax(Max)}, 
      ResultSoFar = Acc2#p1acc.tps,
      {#typeInfo{elements = [Element], seqOrAll = sequence, 
                 min = minMax(Min), max = minMax(Max)}, Acc2#p1acc{tps = [SeqType | ResultSoFar]}};
    true ->
      {Elements2, Acc2} = translateSequence(Elements, [], Acc),
      {#typeInfo{elements = Elements2, seqOrAll = sequence, 
                 min = minMax(Min), max = minMax(Max)}, Acc2}
  end;


%% special case for simpleContentType
translateComplexTypeModel(SCType = #simpleContentType{}, Acc = #p1acc{}) ->
  %% add a type for this element, with the attributes (if there are any).
  %% this will be a 'mixed' type: it will have a '#text' alternative
  Element = #elementInfo{alternatives=[#alternative{tag="#text", type="##string", real=false}], 
                                             min=0, max=1}, 
  {TheAttributes, AnyAttr} = translateSimpleContentTypeModel(SCType, Acc),
  {#typeInfo{elements = [Element], seqOrAll = text, attributes=TheAttributes, anyAttr = AnyAttr}, Acc};

translateComplexTypeModel(#complexContentType{model = Model, mixed = Mixed}, Acc) ->
  {Type, Acc2} = translateComplexTypeModel(Model, Acc),
  {Type#typeInfo{mixed = Mixed}, Acc2};

translateComplexTypeModel(#allType{elements=Elements, minOccurs=Min, maxOccurs=Max}, Acc) ->
  {Elements2, Acc2} = translateSequence(Elements, [], Acc),
  {#typeInfo{elements = Elements2, seqOrAll = all, 
             min = minMax(Min), max = minMax(Max)}, Acc2};

%% -record(anyType, {elInfo, any, minOccurs, maxOccurs, namespace, processContents}).
%% -record(anyInfo, {prCont, ns}). %% for any elements
%% -record(alt, {tag, tp, nxt = [], mn = 1, mx = 1, rl = true, anyInfo}).
translateComplexTypeModel(#anyType{minOccurs=Min, maxOccurs = Max, namespace = Ns, processContents = Pc}, 
                          Acc = #p1acc{tns = Tns}) ->
   AnyInfo = #anyInfo{prCont = case Pc of undefined -> "strict"; _ -> Pc end, 
                      ns = case Ns of undefined -> "##any"; _ -> Ns end,
                      tns = Tns}, 
   Elements = [#elementInfo{alternatives=[#alternative{tag="#any", type="any", real=true, anyInfo = AnyInfo}], 
                            min= minMax(Min), max= minMax(Max)}], 
   {#typeInfo{elements = Elements, seqOrAll = any}, Acc};
%% -record(groupRefType, {ref, minOccurs, maxOccurs}).
translateComplexTypeModel(#groupRefType{ref=Ref, minOccurs=Min, maxOccurs = Max}, Acc = #p1acc{nss = Nss}) ->
  Elements = [#elementInfo{alternatives=[#alternative{tag="##TODO", type=erlsom_lib:makeGroupRef(Ref, Nss), real=false}], 
                           min= minMax(Min), max= minMax(Max)}], 
  {#typeInfo{elements = Elements, seqOrAll = groupRef}, Acc};


%% -record(choiceType, {id, minOccurs, maxOccurs, alternatives}).
translateComplexTypeModel(#choiceType{minOccurs=Min, maxOccurs = Max, alternatives=Alternatives}, Acc) ->
  {TheAlternatives, Acc2} = translateAlternatives(Alternatives, [], Acc),
  Element = #elementInfo{alternatives=TheAlternatives, min=minMax(Min), max=minMax(Max)}, 
  {#typeInfo{elements = [Element], seqOrAll = choice}, Acc2};
  
  
%% -record(extensionTypeC, {base, annotation, model, attributes}).
translateComplexTypeModel(#extensionTypeC{base=Base, model = Model, attributes = Attributes, anyAttribute = AnyAttr}, 
                          Acc = #p1acc{tns = Tns}) ->
   {Type, Acc2} = translateComplexTypeModel(Model, Acc),
   TheAttributes = translateAttributes(Attributes, Acc),
   AnyAttrValue = translateAnyAttrValue(AnyAttr, Tns),
   %% add the relation to the type hierarchy
   {Type#typeInfo{extends = Base, attributes = TheAttributes, anyAttr = AnyAttrValue, base = Base}, Acc2};

%% -record(restrictionTypeC, {base, annotation, model, attributes}).
translateComplexTypeModel(#restrictionTypeC{base = Base, model = Model, attributes = Attributes, anyAttribute = AnyAttr}, 
                          Acc) ->
   {Type, Acc2} = translateComplexTypeModel(Model, Acc),
   TheAttributes = translateAttributes(Attributes, Acc),
   %% add the relation to the type hierarchy
   {Type#typeInfo{attributes = TheAttributes, anyAttr = AnyAttr, restricts = Base, base = Base}, Acc2}. 

translateAlternatives([Alternative | Tail], AlternativesSoFar, Acc) ->
  Path = Acc#p1acc.path,
  {TheAlternative, Acc2} = translateAlternative(Alternative, Acc),
  translateAlternatives(Tail, [TheAlternative | AlternativesSoFar], Acc2#p1acc{path = Path});

translateAlternatives([], AlternativesSoFar, Acc) ->
  {lists:reverse(AlternativesSoFar), Acc}.

%% TODO: seqCnt should be passed from one element to the next, but not down into the
%% tree (it should be reset to 0) (the first sequence-in-sequnce should always have nr. 1).
%% -record(localElementType, {name, type, ref, minOccurs, maxOccurs, simpleOrComplex}).
translateSequence([LocalElement = #localElementType{} |Tail], ElementsSoFar, Acc) ->
  {Element, Acc2} = translateLocalElement(LocalElement, Acc),
  translateSequence(Tail, [Element | ElementsSoFar], Acc2);
translateSequence([ChoiceElement = #choiceType{} |Tail], ElementsSoFar, Acc) ->
  {Element, Acc2} = translateElement(ChoiceElement, Acc),
  translateSequence(Tail, [Element | ElementsSoFar], Acc2);
translateSequence([AnyElement = #anyType{} |Tail], ElementsSoFar, Acc) ->
  {Element, Acc2} = translateElement(AnyElement, Acc),
  translateSequence(Tail, [Element | ElementsSoFar], Acc2);
translateSequence([GroupElement = #groupRefType{} |Tail], ElementsSoFar, Acc) ->
  {Element, Acc2} = translateElement(GroupElement, Acc),
  translateSequence(Tail, [Element | ElementsSoFar], Acc2);
translateSequence([SequenceElement = #sequenceType{} | Tail], ElementsSoFar, Acc) ->
  {Element, Acc2} = translateSequenceInSequence(SequenceElement, Acc),
  translateSequence(Tail, [Element | ElementsSoFar], Acc2);

translateSequence(undefined, ElementsSoFar, Acc) ->
  {lists:reverse(ElementsSoFar), Acc};
translateSequence([], ElementsSoFar, Acc) ->
  {lists:reverse(ElementsSoFar), Acc}.
  
translateElement(#choiceType{minOccurs=Min, maxOccurs = Max, alternatives=Alternatives}, Acc) ->
  {TheAlternatives, Acc2} = translateAlternatives(Alternatives, [], Acc),
  Element = #elementInfo{alternatives=TheAlternatives, min=minMax(Min), max=minMax(Max)}, 
  {Element, Acc2};

translateElement(#anyType{minOccurs=Min, maxOccurs = Max, namespace = Ns, processContents = Pc}, 
                 Acc = #p1acc{tns = Tns}) ->
   AnyInfo = #anyInfo{prCont = case Pc of undefined -> "strict"; _ -> Pc end, 
                      ns = case Ns of undefined -> "##any"; _ -> Ns end,
                      tns = Tns}, 
  Element = #elementInfo{alternatives=[#alternative{tag="#any", type="any", real=true, anyInfo = AnyInfo}], min=minMax(Min), 
                         max=minMax(Max)}, 
  {Element, Acc};

translateElement(#groupRefType{ref=Ref, minOccurs=Min, maxOccurs = Max}, Acc = #p1acc{nss = Nss}) ->
  Element = #elementInfo{alternatives=[#alternative{tag="##TODO", type=erlsom_lib:makeGroupRef(Ref, Nss), real=false}], 
                           min=minMax(Min), max=minMax(Max)}, 
  {Element, Acc}.

translateSequenceInSequence(Sequence = #sequenceType{minOccurs=Min, maxOccurs=Max}, 
                            %% Acc = #p1acc{seqCnt = Count, nsp = Prefix, path = Path, efd = Efd}) ->
                            Acc = #p1acc{seqCnt = Count, path = Path}) ->
  %% sequence in sequence is like a group-ref. The sequence itself is the group.
  Count2 = Count + 1,
  Name = "SEQ" ++ integer_to_list(Count2),
  %% TypeName = erlsom_lib:makeTypeName(Name, Efd, Path, Prefix),
  {SeqType = #typeInfo{typeName = TN}, Acc2} = 
    makeNewType(Path ++ Name, Sequence#sequenceType{minOccurs = undefined, maxOccurs = undefined}, 
                                Acc#p1acc{seqCnt = Count2}),
  Element = #elementInfo{alternatives=[#alternative{tag="##TODO", type= TN, real=false}], 
                 min=minMax(Min), max=minMax(Max)}, 
  ResultSoFar = Acc2#p1acc.tps,
  {Element, Acc2#p1acc{tps = [SeqType | ResultSoFar], path = Path}}.

%% -record(elementInfo, {alternatives, min, max}).
%% -record(alternative, {tag, type, real}).
translateLocalElement(Element = #localElementType{minOccurs=Min, maxOccurs = Max}, Acc) ->
  {Alternative, Acc2} = translateQuasiAlternative(Element, Acc),
  {#elementInfo{alternatives= [Alternative], min=minMax(Min), max=minMax(Max)}, Acc2}.

translateAlternative(#localElementType{type=undefined, ref=Ref, simpleOrComplex=undefined,
                                       minOccurs=Min, maxOccurs=Max}, Acc = #p1acc{nss = Nss}) when
  Ref /= undefined ->
  {#alternative{tag= erlsom_lib:makeTagFromRef(Ref, Nss), type=erlsom_lib:makeElementRef(Ref, Nss), real = true,
                min=minMax(Min), max=minMax(Max)}, Acc};

%% A child element exists - russian doll design
translateAlternative(#localElementType{name=Name, type=undefined, ref=undefined, simpleOrComplex=SorC,
                     minOccurs=Min, maxOccurs=Max, form=Form}, Acc = #p1acc{efd = Efd, nsp = Prefix, path = Path}) 
   when SorC /= undefined ->
   Form2 = case Form of undefined -> Efd; _ -> Form end,
   case SorC of 
      #localSimpleTypeType{} ->
         TypeRef = "##string",
         Acc3 = Acc;
      #localComplexTypeType{} ->
         {Type, Acc2} = translateLocalComplexType(SorC, Acc#p1acc{path= Path ++ Name ++ "/"}),
         ResultSoFar = Acc2#p1acc.tps,
         TypeRef = erlsom_lib:makeTypeName(Name, Form2, Path, Prefix),
         ThisType = Type#typeInfo{typeName= TypeRef, global = false, 
                                  typeType = localElement},
         Types = [ThisType | ResultSoFar],
         Acc3 = Acc2#p1acc{tps = Types}
   end,
   {#alternative{tag=erlsom_lib:makeTag(Name, Prefix, Efd), type=TypeRef, real = true, min=minMax(Min), 
                 max=minMax(Max)}, Acc3};

translateAlternative(#localElementType{name=Name, type=Type, ref=undefined, simpleOrComplex=undefined,
                                       minOccurs=Min, maxOccurs = Max, form = Form}, 
                     Acc = #p1acc{efd = Efd, nsp = Prefix, nss = Nss}) ->
   Form2 = case Form of undefined -> Efd; _ -> Form end,
   {#alternative{tag=erlsom_lib:makeTag(Name, Prefix, Form2), type=erlsom_lib:makeTypeRef(Type, Nss), 
                 real = true, min=minMax(Min), max=minMax(Max)}, Acc};
%% -record(groupRefType, {ref, minOccurs, maxOccurs}).
translateAlternative(#groupRefType{ref=Ref, minOccurs=Min, maxOccurs=Max}, Acc = #p1acc{nss = Nss}) ->
  {#alternative{tag="##TODO", type=erlsom_lib:makeGroupRef(Ref, Nss), real=false, min=minMax(Min), max=minMax(Max)}, Acc};
translateAlternative(#anyType{minOccurs=Min, maxOccurs = Max, namespace = Ns, processContents = Pc}, Acc) ->
   AnyInfo = #anyInfo{prCont = case Pc of undefined -> "strict"; _ -> Pc end, 
                     ns = case Ns of undefined -> "##any"; _ -> Ns end}, 
  {#alternative{tag="#any", type="any", real=true, anyInfo = AnyInfo}, Acc};
translateAlternative(Sequence = #sequenceType{minOccurs=Min, maxOccurs=Max}, 
                     Acc = #p1acc{efd = Efd, nsp = Prefix, path = Path, seqCnt = Count}) ->
  Count2 = Count + 1,
  Name = "SEQ" ++ integer_to_list(Count2),
  TypeName = erlsom_lib:makeTypeName(Name, Efd, Path, Prefix),
  %% !! TODO: should minOccurs and maxOccurs be copied to the 
  %% alternative or to the new type?
  Alternative = #alternative{tag="##TODO", type=TypeName, real=false, 
                             min=minMax(Min), max=minMax(Max)}, 
  {SeqType, Acc2} = makeNewType(Path ++ Name, Sequence#sequenceType{minOccurs = undefined, maxOccurs = undefined}, 
                                Acc#p1acc{seqCnt = Count2}),
  ResultSoFar = Acc2#p1acc.tps,
  {Alternative, Acc2#p1acc{tps = [SeqType | ResultSoFar], path = Path}};

translateAlternative(Choice = #choiceType{minOccurs=Min, maxOccurs=Max}, 
                     Acc = #p1acc{efd = Efd, nsp = Prefix, path = Path, seqCnt = Count}) ->
  Count2 = Count + 1,
  Name = "CH" ++ integer_to_list(Count2),
  TypeName = erlsom_lib:makeTypeName(Name, Efd, Path, Prefix),
  Alternative = #alternative{tag="##TODO", type=TypeName, real=false, 
                             min=1, max=1}, 
  {ChoiceType, Acc2} = makeNewType(Path ++ Name, Choice#choiceType{minOccurs = Min, maxOccurs = Max}, 
                                   Acc#p1acc{seqCnt = Count2}),
  ResultSoFar = Acc2#p1acc.tps,
  {Alternative, Acc2#p1acc{tps = [ChoiceType | ResultSoFar], path = Path}}.

%% quasi alternative - outside a choice
translateQuasiAlternative(#localElementType{type=undefined, ref=Ref, simpleOrComplex=undefined}, 
                                            Acc = #p1acc{nss = Nss}) when Ref /= undefined ->
  {#alternative{tag= erlsom_lib:makeTagFromRef(Ref, Nss), type=erlsom_lib:makeElementRef(Ref, Nss), real = true}, Acc};

%% A child element exists - russian doll design
translateQuasiAlternative(#localElementType{name=Name, type=undefined, ref=undefined, simpleOrComplex=SorC,
                                            form=Form},
                          Acc = #p1acc{efd = Efd, nsp = Prefix, path = Path})
   when SorC /= undefined ->
   Form2 = case Form of undefined -> Efd; _ -> Form end,
   case SorC of 
      #localSimpleTypeType{} ->
         TypeRef = "##string",
         Acc3 = Acc;
      #localComplexTypeType{} ->
         {Type, Acc2} = translateLocalComplexType(SorC, Acc#p1acc{path= Path ++ Name ++ "/"}),
         ResultSoFar = Acc2#p1acc.tps,
         TypeRef = erlsom_lib:makeTypeName(Name, Form2, Path, Prefix),
         ThisType = Type#typeInfo{typeName= TypeRef, global = false, 
                                  typeType = localElement},
         Types = [ThisType | ResultSoFar],
         Acc3 = Acc2#p1acc{tps = Types}
   end,
   {#alternative{tag=erlsom_lib:makeTag(Name, Prefix, Efd), type=TypeRef, real = true}, Acc3#p1acc{path=Path}};

translateQuasiAlternative(#localElementType{name=Name, type=Type, ref=undefined, simpleOrComplex=undefined, form = Form}, 
                          Acc = #p1acc{efd = Efd, nsp = Prefix, nss = Nss}) ->
   Form2 = case Form of undefined -> Efd; _ -> Form end,
   {#alternative{tag=erlsom_lib:makeTag(Name, Prefix, Form2), type=erlsom_lib:makeTypeRef(Type, Nss), real = true}, Acc};
%% -record(groupRefType, {ref, minOccurs, maxOccurs}).
translateQuasiAlternative(#groupRefType{ref=Ref}, Acc = #p1acc{nss = Nss}) ->
  {#alternative{tag="##TODO", type=erlsom_lib:makeGroupRef(Ref, Nss), real=false}, Acc}.

%% used to process the old (deprecated) form to pass namespace info to the compiler.
%% each #ns{namespace, prefix} record has to be transated to {Namespace, Prefix, undefined}
%% translateNs(#ns{prefix = Prefix, uri = Ns}) ->
  %% {Ns, Prefix, undefined}.
 %% TODO: sort this out - what is correct? 
translateNs(X) ->
  X.
