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
%%% Applies Erlsom to an XSD.
%%% ====================================================================

%% read the xsd-grammar with the model below.
-module(erlsom_parseXsd).
-export([parseXsd/2]).
-export([xsdModel/1, xsdModel/0]).

-include_lib("erlsom_parse.hrl"). %% the record definitions
-include_lib("erlsom.hrl"). %% the record definitions

%% debug(Text) ->
  %% io:format("parseXsd: ~p\n", [Text]).

%% TODO: this shouldn't have a Namespaces parameter.
%% The model is the XSD Model, the namespaces are for the xsd that is going
%% to be parsed. There is no good reason to have this parameter here.
xsdModel(Namespaces) ->
  AnyAttr = #anyAttr{ns = "##other"},
  AnyInfo = #anyInfo{ns = "##other"},
  #model{tps = [
       #type{nm  = '_document',
	     els = [#el{alts = [#alt{tag = 'xsd:schema', tp = 'schemaType'}, 
                                #alt{tag = 'xsd:element', tp = 'globalElementType'},
                                #alt{tag = 'xsd:complexType', tp = 'globalComplexTypeType'},
                                #alt{tag = 'xsd:simpleType', tp = 'globalSimpleTypeType'},
                                #alt{tag = 'xsd:attribute', tp = 'globalAttributeType'},
                                #alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupDefType'},
                                #alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        nr   = 1}], 
             nr  = 1},

%% -record(schemaType, {targetNamespace, elementFormDefault, imports, elements}).
       #type{nm  = schemaType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:import', tp = 'importType'},
                                #alt{tag = 'xsd:include', tp = 'includeType'},
                                #alt{tag = 'xsd:redefine', tp = 'redefineType'},
                                #alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn   = 0, 
                        mx   = unbound, 
                        nr   = 8},
                    #el{alts = [#alt{tag = 'xsd:element', tp = 'globalElementType'},
                                #alt{tag = 'xsd:complexType', tp = 'globalComplexTypeType'},
                                #alt{tag = 'xsd:simpleType', tp = 'globalSimpleTypeType'},
                                #alt{tag = 'xsd:attribute', tp = 'globalAttributeType'},
                                #alt{tag = 'xsd:group', tp = 'groupDefType'},
                                #alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupDefType'},
                                #alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 9}], 
	     atts = [#att{nm = targetNamespace, nr = 1, opt = true, tp = char},
                     #att{nm = elementFormDefault, nr = 2, opt = true, tp = char}, 
                     #att{nm = attributeFormDefault, nr = 3, opt = true, tp = char}, 
                     #att{nm = blockDefault, nr = 4, opt = true, tp = char}, 
                     #att{nm = finalDefault, nr = 5, opt = true, tp = char}, 
                     #att{nm = version, nr = 6, opt = true, tp = char}, 
                     #att{nm = id, nr = 7, opt = true, tp = char}], 
             nr = 10},

%% -record(importType, {id, namespace, schemaLocation}).
%% global attribute not supported
       #type{nm  = importType,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 4}],
             atts = [#att{nm = id, nr = 1, opt = true, tp = char},
                     #att{nm = namespace, nr = 2, opt = true, tp = char},
                     #att{nm = schemaLocation, nr = 3, opt = true, tp = char}], 
             nr = 5},

%% -record(includeType, {id, schemaLocation, annotation}).
       #type{nm  = includeType,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 3}],
             atts = [#att{nm = id, nr = 1, opt = true, tp = char},
                     #att{nm = schemaLocation, nr = 2, opt = false, tp = char}], 
             nr = 4},

%% -record(redefineType, ....
       #type{nm  = redefineType,
	     els = [#el{alts = [#alt{tag = 'xsd:complexType', tp = 'globalComplexTypeType'},
                                #alt{tag = 'xsd:simpleType', tp = 'globalSimpleTypeType'},
                                #alt{tag = 'xsd:group', tp = 'groupDefType'},
                                #alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupDefType'},
                                #alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 3}],
             atts = [#att{nm = id, nr = 1, opt = true, tp = char},
                     #att{nm = schemaLocation, nr = 2, opt = false, tp = char}], 
             nr = 4},

%% -record(globalElementType, {annotation, name, type, simpleOrComplex}).
       #type{nm = globalElementType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 10},
	            #el{alts = [#alt{tag = 'xsd:simpleType', tp = localSimpleTypeType},
	                        #alt{tag = 'xsd:complexType', tp = localComplexTypeType}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 11},
	            #el{alts = [#alt{tag = 'xsd:unique', tp = ignoreType},
	                        #alt{tag = 'xsd:key', tp = ignoreType},
	                        #alt{tag = 'xsd:keyref', tp = ignoreType}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 12}],
	     atts = [#att{nm = name, nr = 1, opt = false, tp = ascii}, 
	             #att{nm = type, nr = 2, opt = true, tp = qname},
	             #att{nm = default, nr = 3, opt = true, tp = char},
	             #att{nm = fixed, nr = 4, opt = true, tp = char},
	             #att{nm = id, nr = 5, opt = true, tp = char},
	             #att{nm = abstract, nr = 6, opt = true, tp = bool},
	             #att{nm = substitutionGroup, nr = 7, opt = true, tp = qname},
	             #att{nm = final, nr = 8, opt = true, tp = char},
	             #att{nm = nillable, nr = 9, opt = true, tp = bool}], 
             nr = 13},

%% -record(localElementType, {name, type, ref, minOccurs, maxOccurs, simpleOrComplex}).
       #type{nm = localElementType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 10},
	            #el{alts = [#alt{tag = 'xsd:simpleType', tp = localSimpleTypeType},
	                        #alt{tag = 'xsd:complexType', tp = localComplexTypeType}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 11},
	            #el{alts = [#alt{tag = 'xsd:unique', tp = ignoreType},
	                        #alt{tag = 'xsd:key', tp = ignoreType},
	                        #alt{tag = 'xsd:keyref', tp = ignoreType}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 12}],
	     atts = [#att{nm = name, nr = 1, opt = true, tp = ascii}, 
	             #att{nm = type, nr = 2, opt = true, tp = qname},
	             #att{nm = default, nr = 3, opt = true, tp = char},
	             #att{nm = fixed, nr = 4, opt = true, tp = char},
	             #att{nm = form, nr = 5, opt = true, tp = char},
	             #att{nm = ref, nr = 6, opt = true, tp = qname},
                     #att{nm = minOccurs, nr = 7, opt = true, tp = char},
                     #att{nm = maxOccurs, nr = 8, opt = true, tp = char}, 
	             #att{nm = nillable, nr = 9, opt = true, tp = bool}], 
             nr = 13},       

%% -record(globalComplexTypeType, {name, annotation, model, attributes}).
       #type{nm = globalComplexTypeType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 7},
                    #el{alts = [#alt{tag = 'xsd:simpleContent', tp = 'simpleContentType'},
                                #alt{tag = 'xsd:complexContent', tp = 'complexContentType'},
                                #alt{tag = 'xsd:group', tp = 'groupRefType'},
                                #alt{tag = 'xsd:all', tp = 'allType'},
                                #alt{tag = 'xsd:choice', tp = 'choiceType'},
                                #alt{tag = 'xsd:sequence', tp = 'sequenceType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 8}, %% can be more than 1??
                    #el{alts = [#alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupRefType'},
                                #alt{tag = 'xsd:attribute', tp = 'localAttributeType'}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 9},
                    #el{alts = [#alt{tag = 'xsd:anyAttribute', tp = 'anyAttributeType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 10}],
	     atts = [#att{nm = name, nr = 1, opt = false, tp = ascii},
	             #att{nm = final, nr = 2, opt = true, tp = char},
	             #att{nm = abstract, nr = 3, opt = true, tp = bool},
	             #att{nm = block, nr = 4, opt = true, tp = char},
	             #att{nm = mixed, nr = 5, opt = true, tp = bool}, 
	             #att{nm = id, nr = 6, opt = true, tp = char}], 
             nr = 11},       

%% -record(localComplexTypeType, {annotation, model, attributes}).
       #type{nm = localComplexTypeType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 2},
                    #el{alts = [#alt{tag = 'xsd:simpleContent', tp = 'simpleContentType'},
                                #alt{tag = 'xsd:group', tp = 'groupRefType'},
                                #alt{tag = 'xsd:complexContent', tp = 'complexContentType'},
                                #alt{tag = 'xsd:all', tp = 'allType'}, 
                                #alt{tag = 'xsd:choice', tp = 'choiceType'},
                                #alt{tag = 'xsd:sequence', tp = 'sequenceType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 3}, %% can be more than 1???
                    #el{alts = [#alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupRefType'},
                                #alt{tag = 'xsd:attribute', tp = 'localAttributeType'}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 4},
                    #el{alts = [#alt{tag = 'xsd:anyAttribute', tp = 'anyAttributeType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 5}],
	     atts = [#att{nm = mixed, nr = 1, opt = true, tp = bool}], 
             nr = 6},      

%% -record(globalSimpleTypeType, {name, annotation, model}).
%% simpleType - restriction, list and union are ignored.
       #type{nm = globalSimpleTypeType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 3},
                    #el{alts =  [#alt{tag = '#any', tp = any, nxt = [], anyInfo = AnyInfo}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 4}],
	     atts = [#att{nm = name, nr = 1, opt = false, tp = ascii}, 
	             #att{nm = id, nr = 2, opt = true, tp = char}],
             nr = 5},      

%% -record(localSimpleTypeType, {annotation, model}).
%% simpleType - restriction, list and union are ignored.
       #type{nm = localSimpleTypeType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 1},
                    #el{alts = [#alt{tag = '#any', tp = any, nxt = [], anyInfo = AnyInfo}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 2}],
	     atts = [], 
             nr = 3},       

%% -record(simpleContentType, {annotation, model}).
%% simpleContent - extension can contain attibutes, so we have to parse
%% one additional level.
       #type{nm = simpleContentType,
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 1},
                    #el{alts = [#alt{tag = 'xsd:extension', tp = 'extensionType'},
                               #alt{tag = 'xsd:restriction', tp = 'restrictionType'}], 
                        mn = 1, 
                        mx = 1, 
                        nr = 2}],
	     atts = [#att{nm = id, nr = 3, opt = true, tp = char}],
             nr = 4},       

%% -record(complexContentType, {annotation, model}).
       #type{nm = complexContentType,
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 1},
                    #el{alts = [#alt{tag = 'xsd:extension', tp = 'extensionTypeC'},
                               #alt{tag = 'xsd:restriction', tp = 'restrictionTypeC'}], 
                        mn = 1, 
                        mx = 1, 
                        nr = 2}],
	     atts = [#att{nm = mixed, nr = 3, opt = true, tp = bool}], 
             nr = 4},       

%% -record(extensionType, {base, annotation, attributes, anyAttribute}).
       #type{nm = extensionType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 2},
                    #el{alts = [#alt{tag = 'xsd:attribute', tp = 'localAttributeType'}, 
                                #alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupRefType'}],
                        mn = 0, 
                        mx = unbound, 
                        nr = 3},
                    #el{alts = [#alt{tag = 'xsd:anyAttribute', tp = 'anyAttributeType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 4}
                        ],
	     atts = [#att{nm = base, nr = 1, opt = false, tp = qname}], 
             nr = 5},

%% -record(restrictionType, {annotation, any}).
       #type{nm = restrictionType,
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 1},
                    #el{alts = [#alt{tag = '#any', tp = any, nxt = ['xsd:attribute',
                                                                    'xsd:attributeGroup',
                                                                    'xsd:anyAttribute'], anyInfo = AnyInfo}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 2},
                    #el{alts = [#alt{tag = 'xsd:attribute', tp = 'localAttributeType'}, 
                                #alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupRefType'}],
                        mn = 0, 
                        mx = unbound, 
                        nr = 3},
                    #el{alts = [#alt{tag = 'xsd:anyAttribute', tp = 'anyAttributeType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 4}
                        ],
	     atts = [#att{nm = base, nr = 5, opt = false, tp = qname}], 
             nr = 6},

%% as part of a complexType
%% -record(extensionTypeC, {base, annotation, model, attributes, anyAttributes}).
       #type{nm = extensionTypeC, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 2},
                    #el{alts = [#alt{tag = 'xsd:sequence', tp = 'sequenceType'}, 
                                #alt{tag = 'xsd:choice', tp = 'choiceType'},
                                #alt{tag = 'xsd:group', tp = 'groupRefType'}],
                        mn = 0, 
                        mx = 1, 
                        nr = 3},
                    #el{alts = [#alt{tag = 'xsd:attribute', tp = 'localAttributeType'}, 
                                #alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupRefType'}],
                        mn = 0, 
                        mx = unbound, 
                        nr = 4},
                    #el{alts = [#alt{tag = 'xsd:anyAttribute', tp = 'anyAttributeType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 5}],
	     atts = [#att{nm = base, nr = 1, opt = false, tp = qname}], 
             nr = 6},

%% as part of a complexType
%% -record(restrictionTypeC, {base, annotation, model, attributes}).
       #type{nm = restrictionTypeC,
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 2},
                    #el{alts = [#alt{tag = 'xsd:sequence', tp = 'sequenceType'}, 
                                #alt{tag = 'xsd:group', tp = 'groupRefType'}],
                        mn = 0, 
                        mx = 1, 
                        nr = 3},
                    #el{alts = [#alt{tag = 'xsd:attribute', tp = 'localAttributeType'}, 
                                #alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupRefType'}],
                        mn = 0, 
                        mx = unbound, 
                        nr = 4},
                    #el{alts = [#alt{tag = 'xsd:anyAttribute', tp = 'anyAttributeType'}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 5}],
	     atts = [#att{nm = base, nr = 1, opt = false, tp = qname}], 
             nr = 6},

%% -record(groupDefType, {name, annotation, model}).
%% groupDefType - definition of a group
       #type{nm = groupDefType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 2},
                    #el{alts = [#alt{tag = 'xsd:all', tp = 'allType'},
                                #alt{tag = 'xsd:choice', tp = 'choiceType'},
                                #alt{tag = 'xsd:sequence', tp = 'sequenceType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 3}],
	     atts = [#att{nm = name, nr = 1, opt = false, tp = ascii}], 
             nr = 4},      

%% -record(anyAttributeType, {elInfo, id, namespace, processContents, annotation}).
       #type{nm = anyAttributeType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 4}],
	     atts = [#att{nm = id, nr = 1, opt = true, tp = char},
                     #att{nm = namespace, nr = 2, opt = true, tp = char},
                     #att{nm = processContents, nr = 3, opt = true, tp = char}], 
             nr = 5},      

%% -record(groupRefType, {ref, minOccurs, maxOccurs}).
%% groupRefType - reference to a group
       #type{nm = groupRefType, 
             anyAttr = AnyAttr,
	     els = [],
	     atts = [#att{nm = ref, nr = 1, opt = false, tp = qname},
                     #att{nm = minOccurs, nr = 2, opt = true, tp = char},
                     #att{nm = maxOccurs, nr = 3, opt = true, tp = char}], 
             nr = 4},      

%% -record(annotationType, {annotation}).
%% annotationType - completely ignored
       #type{nm = annotationType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = '#any', tp = any, nxt = [], anyInfo = AnyInfo}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 1}],
	     atts = [], 
             nr = 2},      

%% completely ignored. Used for key keyref and unique
       #type{nm = ignoreType, 
             anyAttr = #anyAttr{ns = "##any"},
	     els = [#el{alts = [#alt{tag = '#any', tp = any, nxt = [], anyInfo = AnyInfo}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 1}],
	     atts = [], 
             nr = 2},      

%% -record(globalAttributeType, {name, type, model}).
%% attributeType - fixed is ignored, default not supported, simpleType
%% is ignored.
       #type{nm = globalAttributeType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = '#any', tp = any, nxt = [], anyInfo = AnyInfo}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 7}],
	     atts = [#att{nm = name, nr = 1, opt = false, tp = ascii},
                     #att{nm = type, nr = 2, opt = true, tp = qname}, 
                     #att{nm = use, nr = 3, opt = true, tp = char}, 
                     #att{nm = fixed, nr = 4, opt = true, tp = char},
                     #att{nm = default, nr = 5, opt = true, tp = char}, 
                     #att{nm = id, nr = 6, opt = true, tp = char}], 
             nr = 8},      

%% -record(localAttributeType, {name, type, use, model}).
%% attributeType - fixed is ignored, default not supported, simpleType
%% is ignored.
       #type{nm = localAttributeType,
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = '#any', tp = any, nxt = [], anyInfo = AnyInfo}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 8}],
	     atts = [#att{nm = name, nr = 1, opt = true, tp = ascii},
                     #att{nm = type, nr = 2, opt = true, tp = qname},
                     #att{nm = use, nr = 3, opt = true, tp = char}, 
                     #att{nm = ref, nr = 4, opt = true, tp = qname},
                     #att{nm = fixed, nr = 5, opt = true, tp = char}, 
                     #att{nm = form, nr = 6, opt = true, tp = char}, 
                     #att{nm = default, nr = 7, opt = true, tp = char}], 
             nr = 9},      

%% -record(choiceType, {id, minOccurs, maxOccurs, alternatives}).
       #type{nm = choiceType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 1},
	            #el{alts = [#alt{tag = 'xsd:element', tp = 'localElementType'}, %!?! localElementType?
                                #alt{tag = 'xsd:group', tp = 'groupRefType'},
                                #alt{tag = 'xsd:choice', tp = 'choiceType'},
                                #alt{tag = 'xsd:sequence', tp = 'sequenceType'},
                                #alt{tag = 'xsd:any', tp = 'anyType', nxt = []}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 5}],
	     atts = [#att{nm = id, nr = 1, opt = true, tp = char},
                     #att{nm = minOccurs, nr = 2, opt = true, tp = char},
                     #att{nm = maxOccurs, nr = 3, opt = true, tp = char}], 
             nr = 6},

%% -record(sequenceType, {annotation, elements}).
       #type{nm = sequenceType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 1},
                    #el{alts = [#alt{tag = 'xsd:element', tp = 'localElementType'},
                                #alt{tag = 'xsd:group', tp = 'groupRefType'},
                                #alt{tag = 'xsd:any', tp = 'anyType'},
                                #alt{tag = 'xsd:choice', tp = 'choiceType'},
                                #alt{tag = 'xsd:sequence', tp = 'sequenceType'}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 2}],
	     atts = [#att{nm = minOccurs, nr = 3, opt = true, tp = char},
                     #att{nm = maxOccurs, nr = 4, opt = true, tp = char}], 
             nr = 5},       

%% -record(sequenceType, {annotation, elements}).
       #type{nm = allType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 1},
                    #el{alts = [#alt{tag = 'xsd:element', tp = 'localElementType'},
                                #alt{tag = 'xsd:group', tp = 'groupRefType'},
                                #alt{tag = 'xsd:choice', tp = 'choiceType'},
                                #alt{tag = 'xsd:sequence', tp = 'sequenceType'}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 2}],
	     atts = [#att{nm = minOccurs, nr = 3, opt = true, tp = char},
                     #att{nm = maxOccurs, nr = 4, opt = true, tp = char}], 
             nr = 5},       

%% -record(anyType, {elInfo, any, minOccurs, maxOccurs, namespace, processContents}).
%% annotationType - completely ignored
       #type{nm = anyType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = '#any', tp = any, nxt = [], anyInfo = AnyInfo}], 
                        mn = 0, 
                        mx = unbound, 
                        nr = 1}],
	     atts = [#att{nm = minOccurs, nr = 2, opt = true, tp = char},
                     #att{nm = maxOccurs, nr = 3, opt = true, tp = char},
                     #att{nm = namespace, nr = 4, opt = true, tp = char},
                     #att{nm = processContents, nr = 5, opt = true, tp = char}], 
             nr = 6},      

%% -record(attributeGroupDefType, {id, name, annotation, attributes, anyAttribute}).
%% not supported: references to other groups or attributes, anyAttribute
       #type{nm = attributeGroupDefType, 
             anyAttr = AnyAttr,
	     els = [#el{alts = [#alt{tag = 'xsd:annotation', tp = 'annotationType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 3},
                    #el{alts = [#alt{tag = 'xsd:attribute', tp = 'localAttributeType'}, 
                                #alt{tag = 'xsd:attributeGroup', tp = 'attributeGroupRefType'}],
                        mn = 0, 
                        mx = unbound, 
                        nr = 4},
                    #el{alts = [#alt{tag = 'xsd:anyAttribute', tp = 'anyAttributeType'}], 
                        mn = 0, 
                        mx = 1, 
                        nr = 5}],
	     atts = [#att{nm = id, nr = 1, opt = true, tp = char},
                     #att{nm = name, nr = 2, opt = false, tp = ascii}], 
             nr = 6},


%% -record(attributeGroupRefType, {ref}).
       #type{nm = attributeGroupRefType, 
             anyAttr = AnyAttr,
	     els = [],
	     atts = [#att{nm = ref, nr = 1, opt = true, tp = qname}, 
	             #att{nm = id, nr = 2, opt = true, tp = char}],
             nr = 3}], 
       nss = [#ns{prefix = "xsd", uri = "http://www.w3.org/2001/XMLSchema"} | Namespaces],
       th = []}.

xsdModel() ->
  xsdModel([]).

parseXsd(Xsd, Namespaces) ->
  %% -record(anyAttr, {prCont, ns}). %% for anyAttributes
  GrammarModel = xsdModel(Namespaces),
  {ok, ParsedXsd, _Tail} = 
    erlsom_sax:parseDocument(Xsd, GrammarModel, fun erlsom_parse:xml2StructCallback/2),
  ParsedXsd.
