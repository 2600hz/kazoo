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
%%% Header file for erlsom_compile
%%% ====================================================================


%% records for the structures as found in the XSD
-record(schemaType, 
        {elInfo, targetNamespace, elementFormDefault, attributeFormDefault, blockDefault, finalDefault, 
         version, id, imports, elements}).
-record(importType, {elInfo, id, namespace, schemaLocation, annotation}).
-record(includeType, {elInfo, id, schemaLocation, annotation}).
-record(redefineType, {elInfo, id, schemaLocation, elements}).
-record(globalElementType, {elInfo, name, type, default, fixed, id, abstract, substitutionGroup, final, nillable, 
                            annotation, simpleOrComplex, unique}).
-record(localElementType, {elInfo, name, type, default, fixed, form, ref, minOccurs, maxOccurs, nillable, annotation, 
                           simpleOrComplex, unique}).
-record(globalComplexTypeType, {elInfo, name, final, abstract, block, mixed, id, annotation, model, attributes, anyAttribute}).
-record(localComplexTypeType, {elInfo, mixed, annotation, model, attributes, anyAttribute}).
-record(globalSimpleTypeType, {elInfo, name, id, annotation, model}).
-record(localSimpleTypeType, {elInfo, annotation, model}).
-record(simpleContentType, {elInfo, annotation, model, id}).
-record(groupDefType, {elInfo, name, annotation, model}).
-record(groupRefType, {elInfo, ref, minOccurs, maxOccurs}).
-record(annotationType, {elInfo, annotation}).
-record(globalAttributeType, {elInfo, name, type, use, fixed, default, id, model}).
-record(localAttributeType, {elInfo, name, type, use, ref, fixed, form, default, model}).
-record(choiceType, {elInfo, id, minOccurs, maxOccurs, annotation, alternatives}).
-record(sequenceType, {elInfo, annotation, elements, minOccurs, maxOccurs}).
-record(allType, {elInfo, annotation, elements, minOccurs, maxOccurs}).
-record(attributeGroupDefType, {elInfo, id, name, annotation, attributes, anyAttribute}).
-record(attributeGroupRefType, {elInfo, ref, id}).
-record(anyType, {elInfo, any, minOccurs, maxOccurs, namespace, processContents}).
-record(anyAttributeType, {elInfo, id, namespace, processContents, annotation}).
-record(extensionType, {elInfo, base, annotation, attributes, anyAttribute}).
-record(extensionTypeC, {elInfo, base, annotation, model, attributes, anyAttribute}).
-record(restrictionType, {elInfo, annotation, any, attributes, anyAttribute, base}).
-record(restrictionTypeC, {elInfo, base, annotation, model, attributes, anyAttribute}).
-record(complexContentType, {elInfo, annotation, model, mixed}).

%% This is added to the XSD to allow generation of an XML document
-record(namespaceType, {prefix, 'URI'}).

%% the rest is for internal use in the translation of the XSD to the 
%% format used by the parser
%% path is used to give local elements a unique name (the 'path' to the element)
-record(schemaInfo, {targetNamespace, elementFormDefault, namespacePrefix, namespaces, path=[], attGrps, atts, th}).

%% typeInfo - the intermediate format.
%% global (true or false): we need to find out in the
%% end whether this type should be available as 'top level' element in the 
%% xml document.
-record(typeInfo, {typeName, 
                   global, 
                   typeType, 
                   typeRef, 
                   elements, 
                   attributes = [],
		   anyAttr,
                   seqOrAll,
                   extends,
                   restricts,
                   mixed,
                   base,
                   substitutionGroup,
                   min = 1,
                   max = 1}).

-record(elementInfo, {alternatives, min = 1, max = 1}).
-record(alternative, {tag, type, real, min = 1, max = 1, anyInfo}).
-record(attrib, {name, optional, type, ref}).
-record(attGrp, {name, atts, anyAttr}).
