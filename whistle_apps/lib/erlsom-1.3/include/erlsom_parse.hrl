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
%%% header file for erslom_parse
%%% ====================================================================

%% header file for erlsom_parse. Contains the record definitions.

%% the records that form the model - see erlsom_parse.erl for a 
%% description.

-record(model, {tps, nss, 
                tns, %% target namespace (the URI, a string)
                th %% type hierarchy, see 'tree'-functions in erlsom_lib
               }).  
-record(type, {nm, tp = sequence, els, atts = [], anyAttr, nillable, nr, 
               mn = 1, mx = 1, mxd = false, %% mn & mx are only used by erlsom_compile
               typeName}).  %% typeName is the 'real' name, to be used in xsi:type attributes
                            %% for derived types. The 'nm' field is actually a key, which may
                            %% include an additional prefix to differntiate between elements, types
                            %% and groups.
-record(el, {alts, mn = 1, mx = 1, nr}).
-record(alt, {tag, tp, nxt = [], mn = 1, mx = 1, rl = true, anyInfo}).
-record(att, {nm, nr, opt, tp}).
%% -record(ns, {uri, pf}).
-record(elInfo, {anyAttr}).
-record(anyAttr, {prCont, ns, tns}). %% for anyAttributes
-record(anyInfo, {prCont, ns, tns}). %% for any elements

-record(state, {currentState, resultSoFar, model, namespaces, 
                allNamespaces, continuationState}).

-record(cs, {re,     %% remaining elements
             sf,     %% nr of elements of the current type received so far
             er,     %% element record: the result (so far) for this type
             rl,     %% 'real element': do we expect an end-tag?
             mxd}).  %% is this a mixed type?

-record(all, {re,     %% remaining elements
              nr,     %% the sequence number of the current element
              er}).   %% element record: the result (so far) for this type

%% altState is used for parsing alternatives within
%% a choice that can occur more than once
-record(altState, {name,          %% the tag we are processing
                   type,          %% the type of this element
                   real,          %% is this a 'real' element or a group ref
                   receivedSoFar, %% number of elements received
                   acc,           %% values of elements already processed
                   min,           %% minOccurs
                   max}).         %% maxOccurs

-record(anyState, {anyInfo}).
