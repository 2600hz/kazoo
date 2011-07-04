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
%%% adds an XSD to an existing Erlsom Model
%%% ====================================================================

%%% Adds an XSD/namespace to an existing model. This is useful only if the
%%% existing model contains 'any' elements that have to be parsed. A typical
%%% example is the soap envelope. In order to parse the body, the parser needs 
%%% to know it's 'model'.

%%% Compiles the model for the imported xsd, adds the types to the existing 
%%% model, adds all the alternatives from the _document element to the 
%%% _document element of the existing model, adds the namespaces, and finally 
%%% updates the alternatives for all 'any' types in the model.

-module(erlsom_add).
-export([add/3]).
-export([add_xsd_model/1]).
-export([add_model/2]).

-include_lib("erlsom_parse.hrl").
-include_lib("erlsom_compile.hrl").

%% debug(Text) ->
  %% io:format("~p\n", [Text]).


%% -record(model, {tps, nss, tns}).  
%% -record(type, {nm, tp = sequence, els, atts = [], anyAttr, nillable, nr, mn = 1, mx = 1}). 
%% -record(el, {alts, mn = 1, mx = 1, nr}).

%% Returns the new #model.
add(Xsd, Options, Model1) ->
  {ok, Model2} = erlsom:compile_xsd(Xsd, Options),
  add_model(Model1, Model2).

add_xsd_model(Model1) ->
  add_model(Model1, erlsom_parseXsd:xsdModel()).

add_model(Model1 = #model{tps = Tps, nss = Nss, tns = Tns, th = Th}, 
          _Model2 = #model{tps = NewTps, nss = NewNss, th = NewTh}) ->
  [Document | OtherTypes] = Tps,
  #type{nm = '_document', els = [Element]} = Document,
  #el{alts = Alts} = Element,

  [New_Document | OtherNewTypes] = NewTps,
  #type{nm = '_document', els = [NewElement]} = New_Document,
  #el{alts = NewAlts} = NewElement,

  CombinedAlts = lists:umerge(lists:usort(Alts), lists:usort(NewAlts)),
  CombinedElement = Element#el{alts = CombinedAlts},
  CombinedDocument = Document#type{els = [CombinedElement]},
  CombinedTypes = [CombinedDocument | lists:umerge(lists:usort(OtherTypes), lists:usort(OtherNewTypes))],
  CombinedNss = lists:umerge(lists:usort(Nss), lists:usort(NewNss)),
  CombinedTh = lists:umerge(lists:usort(Th), lists:usort(NewTh)),

  Info = #schemaInfo{namespaces = CombinedNss, targetNamespace = Tns},

  UpdatedTypes = erlsom_pass2:pass5(CombinedTypes, Info),

  Model1#model{tps = UpdatedTypes, nss = CombinedNss, th = CombinedTh}.

