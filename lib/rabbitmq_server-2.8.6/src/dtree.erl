%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2013 VMware, Inc.  All rights reserved.
%%

%% A dual-index tree.
%%
%% Entries have the following shape:
%%
%% +----+--------------------+---+
%% | PK | SK1, SK2, ..., SKN | V |
%% +----+--------------------+---+
%%
%% i.e. a primary key, set of secondary keys, and a value.
%%
%% There can be only one entry per primary key, but secondary keys may
%% appear in multiple entries.
%%
%% The set of secondary keys must be non-empty. Or, to put it another
%% way, entries only exist while their secondary key set is non-empty.

-module(dtree).

-export([empty/0, insert/4, take/3, take/2, take_all/2,
         is_defined/2, is_empty/1, smallest/1, size/1]).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-export_type([?MODULE/0]).

-opaque(?MODULE()  :: {gb_tree(), gb_tree()}).

-type(pk()         :: any()).
-type(sk()         :: any()).
-type(val()        :: any()).
-type(kv()         :: {pk(), val()}).

-spec(empty/0      :: () -> ?MODULE()).
-spec(insert/4     :: (pk(), [sk()], val(), ?MODULE()) -> ?MODULE()).
-spec(take/3       :: ([pk()], sk(), ?MODULE()) -> {[kv()], ?MODULE()}).
-spec(take/2       :: (sk(), ?MODULE()) -> {[kv()], ?MODULE()}).
-spec(take_all/2   :: (sk(), ?MODULE()) -> {[kv()], ?MODULE()}).
-spec(is_defined/2 :: (sk(), ?MODULE()) -> boolean()).
-spec(is_empty/1   :: (?MODULE()) -> boolean()).
-spec(smallest/1   :: (?MODULE()) -> kv()).
-spec(size/1       :: (?MODULE()) -> non_neg_integer()).

-endif.

%%----------------------------------------------------------------------------

empty() -> {gb_trees:empty(), gb_trees:empty()}.

%% Insert an entry. Fails if there already is an entry with the given
%% primary key.
insert(PK, [], V, {P, S}) ->
    %% dummy insert to force error if PK exists
    gb_trees:insert(PK, {gb_sets:empty(), V}, P),
    {P, S};
insert(PK, SKs, V, {P, S}) ->
    {gb_trees:insert(PK, {gb_sets:from_list(SKs), V}, P),
     lists:foldl(fun (SK, S0) ->
                         case gb_trees:lookup(SK, S0) of
                             {value, PKS} -> PKS1 = gb_sets:insert(PK, PKS),
                                             gb_trees:update(SK, PKS1, S0);
                             none         -> PKS = gb_sets:singleton(PK),
                                             gb_trees:insert(SK, PKS, S0)
                         end
                 end, S, SKs)}.

%% Remove the given secondary key from the entries of the given
%% primary keys, returning the primary-key/value pairs of any entries
%% that were dropped as the result (i.e. due to their secondary key
%% set becoming empty). It is ok for the given primary keys and/or
%% secondary key to not exist.
take(PKs, SK, {P, S}) ->
    case gb_trees:lookup(SK, S) of
        none         -> {[], {P, S}};
        {value, PKS} -> TakenPKS = gb_sets:from_list(PKs),
                        PKSInter = gb_sets:intersection(PKS, TakenPKS),
                        PKSDiff  = gb_sets_difference  (PKS, PKSInter),
                        {KVs, P1} = take2(PKSInter, SK, P),
                        {KVs, {P1, case gb_sets:is_empty(PKSDiff) of
                                       true  -> gb_trees:delete(SK, S);
                                       false -> gb_trees:update(SK, PKSDiff, S)
                                   end}}
    end.

%% Remove the given secondary key from all entries, returning the
%% primary-key/value pairs of any entries that were dropped as the
%% result (i.e. due to their secondary key set becoming empty). It is
%% ok for the given secondary key to not exist.
take(SK, {P, S}) ->
    case gb_trees:lookup(SK, S) of
        none         -> {[], {P, S}};
        {value, PKS} -> {KVs, P1} = take2(PKS, SK, P),
                        {KVs, {P1, gb_trees:delete(SK, S)}}
    end.

%% Drop all entries which contain the given secondary key, returning
%% the primary-key/value pairs of these entries. It is ok for the
%% given secondary key to not exist.
take_all(SK, {P, S}) ->
    case gb_trees:lookup(SK, S) of
        none         -> {[], {P, S}};
        {value, PKS} -> {KVs, SKS, P1} = take_all2(PKS, P),
                        {KVs, {P1, prune(SKS, PKS, S)}}
    end.

is_defined(SK, {_P, S}) -> gb_trees:is_defined(SK, S).

is_empty({P, _S}) -> gb_trees:is_empty(P).

smallest({P, _S}) -> {K, {_SKS, V}} = gb_trees:smallest(P),
                     {K, V}.

size({P, _S}) -> gb_trees:size(P).

%%----------------------------------------------------------------------------

take2(PKS, SK, P) ->
    gb_sets:fold(fun (PK, {KVs, P0}) ->
                         {SKS, V} = gb_trees:get(PK, P0),
                         SKS1 = gb_sets:delete(SK, SKS),
                         case gb_sets:is_empty(SKS1) of
                             true  -> KVs1 = [{PK, V} | KVs],
                                      {KVs1, gb_trees:delete(PK, P0)};
                             false -> {KVs,  gb_trees:update(PK, {SKS1, V}, P0)}
                         end
                 end, {[], P}, PKS).

take_all2(PKS, P) ->
    gb_sets:fold(fun (PK, {KVs, SKS0, P0}) ->
                         {SKS, V} = gb_trees:get(PK, P0),
                         {[{PK, V} | KVs], gb_sets:union(SKS, SKS0),
                          gb_trees:delete(PK, P0)}
                 end, {[], gb_sets:empty(), P}, PKS).

prune(SKS, PKS, S) ->
    gb_sets:fold(fun (SK0, S0) ->
                         PKS1 = gb_trees:get(SK0, S0),
                         PKS2 = gb_sets_difference(PKS1, PKS),
                         case gb_sets:is_empty(PKS2) of
                             true  -> gb_trees:delete(SK0, S0);
                             false -> gb_trees:update(SK0, PKS2, S0)
                         end
                 end, S, SKS).

gb_sets_difference(S1, S2) ->
    gb_sets:fold(fun gb_sets:delete_any/2, S1, S2).
