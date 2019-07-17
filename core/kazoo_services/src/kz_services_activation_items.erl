%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_activation_items).

-export([empty/0]).
-export([create/1]).
-export([public_json/1]).

-export([calculate_total/1]).

-export([foldl/3]).

-include("services.hrl").

-type items() :: [kz_services_activation_item:item()].
-type fold_fun() :: fun((kz_services_activation_item:item(), Acc) -> Acc).
-export_type([items/0
             ,fold_fun/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> items().
empty() -> [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec foldl(fold_fun(), Acc, items()) -> Acc.
foldl(FoldFun, Acc, Items) ->
    lists:foldl(FoldFun, Acc, Items).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_total(items()) -> float().
calculate_total(ActivationItems) ->
    lists:sum([kz_services_activation_item:total(I) || I <- ActivationItems]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_service_items:items()) -> items().
create(Items) ->
    lists:foldl(fun create_fold/2, empty(), Items).

-spec create_fold(kz_service_item:item(), items()) -> items().
create_fold(Item, Acc) ->
    case kz_services_activation_item:create(Item) of
        'undefined' -> Acc;
        ActivateItem -> [ActivateItem | Acc]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(items()) -> kz_json:objects().
public_json(ActivationItems) ->
    [kz_services_activation_item:public_json(I) || I <- ActivationItems].
