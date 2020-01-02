%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Account document
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_apps_store).

-export([fetch/1, new/1]).
-export([id/0]).
-export([apps/1, apps/2, set_apps/2]).
-export([blacklist/1, blacklist/2, set_blacklist/2]).

-define(ID, <<"apps_store">>).
-define(APPS, <<"apps">>).
-define(BLACKLIST, <<"blacklist">>).

-include("kz_documents.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:api_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
fetch('undefined') ->
    {'error', 'account_id_undefined'};
fetch(Account) ->
    AccoundDb = kzs_util:format_account_db(Account),
    kz_datamgr:open_cache_doc(AccoundDb, ?ID).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new(kz_term:ne_binary()) -> kz_json:object().
new(Account) ->
    Routines = [fun(JObj) -> kz_doc:set_id(JObj, ?ID) end
               ,fun(JObj) ->
                        AccountId = kzs_util:format_account_id(Account),
                        kz_doc:set_account_id(JObj, AccountId)
                end
               ,fun(JObj) ->
                        AccountDb = kzs_util:format_account_db(Account),
                        kz_doc:set_account_db(JObj, AccountDb)
                end
               ],
    lists:foldl(fun(F, JObj) -> F(JObj) end
               ,kz_json:new()
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id() -> kz_term:ne_binary().
id() -> ?ID.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec apps(kz_json:object()) -> kz_json:object().
apps(JObj) ->
    kz_json:get_value(?APPS, JObj, kz_json:new()).

-spec apps(kz_json:object(), any()) -> kz_json:object().
apps(JObj, Default) ->
    kz_json:get_value(?APPS, JObj, Default).

-spec set_apps(kz_json:object(), any()) -> kz_json:object().
set_apps(JObj, Data) ->
    kz_json:set_value(?APPS, Data, JObj).


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec blacklist(kz_json:object()) -> kz_term:ne_binaries().
blacklist(JObj) ->
    kz_json:get_value(?BLACKLIST, JObj, []).

-spec blacklist(kz_json:object(), Default) -> kz_term:ne_binaries() | Default.
blacklist(JObj, Default) ->
    kz_json:get_value(?BLACKLIST, JObj, Default).

-spec set_blacklist(kz_json:object(), kz_json:json_term()) -> kz_json:object().
set_blacklist(JObj, Data) ->
    kz_json:set_value(?BLACKLIST, Data, JObj).
