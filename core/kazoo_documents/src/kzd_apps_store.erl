%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(kzd_apps_store).

-export([fetch/1, new/1]).
-export([id/0]).
-export([apps/1, apps/2, set_apps/2]).
-export([blacklist/1, blacklist/2, set_blacklist/2]).

-define(ID, <<"apps_store">>).
-define(APPS, <<"apps">>).
-define(BLACKLIST, <<"blacklist">>).

-include("kz_documents.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(api_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
fetch('undefined') ->
    {'error', 'account_id_undefined'};
fetch(Account) ->
    AccoundDb = kz_accounts:format_account_id(Account, 'encoded'),
    kz_datamgr:open_cache_doc(AccoundDb, ?ID).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary()) -> kz_json:object().
new(Account) ->
    Routines = [
        fun(JObj) -> kz_doc:set_id(JObj, ?ID) end
        ,fun(JObj) ->
            AccountId = kz_accounts:format_account_id(Account, 'raw'),
            kz_doc:set_account_id(JObj, AccountId)
        end
        ,fun(JObj) ->
            AccountDb = kz_accounts:format_account_id(Account, 'encoded'),
            kz_doc:set_account_db(JObj, AccountDb)
        end
    ],
    lists:foldl(
        fun(F, JObj) -> F(JObj) end
        ,kz_json:new()
        ,Routines
    ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec id() -> ne_binary().
id() -> ?ID.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec apps(kz_json:object()) -> kz_json:object().
-spec apps(kz_json:object(), any()) -> kz_json:object().
apps(JObj) ->
    kz_json:get_value(?APPS, JObj, kz_json:new()).

apps(JObj, Default) ->
    kz_json:get_value(?APPS, JObj, Default).

-spec set_apps(kz_json:object(), any()) -> kz_json:object().
set_apps(JObj, Data) ->
    kz_json:set_value(?APPS, Data, JObj).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec blacklist(kz_json:object()) -> ne_binaries().
-spec blacklist(kz_json:object(), Default) -> ne_binaries() | Default.
blacklist(JObj) ->
    kz_json:get_value(?BLACKLIST, JObj, []).

blacklist(JObj, Default) ->
    kz_json:get_value(?BLACKLIST, JObj, Default).

-spec set_blacklist(kz_json:object(), kz_json:json_term()) -> kz_json:object().
set_blacklist(JObj, Data) ->
    kz_json:set_value(?BLACKLIST, Data, JObj).
