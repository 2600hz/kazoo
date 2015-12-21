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
-spec fetch(api_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
fetch('undefined') ->
    {'error', 'account_id_undefined'};
fetch(Account) ->
    AccoundDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:open_cache_doc(AccoundDb, ?ID).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary()) -> wh_json:object().
new(Account) ->
    Routines = [
        fun(JObj) -> wh_doc:set_id(JObj, ?ID) end
        ,fun(JObj) ->
            AccountId = wh_util:format_account_id(Account, 'raw'),
            wh_doc:set_account_id(JObj, AccountId)
        end
        ,fun(JObj) ->
            AccountDb = wh_util:format_account_id(Account, 'encoded'),
            wh_doc:set_account_db(JObj, AccountDb)
        end
    ],
    lists:foldl(
        fun(F, JObj) -> F(JObj) end
        ,wh_json:new()
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
-spec apps(wh_json:object()) -> wh_json:object().
-spec apps(wh_json:object(), any()) -> wh_json:object().
apps(JObj) ->
    wh_json:get_value(?APPS, JObj, wh_json:new()).

apps(JObj, Default) ->
    wh_json:get_value(?APPS, JObj, Default).

-spec set_apps(wh_json:object(), any()) -> wh_json:object().
set_apps(JObj, Data) ->
    wh_json:set_value(?APPS, Data, JObj).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec blacklist(wh_json:object()) -> ne_binaries().
-spec blacklist(wh_json:object(), any()) -> ne_binaries().
blacklist(JObj) ->
    wh_json:get_value(?BLACKLIST, JObj, []).

blacklist(JObj, Default) ->
    wh_json:get_value(?BLACKLIST, JObj, Default).

-spec set_blacklist(wh_json:object(), any()) -> ne_binaries().
set_blacklist(JObj, Data) ->
    wh_json:set_value(?BLACKLIST, Data, JObj).
