%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(kzd_app).

-export([fetch/2]).

-export([
         id/1
        ,is_published/1
        ,publish/1
        ,unpublish/1
        ,name/1
        ,i18n/1
        ,tags/1
        ,icon/1
        ,api_url/1
        ,source_url/1
        ,author/1
        ,version/1
        ,license/1
        ,price/1
        ,phase/1
        ,screenshots/1
        ,urls/1
        ,account_id/1
        ,users/1
        ,allowed_users/1
        ,masqueradable/1
        ]).

-include("kz_documents.hrl").

-define(PUBLISHED, <<"published">>).
-define(NAME, <<"name">>).
-define(I18N, <<"i18n">>).
-define(TAGS, <<"tags">>).
-define(ICON, <<"icon">>).
-define(API_URL, <<"api_url">>).
-define(SOURCE_URL, <<"source_url">>).
-define(AUTHOR, <<"author">>).
-define(VERSION, <<"version">>).
-define(LICENSE, <<"license">>).
-define(PRICE, <<"price">>).
-define(PHASE, <<"phase">>).
-define(SCREENSHOTS, <<"screenshots">>).
-define(URLS, <<"urls">>).
-define(USERS, <<"users">>).
-define(ALLOWED_USERS, <<"allowed_users">>).
-define(MASQUERADABLE, <<"masqueradable">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(api_binary(), api_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
fetch('undefined', _) ->
    {'error', 'account_id_undefined'};
fetch(_, 'undefined') ->
    {'error', 'app_id_undefined'};
fetch(Account, Id) ->
    AccoundDb = kz_util:format_account_id(Account, 'encoded'),
    kz_datamgr:open_cache_doc(AccoundDb, Id).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec id(kz_json:object()) -> ne_binary().
id(JObj) ->
    kz_doc:id(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_published(kz_json:object()) -> boolean().
is_published(JObj) ->
    kz_json:is_true(?PUBLISHED, JObj, 'true').

-spec publish(kz_json:object()) -> kz_json:object().
publish(JObj) ->
    kz_json:set_value(?PUBLISHED, 'true', JObj).

-spec unpublish(kz_json:object()) -> kz_json:object().
unpublish(JObj) ->
    kz_json:set_value(?PUBLISHED, 'false', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec name(kz_json:object()) -> ne_binary().
name(JObj) ->
    kz_json:get_ne_binary_value(?NAME, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec i18n(kz_json:object()) -> kz_json:object().
i18n(JObj) ->
    kz_json:get_json_value(?I18N, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tags(kz_json:object()) -> ne_binaries().
tags(JObj) ->
    kz_json:get_list_value(?TAGS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec icon(kz_json:object()) -> ne_binary().
icon(JObj) ->
    kz_json:get_ne_binary_value(?ICON, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec api_url(kz_json:object()) -> ne_binary().
api_url(JObj) ->
    kz_json:get_ne_binary_value(?API_URL, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec source_url(kz_json:object()) -> ne_binary().
source_url(JObj) ->
    kz_json:get_ne_binary_value(?API_URL, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec author(kz_json:object()) -> ne_binary().
author(JObj) ->
    kz_json:get_ne_binary_value(?AUTHOR, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec version(kz_json:object()) -> ne_binary().
version(JObj) ->
    kz_json:get_ne_binary_value(?VERSION, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec license(kz_json:object()) -> ne_binary().
license(JObj) ->
    kz_json:get_ne_binary_value(?LICENSE, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec price(kz_json:object()) -> api_float().
price(JObj) ->
    kz_json:get_float_value(?PRICE, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec phase(kz_json:object()) -> api_binary().
phase(JObj) ->
    kz_json:get_ne_binary_value(?PHASE, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec screenshots(kz_json:object()) -> ne_binaries().
screenshots(JObj) ->
    kz_json:get_list_value(?SCREENSHOTS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec urls(kz_json:object()) -> kz_json:object().
urls(JObj) ->
    kz_json:get_json_value(?URLS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_id(kz_json:object()) -> ne_binary().
account_id(JObj) ->
    kz_doc:account_id(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec users(kz_json:object()) -> api_binaries().
users(JObj) ->
    kz_json:get_value(?USERS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec allowed_users(kz_json:object()) -> api_binaries().
allowed_users(JObj) ->
    kz_json:get_value(?ALLOWED_USERS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec masqueradable(kz_json:object()) -> boolean().
masqueradable(JObj) ->
    kz_json:is_true(?MASQUERADABLE, JObj, 'true').
