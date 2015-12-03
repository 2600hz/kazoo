%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
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
    ,published/1
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
    ,screenshots/1
    ,urls/1
    ,account_id/1
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
-define(SCREENSHOTS, <<"screenshots">>).
-define(URLS, <<"urls">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(api_binary(), api_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
fetch('undefined', _) ->
    {'error', 'account_id_undefined'};
fetch(_, 'undefined') ->
    {'error', 'app_id_undefined'};
fetch(Account, Id) ->
    AccoundDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:open_cache_doc(AccoundDb, Id).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec id(wh_json:object()) -> ne_binary().
id(JObj) ->
    wh_doc:id(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec published(wh_json:object()) -> ne_binary().
published(JObj) ->
    wh_json:is_true(?PUBLISHED, JObj, 'true').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec name(wh_json:object()) -> ne_binary().
name(JObj) ->
    wh_json:get_binary_value(?NAME, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec i18n(wh_json:object()) -> ne_binary().
i18n(JObj) ->
    wh_json:get_json_value(?I18N, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tags(wh_json:object()) -> ne_binary().
tags(JObj) ->
    wh_json:get_list_value(?TAGS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec icon(wh_json:object()) -> ne_binary().
icon(JObj) ->
    wh_json:get_binary_value(?ICON, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec api_url(wh_json:object()) -> ne_binary().
api_url(JObj) ->
    wh_json:get_binary_value(?API_URL, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec source_url(wh_json:object()) -> ne_binary().
source_url(JObj) ->
    wh_json:get_binary_value(?API_URL, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec author(wh_json:object()) -> ne_binary().
author(JObj) ->
    wh_json:get_binary_value(?AUTHOR, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec version(wh_json:object()) -> ne_binary().
version(JObj) ->
    wh_json:get_binary_value(?VERSION, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec license(wh_json:object()) -> ne_binary().
license(JObj) ->
    wh_json:get_binary_value(?LICENSE, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec price(wh_json:object()) -> ne_binary().
price(JObj) ->
    wh_json:get_float_value(?PRICE, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec screenshots(wh_json:object()) -> ne_binary().
screenshots(JObj) ->
    wh_json:get_list_value(?SCREENSHOTS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec urls(wh_json:object()) -> ne_binary().
urls(JObj) ->
    wh_json:get_json_value(?URLS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_id(wh_json:object()) -> ne_binary().
account_id(JObj) ->
    wh_doc:account_id(JObj).

