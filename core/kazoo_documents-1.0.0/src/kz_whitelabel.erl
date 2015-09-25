%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(kz_whitelabel).

-export([fetch/1]).
-export([port_hide/1]).
-export([port_email/1]).
-export([port_authority/1]).
-export([port_loa/1]).
-export([port_resporg/1]).

-define(ID, <<"whitelabel">>).

-define(PORT, <<"port">>).
-define(PORT_HIDE, [<<"hide_port">>]).
-define(PORT_EMAIL, [?PORT, <<"support_email">>]).
-define(PORT_AUTHORITY, [?PORT, <<"authority">>]).
-define(PORT_LOA, [?PORT, <<"loa">>]).
-define(PORT_RESPORG, [?PORT, <<"resporg">>]).

-include("kz_documents.hrl").

-spec fetch(api_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
fetch('undefined') ->
    {'error', 'account_id_undefined'};
fetch(Account) ->
    AccoundDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:open_doc(AccoundDb, ?ID).

-spec port_hide(wh_json:object()) -> boolean().
port_hide(JObj) ->
    wh_json:is_true(?PORT_HIDE, JObj).

-spec port_email(wh_json:object()) -> api_binary().
port_email(JObj) ->
    wh_json:get_ne_binary_value(?PORT_EMAIL, JObj).

-spec port_authority(wh_json:object()) -> api_binary().
port_authority(JObj) ->
    wh_json:get_ne_binary_value(?PORT_AUTHORITY, JObj).

-spec port_loa(wh_json:object()) -> api_binary().
port_loa(JObj) ->
    wh_json:get_ne_binary_value(?PORT_LOA, JObj).

-spec port_resporg(wh_json:object()) -> api_binary().
port_resporg(JObj) ->
    wh_json:get_ne_binary_value(?PORT_RESPORG, JObj).
