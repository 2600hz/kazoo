%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is interface for accessing to CouchDB document consist
%%%  RADIUS dictionaries filled by the eradius
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_dicts).

-include("circlemaker.hrl").
-include_lib("whistle/src/wh_json.hrl").

%% API
-export([init_dicts/1, get_dicts/1, save_dict/2, delete_dict/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load transformed dictionaries as JSON documents into the system_config or an account database
%% @end
%%--------------------------------------------------------------------
-spec init_dicts('system_config') -> 'ok'; (ne_binary()) -> {'ok', wh_json:objects()} | {error, any()}.
init_dicts('system_config') ->
    'ok' = couch_mgr:revise_docs_from_folder('system_config', 'eradius', "dicts");
init_dicts(AccId) when is_binary(AccId)->
    AccountDb = wh_util:format_account_id(AccId, 'encoded'),
    couch_mgr:save_doc(AccountDb, wh_json:new()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get all dictionaries of an account or system_config as list of its IDs
%% @end
%%--------------------------------------------------------------------
-spec get_dicts(ne_binary() | 'system_config') -> {'ok', ne_binaries()} | {error, any()}.
get_dicts('system_config' = Param) ->
    get_dicts_priv(Param);
get_dicts(AccId) when is_binary(AccId) ->
    AccountDb = wh_util:format_account_id(AccId, 'encoded'),
    get_dicts_priv(AccountDb).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get all dictionaries of an account or system_config as list of its IDs
%% @end
%%--------------------------------------------------------------------
-spec get_dicts_priv(ne_binary() | 'system_config') -> {'ok', ne_binaries()} | {error, any()}.
get_dicts_priv(AccOrSystemId) ->
    case couch_mgr:get_results(AccOrSystemId, <<"aaa/fetch_dicts">>) of
        {'ok', Results} when is_list(Results) ->
            {'ok', Results};
        {'error', Reason} ->
            {'error', Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Store dictionary to an account database
%% @end
%%--------------------------------------------------------------------
-spec save_dict(ne_binary(), wh_json:object()) -> {'ok', wh_json:object()} | {error, any()}.
save_dict(AccId, JsonDoc) when is_binary(AccId) ->
    AccountDb = wh_util:format_account_id(AccId, 'encoded'),
    couch_mgr:save_doc(AccountDb, JsonDoc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Delete dictionary to an account
%% @end
%%--------------------------------------------------------------------
-spec delete_dict(ne_binary(), ne_binary()) -> {ok, wh_json:objects()} | {error, any()}.
delete_dict(AccId, DocId) ->
    AccountDb = wh_util:format_account_id(AccId, 'encoded'),
    couch_mgr:del_doc(AccountDb, DocId).
