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
    'ok' = couch_mgr:revise_docs_from_folder('system_config', 'eradius', "docs");
init_dicts(AccId) when is_binary(AccId)->
    couch_mgr:save_doc(AccId, wh_json:new()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get all dictionaries of an account or system_config as list of its IDs
%% @end
%%--------------------------------------------------------------------
-spec get_dicts(ne_binary()) -> {'ok', ne_binaries()} | {error, any()}; ('system_config') -> {'ok', ne_binaries()} | {error, any()}.
get_dicts(AccOrSystemId) when is_binary(AccOrSystemId); is_atom(AccOrSystemId)->
    case couch_mgr:get_results(AccOrSystemId, <<"aaa/fetch_dict">>) of
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
    couch_mgr:save_doc(AccId, JsonDoc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Delete dictionary to an account
%% @end
%%--------------------------------------------------------------------
-spec delete_dict(ne_binary(), ne_binary()) -> {ok, wh_json:objects()} | {error, any()}.
delete_dict(AccId, DocId) ->
    couch_mgr:del_doc(AccId, DocId).
