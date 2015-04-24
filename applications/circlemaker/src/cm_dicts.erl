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
-export([init_dicts/0, get_dicts/1, save_dict/2, delete_dict/2]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec add_data_as_dict(string()) -> 'ok' | {'error', any()}.
add_data_as_dict(File) ->
    DocName = filename:basename(File, ".json"),
    {'ok', Bin} = file:read_file(File),
    JObj = wh_json:decode(Bin),
    Doc = wh_json:from_list(
        [{<<"_id">>, wh_util:to_binary(DocName)}
        ,{<<"id">>, wh_util:to_binary(DocName)}
        ,{<<"owner">>, <<"system_config">>}
        ,{<<"value">>, JObj}
        ,{<<"pvt_type">>, <<"aaa_dict">>}
        ]),
    case couch_mgr:save_doc(?WH_AAA_DICTS_DB, Doc) of
        {'ok', _} -> 'ok';
        {'error', 'conflict'} ->
            % the dictionary already exists
            {'error', 'conflict'};
        {'error', Reason} ->
            lager:debug("Error when saving RADIUS dictionary ~p. Reason: ~p~n", [DocName, Reason]),
            {'error', Reason}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load transformed dictionaries as JSON documents into the system_config or an account database
%% @end
%%--------------------------------------------------------------------
-spec init_dicts() -> 'ok' | {'error', any()}.
init_dicts() ->
    (couch_mgr:db_exists(?WH_AAA_DICTS_DB) == false) andalso couch_mgr:db_create(?WH_AAA_DICTS_DB),
    Files = filelib:wildcard([code:priv_dir('eradius'), "/*.json"]),
    [add_data_as_dict(F) || F <- Files].

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
