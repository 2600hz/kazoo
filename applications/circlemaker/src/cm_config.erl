%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is interface for accessing to CouchDB document consist
%%%  AAA-servers configuration filled by the eradius
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_config).

-include("circlemaker.hrl").
-include_lib("whistle/src/wh_json.hrl").

%% API
-export([get_aaa_mode/1, get_servers_list/1, init_aaa_doc/1, get_aaa_doc/1, save_aaa_doc/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns default AAA configuration document.
%% @end
%%--------------------------------------------------------------------
-spec new_default_aaa_doc() -> wh_json:object().
new_default_aaa_doc() ->
    Servers = [
        {<<"enabled">>, 'false'},
        {<<"name">>, <<"unique_server_name">>},
        {<<"address">>, <<"127.0.0.1">>},
        {<<"port">>, 1812},
        {<<"secret">>, <<"example_secret">>},
        {<<"aaa_engine">>, <<"radius">>},
        {<<"dicts">>, [<<"dictionary_3gpp">>, <<"dictionary">>]},
        {<<"avp">>, <<"strict">>},
        {<<"retries">>, 3},
        {<<"timeout">>, 5000}
    ],
    KV = [
        {<<"aaa_mode">>, <<"off">>},
        {<<"servers">>, wh_json:from_list(Servers)},
        {<<"authentication">>, [<<"unique_server_name">>]},
        {<<"authorization">>, [<<"unique_server_name">>]},
        {<<"accounting">>, [<<"unique_server_name">>]}
    ],
    wh_json:from_list(KV).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns AAA configuration document for system_config or an account.
%% @end
%%--------------------------------------------------------------------
-spec new_default_aaa_doc('account') -> wh_json:object(); ('system') -> wh_json:object().
new_default_aaa_doc('account') ->
    wh_json:insert_value(<<"pvt_type">>, <<"aaa">>, new_default_aaa_doc());
new_default_aaa_doc('system') ->
    Fields = [{<<"pvt_type">>, <<"config">>}, {<<"_id">>, <<"circlemaker">>}, {<<"id">>, <<"circlemaker">>}],
    wh_json:set_values(Fields, new_default_aaa_doc()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initialize AAA configuration document in the database.
%% AccId should be something like <<"account%2Fd0%2Fb7%2Fcf8509c7f5f1c45c1c6f7bb2fc3a">>.
%% Returns resulted document.
%% @end
%%--------------------------------------------------------------------
-spec init_aaa_doc
    (ne_binary()) -> wh_json:object();
    ('system_config') -> wh_json:object().
init_aaa_doc(AccId) when is_binary(AccId) ->
    'ok' = couch_mgr:revise_views_from_folder(AccId, 'circlemaker'),
    case couch_mgr:get_results(AccId, <<"aaa/fetch_doc">>) of
        {'ok', [{[{<<"id">>, DocId},
                {<<"key">>, _},
                {<<"value">>, _}]}]} ->
            couch_mgr:open_cache_doc(AccId, DocId);
        {'ok', []} ->
            Doc = wh_json:insert_value(<<"account_id">>, AccId, new_default_aaa_doc('account')),
            couch_mgr:save_doc(AccId, Doc);
        {'error', Reason} ->
            {'error', Reason}
    end;
init_aaa_doc('system_config') ->
    case couch_mgr:open_cache_doc(?WH_CONFIG_DB, ?APP_NAME) of
        {'ok', Doc} ->
            {'ok', Doc};
        {'error', 'not_found'} ->
            Doc = new_default_aaa_doc('system'),
            couch_mgr:save_doc(?WH_CONFIG_DB, Doc)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get AAA-mode from document.
%% @end
%%--------------------------------------------------------------------
-spec get_aaa_mode(wh_json:object()) -> ne_binary().
get_aaa_mode(Doc) ->
    wh_json:get_json_value(<<"aaa_mode">>, Doc, <<"off">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get servers list.
%% @end
%%--------------------------------------------------------------------
-spec get_servers_list(wh_json:object()) -> ne_binary().
get_servers_list(Doc) ->
    wh_json:get_json_value(<<"servers">>, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get AAA configuration document from a database
%% @end
%%--------------------------------------------------------------------
-spec get_aaa_doc(wh_json:object()) -> ne_binary(); (atom()) -> ne_binary().
get_aaa_doc(AccId) when is_binary(AccId) ->
    case couch_mgr:get_results(AccId, <<"aaa/fetch_doc">>) of
        {'ok', [{[{<<"id">>, DocId},
            {<<"key">>, _},
            {<<"value">>, _}]}]} ->
            couch_mgr:open_cache_doc(AccId, DocId);
        {'error', Reason} ->
            {'error', Reason}
    end;
get_aaa_doc('system_config') ->
    couch_mgr:open_cache_doc(?WH_CONFIG_DB, ?APP_NAME).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save AAA configuration document to a database
%% @end
%%--------------------------------------------------------------------
-spec save_aaa_doc(ne_binary(), wh_json:object()) -> wh_json:object(); (atom(), wh_json:object()) -> wh_json:object().
save_aaa_doc(AccId, Doc) when is_binary(AccId) ->
    couch_mgr:save_doc(AccId, Doc);
save_aaa_doc('system_config', Doc) ->
    couch_mgr:save_doc(?WH_CONFIG_DB, Doc).
