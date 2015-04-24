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
-include("circlemaker_defs.hrl").
-include_lib("whistle/src/wh_json.hrl").

%% API
-export([get_aaa_mode/1, get_servers_list/1, init_aaa_doc/0, get_aaa_doc/0, save_aaa_doc/1]).

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
    wh_json:from_list(?DEFAULT_AAA_DOCUMENT).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns AAA configuration document for system_config or an account.
%% @end
%%--------------------------------------------------------------------
-spec new_default_aaa_doc('account' | 'system') -> wh_json:object().
new_default_aaa_doc('account') ->
    wh_json:insert_value(<<"pvt_type">>, <<"aaa">>, new_default_aaa_doc());
new_default_aaa_doc('system') ->
    Fields = [{<<"pvt_type">>, <<"config">>}, {<<"_id">>, <<"circlemaker">>}, {<<"id">>, <<"circlemaker">>}],
    wh_json:set_values(Fields, new_default_aaa_doc()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initialize AAA configuration document in the system_config database.
%% Returns resulted document.
%% @end
%%--------------------------------------------------------------------
-spec init_aaa_doc() -> {'ok', wh_json:object()} | {'error', any()}.
init_aaa_doc() ->
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
-spec get_servers_list(wh_json:object()) -> wh_json:object().
get_servers_list(Doc) ->
    wh_json:get_json_value(<<"servers">>, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get AAA configuration document from the system_config database
%% @end
%%--------------------------------------------------------------------
-spec get_aaa_doc() -> {'ok', wh_json:object()} | {'error', any()}.
get_aaa_doc() ->
    couch_mgr:open_cache_doc(?WH_CONFIG_DB, ?APP_NAME).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save AAA configuration document to the system_config database
%% @end
%%--------------------------------------------------------------------
-spec save_aaa_doc(wh_json:object()) -> {'ok', wh_json:object()} | {'error', any()}.
save_aaa_doc(Doc) ->
    couch_mgr:save_doc(?WH_CONFIG_DB, Doc).
