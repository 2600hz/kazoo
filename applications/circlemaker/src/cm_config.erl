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
-export([get_aaa_doc_value/1, get_aaa_doc/0, save_aaa_doc/1]).

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
%% Get a value of AAA configuration document (circlemaker) in the system_config database.
%% Returns value for the key.
%% @end
%%--------------------------------------------------------------------
-spec get_aaa_doc_value(ne_binary()) -> term() | 'undefined'.
get_aaa_doc_value(Key) ->
    DefaultDoc = new_default_aaa_doc('system'),
    DefaultValue = wh_json:get_value(Key, DefaultDoc),
    whapps_config:get(?APP_NAME, Key, DefaultValue).

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
