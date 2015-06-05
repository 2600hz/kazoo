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
-export([get_aaa_mode/1, get_servers_list/1, init_doc/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns default AAA configuration document 
%% @end
%%--------------------------------------------------------------------
-spec get_default_doc() -> wh_json:object().
-spec new_default_doc(account) -> wh_json:object(); (system) -> wh_json:object().

get_default_doc() ->
    Servers = [
        {<<"enabled">>, false},
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

new_default_doc(account) ->
    wh_json:insert_value(<<"pvt_type">>, <<"aaa">>, get_default_doc());
new_default_doc(system) ->
    wh_json:insert_value(<<"pvt_type">>, <<"config">>, get_default_doc()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initialize AAA configuration document in the database
%% Returns resulted document
%% @end
%%--------------------------------------------------------------------
-spec init_doc
    (ne_binary()) -> wh_json:object();
    (system_config) -> wh_json:object().
init_doc(AccId) when is_binary(AccId) ->
    case couch_mgr:get_results(AccId, <<"circlemaker/doc">>) of
        {ok, [DocId]} ->
            couch_mgr:open_cache_doc(AccId, DocId);
        {ok, []} ->
            Doc = new_default_doc(account),
            wh_json:insert_value(<<"account_id">>, <<"aaa">>, get_default_doc()),
            couch_mgr:save_doc(?WH_CONFIG_DB, Doc);
        {error, Reason} ->
            {error, Reason}
    end;
init_doc(system_config) ->
    io:format("Init system_config circlemaker document...~n"),
    case couch_mgr:open_cache_doc(?WH_CONFIG_DB, ?APP_NAME) of
        {ok, Doc} ->
            io:format("System_config circlemaker document already exist~n"),
            {ok, Doc};
        {error, not_found} ->
            io:format("Create new system_config circlemaker document~n"),
            Doc = new_default_doc(system),
            couch_mgr:save_doc(?WH_CONFIG_DB, Doc)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get AAA-mode from document
%% @end
%%--------------------------------------------------------------------
-spec get_aaa_mode(wh_json:object()) -> on|off|inherit.
get_aaa_mode(Doc) ->
    wh_json:get_json_value(<<"aaa_mode">>, Doc, <<"off">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get servers list
%% @end
%%--------------------------------------------------------------------
-spec get_servers_list(wh_json:object()) -> on|off|inherit.
get_servers_list(Doc) ->
    wh_json:get_json_value(<<"servers">>, Doc, <<"off">>).
