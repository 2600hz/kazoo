%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%%
%%% @end
%%% Created :  19 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar).

-include("../include/crossbar.hrl").

-export([start_link/0, stop/0]).

-export([refresh/0, refresh/1]).

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    start_deps(),
    crossbar_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop/0 :: () -> 'ok'.
stop() ->
    ok = application:stop(crossbar).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps/0 :: () -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    wh_util:ensure_started(sasl), % logging
    wh_util:ensure_started(crypto), % random
    wh_util:ensure_started(inets),
    wh_util:ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
    wh_util:ensure_started(webmachine),
    wh_util:ensure_started(whistle_amqp), % amqp wrapper
    wh_util:ensure_started(whistle_couch). % couch wrapper

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verify that an application is running
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'started'.
-spec refresh/1 :: (Account) -> 'ok' when
      Account :: binary() | string().

refresh() ->
    spawn(fun() ->
                  refresh(?SIP_AGG_DB),
                  refresh(?SCHEMAS_DB),
                  refresh(?ACCOUNTS_AGG_DB),
                  lists:foreach(fun(AccountDb) ->
                                        timer:sleep(2000),
                                        refresh(AccountDb)
                                end, whapps_util:get_all_accounts())
          end),
    started.

refresh(Account) when not is_binary(Account) ->
    refresh(wh_util:to_binary(Account));
refresh(?SIP_AGG_DB) ->
    couch_mgr:db_create(?SIP_AGG_DB);
refresh(?SCHEMAS_DB) ->
    couch_mgr:db_create(?SCHEMAS_DB),
    couch_mgr:revise_docs_from_folder(?SCHEMAS_DB, crossbar, "schemas");
refresh(?ACCOUNTS_AGG_DB) ->
    couch_mgr:db_create(?ACCOUNTS_AGG_DB),
    couch_mgr:revise_doc_from_file(?ACCOUNTS_AGG_DB, crossbar, ?ACCOUNTS_AGG_VIEW_FILE),
    couch_mgr:revise_doc_from_file(?ACCOUNTS_AGG_DB, crossbar, ?MAINTENANCE_VIEW_FILE),
    ok;
refresh(Account) ->
    AccountDb = whapps_util:get_db_name(Account, encoded),
    AccountId = whapps_util:get_db_name(Account, raw),
    couch_mgr:revise_docs_from_folder(AccountDb, crossbar, "account"),
    couch_mgr:revise_doc_from_file(AccountDb, crossbar, ?MAINTENANCE_VIEW_FILE),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {error, not_found} ->
            ?LOG("account ~s is missing its local account definition!", [AccountId]);
        {ok, JObj} ->
            couch_mgr:ensure_saved(?ACCOUNTS_AGG_DB, JObj)
    end,
    case couch_mgr:get_results(AccountDb, ?DEVICES_CB_LIST, [{<<"include_docs">>, true}]) of
        {ok, Devices} ->
            [aggregate_device(wh_json:get_value(<<"doc">>, Device)) || Device <- Devices];
        {error, _} ->
            ok
    end,
    ok.

aggregate_device(undefined) ->
    ok;
aggregate_device(Device) ->
    DeviceId = wh_json:get_value(<<"_id">>, Device),
    case couch_mgr:lookup_doc_rev(?SIP_AGG_DB, DeviceId) of
        {ok, Rev} ->
            couch_mgr:ensure_saved(?SIP_AGG_DB, wh_json:set_value(<<"_rev">>, Rev, Device));
        {error, not_found} ->
            couch_mgr:ensure_saved(?SIP_AGG_DB, wh_json:delete_key(<<"_rev">>, Device))
    end,
    ok.
