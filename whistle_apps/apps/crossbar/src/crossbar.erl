%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%%
%%% @end
%%% Created :  19 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0, stop/0]).

-export([refresh/0, refresh/1]).

-define(ACCOUNT_AGG_DB, <<"accounts">>).
-define(ACCOUNT_AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(ACCOUNT_AGG_FILTER, <<"account/export">>).

-define(SIP_AGG_DB, <<"sip_auth">>).
-define(SIP_AGG_FILTER, <<"devices/export_sip">>).

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
-spec stop/0 :: () -> ok.
stop() ->
    ok = application:stop(crossbar).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps/0 :: () -> ok.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    ensure_started(sasl), % logging
    ensure_started(crypto), % random
    ensure_started(inets),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
    ensure_started(webmachine),
    ensure_started(whistle_amqp), % amqp wrapper
    ensure_started(whistle_couch). % couch wrapper

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verify that an application is running
%% @end
%%--------------------------------------------------------------------
-spec ensure_started/1 :: (App) -> ok when
      App :: atom().
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verify that an application is running
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> started.
-spec refresh/1 :: (Account) -> ok when
      Account :: binary() | string().

refresh() ->
    spawn(fun() ->
                  couch_mgr:db_create(?SIP_AGG_DB),
                  couch_mgr:db_create(?ACCOUNT_AGG_DB),
                  couch_mgr:revise_doc_from_file(?ACCOUNT_AGG_DB, crossbar, ?ACCOUNT_AGG_VIEW_FILE),
                  lists:foreach(fun(AccountDb) ->
                                        timer:sleep(2000),
                                        refresh(AccountDb)
                                end, whapps_util:get_all_accounts())
          end),
    started.

refresh(Account) ->
    AccountDb = whapps_util:get_db_name(Account, encoded),
    couch_mgr:revise_docs_from_folder(AccountDb, crossbar, "account"),
    whapps_util:replicate_from_account(AccountDb, ?ACCOUNT_AGG_DB, ?ACCOUNT_AGG_FILTER),
    whapps_util:replicate_from_account(AccountDb, ?SIP_AGG_DB, ?SIP_AGG_FILTER),
    ok.
