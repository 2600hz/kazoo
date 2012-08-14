%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_services_maintenance).

-export([refresh/0]).
-export([reconcile/0, reconcile/1]).

-include_lib("whistle_services/src/whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'ok'.
refresh() ->
    couch_mgr:db_create(?WH_SERVICES_DB),
    couch_mgr:revise_docs_from_folder(?WH_SERVICES_DB, whistle_services, "views").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile/0 :: () -> 'no_return'.
-spec reconcile/1 :: (text()) -> 'no_return'.

reconcile() ->
    reconcile(all).

reconcile(all) ->
    Accounts = whapps_util:get_all_accounts(raw),
    Total = length(Accounts),
    _ = lists:foldr(fun(Account, Current) ->
                            io:format("reconcile services (~p/~p) '~s'~n", [Current, Total, Account]),
                            _ = reconcile(Account),
                            Current + 1
                    end, 1, Accounts),
    no_return;
reconcile(Account) when not is_binary(Account) ->
    reconcile(wh_util:to_binary(Account));
reconcile(Account) ->
    wh_services:reconcile(Account).
