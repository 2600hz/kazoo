%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_maintenance).

%% API
-export([refresh/0]).

-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_types.hrl").

-spec refresh() -> 'ok'.
refresh() ->
    AccountsViews = whapps_util:get_views_json('kamdb', "views/accounts"),
    whapps_util:update_views(?WH_ACCOUNTS_DB, AccountsViews, 'false'),
    AccountViews = whapps_util:get_views_json('kamdb', "views/account"),
    AllAccounts = whapps_util:get_all_accounts(),
    lists:foldl( fun (AccountDB, Acc) -> refresh_db(AccountDB, Acc, AccountViews) end, 'ok' ,AllAccounts).

-spec refresh_db(ne_binary(), term(), list()) -> term().
refresh_db(AccountDB, Acc, Views) ->
    Ret = whapps_util:update_views(AccountDB, Views, 'false'),
    case Acc of
        'ok' -> Ret;
        Other -> Other
    end.
