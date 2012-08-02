%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_services_maintenance).

-export([reconcile/0, reconcile/1]).

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
