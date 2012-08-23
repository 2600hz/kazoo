%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_services_maintenance).

-export([credit/2, credit/3]).
-export([debit/2, debit/3]).
-export([refresh/0]).
-export([reconcile/0, reconcile/1]).
-export([sync/1]).
-export([make_reseller/1]).
-export([demote_reseller/1]).
-export([cascade_reseller_id/2]).
-export([set_reseller_id/2]).

-include_lib("whistle_services/src/whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add arbitrary credit to an account, without charing the accounts
%% credit card
%% @end
%%--------------------------------------------------------------------
-spec credit/2 :: (text(), text()) -> 'no_return'.
-spec credit/3 :: (text(), text(), text()) -> 'no_return'.

credit(Account, Amount) ->    
    credit(Account, Amount, <<"System administrator discretionary credit addition">>).

credit(Account, Amount, Description) when not is_binary(Account) ->
    credit(wh_util:to_binary(Account), Amount, Description);
credit(Account, Amount, Description) when not is_float(Amount) ->    
    credit(Account, wh_util:to_float(Amount), Description);
credit(Account, Amount, Description) when not is_binary(Description) ->    
    credit(Account, Amount, wh_util:to_binary(Description));
credit(Account, Amount, Description) ->    
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    Timestamp = wh_util:current_tstamp(),
    JObj = wh_json:from_list([{<<"reason">>, <<"system administrator reconciliation">>}
                              ,{<<"description">>, Description}
                              ,{<<"account_id">>, AccountId}
                              ,{<<"amount">>, wapi_money:dollars_to_units(Amount)}
                              ,{<<"pvt_account_id">>, AccountId}
                              ,{<<"pvt_account_db">>, AccountDb}
                              ,{<<"pvt_type">>, <<"credit">>}
                              ,{<<"pvt_created">>, Timestamp}
                              ,{<<"pvt_modified">>, Timestamp}
                              ,{<<"pvt_vsn">>, 1}
                             ]),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {ok, _} -> io:format("credited account ~s $~w~n", [AccountId, Amount]);
        {error, R} -> io:format("failed to credited account ~s: ~p~n", [AccountId, R])
    end,
    no_return.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add arbitrary debit to an account, without charing the accounts
%% debit card
%% @end
%%--------------------------------------------------------------------
-spec debit/2 :: (text(), text()) -> 'no_return'.
-spec debit/3 :: (text(), text(), text()) -> 'no_return'.

debit(Account, Amount) ->
    debit(Account, Amount, <<"System administrator discretionary debit addition">>).

debit(Account, Amount, Description) when not is_binary(Account) ->    
    debit(wh_util:to_binary(Account), Amount, Description);
debit(Account, Amount, Description) when not is_float(Amount) ->    
    debit(Account, wh_util:to_float(Amount), Description);
debit(Account, Amount, Description) when not is_binary(Description) ->    
    debit(Account, Amount, wh_util:to_binary(Description));
debit(Account, Amount, Description) ->    
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    Timestamp = wh_util:current_tstamp(),
    JObj = wh_json:from_list([{<<"reason">>, <<"system administrator reconciliation">>}
                              ,{<<"description">>, Description}
                              ,{<<"account_id">>, AccountId}
                              ,{<<"amount">>, wapi_money:dollars_to_units(Amount)}
                              ,{<<"pvt_account_id">>, AccountId}
                              ,{<<"pvt_account_db">>, AccountDb}
                              ,{<<"pvt_type">>, <<"debit">>}
                              ,{<<"pvt_created">>, Timestamp}
                              ,{<<"pvt_modified">>, Timestamp}
                              ,{<<"pvt_vsn">>, 1}
                             ]),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {ok, _} -> io:format("debited account ~s $~w~n", [AccountId, Amount]);
        {error, R} -> io:format("failed to update account ~s ledger: ~p~n", [AccountId, R])
    end,
    no_return.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% maintenance function for the services db
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'ok'.
refresh() ->
    couch_mgr:db_create(?WH_SERVICES_DB),
    couch_mgr:revise_docs_from_folder(?WH_SERVICES_DB, whistle_services, "views").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% scheduals an eventual sync with the bookkeeper and will dirty the
%% full reseller tree (as it normally does when changes occur)
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
    try wh_services:reconcile(Account) of
        Any -> Any
    catch
        _E:_R ->
            io:format("failed to reconcile account ~s(~p): ~p~n", [Account, _E, _R])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% runs an immediate sync with a bookkeeper without dirting the
%% reseller tree (only the one account is affected)
%% @end
%%--------------------------------------------------------------------
-spec sync/1 :: (text()) -> 'ok'.
sync(Account) when not is_binary(Account) ->
    sync(wh_util:to_binary(Account));
sync(Account) ->
    wh_service_sync:sync(Account),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the reseller status on the provided account and update all
%% sub accounts
%% @end
%%--------------------------------------------------------------------
-spec make_reseller/1 :: (text()) -> 'ok'.
make_reseller(Account) when not is_binary(Account) ->
    make_reseller(wh_util:to_binary(Account));
make_reseller(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    _ = update_account_definition(AccountId, <<"pvt_reseller">>, true),
    _ = maybe_update_services(AccountId, <<"pvt_reseller">>, true),
    io:format("promoting account ~s to reseller status, updating sub accounts~n", [AccountId]),
    cascade_reseller_id(AccountId, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Remove reseller status from an account and set all its sub accounts
%% to the next higher reseller
%% @end
%%--------------------------------------------------------------------
-spec demote_reseller/1 :: (text()) -> 'ok'.
demote_reseller(Account) when not is_binary(Account) ->
    demote_reseller(wh_util:to_binary(Account));
demote_reseller(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    _ = update_account_definition(AccountId, <<"pvt_reseller">>, false),
    _ = maybe_update_services(AccountId, <<"pvt_reseller">>, false),
    ResellerId = wh_services:find_reseller_id(AccountId),
    io:format("demoting reseller status for account ~s, and now belongs to reseller ~s~n", [AccountId, ResellerId]),
    cascade_reseller_id(ResellerId, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the reseller_id to the provided value on all the sub-accounts
%% of the provided account
%% @end
%%--------------------------------------------------------------------
-spec cascade_reseller_id/2 :: (text(), text()) -> 'ok'.
cascade_reseller_id(Reseller, Account) when not is_binary(Account) ->
    cascade_reseller_id(Reseller, wh_util:to_binary(Account));
cascade_reseller_id(Reseller, Account) when not is_binary(Reseller) ->
    cascade_reseller_id(wh_util:to_binary(Reseller), Account);
cascade_reseller_id(Reseller, Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    ResellerId = wh_util:format_account_id(Reseller, raw),
    ViewOptions = [{<<"startkey">>, [AccountId]}
                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {error, _R} ->
            lager:debug("unable to determin descendants of ~s: ~p", [AccountId, _R]),
            ok;
        {ok, JObjs} ->
            _ = [set_reseller_id(ResellerId, SubAccountId)
                 || JObj <- JObjs
                        ,(SubAccountId = wh_json:get_value(<<"id">>, JObj)) =/= AccountId
                ],
            ok
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set teh reseller_id to the provided value on the provided account
%% @end
%%--------------------------------------------------------------------
-spec set_reseller_id/2 :: (text(), text()) -> 'ok'.
set_reseller_id(Reseller, Account) when not is_binary(Account) ->
    set_reseller_id(Reseller, wh_util:to_binary(Account));
set_reseller_id(Reseller, Account) when not is_binary(Reseller) ->
    set_reseller_id(wh_util:to_binary(Reseller), Account);
set_reseller_id(Reseller, Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    ResellerId = wh_util:format_account_id(Reseller, raw),
    io:format("setting account ~s reseller id to ~s~n", [AccountId, ResellerId]),
    _ = update_account_definition(AccountId, <<"pvt_reseller_id">>, ResellerId),
    maybe_update_services(AccountId, <<"pvt_reseller_id">>, ResellerId).
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_services/3 :: (ne_binary(), ne_binary(), term()) -> 'ok'.
maybe_update_services(AccountId, Key, Value) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {error, _R} ->
            io:format("unable to open services doc ~s: ~p~n", [AccountId, _R]),
            ok;
        {ok, JObj} ->
            case couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(Key, Value, JObj)) of
                {ok, _} -> ok;
                {error, _R} ->
                    io:format("unable to set ~s on services doc ~s: ~p~n", [Key, AccountId, _R]),
                    ok
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_account_definition/3 :: (ne_binary(), ne_binary(), term()) -> 'ok'.
update_account_definition(AccountId, Key, Value) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {error, _R} ->
            io:format("unable to open account ~s defintion: ~p~n", [AccountId, _R]),
            ok;
        {ok, JObj} ->
            case couch_mgr:save_doc(AccountDb, wh_json:set_value(Key, Value, JObj)) of
                {ok, NewJObj} ->
                    _ = couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, NewJObj),            
                    ok;
                {error, _R} ->
                    io:format("unable to set pvt_reseller on account ~s defintion: ~p~n", [AccountId, _R]),
                    ok
            end
    end.
