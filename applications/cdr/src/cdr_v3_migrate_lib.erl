%%%-------------------------------------------------------------------
%%% @author Ben Wann <bwann@tickbook.local>
%%% @copyright (C) 2013, Ben Wann
%%% @doc
%%%
%%% @end
%%% Created : 22 Jul 2013 by Ben Wann <bwann@tickbook.local>
%%%-------------------------------------------------------------------
-module(cdr_v3_migrate_lib).

%% API
-export([get_n_month_date_list/2
	 ,generate_test_accounts/1
	 ,delete_test_accounts/1
	]).

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
get_n_month_date_list(CurrentDate, NumMonths) ->
    {{CurrentYear, CurrentMonth, _},{_,_,_}} = CurrentDate,
    DateRange = get_last_n_months(CurrentYear, CurrentMonth, NumMonths),
    lists:foldl(fun({Year,Month},Acc) ->	     
			Days = [{Year,Month,Day} || Day <- lists:seq(calendar:last_day_of_the_month(Year,Month), 1, -1)],
			Acc ++ Days
		end, [], DateRange).

get_test_account_details(NumAccounts) ->
    Accounts = [{<<"migratetest", (wh_util:to_binary(X))/binary>>, <<"migratetest",(wh_util:to_binary(X))/binary,".realm.com">>, <<"testuser", (wh_util:to_binary(X))/binary, "-user">>, <<"password">>} 
		|| 
		   X <- lists:seq(1, NumAccounts)],
    lager:debug("Accounts: ~p", [Accounts]),
    Accounts.

generate_test_accounts(NumAccounts) ->
    lists:foreach(fun({AccountName,AccountRealm,User,Pass}) ->
			  crossbar_maintenance:create_account(AccountName, AccountRealm, User, Pass)
		  end, get_test_account_details(NumAccounts)).

delete_test_accounts(NumAccounts) ->
    lists:foreach(fun({AccountName, _AccountRealm, _User, _Pass}) ->
			  case whapps_util:get_accounts_by_name(AccountName) of
			      {'ok', AccountDb} -> 
				  lager:debug("Found Account: ~s", [AccountName]),
				  lager:debug("DB Name: ~s", [AccountDb]),
				  AccountId = wh_util:format_account_id(AccountDb, 'raw'),
				  lager:debug("AccountId ~s", [AccountId]),
				  couch_mgr:del_doc(<<"accounts">>, AccountId),
				  couch_mgr:db_delete(AccountDb);
			      {'multiples', AccountDbs} ->
				  lager:debug("Found multiple DBS for Account Name: ~p", [AccountDbs]);
			      {'error', Reason} ->
				  lager:debug("Failed to find account: ~p [~s]", [Reason, AccountName])
			  end

			      
		  end, get_test_account_details(NumAccounts)).    
		     
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_last_n_months(CurrentYear, CurrentMonth, NumMonths) when CurrentMonth =< 12, CurrentMonth > 0 ->
    get_last_n_months(CurrentYear, CurrentMonth, NumMonths, []).

get_last_n_months(_, _, 0, Acc) ->
    lists:reverse(Acc);
get_last_n_months(CurrentYear, Month, NumMonths, Acc) when NumMonths > 0 ->
    case Month == 1 of
	'true' -> get_last_n_months(CurrentYear - 1, 12, NumMonths - 1, [{CurrentYear, Month} | Acc]);
	'false' -> get_last_n_months(CurrentYear, Month -1, NumMonths - 1, [{CurrentYear, Month} | Acc])
    end.
