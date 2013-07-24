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
	 ,generate_test_accounts/3
	 ,delete_test_accounts/2
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
    [{<<"migratetest", (wh_util:to_binary(X))/binary>>, <<"migratetest",(wh_util:to_binary(X))/binary,".realm.com">>, <<"testuser", (wh_util:to_binary(X))/binary, "-user">>, <<"password">>} 
     || X <- lists:seq(1, NumAccounts)].

generate_test_accounts(NumAccounts, NumMonths, NumCdrs) ->
    CdrJObjFixture = wh_json:load_fixture_from_file('cdr', 'fixtures/cdr.json'),
    lists:foreach(fun(AccountDetail) -> generate_test_account(AccountDetail, NumMonths, NumCdrs, CdrJObjFixture) end, get_test_account_details(NumAccounts)).

generate_test_account({AccountName, AccountRealm, User, Pass}, NumMonths, NumCdrs, CdrJObjFixture) ->
    crossbar_maintenance:create_account(AccountName, AccountRealm, User, Pass),
    wh_cache:flush(),
    case whapps_util:get_account_by_realm(AccountRealm) of
	{'ok', AccountDb} ->
	    {{CurrentYear, CurrentMonth, _},{_,_,_}} = calendar:universal_time(),
	    DateRange = get_last_n_months(CurrentYear, CurrentMonth, NumMonths),
	    lists:foreach(fun(Date) -> generate_test_account_cdrs(Date, AccountDb, NumCdrs, CdrJObjFixture) end, DateRange);
	{'multiples', AccountDbs} ->
	    lager:debug("Found multiple DBS for Account Name: ~p", [AccountDbs]);
	{'error', Reason} ->
	    lager:debug("Failed to find account: ~p [~s]", [Reason, AccountName])
    end.

generate_test_account_cdrs({Year, Month}, AccountDb, NumCdrs, CdrJObjFixture) ->
    Dates = [calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {12,0,0}}) || Day <- lists:seq(1, calendar:last_day_of_the_month(Year, Month))],
    lists:foreach(fun(CreatedAtSeconds) -> 
			  add_cdr_to_test_account(CreatedAtSeconds, AccountDb, CdrJObjFixture, NumCdrs)
		  end, Dates).

add_cdr_to_test_account(CreatedAtSeconds, AccountDb, CdrJObjFixture, NumCdrs) ->
    
    wh_util:for(NumCdrs, fun(_N) ->
				 Props = [{'type', 'cdr'}
					  ,{'crossbar_doc_vsn', 2}
					  ,{'pvt_created', CreatedAtSeconds}
					 ],
				 JObj = wh_doc:update_pvt_parameters(CdrJObjFixture, AccountDb, Props),
				 case couch_mgr:save_doc(AccountDb, JObj) of
				     {'error', 'not_found'} -> lager:debug("Could not save cdr");
				     {'error', _} -> 'ok';
				     {'ok', _} -> 'ok'
				 end
			 end).

    
delete_test_accounts(NumAccounts, NumMonths) ->
    lists:foreach(fun({_AccountName, AccountRealm, _User, _Pass}) ->
			  case whapps_util:get_account_by_realm(AccountRealm) of
			      {'ok', AccountDb} -> 
				  {{CurrentYear, CurrentMonth, _},{_,_,_}} = calendar:universal_time(),
				  Months = get_last_n_months(CurrentYear, CurrentMonth, NumMonths),
				  AccountId = wh_util:format_account_id(AccountDb, 'raw'),
				  AccountDbShards = [wh_util:format_account_id(AccountId, Year, Month) || {Year, Month} <- Months],
				  lists:foreach(fun(AccountShardDb) ->
							couch_mgr:db_delete(AccountShardDb)
						end, AccountDbShards),

				  couch_mgr:del_doc(<<"accounts">>, AccountId),
				  couch_mgr:db_delete(AccountDb);
			      {'multiples', AccountDbs} ->
				  lager:debug("Found multiple DBS for Account Name: ~p", [AccountDbs]);
			      {'error', Reason} ->
				  lager:debug("Failed to find account: ~p [~s]", [Reason, _AccountName])
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
