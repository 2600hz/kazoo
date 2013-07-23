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
    lists:foreach(fun(TestDetail) -> generate_test_account(TestDetail, NumMonths, NumCdrs, CdrJObjFixture) end, get_test_account_details(NumAccounts)).

generate_test_account({AccountName, AccountRealm, User, Pass}, NumMonths, NumCdrs, CdrJObjFixture) ->
    crossbar_maintenance:create_account(AccountName, AccountRealm, User, Pass),
    wh_cache:flush(),
    case whapps_util:get_account_by_realm(AccountRealm) of
	{'ok', AccountDb} ->
	    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
	    {{CurrentYear, CurrentMonth, _},{_,_,_}} = calendar:universal_time(),
	    DateRange = get_last_n_months(CurrentYear, CurrentMonth, NumMonths),
	    lists:foreach(fun(Date) -> generate_test_account_cdrs(Date, AccountId, NumCdrs, CdrJObjFixture) end, DateRange);
	{'multiples', AccountDbs} ->
	    lager:debug("Found multiple DBS for Account Name: ~p", [AccountDbs]);
	{'error', Reason} ->
	    lager:debug("Failed to find account: ~p [~s]", [Reason, AccountName])
    end.

generate_test_account_cdrs({Year, Month}, AccountId, NumCdrs, CdrJObjFixture) ->
    Dates = [calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {12,0,0}}) || Day <- lists:seq(1,NumCdrs)],
    lists:foreach(fun(CreatedAtSeconds) -> add_cdr_to_test_account(CreatedAtSeconds, Year, Month, AccountId, CdrJObjFixture) end, Dates).

add_cdr_to_test_account(CreatedAtSeconds, Year, Month, AccountId, CdrJObjFixture) ->
    AccountShardDb = wh_util:format_account_id(AccountId, Year, Month),
    lager:debug("Account Shard DB: ~s", [AccountShardDb]),
    Props = [{'type', 'cdr'}
	     ,{'crossbar_doc_vsn', 2}
	     ,{'pvt_created', CreatedAtSeconds}
	    ],
    DocId = cdr_util:get_cdr_doc_id(Year, Month),
    JObj = wh_doc:update_pvt_parameters(CdrJObjFixture, AccountShardDb, Props),
    JObj1 = wh_json:set_value(<<"_id">>, DocId, JObj),
    case cdr_util:save_cdr(AccountShardDb, JObj1) of
	{'error', 'max_retries'} -> lager:debug("Too many retries to save, failing");
	'ok' -> 
	    couch_mgr:revise_doc_from_file(AccountShardDb, 'cdr', <<"cdr.json">>),
	    'ok'
    end.
    
delete_test_accounts(NumAccounts, NumMonths) ->
    lists:foreach(fun({AccountName, _AccountRealm, _User, _Pass}) ->
			  case whapps_util:get_accounts_by_name(AccountName) of
			      {'ok', AccountDb} -> 
				  {{CurrentYear, CurrentMonth, _},{_,_,_}} = calendar:universal_time(),
				  Months = get_last_n_months(CurrentYear, CurrentMonth, NumMonths),
				  lager:debug("Found Account: ~s", [AccountName]),
				  lager:debug("DB Name: ~s", [AccountDb]),
				  AccountId = wh_util:format_account_id(AccountDb, 'raw'),
				  lager:debug("AccountId ~s", [AccountId]),
				  
				  AccountDbShards = [wh_util:format_account_id(AccountId, Year, Month) || {Year, Month} <- Months],
				  
				  lists:foreach(fun(AccountShardDb) ->
							couch_mgr:db_delete(AccountShardDb)
						end, AccountDbShards),

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
