%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%% Utility module for V3 Kazoo Migration
%%% @end
%%% @contributors
%%%   Ben Wann
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
-spec get_n_month_date_list({{pos_integer(), pos_integer(), any()}, any()}, pos_integer()) -> any().
get_n_month_date_list({{CurrentYear, CurrentMonth, _}, _}, NumMonths) ->
    DateRange = get_last_n_months(CurrentYear, CurrentMonth, NumMonths),
    lists:foldl(fun get_n_month_date_list_fold/2, [], DateRange).

-spec get_n_month_date_list_fold({pos_integer(), pos_integer()}, list()) -> any().
get_n_month_date_list_fold({Year,Month}, Acc) ->
    [{Year,Month,Day}
     || Day <- lists:seq(calendar:last_day_of_the_month(Year,Month), 1, -1)
    ] ++ Acc.

-spec get_test_account_details(pos_integer()) -> any().
get_test_account_details(NumAccounts) ->
    [{<<"migratetest", (wh_util:to_binary(X))/binary>>
          , <<"migratetest",(wh_util:to_binary(X))/binary,".realm.com">>
          , <<"testuser", (wh_util:to_binary(X))/binary, "-user">>
          , <<"password">>
     } 
     || X <- lists:seq(1, NumAccounts)].

-spec generate_test_accounts(pos_integer(), pos_integer(), pos_integer()) -> any().
generate_test_accounts(NumAccounts, NumMonths, NumCdrs) ->
    CdrJObjFixture = wh_json:load_fixture_from_file('cdr', 'fixtures/cdr.json'),
    lists:foreach(fun(AccountDetail) -> 
                          generate_test_account(AccountDetail, NumMonths, NumCdrs, CdrJObjFixture) 
                  end, get_test_account_details(NumAccounts)).

-spec generate_test_account({ne_binary(),ne_binary(), ne_binary(), ne_binary()}
                            ,pos_integer(), pos_integer()
                            ,wh_json:object()
                           ) -> any().
generate_test_account({AccountName, AccountRealm, User, Pass}, NumMonths, NumCdrs, CdrJObjFixture) ->
    crossbar_maintenance:create_account(AccountName, AccountRealm, User, Pass),
    wh_cache:flush(),
    case whapps_util:get_account_by_realm(AccountRealm) of
	{'ok', AccountDb} ->
	    {{CurrentYear, CurrentMonth, _},{_,_,_}} = calendar:universal_time(),
	    DateRange = get_last_n_months(CurrentYear, CurrentMonth, NumMonths),
	    lists:foreach(fun(Date) -> 
                                  generate_test_account_cdrs(Date, AccountDb, NumCdrs, CdrJObjFixture) 
                          end, DateRange);
	{'multiples', AccountDbs} ->
	    lager:debug("Found multiple DBS for Account Name: ~p", [AccountDbs]);
	{'error', Reason} ->
	    lager:debug("Failed to find account: ~p [~s]", [Reason, AccountName])
    end.

-spec generate_test_account_cdrs({pos_integer(), pos_integer()}
                                 ,ne_binary()
                                 ,pos_integer()
                                 ,wh_json:object()
                                ) -> any().
generate_test_account_cdrs({Year, Month}, AccountDb, NumCdrs, CdrJObjFixture) ->
    Dates = [calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {12,0,0}}) 
             || Day <- lists:seq(1, calendar:last_day_of_the_month(Year, Month))],
    lists:foreach(fun(CreatedAtSeconds) -> 
			  add_cdr_to_test_account(CreatedAtSeconds, AccountDb, CdrJObjFixture, NumCdrs)
		  end, Dates).

-spec add_cdr_to_test_account(pos_integer(), ne_binary(), wh_json:object(), pos_integer()) -> 'ok'.
add_cdr_to_test_account(_, _, _, 0) -> 'ok';
add_cdr_to_test_account(CreatedAtSeconds, AccountDb, CdrJObjFixture, NumCdrs) ->
    Props = [{'type', 'cdr'}
             ,{'crossbar_doc_vsn', 2}
             ,{'pvt_created', CreatedAtSeconds}
            ],
    JObj = wh_doc:update_pvt_parameters(CdrJObjFixture, AccountDb, Props),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {'error', 'not_found'} -> lager:debug("Could not save cdr");
        {'error', _} -> 'ok';
        {'ok', _} -> 'ok'
    end,
    add_cdr_to_test_account(CreatedAtSeconds, AccountDb, CdrJObjFixture, NumCdrs - 1).
    
-spec delete_test_accounts(pos_integer(), pos_integer()) -> any().
delete_test_accounts(NumAccounts, NumMonths) ->
    lists:foreach(fun(AccountDetails) -> 
                          delete_test_account(AccountDetails, NumMonths) 
                  end, get_test_account_details(NumAccounts)).    

-spec delete_test_account({ne_binary(), ne_binary(), ne_binary(), ne_binary()}
                          ,pos_integer()) -> 'ok' | {'error', any()}.
delete_test_account({_AccountName, AccountRealm, _User, _Pass}, NumMonths) ->		     
    case whapps_util:get_account_by_realm(AccountRealm) of
        {'ok', AccountDb} -> 
            {{CurrentYear, CurrentMonth, _},{_,_,_}} = calendar:universal_time(),
            Months = get_last_n_months(CurrentYear, CurrentMonth, NumMonths),
            AccountId = wh_util:format_account_id(AccountDb, 'raw'),
            AccountMODbs = [wh_util:format_account_id(AccountId, Year, Month) 
                            || {Year, Month} <- Months],
            lists:foreach(fun(AccountMODb) ->
                                  couch_mgr:db_delete(AccountMODb)
                          end, AccountMODbs),            
            couch_mgr:del_doc(<<"accounts">>, AccountId),
            couch_mgr:db_delete(AccountDb),
            'ok';
        {'multiples', AccountDbs} ->
            lager:debug("Found multiple DBS for Account Name: ~p", [AccountDbs]),
            {'error', 'not_unique'};
        {'error', Reason} ->
            lager:debug("Failed to find account: ~p [~s]", [Reason, _AccountName]),
            {'error', Reason}
    end.

-spec get_last_n_months(pos_integer(), pos_integer(), pos_integer()) -> list().
get_last_n_months(CurrentYear, CurrentMonth, NumMonths) when CurrentMonth =< 12, CurrentMonth > 0 ->
    get_last_n_months(CurrentYear, CurrentMonth, NumMonths, []).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_last_n_months(_, _, 0, Acc) ->
    lists:reverse(Acc);
get_last_n_months(CurrentYear, 1, NumMonths, Acc) ->
    get_last_n_months(CurrentYear - 1, 12, NumMonths - 1, [{CurrentYear, 1} | Acc]);
get_last_n_months(CurrentYear, Month, NumMonths, Acc) ->
    get_last_n_months(CurrentYear, Month -1, NumMonths - 1, [{CurrentYear, Month} | Acc]).
   
