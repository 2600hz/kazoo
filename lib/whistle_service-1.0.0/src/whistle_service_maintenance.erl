%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(whistle_service_maintenance).

-export([credit/2, credit/3]).
-export([debit/2, debit/3]).

-include_lib("whistle/include/wh_types.hrl").

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
