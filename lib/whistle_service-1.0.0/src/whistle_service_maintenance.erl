%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(whistle_service_maintenance).

-export([credit/2, credit/3]).
-export([debit/2, debit/3]).
-export([sync_account/1]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").

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
%%
%% @end
%%--------------------------------------------------------------------
-spec sync_account/1 :: (text()) -> 'no_return'.
sync_account(Account) when not is_binary(Account) ->
    sync_account(wh_util:to_binary(Account));
sync_account(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    case wh_resellers:fetch(AccountId) of
        {error, no_service_plan} -> 
            io:format("account has no service plans~n", []),
            no_return;
        {ok, R1} ->
            lager:debug("sync devices~n", []),
            {ok, Devices} = couch_mgr:get_all_results(AccountDb, <<"devices/crossbar_listing">>),
            DeviceTypes = [get_device_type(Device) || Device <- Devices],
            R2 = wh_service_devices:update(DeviceTypes, R1, updated),
            
            lager:debug("sync numbers~n", []),
            {ok, PhoneNumbers} = couch_mgr:open_doc(AccountDb, <<"phone_numbers">>),
            R3 = wh_service_numbers:update(PhoneNumbers, R2),

            lager:debug("sync limits~n", []),
            {ok, Limits} = couch_mgr:open_doc(AccountDb, <<"limits">>),
            R4 = wh_service_numbers:update(Limits, R3),

            lager:debug("commit changess~n", []),
            ok = wh_resellers:commit_changes(R4)
    end.


-spec get_device_type/1 :: (wh_json:json_object()) -> ne_binary().
get_device_type(JObj) ->
    DeviceType = wh_json:get_value(<<"device_type">>, JObj
                                   ,wh_json:get_value([<<"value">>, <<"device_type">>], JObj, <<"sip_device">>)),
    case lists:member(DeviceType, [<<"sip_device">>, <<"cellphone">>, <<"softphone">>]) of
        true -> DeviceType;
        false -> <<"sip_device">>
    end.
