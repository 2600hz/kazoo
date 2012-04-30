%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whapps_account_config).

-include("whistle_apps.hrl").

-export([get/2]).

-spec get/2 :: (text(), text()) -> wh_json:json_object().
get(Account, Config) ->
    AccountId = account_to_id(Account),
    case wh_cache:peek({?MODULE, Config, AccountId}) of
        {ok, JObj} -> JObj;
        {error, not_found} ->    
            AccountDb = account_to_db(Account),
            case couch_mgr:open_doc(AccountDb, <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>) of
                {error, _} -> 
                    Default = wh_json:new(),
                    wh_cache:store({?MODULE, Config, AccountId}, Default, 900),
                    Default;
                {ok, JObj} ->
                    wh_cache:store({?MODULE, Config, AccountId}, JObj),
                    JObj
            end
    end.

-spec account_to_id/1 :: (string() | atom() | binary()) -> ne_binary().
account_to_id(Account) when not is_binary(Account) ->
    account_to_id(wh_util:to_binary(Account));
account_to_id(<<"account/", _/binary>>=AccountDb) ->
    wh_util:format_account_id(AccountDb, raw);
account_to_id(<<"account%2F", _/binary>>=AccountDb) ->
    wh_util:format_account_id(AccountDb, raw);
account_to_id(AccountId) ->
    AccountId.

-spec account_to_db/1 :: (string() | atom() | binary()) -> ne_binary().
account_to_db(Account) when not is_binary(Account) ->
    account_to_db(wh_util:to_binary(Account));
account_to_db(<<"account/", _/binary>>=AccountDb) ->
    wh_util:format_account_id(AccountDb, encoded);
account_to_db(<<"account%2F", _/binary>>=AccountDb) ->
    AccountDb;
account_to_db(AccountId) ->
    wh_util:format_account_id(AccountId, encoded).
