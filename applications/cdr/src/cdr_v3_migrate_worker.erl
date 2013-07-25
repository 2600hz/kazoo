%%%-------------------------------------------------------------------
%%% @author Ben Wann <bwann@tickbook.local>
%%% @copyright (C) 2013, Ben Wann
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2013 by Ben Wann <bwann@tickbook.local>
%%%-------------------------------------------------------------------
-module(cdr_v3_migrate_worker).

%% API
-export([migrate_account_cdrs/2]).

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec migrate_account_cdrs(ne_binary(), list()) -> 'ok'.
migrate_account_cdrs(Account, DateList) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    lists:foreach(fun(Date) -> 
                          migrate_cdr_for_date(Account, AccountDb, Date) 
                  end, DateList).			 
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec migrate_account_day(ne_binary(), ne_binary(), {pos_integer(), pos_integer(), any()}) -> any().
migrate_cdr_for_date(AccountId, AccountDb, {Year, Month, _}=Date) ->
    ViewOptions = create_view_options(Date),
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'ok', []} -> [];
        {'error', _E} -> 
            lager:debug("failed to lookup cdrs for ~s: ~p", [AccountDb, _E]), [];
        {'ok', Cdrs} -> 
            lists:foreach(fun(Cdr) -> 
                                  copy_cdr_to_account_mod(AccountId
                                                          ,AccountDb
                                                          ,Cdr
                                                          ,Year
                                                          ,Month
                                                         ) 
                          end, Cdrs)
    end.
    
-spec copy_cdr_to_account_mod(ne_binary(), ne_binary(), wh_json:object(), pos_integer(), pos_integer()) -> any().
copy_cdr_to_account_mod(AccountId, AccountDb, Cdr, Year, Month) ->
    AccountMODb = wh_util:format_account_id(AccountId, Year, Month),
    MODDocId = cdr_util:get_cdr_doc_id(Year, Month),
    JObj = wh_json:set_value(<<"_id">>, MODDocId, Cdr),
    case cdr_util:save_cdr(AccountMODb, JObj) of
        {'error', 'max_retries'} -> lager:error("could not migrate cdr, max_retries reached");
        'ok' -> 'ok'
    end,
    couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_deleted">>, true, Cdr)).

-spec create_view_options({{pos_integer(), pos_integer(), pos_integer()},{pos_integer(),pos_integer(),pos_integer()}}) -> list().
create_view_options(Date) ->
    StartTime = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    EndTime = calendar:datetime_to_gregorian_seconds({Date, {23,59,59}}),
    [{<<"startkey">>, StartTime}, {<<"endkey">>, EndTime}].
