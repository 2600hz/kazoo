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
-spec migrate_account_cdrs(account_id(), wh_proplist()) -> 'ok'.
migrate_account_cdrs(AccountId, _DateList) ->
    lager:info("cdr_v3_migrate_worker: work started for AccountId: ~s", [AccountId]),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    lists:foreach(fun(Date) -> 
                          migrate_cdr_for_date(AccountId, AccountDb, Date) 
                  end, _DateList).			 

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec migrate_cdr_for_date(account_id(), account_db(), wh_date()) -> any().
migrate_cdr_for_date(AccountId, AccountDb, {Year, Month, _}=Date) ->
    ViewOptions = create_view_options(Date),
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'error', _E} -> 
            lager:error("failed to lookup cdrs for ~s: ~p", [AccountDb, _E]), [];
        {'ok', Cdrs} -> 
            lists:foreach(fun(CdrDoc) -> 
                                  copy_cdr_to_account_mod(AccountId
                                                          ,AccountDb
                                                          ,CdrDoc
                                                          ,Year
                                                          ,Month
                                                         ) 
                          end, get_docs_from_view_results(AccountDb, Cdrs, []))
    end.
    
-spec get_docs_from_view_results(account_db(), wh_json:object(), wh_proplist()) -> wh_proplist().
get_docs_from_view_results(_, [], Acc) -> Acc;
get_docs_from_view_results(AccountDb, [NextCdr|RestCdrs], Acc) ->
    CdrId = wh_json:get_value(<<"id">>, NextCdr),
    case couch_mgr:open_doc(AccountDb, CdrId) of
        {'ok', JObj} -> 
            JObj1 = wh_json:delete_key(<<"_rev">>, JObj),
            get_docs_from_view_results(AccountDb, RestCdrs, [JObj1 | Acc]);
        {'error', _}=_E  -> lager:error("cdr_v3_migrate worker: could not load cdr ~p", [_E])
    end.
    

-spec copy_cdr_to_account_mod(account_id(), account_db(), ne_binary(), wh_year(), wh_month()) -> any().
copy_cdr_to_account_mod(AccountId, _AccountDb, CdrDoc, Year, Month) ->
    AccountMODb = wh_util:format_account_id(AccountId, Year, Month),
    MODDocId = cdr_util:get_cdr_doc_id(Year, Month),
    JObj = wh_json:set_values([{<<"_id">>, MODDocId}
                               ,{<<"pvt_account_id">>, AccountId}
                               ,{<<"pvt_account_db">>, AccountMODb}
                              ], CdrDoc),
    case cdr_util:save_cdr(AccountMODb, JObj) of
        {'error', _}=_E -> lager:error("could not migrate cdr ~p", [_E]);
        'ok' -> 'ok'
    end,
    couch_mgr:save_doc(_AccountDb, wh_json:set_value(<<"pvt_deleted">>, true, CdrDoc)).

-spec create_view_options(wh_datetime()) -> wh_proplist().
create_view_options(Date) ->
    StartTime = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    EndTime = calendar:datetime_to_gregorian_seconds({Date, {23,59,59}}),
    [{<<"startkey">>, StartTime}, {<<"endkey">>, EndTime}].
