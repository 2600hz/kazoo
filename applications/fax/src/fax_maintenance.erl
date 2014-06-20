%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------

-module(fax_maintenance).
-include("fax.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([migrate/0]).


-spec migrate() -> 'ok'.
-spec migrate_faxes(atom() | string() | binary()) -> 'ok'.

migrate() ->
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_faxes_fold(A, C, Total) end, 1, Accounts),
    'ok'.

migrate_faxes_fold(AccountDb, Current, Total) ->
    io:format("migrating faxes in database (~p/~p) '~s'~n", [Current, Total, AccountDb]),
    _ = migrate_faxes(AccountDb),
    Current + 1.

migrate_faxes(Account) when not is_binary(Account) ->
    migrate_faxes(wh_util:to_binary(Account));
migrate_faxes(Account) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    case couch_mgr:get_results(AccountDb, <<"media/listing_private_media">>, []) of
        {'ok', []} -> io:format("no private media files in db for fax migration ~s~n", [AccountDb]);
        {'ok', JObjs3}->
            _ = [migrate_fax(AccountDb, JObj) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch private media files in db ~s: ~p~n", [AccountDb, E3])
    end.

-spec migrate_fax(ne_binary(), wh_json:object()) -> 'ok'.
migrate_fax(AccountDb, JObj) ->
    DocId = wh_json:get_value(<<"id">>, JObj),
    {'ok', Doc } = couch_mgr:open_doc(AccountDb, DocId),
    _ = couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_type">>, <<"fax">>, Doc)),
    'ok'.

%% ====================================================================
%% Internal functions
%% ====================================================================


