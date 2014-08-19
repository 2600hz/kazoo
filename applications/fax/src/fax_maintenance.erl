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
-export([migrate/0, migrate/1]).

-spec migrate() -> 'ok'.
-spec migrate(atom() | string() | binary()) -> 'ok'.

migrate() ->
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_faxes_fold(A, C, Total) end, 1, Accounts),
    'ok'.
migrate(Account) when not is_binary(Account) ->
    migrate(wh_util:to_binary(Account));
migrate(Account) ->
    migrate_faxes(Account).

%% ====================================================================
%% Internal functions
%% ====================================================================

migrate_faxes_fold(AccountDb, Current, Total) ->
    io:format("migrating faxes in database (~p/~p) '~s'~n", [Current, Total, AccountDb]),
    _ = migrate_faxes(AccountDb),
    Current + 1.

-spec migrate_faxes(atom() | string() | binary()) -> 'ok'.
migrate_faxes(Account) when not is_binary(Account) ->
    migrate_faxes(wh_util:to_binary(Account));
migrate_faxes(Account) ->
    migrate_private_media(Account),
    recover_private_media(Account).

-spec migrate_private_media(ne_binary()) -> 'ok'.
-spec migrate_private_media(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
-spec maybe_migrate_private_media(ne_binary(), wh_json:object()) -> 'ok'.

migrate_private_media(Account) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"private_media">>}],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs3}->
            _ = [maybe_migrate_private_media(AccountDb, JObj) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch private media files in db ~s: ~p~n", [AccountDb, E3])
    end.

maybe_migrate_private_media(AccountDb, JObj) ->
    DocId = wh_json:get_value(<<"id">>, JObj),
    {'ok', Doc } = couch_mgr:open_doc(AccountDb, DocId),
    migrate_private_media(AccountDb, Doc, wh_json:get_value(<<"media_type">>, Doc)).

migrate_private_media(AccountDb, Doc, <<"tiff">>) ->
    _ = couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_type">>, <<"fax">>, Doc)),
    'ok';
migrate_private_media(_AccountDb, _JObj, _MediaType) -> 'ok'.

-spec recover_private_media(ne_binary()) -> 'ok'.
-spec recover_private_media(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
-spec maybe_recover_private_media(ne_binary(), wh_json:object()) -> 'ok'.

recover_private_media(Account) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"fax">>}],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs3}->
            _ = [maybe_recover_private_media(AccountDb, JObj) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch fax docs in db ~s: ~p~n", [AccountDb, E3])
    end.

maybe_recover_private_media(AccountDb, JObj) ->
    DocId = wh_json:get_value(<<"id">>, JObj),
    {'ok', Doc } = couch_mgr:open_doc(AccountDb, DocId),
    recover_private_media(AccountDb, Doc, wh_json:get_value(<<"media_type">>, Doc)).

recover_private_media(_AccountDb, _Doc, <<"tiff">>) ->
    'ok';
recover_private_media(AccountDb, Doc, _MediaType) ->
    _ = couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_type">>, <<"private_media">>, Doc)),
    'ok'.

