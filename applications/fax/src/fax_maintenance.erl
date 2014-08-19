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
    recover_private_media(Account),
    migrate_faxes_to_modb(Account).

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

-spec migrate_faxes_to_modb(ne_binary()) -> 'ok'.
-spec maybe_migrate_fax_to_modb(ne_binary(), wh_json:object()) -> 'ok'.
-spec migrate_fax_to_modb(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.

migrate_faxes_to_modb(Account) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"fax">>}],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> io:format("no fax docs in db for fax migration ~s~n", [AccountDb]);
        {'ok', JObjs3}->
            _ = [maybe_migrate_fax_to_modb(AccountDb, JObj) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch fax docs in db ~s: ~p~n", [AccountDb, E3])
    end.

maybe_migrate_fax_to_modb(AccountDb, JObj) ->
    DocId = wh_json:get_value(<<"id">>, JObj),
    case couch_mgr:open_doc(AccountDb, DocId) of
        {'ok', Doc} ->
            case wh_json:get_value(<<"_attachments">>, Doc) of
                'undefined' ->
                     case whapps_config:get_is_true(<<"fax">>, <<"delete_empty_faxes">>, 'false') of
                         'true' ->
                             io:format("deleting no attachments fax doc ~s from ~s~n",[DocId, AccountDb]),
                             couch_mgr:del_doc(AccountDb, Doc);
                         'false' -> 'ok'
                     end;
                _Attachments ->
                    migrate_fax_to_modb(AccountDb, DocId, Doc)
            end;
        {'error', E} ->
            io:format("unable to get document ~s for fax migration : ~p",[DocId, E])
    end.

migrate_fax_to_modb(AccountDb, DocId, JObj) ->
    Timestamp = wh_json:get_integer_value(<<"pvt_created">>, JObj, wh_util:current_tstamp()),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    AccountMODb = kazoo_modb:get_modb(AccountDb, Year, Month),
    FaxMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    FaxId = <<(wh_util:to_binary(Year))/binary
             ,(wh_util:pad_month(Month))/binary
             ,"-"
             ,DocId/binary
            >>,
    io:format("moving doc ~s/~s to ~s/~s~n",[AccountDb, DocId, AccountMODb, FaxId]),
    kazoo_modb:create(AccountMODb),
    case couch_mgr:move_doc(AccountDb, DocId, FaxMODb, FaxId, []) of
        {'ok', _JObj} -> io:format("document ~s moved to ~s~n",[DocId, FaxId]);
        {'error', Error} -> io:format("error ~p moving document ~s to ~s~n",[Error, DocId, FaxId])
    end.
