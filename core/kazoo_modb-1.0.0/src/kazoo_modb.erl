%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600HZ, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_modb).

-include("kazoo_modb.hrl").

-export([get_results/3]).
-export([open_doc/2, open_doc/3, open_doc/4]).
-export([save_doc/2, save_doc/3, save_doc/4]).
-export([get_modb/1, get_modb/2, get_modb/3]).
-export([maybe_archive_modb/1]).
-export([refresh_views/1]).
-export([create/1]).
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

-spec get_results(ne_binary(), ne_binary(), wh_proplist()) ->
                         {'ok', wh_json:objects()} |
                         {'error', atom()}.
-spec get_results(ne_binary(), ne_binary(), wh_proplist(), non_neg_integer()) ->
                         {'ok', wh_json:objects()} |
                         {'error', atom()}.
get_results(Account, View, ViewOptions) ->
    get_results(Account, View, ViewOptions, 3).

get_results(_Account, _View, _ViewOptions, Retry) when Retry =< 0 ->
    {'error', 'retries_exceeded'};
get_results(Account, View, ViewOptions, Retry) ->
    AccountMODb = get_modb(Account, ViewOptions),
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    case couch_mgr:get_results(EncodedMODb, View, ViewOptions) of
        {'error', 'not_found'} ->
            get_results_not_found(Account, View, ViewOptions, Retry);
        Results -> Results
    end.

-spec get_results_not_found(ne_binary(), ne_binary(), wh_proplist(), integer()) ->
                                   {'ok', wh_json:objects()}.
get_results_not_found(Account, View, ViewOptions, Retry) ->
    AccountMODb = get_modb(Account, ViewOptions),
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    case couch_mgr:db_exists(EncodedMODb) of
        'true' ->
            refresh_views(AccountMODb),
            get_results(Account, View, ViewOptions, Retry-1);
        'false' ->
            get_results_missing_db(Account, View, ViewOptions, Retry)
    end.

-spec get_results_missing_db(ne_binary(), ne_binary(), wh_proplist(), integer()) ->
                                    {'ok', wh_json:objects()}.
get_results_missing_db(Account, View, ViewOptions, Retry) ->
    AccountMODb = get_modb(Account, ViewOptions),
    case maybe_create(AccountMODb) of
        'true' -> get_results(Account, View, ViewOptions, Retry-1);
        'false' -> {'ok', []}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec open_doc(ne_binary(), ne_binary()) ->
                      {'ok', wh_json:object()} |
                      {'error', atom()}.
-spec open_doc(ne_binary(), ne_binary(), integer()) ->
                      {'ok', wh_json:object()} |
                      {'error', atom()}.
-spec open_doc(ne_binary(), ne_binary(), integer(), integer()) ->
                      {'ok', wh_json:object()} |
                      {'error', atom()}.
open_doc(Account, <<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId) ->
    AccountMODb = get_modb(Account, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    couch_open(AccountMODb, DocId);
open_doc(Account, DocId) ->
    AccountMODb = get_modb(Account),
    couch_open(AccountMODb, DocId).

open_doc(Account, DocId, Timestamp) ->
    AccountMODb = get_modb(Account, Timestamp),
    couch_open(AccountMODb, DocId).

open_doc(Account, DocId, Year, Month) ->
    AccountMODb = get_modb(Account, Year, Month),
    couch_open(AccountMODb, DocId).

-spec couch_open(ne_binary(), ne_binary()) ->
                        {'ok', wh_json:object()} |
                        {'error', atom()}.
couch_open(AccountMODb, DocId) ->
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    case couch_mgr:open_doc(EncodedMODb, DocId) of
        {'ok', _}=Ok -> Ok;
        {'error', _E}=Error ->
            lager:error("fail to open doc ~p in ~p reason: ~p", [DocId, EncodedMODb, _E]),
            Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save_doc(ne_binary(), ne_binary()) ->
                      {'ok', wh_json:object()} |
                      {'error', atom()}.
-spec save_doc(ne_binary(), ne_binary(), integer()) ->
                      {'ok', wh_json:object()} |
                      {'error', atom()}.
-spec save_doc(ne_binary(), ne_binary(), integer(), integer()) ->
                      {'ok', wh_json:object()} |
                      {'error', atom()}.
save_doc(Account, Doc) ->
    AccountMODb = get_modb(Account),
    couch_save(AccountMODb, Doc, 3).

save_doc(Account, Doc, Timestamp) ->
    AccountMODb = get_modb(Account, Timestamp),
    couch_save(AccountMODb, Doc, 3).

save_doc(Account, Doc, Year, Month) ->
    AccountMODb = get_modb(Account, Year, Month),
    couch_save(AccountMODb, Doc, 3).

-spec couch_save(ne_binary(), ne_binary(), integer()) ->
                        {'ok', wh_json:object()} |
                        {'error', atom()}.
couch_save(AccountMODb, _Doc, 0) ->
    lager:error("failed to save doc in ~p", AccountMODb);
couch_save(AccountMODb, Doc, Retry) ->
     EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    case couch_mgr:save_doc(EncodedMODb, Doc) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            lager:warning("modb ~p not found, creating...", [AccountMODb]),
            _ = maybe_create(AccountMODb),
            couch_save(AccountMODb, Doc, Retry-1);
        {'error', _E}=Error ->
            lager:error("account mod save error: ~p", [_E]),
            Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_modb(ne_binary()) -> ne_binary().
-spec get_modb(ne_binary(), wh_proplist() | integer()) -> ne_binary().
-spec get_modb(ne_binary(), integer(), integer()) -> ne_binary().
get_modb(Account) ->
    {Year, Month, _} = erlang:date(),
    get_modb(Account, Year, Month).

get_modb(Account, Props) when is_list(Props) ->
    case props:get_value('month', Props) of
        'undefined' -> get_modb(Account);
        Month ->
            case props:get_value('year', Props) of
                'undefined' ->
                    {Year, _, _} = erlang:date(),
                    get_modb(Account, Year, Month);
                Year ->
                    get_modb(Account, Year, Month)
             end
     end;
get_modb(Account, Timestamp) ->
    wh_util:format_account_mod_id(Account, Timestamp).

get_modb(Account, Year, Month) ->
    wh_util:format_account_mod_id(Account, Year, Month).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_create(ne_binary()) -> boolean().
maybe_create(<<_:32/binary, "-", Year:4/binary, Month:2/binary>>=AccountMODb) ->
    {Y, M, _} = erlang:date(),
    case {wh_util:to_binary(Y), wh_util:pad_month(M)} of
        {Year, Month} ->
            create(AccountMODb),
            'true';
        _ -> 'false'
    end;
maybe_create(<<"account/", AccountId/binary>>) ->
    maybe_create(binary:replace(AccountId, <<"/">>, <<>>, ['global']));
maybe_create(<<"account%2F", AccountId/binary>>) ->
    maybe_create(binary:replace(AccountId, <<"%2F">>, <<>>, ['global'])).

-spec create(ne_binary()) -> 'ok'.
create(AccountMODb) ->
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    do_create(AccountMODb, couch_mgr:db_exists(EncodedMODb)).

-spec do_create(ne_binary(), boolean()) -> 'ok'.
do_create(_AccountMODb, 'true') -> 'ok';
do_create(AccountMODb, 'false') ->
    lager:debug("create modb ~p", [AccountMODb]),
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    _ = couch_mgr:db_create(EncodedMODb),
    _ = refresh_views(EncodedMODb),
    create_routines(AccountMODb).

-spec refresh_views(ne_binary()) -> 'ok'.
refresh_views(AccountMODb) ->
    lager:debug("init modb ~p", [AccountMODb]),
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    _ = couch_mgr:revise_views_from_folder(EncodedMODb, ?MODULE),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_routines(ne_binary()) -> 'ok'.
create_routines(AccountMODb) ->
    Routines = whapps_config:get(?CONFIG_CAT, <<"routines">>, []),
    lists:foldl(
        fun(Mod, _) ->
            Module = wh_util:to_atom(Mod),
            _ = Module:modb(AccountMODb),
            'ok'
        end
        ,'ok'
        ,Routines
    ).

-spec maybe_archive_modb(ne_binary()) -> 'ok'.
maybe_archive_modb(AccountMODb) ->
    {Year, Month, _} = erlang:date(),

    case should_archive(AccountMODb, Year, Month) of
        'true' ->
            lager:info("account modb ~s needs archiving", [AccountMODb]),
            'ok' = archive_modb(AccountMODb),
            lager:info("account modb ~s archived, removing the db", [AccountMODb]),
            Rm = couch_mgr:db_delete(AccountMODb),
            lager:info("account modb ~s deleted: ~p", [AccountMODb, Rm]);
        'false' ->
            lager:info("account modb ~s still current enough to keep", [AccountMODb])
    end.

-spec should_archive(ne_binary(), wh_year(), wh_month()) -> boolean().
should_archive(AccountMODb, Year, Month) ->
    case kazoo_modb_util:split_account_mod(AccountMODb) of
        {_AccountId, Year, Month} -> 'false';
        {_AccountId, ModbYear, ModbMonth} ->
            Months = (Year * 12) + Month,
            ModbMonths = (ModbYear * 12) + ModbMonth,
            (Months - ModbMonths) > whapps_config:get_integer(?CONFIG_CAT, <<"active_modbs">>, 6)
    end.

-spec archive_modb(ne_binary()) -> 'ok'.
archive_modb(AccountMODb) ->
    {'ok', DbInfo} = couch_mgr:db_info(AccountMODb),

    MaxDocs = whapps_config:get_integer(?CONFIG_CAT, <<"max_concurrent_docs_to_archive">>, 500),
    Filename = filename:join(["/tmp", <<AccountMODb/binary, ".archive.json">>]),
    {'ok', File} = file:open(Filename, ['write']),

    lager:debug("archiving to ~s", [Filename]),
    archive_modb(AccountMODb, File, MaxDocs, wh_json:get_integer_value(<<"doc_count">>, DbInfo), 0),
    file:close(File).

%% MaxDocs = The biggest set of docs to pull from Couch
%% N = The number of docs in the DB that haven't been archived
%% Pos = Which doc will the next query start from (the offset)
-spec archive_modb(ne_binary(), file:io_device(), pos_integer(), non_neg_integer(), non_neg_integer()) -> 'ok'.
archive_modb(AccountMODb, _File,  _MaxDocs, 0, _Pos) ->
    lager:debug("account modb ~s done with exportation", [AccountMODb]);
archive_modb(AccountMODb, File, MaxDocs, N, Pos) when N =< MaxDocs ->
    lager:debug("fetching next ~b docs", [N]),
    ViewOptions = [{'limit', N}
                   ,{'skip', Pos}
                   ,'include_docs'
                  ],
    case couch_mgr:all_docs(AccountMODb, ViewOptions) of
        {'ok', []} -> lager:debug("no docs left after pos ~p, done", [Pos]);
        {'ok', Docs} ->
            'ok' = archive_modb_docs(File, Docs),
            lager:debug("archived ~p docs, done here", [N]);
        {'error', _E} ->
            lager:debug("error ~p asking for ~p docs from pos ~p", [_E, N, Pos]),
            timer:sleep(500),
            archive_modb(AccountMODb, File, MaxDocs, N, Pos)
    end;
archive_modb(AccountMODb, File, MaxDocs, N, Pos) ->
    lager:debug("fetching next ~b docs", [MaxDocs]),
    ViewOptions = [{'limit', MaxDocs}
                   ,{'skip', Pos}
                   ,'include_docs'
                  ],
    case couch_mgr:all_docs(AccountMODb, ViewOptions) of
        {'ok', []} -> lager:debug("no docs left after pos ~p, done", [Pos]);
        {'ok', Docs} ->
            'ok' = archive_modb_docs(File, Docs),
            lager:debug("archived ~p docs", [MaxDocs]),
            archive_modb(AccountMODb, File, MaxDocs, N - MaxDocs, Pos + MaxDocs);
        {'error', _E} ->
            lager:debug("error ~p asking for ~p docs from pos ~p", [_E, N, Pos]),
            timer:sleep(500),
            archive_modb(AccountMODb, File, MaxDocs, N, Pos)
    end.

-spec archive_modb_docs(file:io_device(), wh_json:objects()) -> 'ok'.
archive_modb_docs(File, Docs) ->
    [archive_modb_doc(File, wh_json:get_value(<<"doc">>, D)) || D <- Docs],
    'ok'.

-spec archive_modb_doc(file:io_device(), wh_json:object()) -> 'ok'.
archive_modb_doc(File, Doc) ->
    'ok' = file:write(File, [wh_json:encode(Doc), $\n]).
