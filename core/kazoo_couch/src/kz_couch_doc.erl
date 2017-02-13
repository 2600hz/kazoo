%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% Util functions used by kazoo_couch
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(kz_couch_doc).

%% Doc related
-export([open_doc/3, open_doc/4
        ,lookup_doc_rev/3
        ,save_doc/4
        ,save_docs/4
        ,del_doc/4
        ,del_docs/4
        ,ensure_saved/4
        ,copy_doc/3
        ,move_doc/3
        ]).

-include("kz_couch.hrl").

%% Throttle how many docs we bulk insert to BigCouch
-define(MAX_BULK_INSERT, 2000).

-type copy_function() :: fun((server(), ne_binary(), kz_json:object(), kz_proplist()) ->
                                    {'ok', kz_json:object()} | couchbeam_error()).
-export_type([copy_function/0]).
-define(COPY_DOC_OVERRIDE_PROPERTY, 'override_existing_document').

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% returns the #db{} record
%% @end
%%------------------------------------------------------------------------------
-spec get_db(server(), ne_binary()) -> db().
get_db(#server{}=Conn, DbName) ->
    kz_couch_util:get_db(Conn, DbName).


%% Document related functions --------------------------------------------------

-spec open_doc(server(), ne_binary(), ne_binary()) ->
                      {'ok', kz_json:object()} |
                      couchbeam_error().
open_doc(Conn, DbName, DocId) ->
    open_doc(Conn, DbName, DocId, []).

-spec open_doc(server(), ne_binary(), ne_binary(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      couchbeam_error().
open_doc(#server{}=Conn, DbName, DocId, Options) ->
    Db = get_db(Conn, DbName),
    do_fetch_doc(Db, DocId, Options).

-spec save_doc(server(), ne_binary(), kz_json:object(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      couchbeam_error().
save_doc(#server{}=Conn, DbName, Doc, Options) ->
    Db = get_db(Conn, DbName),
    do_save_doc(Db, Doc, Options).

-spec save_docs(server(), ne_binary(), kz_json:objects(), kz_proplist()) ->
                       {'ok', kz_json:objects()} |
                       couchbeam_error().
save_docs(#server{}=Conn, DbName, Docs, Options) ->
    Db = get_db(Conn, DbName),
    do_save_docs(Db, Docs, Options).

-spec lookup_doc_rev(server(), ne_binary(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            couchbeam_error().
lookup_doc_rev(#server{}=Conn, DbName, DocId) ->
    case do_fetch_rev(get_db(Conn, DbName), DocId) of
        ?NE_BINARY=Rev -> {'ok', Rev};
        {'error', _}=E -> E
    end.

-spec ensure_saved(server(), ne_binary(), kz_json:object(), kz_proplist()) ->
                          {'ok', kz_json:object()} |
                          couchbeam_error().
ensure_saved(#server{}=Conn, DbName, Doc, Opts) ->
    Db = get_db(Conn, DbName),
    do_ensure_saved(Db, Doc, Opts).

-spec del_doc(server(), ne_binary(), kz_json:object(), kz_proplist()) ->
                     {'ok', kz_json:objects()} |
                     couchbeam_error().
del_doc(#server{}=Conn, DbName, Doc, Options) ->
    Db = get_db(Conn, DbName),
    ?RETRY_504(couchbeam:delete_doc(Db, Doc, Options)).

-spec del_docs(server(), ne_binary(), kz_json:objects(), kz_proplist()) ->
                      {'ok', kz_json:objects()} |
                      couchbeam_error().
del_docs(#server{}=Conn, DbName, Doc, Options) ->
    Db = get_db(Conn, DbName),
    ?RETRY_504(couchbeam:delete_docs(Db, Doc, Options)).

%% Internal Doc functions

-spec do_ensure_saved(couchbeam_db(), kz_json:object(), kz_proplist()) ->
                             {'ok', kz_json:object()} |
                             couchbeam_error().
do_ensure_saved(#db{}=Db, Doc, Opts) ->
    case do_save_doc(Db, Doc, Opts) of
        {'ok', _}=Ok -> Ok;
        {'error', 'conflict'} ->
            case do_fetch_rev(Db, kz_doc:id(Doc)) of
                {'error', 'not_found'} ->
                    do_ensure_saved(Db, kz_doc:delete_revision(Doc), Opts);
                Rev ->
                    do_ensure_saved(Db, kz_doc:set_revision(Doc, Rev), Opts)
            end;
        {'error', _}=E -> E
    end.

-spec do_fetch_rev(couchbeam_db(), ne_binary()) ->
                          ne_binary() |
                          couchbeam_error().
do_fetch_rev(#db{}=Db, DocId) ->
    case kz_term:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> ?RETRY_504(couchbeam:lookup_doc_rev(Db, DocId))
    end.

-spec do_fetch_doc(couchbeam_db(), ne_binary(), kz_proplist()) ->
                          {'ok', kz_json:object()} |
                          couchbeam_error().
do_fetch_doc(#db{}=Db, DocId, Options) ->
    case kz_term:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> ?RETRY_504(couchbeam:open_doc(Db, DocId, Options))
    end.

-spec do_save_doc(couchbeam_db(), kz_json:object() | kz_json:objects(), kz_proplist()) ->
                         {'ok', kz_json:object()} |
                         couchbeam_error().
do_save_doc(#db{}=Db, Docs, Options) when is_list(Docs) ->
    do_save_docs(Db, Docs, Options);
do_save_doc(#db{}=Db, Doc, Options) ->
    ?RETRY_504(couchbeam:save_doc(Db, Doc, Options)).

-spec do_save_docs(couchbeam_db(), kz_json:objects(), kz_proplist()) ->
                          {'ok', kz_json:objects()} |
                          couchbeam_error().
do_save_docs(#db{}=Db, Docs, Options) ->
    do_save_docs(Db, Docs, Options, []).

-spec do_save_docs(couchbeam_db(), kz_json:objects(), kz_proplist(), kz_json:objects()) ->
                          {'ok', kz_json:objects()} |
                          couchbeam_error().
do_save_docs(#db{}=Db, Docs, Options, Acc) ->
    try lists:split(?MAX_BULK_INSERT, Docs) of
        {Save, Cont} ->
            case perform_save_docs(Db, Save, Options) of
                {'error', _}=E -> E;
                {'ok', JObjs} -> do_save_docs(Db, Cont, Options, JObjs ++ Acc)
            end
    catch
        'error':'badarg' ->
            case perform_save_docs(Db, Docs, Options) of
                {'ok', JObjs} -> {'ok', JObjs ++ Acc};
                {'error', _}=E -> E
            end
    end.

-spec perform_save_docs(couchbeam_db(), kz_json:objects(), kz_proplist()) ->
                               {'ok', kz_json:objects()} |
                               couchbeam_error().
perform_save_docs(Db, Docs, Options) ->
    ?RETRY_504(couchbeam:save_docs(Db, Docs, Options)).

%% Helpers for getting Couchbeam records ---------------------------------------

-define(DELETE_KEYS, [<<"_rev">>, <<"id">>, <<"_attachments">>]).

-spec default_copy_function(boolean()) -> copy_function().
default_copy_function('true') -> fun ensure_saved/4;
default_copy_function('false') -> fun save_doc/4.

-spec copy_doc(server(), copy_doc(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      couchbeam_error().
copy_doc(#server{}=Conn, #kz_copy_doc{source_dbname = SourceDb
                                     ,dest_dbname='undefined'
                                     }=CopySpec, Options) ->
    copy_doc(Conn, CopySpec#kz_copy_doc{dest_dbname=SourceDb
                                       ,dest_doc_id=kz_binary:rand_hex(16)
                                       }, Options);
copy_doc(#server{}=Conn, #kz_copy_doc{dest_doc_id='undefined'}=CopySpec, Options) ->
    copy_doc(Conn, CopySpec#kz_copy_doc{dest_doc_id=kz_binary:rand_hex(16)}, Options);
copy_doc(#server{}=Conn, CopySpec, Options) ->
    SaveFun = default_copy_function(props:is_defined(?COPY_DOC_OVERRIDE_PROPERTY, Options)),
    copy_doc(Conn, CopySpec, SaveFun, props:delete(?COPY_DOC_OVERRIDE_PROPERTY, Options)).


-spec copy_doc(server(), copy_doc(), copy_function(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      couchbeam_error().
copy_doc(#server{}=Conn, CopySpec, CopyFun, Options) ->
    #kz_copy_doc{source_dbname = SourceDbName
                ,source_doc_id = SourceDocId
                ,dest_dbname = DestDbName
                ,dest_doc_id = DestDocId
                } = CopySpec,
    case open_doc(Conn, SourceDbName, SourceDocId, Options) of
        {'ok', SourceDoc} ->
            Props = [{<<"_id">>, DestDocId}
                     | maybe_set_account_db(kz_doc:account_db(SourceDoc), SourceDbName, DestDbName)
                    ],
            DestinationDoc = kz_json:set_values(Props, kz_json:delete_keys(?DELETE_KEYS, SourceDoc)),
            case CopyFun(Conn, DestDbName, DestinationDoc, Options) of
                {'ok', _JObj} ->
                    Attachments = kz_doc:attachments(SourceDoc, kz_json:new()),
                    copy_attachments(Conn, CopySpec, kz_json:get_values(Attachments));
                Error -> Error
            end;
        Error -> Error
    end.

-spec copy_attachments(server(), copy_doc(), {kz_json:json_terms(), kz_json:path()}) ->
                              {'ok', ne_binary()} |
                              {'error', any()}.
copy_attachments(#server{}=Conn, CopySpec, {[], []}) ->
    #kz_copy_doc{dest_dbname = DestDbName
                ,dest_doc_id = DestDocId
                } = CopySpec,
    open_doc(Conn, DestDbName, DestDocId, []);
copy_attachments(#server{}=Conn, CopySpec, {[JObj | JObjs], [Key | Keys]}) ->
    #kz_copy_doc{source_dbname = SourceDbName
                ,source_doc_id = SourceDocId
                ,dest_dbname = DestDbName
                ,dest_doc_id = DestDocId
                } = CopySpec,
    case kz_couch_attachments:fetch_attachment(Conn, SourceDbName, SourceDocId, Key) of
        {'ok', Contents} ->
            ContentType = kz_json:get_value([<<"content_type">>], JObj),
            Opts = [{'headers', [{'content_type', kz_term:to_list(ContentType)}]}],
            case kz_couch_attachments:put_attachment(Conn, DestDbName, DestDocId, Key, Contents, Opts) of
                {'ok', _AttachmentDoc} ->
                    copy_attachments(Conn, CopySpec, {JObjs, Keys});
                Error -> Error
            end;
        Error -> Error
    end.

-spec maybe_set_account_db(api_binary(), ne_binary(), ne_binary()) -> kz_proplist().
maybe_set_account_db(DB, DB, DestDbName) ->
    [{<<"pvt_account_db">>, DestDbName}];
maybe_set_account_db(_, _, _) -> [].

-spec move_doc(server(), copy_doc(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      couchbeam_error().
move_doc(Conn, CopySpec, Options) ->
    #kz_copy_doc{source_dbname = SourceDbName
                ,source_doc_id = SourceDocId
                } = CopySpec,
    case copy_doc(Conn, CopySpec, Options) of
        {'ok', _JObj}=Ok ->
            maybe_remove_doc(Conn, SourceDbName, SourceDocId, Options),
            Ok;
        Error -> Error
    end.

-spec maybe_remove_doc(server(), ne_binary(), ne_binary(), kz_proplist()) -> 'ok'.
maybe_remove_doc(Conn, SourceDbName, SourceDocId, Options) ->
    case open_doc(Conn, SourceDbName, SourceDocId, Options) of
        {'ok', SourceDoc} ->
            _ = del_doc(Conn, SourceDbName, SourceDoc, []),
            'ok';
        _ -> 'ok'
    end.
