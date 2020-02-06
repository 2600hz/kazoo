%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc data adapter behaviour
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_doc).

%% Doc related
-export([open_doc/4
        ,lookup_doc_rev/3
        ,save_doc/4
        ,save_docs/4
        ,del_doc/4
        ,del_docs/4
        ,ensure_saved/4
        ,copy_doc/4
        ,move_doc/4
        ]).

-include("kz_data.hrl").

-type copy_function() :: fun((map(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) ->
                                    {'ok', kz_json:object()} | data_error()).
-export_type([copy_function/0]).
-define(COPY_DOC_OVERRIDE_PROPERTY, 'override_existing_document').
-define(COPY_TRANSFORM, 'transform').

%% Document related functions --------------------------------------------------

-spec open_doc(map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
open_doc(#{server := {App, Conn}}, DbName, DocId, Options) ->
    handle_opened_doc(App:open_doc(Conn, DbName, DocId, Options), Options).

-spec handle_opened_doc({'ok', kz_json:object()}|data_error(), kz_term:proplist()) ->
          {'ok', kz_json:object()} | data_error().
handle_opened_doc({'error', _}=Error, _Options) -> Error;
handle_opened_doc({'ok', Doc}, Options) ->
    handle_opened_doc(Doc, kz_doc:is_soft_deleted(Doc), props:get_is_true('deleted', Options, 'false')).

-spec handle_opened_doc(kz_json:object(), boolean(), boolean()) ->
          {'ok', kz_json:object()} |
          {'error', 'not_found'}.
handle_opened_doc(Doc, 'false',  _ReturnDeleted) -> {'ok', Doc};
handle_opened_doc(Doc, 'true', 'true') -> {'ok', Doc};
handle_opened_doc(Doc, 'true', 'false') ->
    lager:info("denying access to soft deleted doc: ~s", [kz_doc:id(Doc)]),
    {'error', 'not_found'}.

-spec save_doc(map(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
save_doc(#{server := {App, Conn}}, DbName, Doc, Options) ->
    {PreparedDoc, PublishDoc} = prepare_doc_for_save(DbName, Doc),
    try App:save_doc(Conn, DbName, PreparedDoc, Options) of
        {'ok', JObj}=Ok ->
            kzs_publish:maybe_publish_doc(DbName, PublishDoc, JObj),
            update_cache(DbName, kz_doc:id(JObj), JObj, kz_doc:is_deleted(JObj)),
            Ok;
        {'error', 'conflict'}=Error ->
            kzs_cache:flush_cache_doc(DbName, kz_doc:id(PreparedDoc)),
            Error;
        Else -> Else
    catch
        Ex:Er ->
            ?LOG_DEBUG("exception ~p : ~p", [Ex, Er]),
            {'error', 'failed'}
    end.

update_cache(DbName, DocId, _JObj, 'true') ->
    kzs_cache:flush_cache_doc(DbName, DocId);
update_cache(DbName, DocId, JObj, 'false') ->
    %% Some save operations will return "rev" and not a full doc
    case kz_json:get_value(kz_doc:path_revision(), JObj) of
        'undefined' ->
            kzs_cache:flush_cache_doc(DbName, DocId);
        _Rev ->
            kzs_cache:add_to_doc_cache(DbName, DocId, JObj)
    end.

-spec save_docs(map(), kz_term:ne_binary(), kz_json:objects(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          data_error().
save_docs(#{server := {App, Conn}}, DbName, Docs, Options) ->
    {PreparedDocs, Publish} = lists:unzip([prepare_doc_for_save(DbName, D) || D <- Docs]),
    try App:save_docs(Conn, DbName, PreparedDocs, Options) of
        {'ok', JObjs}=Ok ->
            kzs_publish:maybe_publish_docs(DbName, Publish, JObjs),
            _ = [update_cache(DbName, kz_doc:id(JObj), JObj, 'true') || JObj <- JObjs],
            Ok;
        Else -> Else
    catch
        _Ex:Er -> {'error', {_Ex, Er}}
    end.

-spec lookup_doc_rev(map(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          data_error().
lookup_doc_rev(#{server := {App, Conn}}, DbName, DocId) ->
    App:lookup_doc_rev(Conn, DbName, DocId).

-spec ensure_saved(map(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
ensure_saved(#{server := {App, Conn}}=Map, DbName, Doc, Options) ->
    {PreparedDoc, PublishDoc} = prepare_doc_for_save(DbName, Doc),
    try App:ensure_saved(Conn, DbName, PreparedDoc, Options) of
        {'ok', JObj}=Ok ->
            kzs_publish:maybe_publish_doc(DbName, PublishDoc, JObj),
            kzs_cache:add_to_doc_cache(DbName, kz_doc:id(JObj), JObj),
            _ = maybe_ensure_saved_others(kz_doc:id(Doc), Map, DbName, Doc, Options),
            Ok;
        Else -> Else
    catch
        Ex:Er ->
            lager:error("exception ~p : ~p", [Ex, Er]),
            'failed'
    end.

maybe_ensure_saved_others(<<"_design", _/binary>>, Map, DbName, Doc, Options) ->
    Others = maps:get('others', Map, []),
    lists:all(fun({_Tag, M1}) ->
                      case ensure_saved(#{server => M1}, DbName, Doc, Options) of
                          {'ok', _} -> 'true';
                          _ -> 'false'
                      end
              end, Others),
    'ok';
maybe_ensure_saved_others(_, _, _, _, _) -> 'ok'.


-spec del_doc(map(), kz_term:ne_binary(), kz_json:object() | kz_term:ne_binary(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
del_doc(Server, DbName, ?NE_BINARY=DocId, Options) ->
    case open_doc(Server, DbName, DocId, Options) of
        {'error', _}=Err -> Err;
        {'ok', JObj} -> del_doc(Server, DbName, JObj, Options)
    end;
del_doc(#{server := {App, Conn}}=Server, DbName, Doc, Options) ->
    DelDoc = prepare_doc_for_del(Server, DbName, Doc),
    {PreparedDoc, PublishDoc} = prepare_doc_for_save(DbName, DelDoc),
    try App:del_doc(Conn, DbName, PreparedDoc, Options) of
        {'ok', [JObj|_]} ->
            kzs_publish:maybe_publish_doc(DbName, PublishDoc, JObj),
            kzs_cache:flush_cache_doc(DbName, kz_doc:id(PreparedDoc)),
            {'ok', JObj};
        Else -> Else
    catch
        Ex:Er ->
            lager:error("exception ~p: ~p", [Ex, Er]),
            'failed'
    end.

-spec del_docs(map(), kz_term:ne_binary(), kz_json:objects() | kz_term:ne_binaries(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          data_error().
del_docs(Server, DbName, Docs, Options) ->
    do_delete_docs(Server, DbName, prepare_docs_for_deletion(Server, DbName, Docs), Options).

do_delete_docs(_Server, _DbName, [], _Options) ->
    lager:debug("no docs to delete"),
    {'ok', []};
do_delete_docs(#{server := {App, Conn}}, DbName, DelDocs, Options) ->
    {PreparedDocs, Publish} = lists:unzip([prepare_doc_for_save(DbName, D) || D <- DelDocs]),
    try App:del_docs(Conn, DbName, PreparedDocs, Options) of
        {'ok', JObjs}=Ok ->
            kzs_publish:maybe_publish_docs(DbName, Publish, JObjs),
            _ = [kzs_cache:flush_cache_doc(DbName, kz_doc:id(JObj)) || JObj <- JObjs],
            Ok;
        Else -> Else
    catch
        _Ex:Er -> {'error', {_Ex, Er}}
    end.

-spec prepare_docs_for_deletion(map(), kz_term:ne_binary(), [kz_json:object() | kz_term:ne_binary()]) ->
          kz_json:objects().
prepare_docs_for_deletion(Server, DbName, Docs) ->
    {NeedsRev, HasRev} = lists:partition(fun needs_rev/1, Docs),

    NeededRevDocs = [prepare_doc_for_del(Server, DbName, D) || D <- get_revisions(Server, DbName, NeedsRev)],
    HasRevDeletionDocs = [prepare_doc_for_del(Server, DbName, D) || D <- HasRev],

    HasRevDeletionDocs ++ NeededRevDocs.

get_revisions(Server, DbName, NeedsRev) ->
    IDs = lists:foldl(fun get_ids/2, [], NeedsRev),
    case kzs_view:all_docs(Server, DbName, [{'keys', IDs}]) of
        {'ok', Docs} ->
            lists:foldl(fun get_revision_from_all_docs_result/2, [], Docs);
        {'error', 'not_found'} -> [];
        {'error', _E} ->
            lager:debug("all_docs error: ~p", [_E]),
            []
    end.

get_revision_from_all_docs_result(Result, Acc) ->
    get_revision_from_all_docs_result(Result, Acc, kz_doc:id(Result)).

get_revision_from_all_docs_result(_Result, Acc, 'undefined') -> Acc;
get_revision_from_all_docs_result(Result, Acc, DocId) ->
    Doc = kz_doc:setters([{fun kz_doc:set_id/2, DocId}
                         ,{fun kz_doc:set_revision/2, kz_json:get_value([<<"value">>, <<"rev">>], Result)}
                         ]),
    [Doc | Acc].

get_ids(<<DocID/binary>>, Acc) -> [DocID | Acc];
get_ids(JObj, Acc) -> [kz_doc:id(JObj) | Acc].

-spec needs_rev(kz_term:ne_binary() | kz_json:object()) -> boolean().
needs_rev(<<_DocId/binary>>) -> 'true';
needs_rev(Doc) -> kz_doc:revision(Doc) =:= 'undefined'.

-spec prepare_doc_for_del(map(), kz_term:ne_binary(), kz_json:object() | kz_term:ne_binary()) ->
          kz_json:object().
prepare_doc_for_del(Server, Db, ?NE_BINARY=DocId) ->
    prepare_doc_for_del(Server, Db, kz_json:from_list([{<<"_id">>, DocId}]));
prepare_doc_for_del(Server, DbName, Doc) ->
    Id = kz_doc:id(Doc),
    Rev0 = kz_doc:revision(Doc),
    DocRev = case Rev0 =:= 'undefined'
                 andalso lookup_doc_rev(Server, DbName, Id)
             of
                 'false' -> Rev0;
                 {'ok', Rev} -> Rev;
                 {'error', 'not_found'} -> 'undefined'
             end,
    kz_json:from_list(
      [{<<"_id">>, Id}
      ,{<<"_rev">>, DocRev}
      ,{<<"_deleted">>, 'true'}
       | kzs_publish:publish_fields(Doc)
      ]).

-spec prepare_doc_for_save(kz_term:ne_binary(), kz_json:object()) -> {kz_json:object(), kz_json:object()}.
prepare_doc_for_save(Db, JObj) ->
    Doc = kz_json:delete_key(<<"id">>, JObj),
    prepare_doc_for_save(Db, Doc, kz_term:is_empty(kz_doc:id(Doc))).

-spec prepare_doc_for_save(kz_term:ne_binary(), kz_json:object(), boolean()) -> {kz_json:object(), kz_json:object()}.
prepare_doc_for_save(_Db, JObj, 'true') ->
    prepare_publish(maybe_set_docid(JObj));
prepare_doc_for_save(Db, JObj, 'false') ->
    kzs_cache:flush_cache_doc(Db, JObj),
    prepare_publish(JObj).

-spec prepare_publish(kz_json:object()) -> {kz_json:object(), kz_json:object()}.
prepare_publish(JObj) ->
    {maybe_tombstone(JObj), kz_json:from_list(kzs_publish:publish_fields(JObj))}.

-spec maybe_tombstone(kz_json:object()) -> kz_json:object().
maybe_tombstone(JObj) ->
    maybe_tombstone(JObj, kz_doc:is_deleted(JObj)).

-spec maybe_tombstone(kz_json:object(), boolean()) -> kz_json:object().
maybe_tombstone(JObj, 'true') ->
    kz_json:from_list(
      [{<<"_id">>, kz_doc:id(JObj)}
      ,{<<"_rev">>, kz_doc:revision(JObj)}
      ,{<<"_deleted">>, 'true'}
      ]);
maybe_tombstone(JObj, 'false') -> JObj.

-spec maybe_set_docid(kz_json:object()) -> kz_json:object().
maybe_set_docid(Doc) ->
    case kz_doc:id(Doc) of
        'undefined' -> kz_doc:set_id(Doc, kz_datamgr:get_uuid());
        _ -> Doc
    end.

-spec default_copy_function(boolean()) -> copy_function().
default_copy_function('true') -> fun ensure_saved/4;
default_copy_function('false') -> fun save_doc/4.

-spec copy_doc(map(), map(), copy_doc(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
copy_doc(Src, Dst, CopySpec, Options) ->
    SaveFun = default_copy_function(props:is_defined(?COPY_DOC_OVERRIDE_PROPERTY, Options)),
    copy_doc(Src, Dst, CopySpec, SaveFun, props:delete(?COPY_DOC_OVERRIDE_PROPERTY, Options)).

-spec copy_doc(map(), map(), copy_doc(), copy_function(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
copy_doc(Src, Dst, CopySpec, CopyFun, Opts) ->
    #copy_doc{source_dbname = SourceDbName
             ,source_doc_id = SourceDocId
             ,dest_dbname = DestDbName
             ,dest_doc_id = DestDocId
             } = CopySpec,
    Transform = props:get_value(?COPY_TRANSFORM, Opts),
    Options = props:delete(?COPY_TRANSFORM, Opts),
    case open_doc(Src, SourceDbName, SourceDocId, Options) of
        {'ok', SourceDoc} ->
            Props = [{<<"_id">>, DestDocId}
                    ,{<<"pvt_account_db">>, DestDbName}
                    ],
            DestinationDoc = kz_json:set_values(Props, kz_json:delete_keys(?DELETE_KEYS, SourceDoc)),
            Doc = copy_transform(Transform, SourceDoc, DestinationDoc),
            case CopyFun(Dst, DestDbName, Doc, Options) of
                {'ok', JObj} ->
                    Attachments = kz_doc:attachments(SourceDoc, kz_json:new()),
                    copy_attachments(Src, Dst, CopySpec, kz_json:get_values(Attachments), kz_doc:revision(JObj));
                Error -> Error
            end;
        Error -> Error
    end.

-spec copy_transform('undefined' | transform_fun(), kz_json:object(), kz_json:object()) -> kz_json:object().
copy_transform('undefined', _SourceDoc, DestinationDoc) ->
    DestinationDoc;
copy_transform(Fun, SourceDoc, DestinationDoc) ->
    Fun(SourceDoc, DestinationDoc).

-spec copy_attachments(map(), map(), copy_doc(), {kz_json:json_terms(), kz_json:path()}, kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
copy_attachments(_Src, Dst, CopySpec, {[], []}, _) ->
    #copy_doc{dest_dbname = DestDbName
             ,dest_doc_id = DestDocId
             } = CopySpec,
    open_doc(Dst, DestDbName, DestDocId, []);
copy_attachments(Src, Dst, CopySpec, {[JObj | JObjs], [Key | Keys]}, Rev) ->
    #copy_doc{source_dbname = SourceDbName
             ,source_doc_id = SourceDocId
             ,dest_dbname = DestDbName
             ,dest_doc_id = DestDocId
             } = CopySpec,
    case kzs_attachments:fetch_attachment(Src, SourceDbName, SourceDocId, Key) of
        {'ok', Contents} ->
            ContentType = kz_json:get_value([<<"content_type">>], JObj),
            Opts = [{'content_type', kz_term:to_list(ContentType)}
                   ,{'rev', Rev}
                   ],
            case kzs_attachments:put_attachment(Dst, DestDbName, DestDocId, Key, Contents, Opts) of
                {'ok', AttachmentDoc} ->
                    copy_attachments(Src, Dst, CopySpec, {JObjs, Keys}, kz_doc:revision(AttachmentDoc));
                {'ok', AttachmentDoc, _Headers} ->
                    copy_attachments(Src, Dst, CopySpec, {JObjs, Keys}, kz_doc:revision(AttachmentDoc));
                Error -> Error
            end;
        Error -> Error
    end.

-spec move_doc(map(), map(), copy_doc(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          data_error().
move_doc(Src, Dst, CopySpec, Options) ->
    #copy_doc{source_dbname = SourceDbName
             ,source_doc_id = SourceDocId
             } = CopySpec,
    case copy_doc(Src, Dst, CopySpec, Options) of
        {'ok', JObj} ->
            _ = del_doc(Src, SourceDbName, SourceDocId, []),
            {'ok', JObj};
        Error -> Error
    end.
