%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
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

-type copy_function() :: fun((map(), ne_binary(), kz_json:object(), kz_proplist()) ->
                                    {'ok', kz_json:object()} | data_error()).
-export_type([copy_function/0]).
-define(COPY_DOC_OVERRIDE_PROPERTY, 'override_existing_document').
-define(COPY_TRANSFORM, 'transform').

%% Document related functions --------------------------------------------------

-spec open_doc(map(), ne_binary(), ne_binary(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
open_doc(#{server := {App, Conn}}, DbName, DocId, Options) ->
    App:open_doc(Conn, DbName, DocId, Options).

-spec save_doc(map(), ne_binary(), kz_json:object(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
save_doc(#{server := {App, Conn}}, DbName, Doc, Options) ->
    {PreparedDoc, PublishDoc} = prepare_doc_for_save(DbName, Doc),
    try App:save_doc(Conn, DbName, PreparedDoc, Options) of
        {'ok', JObj}=Ok -> kzs_publish:maybe_publish_doc(DbName, PublishDoc, JObj),
                           Ok;
        Else -> Else
    catch
        Ex:Er -> lager:error("exception ~p : ~p", [Ex, Er]),
                 'failed'
    end.



-spec save_docs(map(), ne_binary(), kz_json:objects(), kz_proplist()) ->
                       {'ok', kz_json:objects()} |
                       data_error().
save_docs(#{server := {App, Conn}}, DbName, Docs, Options) ->
    {PreparedDocs, Publish} = lists:unzip([prepare_doc_for_save(DbName, D) || D <- Docs]),
    try App:save_docs(Conn, DbName, PreparedDocs, Options) of
        {'ok', JObjs}=Ok -> kzs_publish:maybe_publish_docs(DbName, Publish, JObjs),
                            Ok;
        Else -> Else
    catch
        _Ex:Er -> {'error', {_Ex, Er}}
    end.

-spec lookup_doc_rev(map(), ne_binary(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            data_error().
lookup_doc_rev(#{server := {App, Conn}}, DbName, DocId) ->
    App:lookup_doc_rev(Conn, DbName, DocId).

-spec ensure_saved(map(), ne_binary(), kz_json:object(), kz_proplist()) ->
                          {'ok', kz_json:object()} |
                          data_error().
ensure_saved(#{server := {App, Conn}}=Map, DbName, Doc, Options) ->
    {PreparedDoc, PublishDoc} = prepare_doc_for_save(DbName, Doc),
    try App:ensure_saved(Conn, DbName, PreparedDoc, Options) of
        {'ok', JObj}=Ok -> kzs_publish:maybe_publish_doc(DbName, PublishDoc, JObj),
                           _ = maybe_ensure_saved_others(kz_doc:id(Doc), Map, DbName, Doc, Options),
                           Ok;
        Else -> Else
    catch
        Ex:Er -> lager:error("exception ~p : ~p", [Ex, Er]),
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


-spec del_doc(map(), ne_binary(), kz_json:object() | ne_binary(), kz_proplist()) ->
                     {'ok', kz_json:object()} |
                     data_error().
del_doc(Server, DbName, DocId, Options)
  when is_binary(DocId) ->
    case open_doc(Server, DbName, DocId, Options) of
        {'error', _}=Err -> Err;
        {'ok', JObj} -> del_doc(Server, DbName, JObj, Options)
    end;
del_doc(#{server := {App, Conn}}=Server, DbName, Doc, Options) ->
    DelDoc = prepare_doc_for_del(Server,DbName, Doc),
    {PreparedDoc, PublishDoc} = prepare_doc_for_save(DbName, DelDoc),
    try App:del_doc(Conn, DbName, PreparedDoc, Options) of
        {'ok', [JObj|_]} ->
            kzs_publish:maybe_publish_doc(DbName, PublishDoc, JObj),
            {'ok', JObj};
        Else -> Else
    catch
        Ex:Er -> lager:error("exception ~p : ~p", [Ex, Er]),
                 'failed'
    end.

-spec del_docs(map(), ne_binary(), kz_json:objects() | ne_binaries(), kz_proplist()) ->
                      {'ok', kz_json:objects()} |
                      data_error().
del_docs(#{server := {App, Conn}}=Server, DbName, Docs, Options) ->
    DelDocs = [prepare_doc_for_del(Server,DbName, D) || D <- Docs],
    {PreparedDocs, Publish} = lists:unzip([prepare_doc_for_save(DbName, D) || D <- DelDocs]),
    try App:del_docs(Conn, DbName, PreparedDocs, Options) of
        {'ok', JObjs}=Ok ->
            kzs_publish:maybe_publish_docs(DbName, Publish, JObjs),
            Ok;
        Else -> Else
    catch
        _Ex:Er -> {'error', {_Ex, Er}}
    end.

-spec prepare_doc_for_del(map(), ne_binary(), kz_json:object() | ne_binary()) ->
                                 kz_json:object().
prepare_doc_for_del(Server, Db, <<_/binary>> = DocId) ->
    prepare_doc_for_del(Server, Db, kz_json:from_list([{<<"_id">>, DocId}]));
prepare_doc_for_del(Server, DbName, Doc) ->
    Id = kz_doc:id(Doc),
    DocRev = case kz_doc:revision(Doc) of
                 'undefined' ->
                     {'ok', Rev} = lookup_doc_rev(Server, DbName, Id),
                     Rev;
                 Rev -> Rev
             end,
    kz_json:from_list(
      props:filter_undefined(
        [{<<"_id">>, Id}
        ,{<<"_rev">>, DocRev}
        ,{<<"_deleted">>, 'true'}
         | kzs_publish:publish_fields(Doc)
        ])).

-spec prepare_doc_for_save(ne_binary(), kz_json:object()) -> {kz_json:object(), kz_json:object()}.
-spec prepare_doc_for_save(ne_binary(), kz_json:object(), boolean()) -> {kz_json:object(), kz_json:object()}.
prepare_doc_for_save(Db, JObj) ->
    prepare_doc_for_save(Db, JObj, kz_util:is_empty(kz_doc:id(JObj))).

prepare_doc_for_save(_Db, JObj, 'true') ->
    prepare_publish(maybe_set_docid(JObj));
prepare_doc_for_save(Db, JObj, 'false') ->
    kzs_cache:flush_cache_doc(Db, JObj),
    prepare_publish(JObj).

-spec prepare_publish(kz_json:object()) -> {kz_json:object(), kz_json:object()}.
prepare_publish(JObj) ->
    {maybe_tombstone(JObj), kz_json:from_list(kzs_publish:publish_fields(JObj))}.

-spec maybe_tombstone(kz_json:object()) -> kz_json:object().
-spec maybe_tombstone(kz_json:object(), boolean()) -> kz_json:object().
maybe_tombstone(JObj) ->
    maybe_tombstone(JObj, kz_json:is_true(<<"_deleted">>, JObj, 'false')).

maybe_tombstone(JObj, 'true') ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"_id">>, kz_doc:id(JObj)}
        ,{<<"_rev">>, kz_doc:revision(JObj)}
        ,{<<"_deleted">>, 'true'}
        ]
       )
     );
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

-spec copy_doc(map(), map(), copy_doc(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      data_error().
copy_doc(Src, Dst, CopySpec, Options) ->
    SaveFun = default_copy_function(props:is_defined(?COPY_DOC_OVERRIDE_PROPERTY, Options)),
    copy_doc(Src, Dst, CopySpec, SaveFun, props:delete(?COPY_DOC_OVERRIDE_PROPERTY, Options)).


-spec copy_doc(map(), map(), copy_doc(), copy_function(), kz_proplist()) ->
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
                     | maybe_set_account_db(kz_doc:account_db(SourceDoc), SourceDbName, DestDbName)
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

-spec copy_attachments(map(), map(), copy_doc(), {kz_json:json_terms(), kz_json:path()}, ne_binary()) ->
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
            Opts = [{'content_type', kz_util:to_list(ContentType)}
                   ,{'rev', Rev}
                   ],
            case kzs_attachments:put_attachment(Dst, DestDbName, DestDocId, Key, Contents, Opts) of
                {'ok', AttachmentDoc} ->
                    copy_attachments(Src, Dst, CopySpec, {JObjs, Keys}, kz_doc:revision(AttachmentDoc));
                Error -> Error
            end;
        Error -> Error
    end.

-spec maybe_set_account_db(api_binary(), ne_binary(), ne_binary()) -> kz_proplist().
maybe_set_account_db(DB, DB, DestDbName) ->
    [{<<"pvt_account_db">>, DestDbName}];
maybe_set_account_db(_1, _, _) -> [].

-spec move_doc(map(), map(), copy_doc(), kz_proplist()) ->
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
