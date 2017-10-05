%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_media_maintenance).

-export([remove_empty_media_docs/0
        ,remove_empty_media_docs/1
        ,migrate/0, migrate_prompts/0
        ,import_prompts/1, import_prompts/2
        ,import_prompt/1, import_prompt/2
        ,set_account_language/2
        ,refresh/0
        ,fix_media_names/0
        ]).

-include("kazoo_media.hrl").

-spec migrate() -> 'no_return'.
migrate() ->
    io:format("migrating relevant settings from system_config/callflow to system_config/~s~n", [?CONFIG_CAT]),

    maybe_migrate_system_config(<<"callflow">>),

    io:format("migrating relevant settings from system_config/media_mgr to system_config/~s~n", [?CONFIG_CAT]),
    maybe_migrate_system_config(<<"media_mgr">>, 'true'),

    'no_return'.

-spec migrate_prompts() -> 'no_return'.
migrate_prompts() ->
    io:format("Now updating existing system_media docs to be internationalizable!~n", []),
    'no_return'.

-spec set_account_language(ne_binary(), ne_binary()) -> 'ok'.
set_account_language(Account, Language) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    OldLang = kz_media_util:prompt_language(AccountId),

    try kapps_account_config:set(AccountId
                                ,?CONFIG_CAT
                                ,?PROMPT_LANGUAGE_KEY
                                ,kz_term:to_lower_binary(Language)
                                )
    of
        _Config ->
            io:format("successfully updated account ~s's language from '~s' to '~s'~n"
                     ,[AccountId, OldLang, Language]
                     )
    catch
        _E:_R -> 'ok'
    end.

-spec import_prompts(file:filename_all()) -> 'ok'.
-spec import_prompts(file:filename_all(), text()) -> 'ok'.
import_prompts(DirPath) ->
    import_prompts(DirPath, kz_media_util:default_prompt_language()).

import_prompts(DirPath, Lang) ->
    case filelib:is_dir(DirPath) of
        'false' ->
            io:format("not a directory, or is inaccessible: '~s'\n", [DirPath]);
        'true' ->
            kz_datamgr:db_create(?KZ_MEDIA_DB),
            MediaPath = filename:join([DirPath, "*.{wav,mp3}"]),
            case filelib:wildcard(kz_term:to_list(MediaPath)) of
                [] -> io:format("failed to find media files in '~s'~n", [DirPath]);
                Files -> import_files(DirPath, Lang, Files)
            end
    end.

-spec import_files(file:filename_all(), ne_binary(), [file:filename_all()]) -> 'ok'.
import_files(Path, Lang, Files) ->
    io:format("importing prompts from '~s' with language '~s'~n", [Path, Lang]),
    case import_prompts_from_files(Files, Lang) of
        [] -> io:format("importing went successfully~n");
        Errors ->
            io:format("errors encountered during import:~n"),
            _ = [io:format("  '~s': ~p~n", [F, Err]) || {F, Err} <- Errors],
            'ok'
    end.

-spec import_prompts_from_files([file:filename_all()], ne_binary()) ->
                                       [{file:filename_all(), {'error', _}}].
import_prompts_from_files(Files, Lang) ->
    [{File, Err}
     || File <- Files,
        (Err = (catch import_prompt(File, Lang))) =/= 'ok'
    ].

-spec import_prompt(file:filename_all()) -> 'ok' | {'error', any()}.
-spec import_prompt(file:filename_all(), text()) -> 'ok' | {'error', any()}.
-spec import_prompt(file:filename_all(), text(), ne_binary()) -> 'ok' | {'error', any()}.

import_prompt(Path) ->
    import_prompt(Path, kz_media_util:default_prompt_language()).

import_prompt(Path, Lang) ->
    kz_datamgr:db_create(?KZ_MEDIA_DB),
    timer:sleep(250),
    case file:read_file(Path) of
        {'ok', Contents} ->
            io:format("importing prompt '~s' with language '~s'~n", [Path, Lang]),
            import_prompt(Path, Lang, Contents);
        {'error', E}=Error ->
            io:format("failed to open path '~s' for importing: ~p~n", [Path, E]),
            Error
    end.

import_prompt(Path0, Lang0, Contents) ->
    Lang = kz_term:to_binary(Lang0),
    Path = kz_term:to_binary(Path0),

    ContentLength = byte_size(Contents),

    MetaJObj = media_meta_doc(Path, Lang, ContentLength),

    case kz_datamgr:ensure_saved(?KZ_MEDIA_DB, MetaJObj) of
        {'ok', MetaJObj1} ->
            io:format("  saved metadata about '~s'~n", [Path]),

            AttachmentName = iolist_to_binary([kzd_media:prompt_id(MetaJObj1)
                                              ,kz_term:to_binary(filename:extension(Path))
                                              ]),
            upload_prompt(kz_doc:id(MetaJObj1)
                         ,AttachmentName
                         ,Contents
                         ,[{'content_type', kz_json:get_string_value(<<"content_type">>, MetaJObj1)}
                          ,{'content_length', ContentLength}
                          ,{'rev', kz_doc:revision(MetaJObj1)}
                          ]
                         );
        {'error', E}=Error ->
            io:format("  error saving metadata: ~p~n", [E]),
            Error
    end.

-spec media_meta_doc(file:filename_all(), ne_binary(), pos_integer()) ->
                            kz_json:object().
media_meta_doc(Path, Lang, ContentLength) ->
    MediaDoc = base_media_doc(Path, Lang, ContentLength),
    kz_doc:update_pvt_parameters(MediaDoc
                                ,?KZ_MEDIA_DB
                                ,[{'type', kzd_media:type()}
                                 ,{'now', kz_time:now_s()}
                                 ]
                                ).

-spec base_media_doc(file:filename_all(), ne_binary(), pos_integer()) ->
                            kz_json:object().
base_media_doc(Path, Lang, ContentLength) ->
    PromptName = prompt_name_from_path(Path),
    ContentType = content_type_from_path(Path),
    ID = kz_media_util:prompt_id(PromptName, Lang),

    io:format("  importing as '~s'~n", [ID]),

    kz_json:from_list(
      [{<<"_id">>, ID}
      ,{<<"name">>, ID}
      ,{<<"prompt_id">>, PromptName}
      ,{<<"description">>, media_description(PromptName, Lang)}
      ,{<<"content_length">>, ContentLength}
      ,{<<"language">>, kz_term:to_lower_binary(Lang)}
      ,{<<"content_type">>, ContentType}
      ,{<<"source_type">>, kz_term:to_binary(?MODULE)}
      ,{<<"streamable">>, 'true'}
      ]
     ).

-spec prompt_name_from_path(file:filename_all()) -> ne_binary().
prompt_name_from_path(Path) ->
    Extension = filename:extension(Path),
    kz_term:to_binary(filename:basename(Path, Extension)).

-spec content_type_from_path(file:filename_all()) -> ne_binary().
content_type_from_path(Path) ->
    {Category, Type, _} = cow_mimetypes:all(Path),
    filename:join([Category, Type]).

-spec media_description(ne_binary(), ne_binary()) -> ne_binary().
media_description(PromptName, Lang) ->
    <<"System prompt in ", Lang/binary, " for ", PromptName/binary>>.

-spec upload_prompt(ne_binary(), ne_binary(), ne_binary(), kz_proplist()) ->
                           'ok' |
                           {'error', any()}.
-spec upload_prompt(ne_binary(), ne_binary(), ne_binary(), kz_proplist(), non_neg_integer()) ->
                           'ok' |
                           {'error', any()}.
upload_prompt(ID, AttachmentName, Contents, Options) ->
    upload_prompt(ID, AttachmentName, Contents, Options, 3).

upload_prompt(_ID, _AttachmentName, _Contents, _Options, 0) ->
    io:format("  retries exceeded for uploading ~s to ~s~n", [_AttachmentName, _ID]),
    {'error', 'retries_exceeded'};
upload_prompt(ID, AttachmentName, Contents, Options, Retries) ->
    case kz_datamgr:put_attachment(?KZ_MEDIA_DB, ID, AttachmentName, Contents, Options) of
        {'ok', _MetaJObj} ->
            io:format("  uploaded prompt binary to ~s as ~s~n", [ID, AttachmentName]);
        {'error', 'conflict'} ->
            io:format("  conflict when uploading media binary; checking doc to see if it was actually successful~n"),
            maybe_retry_upload(ID, AttachmentName, Contents, Options, Retries);
        {'error', E} ->
            io:format("  error uploading prompt binary: ~p~n", [E]),
            maybe_cleanup_metadoc(ID, E)
    end.

-spec maybe_cleanup_metadoc(ne_binary(), any()) -> {'error', any()}.
maybe_cleanup_metadoc(ID, E) ->
    io:format("  deleting metadata from ~s~n", [?KZ_MEDIA_DB]),
    case kz_datamgr:del_doc(?KZ_MEDIA_DB, ID) of
        {'ok', _} ->
            io:format("  removed metadata for ~s~n", [ID]),
            {'error', E};
        {'error', E1}=Error ->
            io:format("  failed to remove metadata for ~s: ~p~n", [ID, E1]),
            Error
    end.

-spec maybe_retry_upload(ne_binary(), ne_binary(), ne_binary(), kz_proplist(), non_neg_integer()) ->
                                'ok' |
                                {'error', any()}.
maybe_retry_upload(ID, AttachmentName, Contents, Options, Retries) ->
    case kz_datamgr:open_doc(?KZ_MEDIA_DB, ID) of
        {'ok', JObj} ->
            case kz_doc:attachment(JObj, AttachmentName) of
                'undefined' ->
                    io:format("  attachment does not appear on the document, retrying after a pause~n"),
                    timer:sleep(?MILLISECONDS_IN_SECOND),
                    upload_prompt(ID, AttachmentName, Contents, Options, Retries-1);
                _Attachment ->
                    io:format("  attachment appears to have uploaded successfully!~n")
            end;
        {'error', E}=Error ->
            io:format("  failed to open the media doc again: ~p~n", [E]),
            Error
    end.

-spec refresh() -> 'ok'.
refresh() ->
    Req = [{<<"Database">>, ?KZ_MEDIA_DB}
          ,{<<"Classification">>, kz_datamgr:db_classification(?KZ_MEDIA_DB)}
          ,{<<"Action">>, <<"refresh_views">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    {'ok', _} = kz_amqp_worker:call(Req
                                   ,fun kapi_maintenance:publish_req/1
                                   ,fun kapi_maintenance:resp_v/1
                                   ),
    'ok'.

-spec maybe_migrate_system_config(ne_binary()) -> 'ok'.
-spec maybe_migrate_system_config(ne_binary(), boolean()) -> 'ok'.
maybe_migrate_system_config(ConfigId) ->
    maybe_migrate_system_config(ConfigId, 'false').

maybe_migrate_system_config(ConfigId, DeleteAfter) ->
    case kz_datamgr:open_doc(?KZ_CONFIG_DB, ConfigId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', JObj} ->
            migrate_system_config(kz_doc:public_fields(JObj)),
            maybe_delete_system_config(ConfigId, DeleteAfter)
    end.

-spec maybe_delete_system_config(ne_binary(), boolean()) -> 'ok'.
maybe_delete_system_config(_ConfigId, 'false') -> 'ok';
maybe_delete_system_config(ConfigId, 'true') ->
    {'ok', _} = kz_datamgr:del_doc(?KZ_CONFIG_DB, ConfigId),
    io:format("deleted ~s from ~s", [ConfigId, ?KZ_CONFIG_DB]).

-spec migrate_system_config(kz_json:object()) -> 'ok'.
migrate_system_config(ConfigJObj) ->
    {'ok', MediaJObj} = get_media_config_doc(),

    UpdatedMediaJObj = kz_json:foldl(fun migrate_system_config_fold/3
                                    ,MediaJObj
                                    ,ConfigJObj
                                    ),
    io:format("saving updated media config~n", []),
    {'ok', _} = kz_datamgr:save_doc(?KZ_CONFIG_DB, UpdatedMediaJObj),
    'ok'.

-spec get_media_config_doc() -> {'ok', kz_json:object()}.
get_media_config_doc() ->
    case kz_datamgr:open_doc(?KZ_CONFIG_DB, ?CONFIG_CAT) of
        {'ok', _MediaJObj}=OK -> OK;
        {'error', 'not_found'} ->
            {'ok', kz_json:from_list([{<<"_id">>, ?CONFIG_CAT}])}
    end.

-spec migrate_system_config_fold(ne_binary(), kz_json:json_term(), kz_json:object()) ->
                                        kz_json:object().
migrate_system_config_fold(<<"default">> = Node, Settings, MediaJObj) ->
    io:format("migrating node '~s' settings~n", [Node]),
    migrate_node_config(Node, Settings, MediaJObj, ?CONFIG_KVS);
migrate_system_config_fold(Node, Settings, MediaJObj) ->
    case binary:split(Node, <<"@">>) of
        [_User, _Domain] ->
            io:format("migrating node '~s' settings~n", [Node]),
            migrate_node_config(Node, Settings, MediaJObj, ?CONFIG_KVS);
        _Split ->
            io:format("skipping non-node '~s'~n", [Node]),
            MediaJObj
    end.

-spec migrate_node_config(ne_binary(), kz_json:object(), kz_json:object(), kz_proplist()) -> kz_json:object().
migrate_node_config(_Node, _Settings, MediaJObj, []) -> MediaJObj;
migrate_node_config(Node, Settings, MediaJObj, [{K, V} | KVs]) ->
    case kz_json:get_value(K, Settings) of
        'undefined' ->
            io:format("  maybe setting ~p for node ~p to default '~p'~n", [K, Node, V]),
            migrate_node_config(Node, Settings, maybe_update_media_config(Node, K, V, MediaJObj), KVs);
        NodeV ->
            io:format("  maybe setting ~p for node ~p to '~p'~n", [K, Node, NodeV]),

            migrate_node_config(Node, Settings, set_node_value(Node, K, NodeV, MediaJObj), KVs)
    end.

-spec set_node_value(ne_binary(), kz_json:path(), ne_binary(), kz_json:object()) ->
                            kz_json:object().
set_node_value(Node, <<_/binary>> = K, V, MediaJObj) ->
    set_node_value(Node, [K], V, MediaJObj);
set_node_value(Node, K, V, MediaJObj) ->
    kz_json:set_value([Node | K], V, MediaJObj).

-spec maybe_update_media_config(ne_binary(), kz_json:path(), api_binary(), kz_json:object()) ->
                                       kz_json:object().
maybe_update_media_config(_Node, _K, 'undefined', MediaJObj) ->
    io:format("    no value to set for ~p~n", [_K]),
    MediaJObj;
maybe_update_media_config(Node, <<_/binary>> = K, V, MediaJObj) ->
    maybe_update_media_config(Node, [K], V, MediaJObj);
maybe_update_media_config(Node, K, V, MediaJObj) ->
    Key = [Node | K],
    case kz_json:get_value(Key, MediaJObj) of
        'undefined' -> kz_json:set_value(Key, V, MediaJObj);
        V ->
            io:format("    media config has matching value for ~p~n", [Key]),
            MediaJObj;
        _V ->
            io:format("    media config has existing value '~p' for ~p~n", [_V, Key]),
            MediaJObj
    end.

-spec remove_empty_media_docs() -> 'no_return'.
remove_empty_media_docs() ->
    {'ok', JObjs} = kz_datamgr:all_docs(?KZ_MEDIA_DB, ['include_docs']),
    remove_empty_system_media(JObjs).

-spec remove_empty_system_media(kz_json:objects()) -> 'no_return'.
remove_empty_system_media([]) -> 'no_return';
remove_empty_system_media([JObj|JObjs]) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    Id = kz_json:get_value(<<"id">>, JObj),
    case kz_json:get_ne_value(<<"_attachments">>, Doc) =:= 'undefined'
        andalso binary:match(Id, <<"_design">>) =:= 'nomatch'
    of
        'true' ->
            _ = io:format("media document ~s has no attachments, removing~n", [Id]),
            _ = kz_datamgr:del_doc(?KZ_MEDIA_DB, Doc),
            remove_empty_system_media(JObjs);
        'false' -> remove_empty_system_media(JObjs)
    end.

-spec remove_empty_media_docs(ne_binary()) -> 'ok'.
remove_empty_media_docs(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    remove_empty_media_docs(AccountId, AccountDb).

-spec remove_empty_media_docs(ne_binary(), ne_binary()) -> 'ok'.
remove_empty_media_docs(AccountId, AccountDb) ->
    case kz_datamgr:get_results(AccountDb, <<"media/crossbar_listing">>, ['include_docs']) of
        {'ok', []} ->
            io:format("no media docs in account ~s~n", [AccountId]);
        {'ok', MediaDocs} ->
            io:format("found ~b media docs in account ~s~n", [length(MediaDocs), AccountId]),
            Filename = media_doc_filename(AccountId, kz_time:now_s()),
            io:format("archiving removed media docs to ~s~n", [Filename]),
            {'ok', File} = file:open(Filename, ['write', 'binary', 'append']),
            catch remove_empty_media_docs(AccountId, AccountDb, File, MediaDocs),
            'ok' = file:close(File);
        {'error', _E} ->
            io:format("error looking up media docs in account ~s: ~p~n", [AccountId, _E])
    end.

-spec media_doc_filename(ne_binary(), non_neg_integer()) -> file:name().
media_doc_filename(AccountId, Timestamp) ->
    Path = ["/tmp/empty_media_", AccountId, "_", kz_term:to_binary(Timestamp), ".json"],
    binary_to_list(list_to_binary(Path)).

-spec remove_empty_media_docs(ne_binary(), ne_binary(), file:io_device(), kz_json:objects()) -> 'ok'.
remove_empty_media_docs(AccountId, _AccountDb, _Filename, []) ->
    io:format("finished cleaning up empty media docs for account ~s~n", [AccountId]);
remove_empty_media_docs(AccountId, AccountDb, File, [Media|MediaDocs]) ->
    maybe_remove_media_doc(AccountDb, File, kz_json:get_value(<<"doc">>, Media)),
    remove_empty_media_docs(AccountId, AccountDb, File, MediaDocs).

-spec maybe_remove_media_doc(ne_binary(), file:io_device(), kz_json:object()) -> 'ok'.
maybe_remove_media_doc(AccountDb, File, MediaJObj) ->
    DocId = kz_doc:id(MediaJObj),
    case kz_doc:attachments(MediaJObj) of
        'undefined' ->
            io:format("media doc ~s has no attachments, archiving and removing~n", [DocId]),
            _R = file:write(File, [kz_json:encode(MediaJObj), $\n]),
            io:format("dumping media doc ~s to file : ~p\n", [DocId, _R]),
            remove_media_doc(AccountDb, MediaJObj);
        _Attachments ->
            io:format("media doc ~s has attachments, leaving alone~n", [kz_doc:id(MediaJObj)])
    end.

-spec remove_media_doc(ne_binary(), kz_json:object()) -> 'ok'.
remove_media_doc(AccountDb, MediaJObj) ->
    {'ok', _Doc} = kz_datamgr:del_doc(AccountDb, MediaJObj),
    io:format("removed media doc ~s~n", [kz_doc:id(MediaJObj)]).

filter_media_names(JObj) ->
    kz_doc:id(JObj) =/= kz_http_util:urldecode(kz_doc:id(JObj)).

-spec fix_media_name(kz_json:object()) -> 'ok'.
fix_media_name(JObj) ->
    FromId = kz_doc:id(JObj),
    ToId = kz_http_util:urldecode(kz_doc:id(JObj)),
    Options = [{'transform', fun(_, B) -> kz_json:set_value(<<"name">>, ToId, B) end}],
    case kz_datamgr:move_doc(?KZ_MEDIA_DB, FromId, ?KZ_MEDIA_DB, ToId, Options) of
        {'ok', _} -> lager:info("renamed media doc from ~s to ~s", [FromId, ToId]);
        {'error', Error} -> lager:info("error renaming media doc from ~s to ~s : ~p", [FromId, ToId, Error])
    end.

-spec fix_media_names() -> any().
fix_media_names() ->
    case kz_datamgr:all_docs(?KZ_MEDIA_DB) of
        {'ok', JObjs} ->
            case [ JObj || JObj <- JObjs, filter_media_names(JObj)] of
                [] -> kapps_config:set(?CONFIG_CAT, <<"fix_media_names">>, 'false');
                List -> lists:foreach(fun fix_media_name/1, List)
            end;
        {'error', Error} ->
            lager:debug("error '~p' getting media names", [Error])
    end.
