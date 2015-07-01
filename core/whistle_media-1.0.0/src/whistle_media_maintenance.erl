%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_media_maintenance).

-export([remove_empty_media_docs/1
         ,migrate/0, migrate_prompts/0
         ,import_prompts/1, import_prompts/2
         ,import_prompt/1, import_prompt/2
         ,set_account_language/2
         ,refresh/0
        ]).

-include("whistle_media.hrl").

-spec migrate() -> 'no_return'.
migrate() ->
    io:format("migrating relevant settings from system_config/callflow to system_config/~s~n", [?WHM_CONFIG_CAT]),

    maybe_migrate_system_config(<<"callflow">>),

    io:format("migrating relevant settings from system_config/media_mgr to system_config/~s~n", [?WHM_CONFIG_CAT]),
    maybe_migrate_system_config(<<"media_mgr">>, 'true'),

    'no_return'.

-spec migrate_prompts() -> 'no_return'.
migrate_prompts() ->
    io:format("Now updating existing system_media docs to be internationalizable!~n", []),
    'no_return'.

-spec set_account_language(ne_binary(), ne_binary()) -> 'ok'.
set_account_language(Account, Language) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    OldLang = wh_media_util:prompt_language(AccountId),

    try whapps_account_config:set(AccountId
                                  ,?WHM_CONFIG_CAT
                                  ,?PROMPT_LANGUAGE_KEY
                                  ,wh_util:to_lower_binary(Language)
                                 )
    of
        _Config ->
            io:format("successfully updated account ~s's language from '~s' to '~s'~n"
                      ,[AccountId, OldLang, Language]
                     )
    catch
        _E:_R -> 'ok'
    end.

import_prompts(Path) ->
    import_prompts(Path, wh_media_util:default_prompt_language()).

import_prompts(Path, Lang) ->
    couch_mgr:db_create(?WH_MEDIA_DB),
    MediaPath = filename:join([Path, "*.{wav,mp3}"]),
    case filelib:wildcard(wh_util:to_list(MediaPath)) of
        [] ->
            io:format("failed to find media files in '~s'~n", [Path]);
        Files ->
            import_files(Path, Lang, Files)
    end.

-spec import_files(ne_binary(), ne_binary(), ne_binaries()) -> 'ok'.
import_files(Path, Lang, Files) ->
    io:format("importing prompts from '~s' with language '~s'~n", [Path, Lang]),
    case import_prompts_from_files(Files, Lang) of
        [] -> io:format("importing went successfully~n");
        Errors ->
            io:format("errors encountered during import:~n"),
            _ = [io:format("  '~s': ~p~n", [F, Err]) || {F, Err} <- Errors],
            'ok'
    end.

-spec import_prompts_from_files(ne_binaries(), ne_binary()) ->
                                       [] | [{ne_binary(), 'ok' | {'error', _}},...].
import_prompts_from_files(Files, Lang) ->
     [{F, Err}
      || F <- Files,
         (Err = (catch import_prompt(F, Lang))) =/= 'ok'
     ].

-spec import_prompt(text()) -> 'ok' | {'error', _}.
-spec import_prompt(text(), text()) -> 'ok' | {'error', _}.
-spec import_prompt(text(), text(), ne_binary()) -> 'ok' | {'error', _}.

import_prompt(Path) ->
    import_prompt(Path, wh_media_util:default_prompt_language()).

import_prompt(Path, Lang) ->
    couch_mgr:db_create(?WH_MEDIA_DB),
    timer:sleep(250),
    case file:read_file(Path) of
        {'ok', Contents} ->
            io:format("importing prompt '~s' with language '~s'~n", [Path, Lang]),
            import_prompt(Path, Lang, Contents);
        {'error', E} ->
            io:format("failed to open path '~s' for importing: ~p~n", [Path, E]),
            {'error', E}
    end.

import_prompt(Path0, Lang0, Contents) ->
    Lang = wh_util:to_binary(Lang0),
    Path = wh_util:to_binary(Path0),

    Extension = filename:extension(Path),
    PromptName = wh_util:to_binary(filename:basename(Path, Extension)),

    {Category, Type, _} = cow_mimetypes:all(Path),

    ContentLength = iolist_size(Contents),

    ID = wh_media_util:prompt_id(PromptName, Lang),

    io:format("  importing as '~s'~n", [ID]),

    Now = wh_util:current_tstamp(),
    ContentType = <<Category/binary, "/", Type/binary>>,

    MetaJObj = wh_json:from_list(
                 [{<<"_id">>, ID}
                  ,{<<"name">>, ID}
                  ,{<<"prompt_id">>, PromptName}
                  ,{<<"description">>, <<"System prompt in ", Lang/binary, " for ", PromptName/binary>>}
                  ,{<<"content_length">>, ContentLength}
                  ,{<<"language">>, wh_util:to_lower_binary(Lang)}
                  ,{<<"content_type">>, ContentType}
                  ,{<<"source_type">>, ?MODULE}
                  ,{<<"streamable">>, 'true'}
                  ,{<<"pvt_type">>, <<"media">>}
                  ,{<<"pvt_created">>, Now}
                  ,{<<"pvt_modified">>, Now}
                  ,{<<"pvt_account_db">>, ?WH_MEDIA_DB}
                 ]),

    case couch_mgr:ensure_saved(?WH_MEDIA_DB, MetaJObj) of
        {'ok', MetaJObj1} ->
            io:format("  saved metadata about '~s'~n", [Path]),
            upload_prompt(ID
                          ,<<PromptName/binary, (wh_util:to_binary(Extension))/binary>>
                          ,Contents
                          ,[{'content_type', wh_util:to_list(ContentType)}
                            ,{'content_length', ContentLength}
                            ,{'rev', wh_doc:revision(MetaJObj1)}
                           ]
                         );
        {'error', E} ->
            io:format("  error saving metadata: ~p~n", [E]),
            {'error', E}
    end.

-spec upload_prompt(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                           'ok' |
                           {'error', _}.
-spec upload_prompt(ne_binary(), ne_binary(), ne_binary(), wh_proplist(), non_neg_integer()) ->
                           'ok' |
                           {'error', _}.
upload_prompt(ID, AttachmentName, Contents, Options) ->
    upload_prompt(ID, AttachmentName, Contents, Options, 3).

upload_prompt(_ID, _AttachmentName, _Contents, _Options, 0) ->
    io:format("  retries exceeded for uploading ~s to ~s~n", [_AttachmentName, _ID]),
    {'error', 'retries_exceeded'};
upload_prompt(ID, AttachmentName, Contents, Options, Retries) ->
    case couch_mgr:put_attachment(?WH_MEDIA_DB, ID, AttachmentName, Contents, Options) of
        {'ok', _MetaJObj} ->
            io:format("  uploaded prompt binary to ~s as ~s~n", [ID, AttachmentName]);
        {'error', 'conflict'} ->
            io:format("  conflict when uploading media binary; checking doc to see if it was actually successful~n"),
            maybe_retry_upload(ID, AttachmentName, Contents, Options, Retries);
        {'error', E} ->
            io:format("  error uploading prompt binary: ~p~n", [E]),
            maybe_cleanup_metadoc(ID, E)
    end.

-spec maybe_cleanup_metadoc(ne_binary(), _) -> {'error', _}.
maybe_cleanup_metadoc(ID, E) ->
    io:format("  deleting metadata from ~s~n", [?WH_MEDIA_DB]),
    case couch_mgr:del_doc(?WH_MEDIA_DB, ID) of
        {'ok', _} ->
            io:format("  removed metadata for ~s~n", [ID]),
            {'error', E};
        {'error', E1} ->
            io:format("  failed to remove metadata for ~s: ~p~n", [ID, E1]),
            {'error', E1}
    end.

-spec maybe_retry_upload(ne_binary(), ne_binary(), ne_binary(), wh_proplist(), non_neg_integer()) ->
                                'ok' |
                                {'error', _}.
maybe_retry_upload(ID, AttachmentName, Contents, Options, Retries) ->
    case couch_mgr:open_doc(?WH_MEDIA_DB, ID) of
        {'ok', JObj} ->
            case wh_doc:attachment(JObj, AttachmentName) of
                'undefined' ->
                    io:format("  attachment does not appear on the document, retrying after a pause~n"),
                    timer:sleep(?MILLISECONDS_IN_SECOND),
                    upload_prompt(ID, AttachmentName, Contents, Options, Retries-1);
                _Attachment ->
                    io:format("  attachment appears to have uploaded successfully!")
            end;
        {'error', E} ->
            io:format("  failed to open the media doc again: ~p~n", [E]),
            {'error', E}
    end.

-spec refresh() -> 'ok'.
refresh() ->
    couch_mgr:revise_doc_from_file(?WH_MEDIA_DB, 'crossbar', "account/media.json"),
    'ok'.

-spec maybe_migrate_system_config(ne_binary()) -> 'ok'.
-spec maybe_migrate_system_config(ne_binary(), boolean()) -> 'ok'.
maybe_migrate_system_config(ConfigId) ->
    maybe_migrate_system_config(ConfigId, 'false').

maybe_migrate_system_config(ConfigId, DeleteAfter) ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, ConfigId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', JObj} ->
            migrate_system_config(wh_doc:public_fields(JObj)),
            maybe_delete_system_config(ConfigId, DeleteAfter)
    end.

-spec maybe_delete_system_config(ne_binary(), boolean()) -> 'ok'.
maybe_delete_system_config(_ConfigId, 'false') -> 'ok';
maybe_delete_system_config(ConfigId, 'true') ->
    {'ok', _} = couch_mgr:del_doc(?WH_CONFIG_DB, ConfigId),
    io:format("deleted ~s from ~s", [ConfigId, ?WH_CONFIG_DB]).

-spec migrate_system_config(wh_json:object()) -> 'ok'.
migrate_system_config(ConfigJObj) ->
    {'ok', MediaJObj} = get_media_config_doc(),

    UpdatedMediaJObj = wh_json:foldl(fun migrate_system_config_fold/3
                                     ,MediaJObj
                                     ,ConfigJObj
                                    ),
    io:format("saving updated media config~n", []),
    {'ok', _} = couch_mgr:save_doc(?WH_CONFIG_DB, UpdatedMediaJObj),
    'ok'.

-spec get_media_config_doc() -> {'ok', wh_json:object()}.
get_media_config_doc() ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, ?WHM_CONFIG_CAT) of
        {'ok', _MediaJObj}=OK -> OK;
        {'error', 'not_found'} ->
            {'ok', wh_json:from_list([{<<"_id">>, ?WHM_CONFIG_CAT}])}
    end.

-spec migrate_system_config_fold(ne_binary(), wh_json:json_term(), wh_json:object()) ->
                                        wh_json:object().
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

-spec migrate_node_config(ne_binary(), wh_json:object(), wh_json:object(), wh_proplist()) -> wh_json:object().
migrate_node_config(_Node, _Settings, MediaJObj, []) -> MediaJObj;
migrate_node_config(Node, Settings, MediaJObj, [{K, V} | KVs]) ->
    case wh_json:get_value(K, Settings) of
        'undefined' ->
            io:format("  maybe setting ~p for node ~p to default '~p'~n", [K, Node, V]),
            migrate_node_config(Node, Settings, maybe_update_media_config(Node, K, V, MediaJObj), KVs);
        NodeV ->
            io:format("  maybe setting ~p for node ~p to '~p'~n", [K, Node, NodeV]),

            migrate_node_config(Node, Settings, set_node_value(Node, K, NodeV, MediaJObj), KVs)
    end.

-spec set_node_value(ne_binary(), wh_json:keys(), ne_binary(), wh_json:object()) ->
                            wh_json:object().
set_node_value(Node, <<_/binary>> = K, V, MediaJObj) ->
    set_node_value(Node, [K], V, MediaJObj);
set_node_value(Node, K, V, MediaJObj) ->
    wh_json:set_value([Node | K], V, MediaJObj).

-spec maybe_update_media_config(ne_binary(), wh_json:keys(), api_binary(), wh_json:object()) ->
                                       wh_json:object().
maybe_update_media_config(_Node, _K, 'undefined', MediaJObj) ->
    io:format("    no value to set for ~p~n", [_K]),
    MediaJObj;
maybe_update_media_config(Node, <<_/binary>> = K, V, MediaJObj) ->
    maybe_update_media_config(Node, [K], V, MediaJObj);
maybe_update_media_config(Node, K, V, MediaJObj) ->
    Key = [Node | K],
    case wh_json:get_value(Key, MediaJObj) of
        'undefined' -> wh_json:set_value(Key, V, MediaJObj);
        V ->
            io:format("    media config has matching value for ~p~n", [Key]),
            MediaJObj;
        _V ->
            io:format("    media config has existing value '~p' for ~p~n", [_V, Key]),
            MediaJObj
    end.

-spec remove_empty_media_docs(ne_binary()) -> 'ok'.
remove_empty_media_docs(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    remove_empty_media_docs(AccountId, AccountDb).

-spec remove_empty_media_docs(ne_binary(), ne_binary()) -> 'ok'.
remove_empty_media_docs(AccountId, AccountDb) ->
    case couch_mgr:get_results(AccountDb, <<"media/crossbar_listing">>, ['include_docs']) of
        {'ok', []} ->
            io:format("no media docs in account ~s~n", [AccountId]);
        {'ok', MediaDocs} ->
            io:format("found ~b media docs in account ~s~n", [length(MediaDocs), AccountId]),
            Filename = media_doc_filename(AccountId, wh_util:current_tstamp()),
            io:format("archiving removed media docs to ~s~n", [Filename]),
            {'ok', File} = file:open(Filename, ['write', 'binary', 'append']),
            catch remove_empty_media_docs(AccountId, AccountDb, File, MediaDocs),
            'ok' = file:close(File);
        {'error', _E} ->
            io:format("error looking up media docs in account ~s: ~p~n", [AccountId, _E])
    end.

-spec media_doc_filename(ne_binary(), non_neg_integer()) -> file:name().
media_doc_filename(AccountId, Timestamp) ->
    Path = ["/tmp/empty_media_", AccountId, "_", wh_util:to_binary(Timestamp), ".json"],
    binary_to_list(list_to_binary(Path)).

-spec remove_empty_media_docs(ne_binary(), ne_binary(), file:io_device(), wh_json:objects()) -> 'ok'.
remove_empty_media_docs(AccountId, _AccountDb, _Filename, []) ->
    io:format("finished cleaning up empty media docs for account ~s~n", [AccountId]);
remove_empty_media_docs(AccountId, AccountDb, File, [Media|MediaDocs]) ->
    maybe_remove_media_doc(AccountDb, File, wh_json:get_value(<<"doc">>, Media)),
    remove_empty_media_docs(AccountId, AccountDb, File, MediaDocs).

-spec maybe_remove_media_doc(ne_binary(), file:io_device(), wh_json:object()) -> 'ok'.
maybe_remove_media_doc(AccountDb, File, MediaJObj) ->
    DocId = wh_doc:id(MediaJObj),
    case wh_doc:attachments(MediaJObj) of
        'undefined' ->
            io:format("media doc ~s has no attachments, archiving and removing~n", [DocId]),
            _R = file:write(File, [wh_json:encode(MediaJObj), $\n]),
            io:format("dumping media doc ~s to file : ~p\n", [DocId, _R]),
            remove_media_doc(AccountDb, MediaJObj);
        _Attachments ->
            io:format("media doc ~s has attachments, leaving alone~n", [wh_doc:id(MediaJObj)])
    end.

-spec remove_media_doc(ne_binary(), wh_json:object()) -> 'ok'.
remove_media_doc(AccountDb, MediaJObj) ->
    {'ok', _Doc} = couch_mgr:del_doc(AccountDb, MediaJObj),
    io:format("removed media doc ~s~n", [wh_doc:id(MediaJObj)]).
