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
         ,refresh/0
        ]).

-include("whistle_media.hrl").

-spec migrate() -> 'no_return'.
migrate() ->
    io:format("migrating relevant settings from system_config/callflow to system_config/~s~n", [?WHS_CONFIG_CAT]),

    migrate_system_config(<<"callflow">>),

    io:format("migrating relevant settings from system_config/media_mgr to system_config/~s~n", [?WHS_CONFIG_CAT]),
    migrate_system_config(<<"media_mgr">>),
    'no_return'.

-spec migrate_prompts() -> 'no_return'.
migrate_prompts() ->
    io:format("Now updating existing system_media docs to be internationalizable!~n", []),
    'no_return'.

import_prompts(Path) ->
    import_prompts(Path, wh_media_util:default_prompt_language()).

import_prompts(Path, Lang) ->
    case filelib:wildcard([Path, "/*.{wav,mp3}"]) of
        [] ->
            io:format("failed to find media files in '~s'~n", [Path]);
        Files ->
            io:format("importing prompts from '~s' with language '~s'~n", [Path, Lang]),
            case [{F, Err}
                  || F <- Files,
                     (Err = (catch import_prompt(F, Lang))) =/= 'ok'
                 ]
            of
                [] -> io:format("importing went successfully~n", []);
                Errors ->
                    io:format("errors encountered during import:~n"),
                    [io:format("  '~s': ~p~n", [F, Err]) || {F, Err} <- Errors],
                    'ok'
            end
    end.

-spec import_prompt(text()) -> 'ok' | {'error', _}.
-spec import_prompt(text(), text()) -> 'ok' | {'error', _}.
-spec import_prompt(text(), text(), ne_binary()) -> 'ok' | {'error', _}.

import_prompt(Path) ->
    import_prompt(Path, wh_media_util:default_prompt_language()).

import_prompt(Path, Lang) ->
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

    ContentLength = byte_size(Contents),

    ID = wh_media_util:prompt_id(PromptName, Lang),

    io:format("  importing as '~s'~n", [ID]),

    Now = wh_util:current_tstamp(),

    MetaJObj = wh_json:from_list(
                 [{<<"_id">>, ID}
                  ,{<<"name">>, ID}
                  ,{<<"prompt_id">>, PromptName}
                  ,{<<"description">>, <<"System prompt in ", Lang/binary, " for ", PromptName/binary>>}
                  ,{<<"content_length">>, ContentLength}
                  ,{<<"language">>, wh_util:to_lower_binary(Lang)}
                  ,{<<"content_type">>, <<Category/binary, "/", Type/binary>>}
                  ,{<<"source_type">>, ?MODULE}
                  ,{<<"streamable">>, 'true'}
                  ,{<<"pvt_type">>, <<"media">>}
                  ,{<<"pvt_created">>, Now}
                  ,{<<"pvt_modified">>, Now}
                  ,{<<"pvt_account_db">>, ?WH_MEDIA_DB}
                 ]),

    case couch_mgr:ensure_saved(?WH_MEDIA_DB, MetaJObj) of
        {'ok', _MetaJObj1} ->
            io:format("  saved metadata about '~s'~n", [Path]),
            upload_prompt(ID, PromptName, Contents);
        {'error', E} ->
            io:format("  error saving metadata: ~p~n", [E]),
            {'error', E}
    end.

-spec upload_prompt(ne_binary(), ne_binary(), ne_binary()) -> 'ok' | {'error', _}.
upload_prompt(ID, AttachmentName, Contents) ->
    case couch_mgr:put_attachment(?WH_MEDIA_DB, ID, AttachmentName, Contents) of
        {'ok', _MetaJObj} ->
            io:format("  uploaded prompt binary to ~s as ~s~n", [ID, AttachmentName]);
        {'error', E} ->
            io:format("  error uploading prompt binary: ~p~n", [E]),
            io:format("  deleting metadata from ~s~n", [?WH_MEDIA_DB]),
            case couch_mgr:del_doc(?WH_MEDIA_DB, ID) of
                {'ok', _} ->
                    io:format("  removed metadata for ~s~n", [ID]),
                    {'error', E};
                {'error', E1} ->
                    io:format("  failed to remove metadata for ~s: ~p~n", [ID, E1]),
                    {'error', E1}
            end
    end.

-spec refresh() -> 'ok'.
refresh() ->
    couch_mgr:revise_doc_from_file(?WH_MEDIA_DB, 'crossbar', "account/media.json"),
    'ok'.

-spec migrate_system_config(ne_binary()) -> 'ok'.
migrate_system_config(ConfigId) ->
    {'ok', ConfigJObj} = couch_mgr:open_doc(?WH_CONFIG_DB, ConfigId),
    {'ok', MediaJObj} = get_media_config_doc(),

    UpdatedMediaJObj = wh_json:foldl(fun migrate_system_config_fold/3, MediaJObj, ConfigJObj),
    io:format("saving updated media config~n", []),
    {'ok', _} = couch_mgr:save_doc(?WH_CONFIG_DB, UpdatedMediaJObj),
    'ok'.

-spec get_media_config_doc() -> {'ok', wh_json:object()}.
get_media_config_doc() ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, ?WHS_CONFIG_CAT) of
        {'ok', _MediaJObj}=OK -> OK;
        {'error', 'not_found'} -> {'ok', wh_json:from_list([{<<"_id">>, ?WHS_CONFIG_CAT}])}
    end.

-spec migrate_system_config_fold(ne_binary(), wh_json:json_term(), wh_json:object()) -> wh_json:object().
migrate_system_config_fold(<<"_id">>, _Id, MediaJObj) ->
    MediaJObj;
migrate_system_config_fold(<<"_rev">>, _Rev, MediaJObj) ->
    MediaJObj;
migrate_system_config_fold(Node, Settings, MediaJObj) ->
    io:format("migrating node '~s' settings~n", [Node]),
    migrate_node_config(Node, Settings, MediaJObj, ?CONFIG_KVS).

-spec migrate_node_config(ne_binary(), wh_json:object(), wh_json:object(), wh_proplist()) -> wh_json:object().
migrate_node_config(_Node, _Settings, MediaJObj, []) -> MediaJObj;
migrate_node_config(Node, Settings, MediaJObj, [{K, V} | KVs]) ->
    case wh_json:get_value(K, Settings) of
        'undefined' ->
            io:format("  maybe setting ~p for node ~p to default '~p'~n", [K, Node, V]),
            migrate_node_config(Node, Settings, maybe_update_media_config(Node, K, V, MediaJObj), KVs);
        NodeV ->
            io:format("  maybe setting ~p for node ~p to '~p'~n", [K, Node, NodeV]),
            migrate_node_config(Node, Settings, wh_json:set_value([Node, K], NodeV, MediaJObj), KVs)
    end.

-spec maybe_update_media_config(ne_binary(), ne_binary(), api_binary(), wh_json:object()) -> wh_json:object().
maybe_update_media_config(_Node, _K, 'undefined', MediaJObj) ->
    io:format("    no value to set for ~p~n", [_K]),
    MediaJObj;
maybe_update_media_config(Node, K, V, MediaJObj) ->
    Key = [Node, K],
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
            Filename = list_to_binary(["/tmp/empty_media_", AccountId, "_", wh_util:to_binary(wh_util:current_tstamp()), ".json"]),
            {'ok', File} = file:open(Filename, ['write', 'binary']),
            io:format("archiving removed media docs to ~s~n", [Filename]),
            remove_empty_media_docs(AccountId, AccountDb, File, MediaDocs);
        {'error', _E} ->
            io:format("error looking up media docs in account ~s: ~p~n", [AccountId, _E])
    end.

-spec remove_empty_media_docs(ne_binary(), ne_binary(), file:io_device(), wh_json:objects()) -> 'ok'.
remove_empty_media_docs(AccountId, _AccountDb, File, []) ->
    file:close(File),
    io:format("finished cleaning up empty media docs for account ~s~n", [AccountId]);
remove_empty_media_docs(AccountId, AccountDb, File, [Media|MediaDocs]) ->
    maybe_remove_media_doc(AccountDb, File, wh_json:get_value(<<"doc">>, Media)),
    remove_empty_media_docs(AccountId, AccountDb, File, MediaDocs).

-spec maybe_remove_media_doc(ne_binary(), file:io_device(), wh_json:object()) -> 'ok'.
maybe_remove_media_doc(AccountDb, File, MediaJObj) ->
    case wh_json:get_value(<<"_attachments">>, MediaJObj) of
        'undefined' ->
            io:format("media doc ~s has undefined attachments, archiving and removing~n"
                      ,[wh_json:get_value(<<"_id">>, MediaJObj)]
                     ),
            file:write(File, wh_json:encode(MediaJObj)),
            file:write(File, [$\n]),
            remove_media_doc(AccountDb, MediaJObj);
        [] ->
            io:format("media doc ~s has no attachments, archiving and removing~n"
                      ,[wh_json:get_value(<<"_id">>, MediaJObj)]
                     ),
            file:write(File, wh_json:encode(MediaJObj)),
            file:write(File, [$\n]),
            remove_media_doc(AccountDb, MediaJObj);
        _Attachments ->
            io:format("media doc ~s has attachments, leaving alone~n", [wh_json:get_value(<<"_id">>, MediaJObj)])
    end.

-spec remove_media_doc(ne_binary(), wh_json:object()) -> 'ok'.
remove_media_doc(AccountDb, MediaJObj) ->
    {'ok', _Doc} = couch_mgr:del_doc(AccountDb, MediaJObj),
    io:format("removed media doc ~s~n", [wh_json:get_value(<<"_id">>, MediaJObj)]).
