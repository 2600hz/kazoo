%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_media_maintenance).

-export([remove_empty_media_docs/1
         ,migrate/0
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

-spec refresh() -> 'ok'.
refresh() ->
    couch_mgr:revise_doc_from_file(?WH_MEDIA_DB, 'crossbar', "account/media.json"),
    'ok'.

migrate_system_config(ConfigId) ->
    {'ok', ConfigJObj} = couch_mgr:open_doc(?WH_CONFIG_DB, ConfigId),
    {'ok', MediaJObj} = get_media_config_doc(),

    UpdatedMediaJObj = wh_json:foldl(fun migrate_system_config_fold/3, MediaJObj, ConfigJObj),
    io:format("saving updated media config~n", []),
    couch_mgr:save_doc(?WH_CONFIG_DB, UpdatedMediaJObj).

-spec get_media_config_doc() -> {'ok', wh_json:object()}.
get_media_config_doc() ->
    case couch_mgr:open_doc(?WH_CONFIG_DB, ?WHS_CONFIG_CAT) of
        {'ok', _MediaJObj}=OK -> OK;
        {'error', 'not_found'} -> {'ok', wh_json:from_list([{<<"_id">>, ?WHS_CONFIG_CAT}])}
    end.

migrate_system_config_fold(<<"_id">>, _Id, MediaJObj) ->
    MediaJObj;
migrate_system_config_fold(<<"_rev">>, _Rev, MediaJObj) ->
    MediaJObj;
migrate_system_config_fold(Node, Settings, MediaJObj) ->
    io:format("migrating node '~s' settings~n", [Node]),
    migrate_node_config(Node, Settings, MediaJObj, ?CONFIG_KVS).

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

remove_empty_media_docs(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),

    remove_empty_media_docs(AccountId, AccountDb).

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

remove_empty_media_docs(AccountId, _AccountDb, File, []) ->
    file:close(File),
    io:format("finished cleaning up empty media docs for account ~s~n", [AccountId]);
remove_empty_media_docs(AccountId, AccountDb, File, [Media|MediaDocs]) ->
    maybe_remove_media_doc(AccountDb, File, wh_json:get_value(<<"doc">>, Media)),
    remove_empty_media_docs(AccountId, AccountDb, File, MediaDocs).

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

remove_media_doc(AccountDb, MediaJObj) ->
    {'ok', _Doc} = couch_mgr:del_doc(AccountDb, MediaJObj),
    io:format("removed media doc ~s~n", [wh_json:get_value(<<"_id">>, MediaJObj)]).
