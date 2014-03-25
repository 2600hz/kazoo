-module(whistle_media_maintenance).

-export([remove_empty_media_docs/1]).

-include("whistle_media.hrl").

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
