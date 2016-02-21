%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% Util functions used by whistle_couch
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(kz_couch_db).


%% DB operations
-export([db_compact/2
         ,db_create/2
         ,db_create/3
         ,db_delete/2
         ,db_replicate/2
         ,db_view_cleanup/2
         ,db_info/1
         ,db_info/2
         ,db_exists/2
         ,db_archive/2
        ]).

-include("kz_couch.hrl").

-type db_create_options() :: [{'q',integer()} | {'n',integer()}].

get_db(#server{}=Conn, DbName) ->
    kz_couch_util:get_db(Conn, DbName).
              
%%% DB-related functions ---------------------------------------------
-spec db_compact(server(), ne_binary()) -> boolean().
db_compact(#server{}=Conn, DbName) ->
    Db = get_db(Conn, DbName),
    do_db_compact(Db).

-spec db_create(server(), ne_binary()) -> boolean().
db_create(#server{}=Conn, DbName) ->
    db_create(Conn, DbName, []).

-spec db_create(server(), ne_binary(), db_create_options()) -> boolean().
db_create(#server{}=Conn, DbName, Options) ->
    case couchbeam:create_db(Conn, wh_util:to_list(DbName), [], Options) of
        {'error', _} -> 'false';
        {'ok', _} -> 'true'
    end.

-spec db_delete(server(), ne_binary()) -> boolean().
db_delete(#server{}=Conn, DbName) ->
    case couchbeam:delete_db(Conn, wh_util:to_list(DbName)) of
        {'error', _} -> 'false';
        {'ok', _} -> 'true'
    end.

-spec db_replicate(server(), wh_json:object() | wh_proplist()) ->
                                {'ok', wh_json:object()} |
                                couchbeam_error().
db_replicate(#server{}=Conn, Prop) when is_list(Prop) ->
    couchbeam:replicate(Conn, wh_json:from_list(Prop));
db_replicate(#server{}=Conn, JObj) ->
    couchbeam:replicate(Conn, JObj).

-spec db_view_cleanup(server(), ne_binary()) -> boolean().
db_view_cleanup(#server{}=Conn, DbName) ->
    do_db_view_cleanup(get_db(Conn, DbName)).

-spec db_info(server()) ->
                     {'ok', ne_binaries()} |
                     couchbeam_error().
db_info(#server{}=Conn) ->
    ?RETRY_504(couchbeam:all_dbs(Conn)).

-spec db_info(server(), ne_binary()) ->
                     {'ok', wh_json:object()} |
                     couchbeam_error().
db_info(#server{}=Conn, DbName) ->
    ?RETRY_504(couchbeam:db_info(get_db(Conn, DbName))).

-spec db_exists(server(), ne_binary()) -> boolean().
db_exists(#server{}=Conn, DbName) ->
    couchbeam:db_exists(Conn, wh_util:to_list(DbName)).

-spec db_archive(server(), ne_binary()) -> 'ok'.
db_archive(#server{}=Conn, Filename) ->
    {'ok', DbInfo} = db_info(Conn),
    {'ok', File} = file:open(Filename, ['write']),
    'ok' = file:write(File, <<"[">>),
    io:format("archiving to ~s~n", [Filename]),
    MaxDocs = whapps_config:get_integer(?CONFIG_CAT, <<"max_concurrent_docs_to_archive">>, 500),
    archive(Conn, File, MaxDocs, wh_json:get_integer_value(<<"doc_count">>, DbInfo), 0),
    'ok' = file:write(File, <<"]">>),
    file:close(File).

%% MaxDocs = The biggest set of docs to pull from Couch
%% N = The number of docs in the DB that haven't been archived
%% Pos = Which doc will the next query start from (the offset)
-spec archive(server(), file:io_device(), pos_integer(), non_neg_integer(), non_neg_integer()) -> 'ok'.
archive(Db, _File,  _MaxDocs, 0, _Pos) ->
    io:format("    archive ~s complete~n", [Db]);
archive(Db, File, MaxDocs, N, Pos) when N =< MaxDocs ->
    ViewOptions = [{'limit', N}
                   ,{'skip', Pos}
                   ,'include_docs'
                  ],
    case kzs_mgr:all_docs(Db, ViewOptions) of
        {'ok', []} -> io:format("    no docs left after pos ~p~n", [Pos]);
        {'ok', Docs} ->
            'ok' = archive_docs(File, Docs),
            io:format("    archived ~p docs~n", [N]);
        {'error', _E} ->
            io:format("    error ~p asking for ~p docs from pos ~p~n", [_E, N, Pos]),
            timer:sleep(500),
            archive(Db, File, MaxDocs, N, Pos)
    end;
archive(Db, File, MaxDocs, N, Pos) ->
    ViewOptions = [{'limit', MaxDocs}
                   ,{'skip', Pos}
                   ,'include_docs'
                  ],
    case kzs_mgr:all_docs(Db, ViewOptions) of
        {'ok', []} -> io:format("    no docs left after pos ~p~n", [Pos]);
        {'ok', Docs} ->
            'ok' = archive_docs(File, Docs),
            io:format("    archived ~p docs~n", [MaxDocs]),
            archive(Db, File, MaxDocs, N - MaxDocs, Pos + MaxDocs);
        {'error', _E} ->
            io:format("    error ~p asking for ~p docs from pos ~p~n", [_E, N, Pos]),
            timer:sleep(500),
            archive(Db, File, MaxDocs, N, Pos)
    end.

-spec archive_docs(file:io_device(), wh_json:objects()) -> 'ok'.
archive_docs(_, []) -> 'ok';
archive_docs(File, [Doc]) ->
    'ok' = file:write(File, [wh_json:encode(Doc), $\n]);
archive_docs(File, [Doc|Docs]) ->
    'ok' = file:write(File, [wh_json:encode(Doc), $,, $\n]),
    archive_docs(File, Docs).

%% Internal DB-related functions -----------------------------------------------

-spec do_db_compact(db()) -> boolean().
do_db_compact(#db{}=Db) ->
    case ?RETRY_504(couchbeam:compact(Db)) of
        'ok' -> 'true';
        {'error', {'conn_failed', {'error', 'timeout'}}} ->
            lager:debug("connection timed out"),
            'false';
        {'error', 'not_found'} ->
            lager:debug("db_compact failed because db wasn't found"),
            'false';
        {'error', _E} ->
            lager:debug("failed to compact: ~p", [_E]),
            'false'
    end.

-spec do_db_view_cleanup(db()) -> boolean().
do_db_view_cleanup(#db{}=Db) ->
    'ok' =:= ?RETRY_504(couchbeam:view_cleanup(Db)).
