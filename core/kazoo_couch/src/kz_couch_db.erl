%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% Util functions used by kazoo_couch
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
        ,db_archive/3
        ,db_list/2
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
    case do_db_create_db(Conn, DbName, Options, []) of
        {'error', Error} ->
            lager:error("failed to create database ~s : ~p", [DbName, Error]),
            'false';
        {'ok', _} -> 'true'
    end.

do_db_create_db(#server{url=ServerUrl, options=Opts}=Server, DbName, Options, Params) ->
    Options1 = couchbeam_util:propmerge1(Options, Opts),
    Url = hackney_url:make_url(ServerUrl, DbName, Params),
    Resp = couchbeam_httpc:db_request(put, Url, [], <<>>, Options1, [201, 202]),
    case Resp of
        {ok, _Status, _Headers, Ref} ->
            hackney:skip_body(Ref),
            {ok, #db{server=Server, name=DbName, options=Options1}};
        {error, precondition_failed} ->
            {error, db_exists};
        Error ->
            Error
    end.

-spec db_delete(server(), ne_binary()) -> boolean().
db_delete(#server{}=Conn, DbName) ->
    case couchbeam:delete_db(Conn, kz_util:to_list(DbName)) of
        {'error', _} -> 'false';
        {'ok', _} -> 'true'
    end.

-spec db_replicate(server(), kz_json:object() | kz_proplist()) ->
                          {'ok', kz_json:object()} |
                          couchbeam_error().
db_replicate(#server{}=Conn, Prop) when is_list(Prop) ->
    couchbeam:replicate(Conn, kz_json:from_list(Prop));
db_replicate(#server{}=Conn, JObj) ->
    couchbeam:replicate(Conn, JObj).

-spec db_list(server(), view_options()) ->
                     {'ok', kz_json:objects() | ne_binaries()} | couchbeam_error().
db_list(#server{}=Conn, Options) ->
    couchbeam:all_dbs(Conn, Options).

-spec db_view_cleanup(server(), ne_binary()) -> boolean().
db_view_cleanup(#server{}=Conn, DbName) ->
    do_db_view_cleanup(get_db(Conn, DbName)).

-spec db_info(server()) ->
                     {'ok', ne_binaries()} |
                     couchbeam_error().
db_info(#server{}=Conn) ->
    ?RETRY_504(couchbeam:all_dbs(Conn)).

-spec db_info(server(), ne_binary()) ->
                     {'ok', kz_json:object()} |
                     couchbeam_error().
db_info(#server{}=Conn, DbName) ->
    ?RETRY_504(couchbeam:db_info(get_db(Conn, DbName))).

-spec db_exists(server(), ne_binary()) -> boolean().
db_exists(#server{}=Conn, DbName) ->
    couchbeam:db_exists(Conn, kz_util:to_list(DbName)).

-spec db_archive(server(), ne_binary(), ne_binary()) -> 'ok'.
db_archive(#server{}=Conn, DbName, Filename) ->
    Db = get_db(Conn, DbName),
    {'ok', DbInfo} = db_info(Conn, DbName),
    {'ok', File} = file:open(Filename, ['write']),
    'ok' = file:write(File, <<"[">>),
    io:format("archiving to ~s~n", [Filename]),
    MaxDocs = kapps_config:get_integer(?CONFIG_CAT, <<"max_concurrent_docs_to_archive">>, 500),
    archive(Db, DbName, File, MaxDocs, kz_json:get_integer_value(<<"doc_count">>, DbInfo), 0),
    'ok' = file:write(File, <<"]">>),
    file:close(File).

%% MaxDocs = The biggest set of docs to pull from Couch
%% N = The number of docs in the DB that haven't been archived
%% Pos = Which doc will the next query start from (the offset)
-spec archive(db(), ne_binary(), file:io_device(), pos_integer(), non_neg_integer(), non_neg_integer()) -> 'ok'.
archive(_Db, DbName, _File,  _MaxDocs, 0, _Pos) ->
    io:format("    archive ~s complete~n", [DbName]);
archive(#db{}=Db, DbName, File, MaxDocs, N, Pos) when N =< MaxDocs ->
    ViewOptions = [{'limit', MaxDocs}
                  ,{'skip', Pos}
                  ,'include_docs'
                  ],
    case kz_couch_view:all_docs(Db, ViewOptions) of
        {'ok', []} -> io:format("    no docs left after pos ~p~n", [Pos]);
        {'ok', Docs} ->
            'ok' = archive_docs(File, Docs),
            io:format("    archived ~p docs~n", [N]);
        {'error', _E} ->
            io:format("    error ~p asking for ~p docs from pos ~p~n", [_E, N, Pos]),
            timer:sleep(500),
            archive(Db, DbName, File, MaxDocs, N, Pos)
    end;
archive(Db, DbName, File, MaxDocs, N, Pos) ->
    ViewOptions = [{'limit', MaxDocs}
                  ,{'skip', Pos}
                  ,'include_docs'
                  ],
    case kz_couch_view:all_docs(Db, ViewOptions) of
        {'ok', []} -> io:format("    no docs left after pos ~p~n", [Pos]);
        {'ok', Docs} ->
            'ok' = archive_docs(File, Docs),
            io:format("    archived ~p docs~n", [MaxDocs]),
            archive(Db, DbName, File, MaxDocs, N - MaxDocs, Pos + MaxDocs);
        {'error', _E} ->
            io:format("    error ~p asking for ~p docs from pos ~p~n", [_E, N, Pos]),
            timer:sleep(500),
            archive(Db, DbName, File, MaxDocs, N, Pos)
    end.

-spec archive_docs(file:io_device(), kz_json:objects()) -> 'ok'.
archive_docs(_, []) -> 'ok';
archive_docs(File, [Doc]) ->
    'ok' = file:write(File, [kz_json:encode(Doc), $\n]);
archive_docs(File, [Doc|Docs]) ->
    'ok' = file:write(File, [kz_json:encode(Doc), $,, $\n]),
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
