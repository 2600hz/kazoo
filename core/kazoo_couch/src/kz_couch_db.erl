%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Util functions used by kazoo_couch
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
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
        ,db_import/3
        ,db_list/2
        ]).

-include("kz_couch.hrl").

-type db_create_options() :: [{'q',integer()} | {'n',integer()}].

get_db(#server{}=Conn, DbName) ->
    kz_couch_util:get_db(Conn, DbName).

%%% DB-related functions ---------------------------------------------
-spec db_compact(server(), kz_term:ne_binary()) -> boolean().
db_compact(#server{}=Conn, DbName) ->
    Db = get_db(Conn, DbName),
    do_db_compact(Db).

-spec db_create(server(), kz_term:ne_binary()) -> boolean().
db_create(#server{}=Conn, DbName) ->
    db_create(Conn, DbName, []).

-spec db_create(server(), kz_term:ne_binary(), db_create_options()) -> boolean().
db_create(#server{}=Conn, DbName, Options) ->
    case do_db_create_db(Conn, DbName, Options, []) of
        {'ok', _} -> 'true';
        {'error', 'db_exists'} ->
            lager:warning("db ~s already exists", [DbName]),
            'true';
        {'error', _Error} ->
            lager:error("failed to create database ~s: ~p", [DbName, _Error]),
            'false'
    end.

do_db_create_db(#server{url=ServerUrl, options=Opts}=Server, DbName, Options, Params) ->
    Options1 = couchbeam_util:propmerge1(Options, Opts),
    Url = hackney_url:make_url(ServerUrl, DbName, Params),
    Resp = couchbeam_httpc:db_request('put', Url, [], <<>>, Options1, [201, 202]),
    case Resp of
        {'ok', _Status, _Headers, Ref} ->
            _ = hackney:skip_body(Ref),
            {'ok', #db{server=Server, name=DbName, options=Options1}};
        {'error', 'precondition_failed'} ->
            {'error', 'db_exists'};
        {'error', {'bad_response', {500, _Headers, Body}}}=Error ->
            Reason = kz_json:get_value(<<"reason">>, kz_json:decode(Body)),
            case check_db_create_error(Server, DbName, Reason) of
                'true' -> {'error', 'db_exists'};
                'false' -> Error
            end;
        Error ->
            Error
    end.

-spec check_db_create_error(server(), kz_term:ne_binary(), any()) -> boolean().
check_db_create_error(Server, DbName, <<"conflict">>) ->
    lager:warning("db ~s creation failed with HTTP error 500 and conflict reason, checking db exists", [DbName]),
    db_exists(Server, DbName);
check_db_create_error(_Server, _DbName, _Reason) ->
    'false'.

-spec db_delete(server(), kz_term:ne_binary()) -> boolean().
db_delete(#server{}=Conn, DbName) ->
    case couchbeam:delete_db(Conn, kz_term:to_list(DbName)) of
        {'error', _} -> 'false';
        {'ok', _} -> 'true'
    end.

-spec db_replicate(server(), kz_json:object() | kz_term:proplist()) ->
                          {'ok', kz_json:object()} |
                          couchbeam_error().
db_replicate(#server{}=Conn, Prop) when is_list(Prop) ->
    couchbeam:replicate(Conn, kz_json:from_list(Prop));
db_replicate(#server{}=Conn, JObj) ->
    couchbeam:replicate(Conn, JObj).

-spec db_list(server(), view_options()) ->
                     {'ok', kz_json:objects() | kz_term:ne_binaries()} | couchbeam_error().
db_list(#server{}=Conn, Options) ->
    couchbeam:all_dbs(Conn, Options).

-spec db_view_cleanup(server(), kz_term:ne_binary()) -> boolean().
db_view_cleanup(#server{}=Conn, DbName) ->
    do_db_view_cleanup(get_db(Conn, DbName)).

-spec db_info(server()) ->
                     {'ok', kz_term:ne_binaries()} |
                     couchbeam_error().
db_info(#server{}=Conn) ->
    ?RETRY_504(couchbeam:all_dbs(Conn)).

-spec db_info(server(), kz_term:ne_binary()) ->
                     {'ok', kz_json:object()} |
                     couchbeam_error().
db_info(#server{}=Conn, DbName) ->
    ?RETRY_504(couchbeam:db_info(get_db(Conn, DbName))).

-spec db_exists(server(), kz_term:ne_binary()) -> boolean().
db_exists(#server{}=Conn, DbName) ->
    couchbeam:db_exists(Conn, kz_term:to_list(DbName)).

-spec db_archive(server(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                        'ok' |
                        couchbeam_error().
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
-spec archive(db(), kz_term:ne_binary(), file:io_device(), pos_integer(), non_neg_integer(), non_neg_integer()) -> 'ok'.
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
            'ok' = maybe_add_comma(File, Pos),
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
            'ok' = maybe_add_comma(File, Pos),
            'ok' = archive_docs(File, Docs),
            io:format("    archived ~p docs~n", [MaxDocs]),
            archive(Db, DbName, File, MaxDocs, N - MaxDocs, Pos + MaxDocs);
        {'error', _E} ->
            io:format("    error ~p asking for ~p docs from pos ~p~n", [_E, N, Pos]),
            timer:sleep(500),
            archive(Db, DbName, File, MaxDocs, N, Pos)
    end.

%% when writing 2nd+ page of results, the previous page's last JObj will not have a comma after it
%% so you would get files like
%% [Pass1JObj,
%%  Pass1JObj
%%  Pass2JObj,
%%  Pass2JObj
%% Which is invalid JSON
-spec maybe_add_comma(file:io_device(), non_neg_integer()) -> 'ok'.
maybe_add_comma(_File, 0) -> 'ok';
maybe_add_comma(File, _Pos) ->
    'ok' = file:write(File, [$,]).

-spec archive_docs(file:io_device(), kz_json:objects()) -> 'ok'.
archive_docs(_, []) -> 'ok';
archive_docs(File, [Doc]) ->
    'ok' = file:write(File, [kz_json:encode(Doc), $\n]);
archive_docs(File, [Doc|Docs]) ->
    'ok' = file:write(File, [kz_json:encode(Doc), $,, $\n]),
    archive_docs(File, Docs).

-spec db_import(server(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                       'ok' |
                       couchbeam_error().
db_import(#server{}=Conn, DbName, Filename) ->
    case file:read_file(Filename) of
        {'ok', Text} -> do_db_import(Conn, DbName, kz_json:decode(Text));
        Error -> Error
    end.

-spec do_db_import(server(), kz_term:ne_binary(), kz_json:objects()) ->
                          'ok' |
                          couchbeam_error().
do_db_import(#server{}=Conn, DbName, Docs) ->
    JObjs = [cleanup_for_import(Doc)
             || Doc <- Docs,
                filter_views(kz_doc:id(Doc))
            ],
    kz_couch_doc:save_docs(Conn, DbName, JObjs, []).

cleanup_for_import(Doc) ->
    JObj = kz_json:get_json_value(<<"doc">>, Doc),
    kz_json:delete_keys([<<"_rev">>, <<"_attachments">>], JObj).

filter_views(<<"_design", _/binary>>) -> 'false';
filter_views(_Id) -> 'true'.

%% Internal DB-related functions -----------------------------------------------

-spec do_db_compact(db()) -> boolean().
do_db_compact(#db{}=Db) ->
    case ?RETRY_504(couchbeam:compact(Db)) of
        'ok' ->
            lager:debug("compaction has started"),
            'true';
        {'ok', _OK} ->
            lager:debug("compaction has started: ~p", [_OK]),
            'true';
        {'error', {'conn_failed', {'error', 'timeout'}}} ->
            lager:warning("connection timed out"),
            'false';
        {'error', 'not_found'} ->
            lager:warning("db_compact failed because db wasn't found"),
            'false';
        {'error', _E} ->
            lager:warning("failed to compact: ~p", [_E]),
            'false'
    end.

-spec do_db_view_cleanup(db()) -> boolean().
do_db_view_cleanup(#db{}=Db) ->
    case ?RETRY_504(couchbeam:view_cleanup(Db)) of
        {'ok', JObj} ->
            kz_json:is_true(<<"ok">>, JObj);
        {'error', {'conn_failed', {'error', 'timeout'}}} ->
            lager:warning("connection timed out"),
            'false';
        {'error', 'not_found'} ->
            lager:warning("db_view_cleanup failed because db wasn't found"),
            'false';
        {'error', _E} ->
            lager:warning("failed to clean up views: ~p", [_E]),
            'false'
    end.
