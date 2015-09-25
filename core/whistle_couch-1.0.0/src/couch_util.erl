%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% Util functions used by whistle_couch
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(couch_util).

-export([db_classification/1]).
-export([archive/1
         ,archive/2
        ]).

-export([get_new_connection/4
         ,get_db/2
         ,server_url/1
         ,db_url/2
         ,server_info/1
         ,format_error/1
        ]).

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
        ]).

%% Doc related
-export([open_cache_doc/4
         ,add_to_doc_cache/3
         ,flush_cache_doc/2
         ,flush_cache_doc/3
         ,flush_cache_docs/0
         ,flush_cache_docs/1
         ,flush_cache_docs/2
         ,flush_cache_docs/3
         ,open_doc/4
         ,lookup_doc_rev/3
         ,save_doc/4
         ,save_docs/4
         ,del_doc/3
         ,del_docs/3
         ,ensure_saved/4
         ,copy_doc/3
         ,move_doc/3
        ]).

%% View-related
-export([design_compact/3
         ,design_info/3
         ,all_design_docs/3
         ,get_results/4
         ,get_results_count/4
         ,all_docs/3
        ]).

%% Attachment-related
-export([fetch_attachment/4
         ,stream_attachment/5
         ,put_attachment/5
         ,put_attachment/6
         ,delete_attachment/4
         ,delete_attachment/5
        ]).

%% Settings-related
-export([max_bulk_insert/0]).

-include_lib("wh_couch.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").
-include_lib("whistle/include/wapi_conf.hrl").

%% Throttle how many docs we bulk insert to BigCouch
-define(MAX_BULK_INSERT, 2000).
-define(RETRY_504(F), retry504s(fun() -> F end)).

-define(PUBLISH_FIELDS, [<<"pvt_type">>
                         ,<<"pvt_account_id">>
                         ,<<"pvt_created">>
                         ,<<"pvt_modified">>
                        ]).

-type db_create_options() :: [{'q',integer()} | {'n',integer()}].

-type ddoc() :: ne_binary() | 'all_docs' | 'design_docs'.

-type db_classifications() :: 'account' | 'modb' | 'acdc' |
                              'numbers' | 'aggregate' | 'system' |
                              'deprecated' | 'undefined'.

-export_type([db_create_options/0
              ,couchbeam_errors/0
              ,db_classifications/0
              ,view_options/0
             ]).

-define(DELETE_KEYS, [<<"_rev">>, <<"id">>, <<"_attachments">>]).

-type copy_function() :: fun((server(), ne_binary(), wh_json:object(), wh_proplist()) ->
                              {'ok', wh_json:object()} | couchbeam_error()).
-export_type([copy_function/0]).
-define(COPY_DOC_OVERRIDE_PROPERTY, 'override_existing_document').

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_classification(text()) -> db_classifications().
db_classification(Db) when not is_binary(Db) ->
    db_classification(wh_util:to_binary(Db));
db_classification(<<"ts">>) -> 'depreciated';
db_classification(<<"crossbar_schemas">>) -> 'deprecated';
db_classification(<<"registrations">>) -> 'deprecated';
db_classification(<<"crossbar%2Fsessions">>) -> 'deprecated';
db_classification(<<"sms">>) -> 'deprecated';
db_classification(<<"signups">>) -> 'system'; %% Soon to be deprecated
db_classification(?WH_PROVISIONER_DB) -> 'system'; %% Soon to be deprecated
db_classification(?WH_ACCOUNTS_DB) -> 'aggregate';
db_classification(?KZ_TOKEN_DB) -> 'aggregate';
db_classification(?WH_SIP_DB) -> 'aggregate';
db_classification(?WH_FAXES_DB) -> 'aggregate';
db_classification(?KZ_ACDC_DB) -> 'aggregate';
db_classification(?WH_SERVICES_DB) -> 'aggregate';
db_classification(?KZ_PORT_REQUESTS_DB) -> 'aggregate';
db_classification(?KZ_WEBHOOKS_DB) -> 'aggregate';
db_classification(<<?WNM_DB_PREFIX_L, _Prefix:5/binary>>) -> 'numbers';
db_classification(<<"numbers%2F", _Prefix:5/binary>>) -> 'numbers';
db_classification(<<"numbers%2f", _Prefix:5/binary>>) -> 'numbers';
db_classification(<<"account/", _AccountId:34/binary, "-", _Date:6/binary>>) -> 'modb';
db_classification(<<"account%2F", _AccountId:38/binary, "-", _Date:6/binary>>) -> 'modb';
db_classification(<<"account%2f", _AccountId:38/binary, "-", _Date:6/binary>>) -> 'modb';
db_classification(<<"account/", _AccountId:34/binary>>) -> 'account';
db_classification(<<"account%2f", _AccountId:38/binary>>) -> 'account';
db_classification(<<"account%2F", _AccountId:38/binary>>) -> 'account';
db_classification(?WH_RATES_DB) -> 'system';
db_classification(?WH_OFFNET_DB) -> 'system';
db_classification(?WH_ANONYMOUS_CDR_DB) -> 'system';
db_classification(?WH_DEDICATED_IP_DB) -> 'system';
db_classification(?WH_CONFIG_DB) -> 'system';
db_classification(?WH_MEDIA_DB) -> 'system';
db_classification(?WH_SCHEMA_DB) -> 'system';
db_classification(?KZ_OAUTH_DB) -> 'system';
db_classification(_Database) ->
    lager:debug("unknown type for database ~s", [_Database]),
    'undefined'.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec archive(ne_binary()) -> 'ok'.
-spec archive(ne_binary(), ne_binary()) -> 'ok'.
archive(Db) ->
    Folder = whapps_config:get(?CONFIG_CAT, <<"default_archive_folder">>, <<"/tmp">>),
    archive(Db, filename:join([<<Folder/binary, "/", Db/binary, ".json">>])).

archive(Db, Filename) ->
    {'ok', DbInfo} = couch_mgr:db_info(Db),
    {'ok', File} = file:open(Filename, ['write']),
    'ok' = file:write(File, <<"[">>),
    io:format("archiving to ~s~n", [Filename]),
    MaxDocs = whapps_config:get_integer(?CONFIG_CAT, <<"max_concurrent_docs_to_archive">>, 500),
    archive(Db, File, MaxDocs, wh_json:get_integer_value(<<"doc_count">>, DbInfo), 0),
    'ok' = file:write(File, <<"]">>),
    file:close(File).

%% MaxDocs = The biggest set of docs to pull from Couch
%% N = The number of docs in the DB that haven't been archived
%% Pos = Which doc will the next query start from (the offset)
-spec archive(ne_binary(), file:io_device(), pos_integer(), non_neg_integer(), non_neg_integer()) -> 'ok'.
archive(Db, _File,  _MaxDocs, 0, _Pos) ->
    io:format("    archive ~s complete~n", [Db]);
archive(Db, File, MaxDocs, N, Pos) when N =< MaxDocs ->
    ViewOptions = [{'limit', N}
                   ,{'skip', Pos}
                   ,'include_docs'
                  ],
    case couch_mgr:all_docs(Db, ViewOptions) of
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
    case couch_mgr:all_docs(Db, ViewOptions) of
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

%%------------------------------------------------------------------------------
%% @public
%% @doc How many documents are chunked when doing a bulk save
%% @end
%%------------------------------------------------------------------------------
-spec max_bulk_insert() -> ?MAX_BULK_INSERT.
max_bulk_insert() -> ?MAX_BULK_INSERT.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_new_connection(nonempty_string() | ne_binary(), pos_integer(), string(), string()) ->
                                server() |
                                {'error', 'timeout' | 'ehostunreach' | _}.
get_new_connection(Host, Port, "", "") ->
    get_new_conn(Host, Port, ?IBROWSE_OPTS);
get_new_connection(Host, Port, User, Pass) ->
    get_new_conn(Host, Port, [{'basic_auth', {User, Pass}} | ?IBROWSE_OPTS]).

-spec get_new_conn(nonempty_string() | ne_binary(), pos_integer(), wh_proplist()) ->
                          server() |
                          {'error', 'timeout'} |
                          {'error', 'ehostunreach'}.
get_new_conn(Host, Port, Opts) ->
    Conn = couchbeam:server_connection(wh_util:to_list(Host), Port, "", Opts),
    lager:debug("new connection to host ~s:~b, testing: ~p", [Host, Port, Conn]),
    case server_info(Conn) of
        {'ok', ConnData} ->
            CouchVersion = wh_json:get_value(<<"version">>, ConnData),
            BigCouchVersion = wh_json:get_value(<<"bigcouch">>, ConnData),
            lager:info("connected successfully to ~s:~b", [Host, Port]),
            lager:debug("responding CouchDB version: ~p", [CouchVersion]),
            lager:debug("responding BigCouch version: ~p", [BigCouchVersion]),
            Conn;
        {'error', {'conn_failed', {'error', 'timeout'}}} ->
            lager:warning("connection timed out for ~s:~p", [Host, Port]),
            {'error', 'timeout'};
        {'error', {'conn_failed', {'error', 'ehostunreach'}}} ->
            lager:warning("connection to ~s:~p failed: Host is unreachable", [Host, Port]),
            {'error', 'ehostunreach'};
        {'error', _E}=E ->
            lager:warning("connection to ~s:~p failed: ~p", [Host, Port, _E]),
            E
    end.

-spec server_info(server()) -> {'ok', wh_json:object()} |
                               {'error', any()}.
server_info(#server{}=Conn) -> couchbeam:server_info(Conn).

-spec server_url(server()) -> ne_binary().
server_url(#server{host=Host, port=Port, options=Options}) ->
    UserPass = case props:get_value('basic_auth', Options) of
                   'undefined' -> <<>>;
                   {U, P} -> list_to_binary([U, <<":">>, P])
               end,
    Protocol = case wh_util:is_true(props:get_value('is_ssl', Options)) of
                   'false' -> <<"http">>;
                   'true' -> <<"https">>
               end,

    list_to_binary([Protocol, <<"://">>, UserPass
                    ,<<"@">>, wh_util:to_binary(Host)
                    ,<<":">>, wh_util:to_binary(Port)
                    ,<<"/">>
                   ]).

-spec db_url(server(), ne_binary()) -> ne_binary().
db_url(#server{}=Conn, DbName) ->
    Server = server_url(Conn),
    list_to_binary([Server, DbName]).

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
        {'ok', _} ->
            _ = maybe_publish_db(DbName, 'created'),
            'true'
    end.

-spec db_delete(server(), ne_binary()) -> boolean().
db_delete(#server{}=Conn, DbName) ->
    case couchbeam:delete_db(Conn, wh_util:to_list(DbName)) of
        {'error', _} -> 'false';
        {'ok', _} ->
            _ = maybe_publish_db(DbName, 'deleted'),
            'true'
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

%%% View-related functions -----------------------------------------------------
-spec design_compact(server(), ne_binary(), ne_binary()) -> boolean().
design_compact(#server{}=Conn, DbName, Design) ->
    case couchbeam:compact(get_db(Conn, DbName), Design) of
        {'error', _E} ->
            lager:debug("failed to compact design doc: ~p", [format_error(_E)]),
            'false';
        'ok' -> 'true'
    end.

-spec design_info(server(), ne_binary(), ne_binary()) ->
                         {'ok', wh_json:object()} |
                         couchbeam_error().
design_info(#server{}=Conn, DBName, Design) ->
    Db = get_db(Conn, DBName),
    do_get_design_info(Db, Design).

-spec all_design_docs(server(), ne_binary(), view_options()) ->
                             {'ok', wh_json:objects()} |
                             couchbeam_error().
all_design_docs(#server{}=Conn, DBName, Options) ->
    Db = get_db(Conn, DBName),
    do_fetch_results(Db, 'design_docs', Options).

-spec all_docs(server(), ne_binary(), view_options()) ->
                      {'ok', wh_json:objects()} |
                      couchbeam_error().
all_docs(#server{}=Conn, DbName, Options) ->
    Db = get_db(Conn, DbName),
    do_fetch_results(Db, 'all_docs', Options).

-spec get_results(server(), ne_binary(), ne_binary(), view_options()) ->
                         {'ok', wh_json:objects() | wh_json:keys()} |
                         couchbeam_error().
get_results(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = get_db(Conn, DbName),
    do_fetch_results(Db, DesignDoc, ViewOptions).

%% This function assumes a "reduce" function that returns the count of docs exists
%% Need to see how to get couchbeam to return the "rows" property instead of the result
%% list; that would be better, but for not, setting the view's "reduce" to the _count
%% function will suffice (provided a reduce isn't already defined).
-spec get_results_count(server(), ne_binary(), ne_binary(), view_options()) ->
                               {'ok', integer()} |
                               couchbeam_error().
get_results_count(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = get_db(Conn, DbName),
    do_fetch_results_count(Db, DesignDoc, ViewOptions).

%% Design Doc/View internal functions
-spec do_fetch_results(couchbeam_db(), ddoc(), view_options()) ->
                              {'ok', wh_json:objects() | ne_binaries()} |
                              couchbeam_error().
do_fetch_results(Db, DesignDoc, Options) ->
    ?RETRY_504(
       case couchbeam_view:fetch(Db, DesignDoc, Options) of
           {'ok', JObj} -> {'ok', wh_json:get_value(<<"rows">>, JObj, JObj)};
           {'error', _T, E} -> {'error', format_error(E)};
           {'error', E} -> {'error', format_error(E)}
       end
      ).

format_error({'failure', 404}) -> 'not_found';
format_error({'failure', 400}) -> 'client_error';
format_error({'http_error', {'status', 504}}) -> 'gateway_timeout';
format_error({'conn_failed', {'error', 'timeout'}}) -> 'connection_timeout';
format_error({'conn_failed', {'error', 'enetunreach'}}) -> 'network_unreachable';
format_error({'conn_failed', {'error', 'system_limit'}}) ->
    lager:critical("system limit reached for database operations!!"),
    'system_limit';
format_error({'conn_failed',{'error','econnrefused'}}) ->
    lager:warning("connection is being refused"),
    'econnrefused';
format_error({'ok', "500", _Headers, Body}) ->
    case wh_json:get_value(<<"error">>, wh_json:decode(Body)) of
        <<"timeout">> -> 'server_timeout';
        _Error ->
            lager:warning("server error: ~s", [Body]),
            'server_error'
    end;
format_error('timeout') -> 'timeout';
format_error(E) ->
    lager:warning("unformatted error: ~p", [E]),
    E.

-spec do_fetch_results_count(couchbeam_db(), ddoc(), view_options()) ->
                                    {'ok', api_integer()} |
                                    couchbeam_error().
do_fetch_results_count(Db, DesignDoc, Options) ->
    ?RETRY_504(
       case couchbeam_view:count(Db, DesignDoc, Options) of
           {'error', E} -> {'error', format_error(E)};
           N -> {'ok', N}
       end
      ).

-spec do_get_design_info(couchbeam_db(), ne_binary()) ->
                                {'ok', wh_json:object()} |
                                couchbeam_error().
do_get_design_info(#db{}=Db, Design) ->
    ?RETRY_504(couchbeam:design_info(Db, Design)).

%% Document related functions --------------------------------------------------

-spec open_cache_doc(server(), ne_binary(), ne_binary(), wh_proplist()) ->
                                  {'ok', wh_json:object()} |
                                  couchbeam_error().
open_cache_doc(#server{}=Conn, DbName, DocId, Options) ->
    case wh_cache:peek_local(?WH_COUCH_CACHE, {?MODULE, DbName, DocId}) of
        {'ok', {'error', _}=E} -> E;
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            case open_doc(Conn, DbName, DocId, remove_non_couchbeam_options(Options)) of
                {'error', _}=E ->
                    maybe_cache_failure(DbName, DocId, Options, E),
                    E;
                {'ok', JObj}=Ok ->
                    add_to_doc_cache(DbName, DocId, JObj),
                    Ok
            end
    end.

-spec remove_non_couchbeam_options(wh_proplist()) -> wh_proplist().
remove_non_couchbeam_options(Options) ->
    props:delete_keys(['cache_failures'], Options).

-spec maybe_cache_failure(ne_binary(), ne_binary(), wh_proplist(), couchbeam_error()) -> 'ok'.
-spec maybe_cache_failure(ne_binary(), ne_binary(), wh_proplist(), couchbeam_error(), atoms()) -> 'ok'.
maybe_cache_failure(DbName, DocId, Options, Error) ->
    case props:get_value('cache_failures', Options) of
        ErrorCodes when is_list(ErrorCodes) ->
            maybe_cache_failure(DbName, DocId, Options, Error, ErrorCodes);
        'true' -> add_to_doc_cache(DbName, DocId, Error);
        _ -> 'ok'
    end.

maybe_cache_failure(DbName, DocId, _Options, {'error', ErrorCode}=Error, ErrorCodes) ->
    case lists:member(ErrorCode, ErrorCodes) of
        'true' -> add_to_doc_cache(DbName, DocId, Error);
        'false' -> 'ok'
    end.

-spec add_to_doc_cache(ne_binary(), ne_binary(), wh_json:object() | couchbeam_error()) -> 'ok'.
add_to_doc_cache(DbName, DocId, CacheValue) ->
    CacheProps = [{'origin', {'db', DbName, DocId}}],
    case wh_json:is_json_object(CacheValue) of
        'true' ->
           cache_if_not_media(CacheProps, DbName, DocId, CacheValue);
        'false' ->
            wh_cache:store_local(?WH_COUCH_CACHE, {?MODULE, DbName, DocId}, CacheValue, CacheProps)
    end.

-spec cache_if_not_media(wh_proplist(), ne_binary(), ne_binary(), wh_json:object() | couchbeam_error()) -> 'ok'.
cache_if_not_media(CacheProps, DbName, DocId, CacheValue) ->
    %% NOTE: this is currently necessary because when a http_put is issued to
    %%   freeswitch and the media is uploaded it goes directly to bigcouch
    %%   and therefore no doc change notice is pushed.  This results in the
    %%   doc cache containing a document tha thas no attachements (or the wrong
    %%   attachments).  What needs to happen is a change notice get sent on the
    %%   message bus anytime a http_put is issued (or maybe if the store
    %%   url is built in media IF everything uses that helper function,
    %    which is not currently the case...)
    case wh_doc:type(CacheValue) of
        <<"media">> -> 'ok';
        <<"private_media">> -> 'ok';
        _Else ->
            wh_cache:store_local(?WH_COUCH_CACHE, {?MODULE, DbName, DocId}, CacheValue, CacheProps)
    end.

-spec flush_cache_doc(ne_binary() | db(), ne_binary() | wh_json:object()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc) ->
    flush_cache_doc(#db{name=Name}, Doc, []).

-spec flush_cache_doc(ne_binary() | db(), ne_binary() | wh_json:object(), wh_proplist()) -> 'ok'.
flush_cache_doc(#db{name=Name}, Doc, Options) ->
    flush_cache_doc(wh_util:to_binary(Name), Doc, Options);
flush_cache_doc(DbName, DocId, _Options) when is_binary(DocId) ->
    wh_cache:erase_local(?WH_COUCH_CACHE, {?MODULE, DbName, DocId});
flush_cache_doc(DbName, Doc, Options) ->
    flush_cache_doc(DbName, wh_doc:id(Doc), Options).

-spec flush_cache_docs() -> 'ok'.
flush_cache_docs() -> wh_cache:flush_local(?WH_COUCH_CACHE).

-spec flush_cache_docs(ne_binary() | db()) -> 'ok'.
flush_cache_docs(#db{name=Name}) ->
    flush_cache_docs(wh_util:to_binary(Name));
flush_cache_docs(DbName) ->
    Filter = fun({?MODULE, DbName1, _DocId}=K, _) when DbName1 =:= DbName ->
                     wh_cache:erase_local(?WH_COUCH_CACHE, K),
                     'true';
                (_, _) -> 'false'
             end,
    _ = wh_cache:filter_local(?WH_COUCH_CACHE, Filter),
    'ok'.

-spec flush_cache_docs(ne_binary() | db(), ne_binaries() | wh_json:objects()) -> 'ok'.
flush_cache_docs(Db, Docs) ->
    flush_cache_docs(Db, Docs, []).

-spec flush_cache_docs(ne_binary() | db(), ne_binaries() | wh_json:objects(), wh_proplist()) -> 'ok'.
flush_cache_docs(Db, Docs, Options) ->
    _ = [flush_cache_doc(Db, Doc, Options)
         || Doc <- Docs
        ],
    'ok'.

-spec open_doc(server(), ne_binary(), ne_binary(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      couchbeam_error().
open_doc(#server{}=Conn, DbName, DocId, Options) ->
    Db = get_db(Conn, DbName),
    do_fetch_doc(Db, DocId, Options).

-spec save_doc(server(), ne_binary(), wh_json:object(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      couchbeam_error().
save_doc(#server{}=Conn, DbName, Doc, Options) ->
    Db = get_db(Conn, DbName),
    do_save_doc(Db, Doc, Options).

-spec save_docs(server(), ne_binary(), wh_json:objects(), wh_proplist()) ->
                       {'ok', wh_json:objects()} |
                       couchbeam_error().
save_docs(#server{}=Conn, DbName, Docs, Options) ->
    Db = get_db(Conn, DbName),
    do_save_docs(Db, Docs, Options).

-spec lookup_doc_rev(server(), ne_binary(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            couchbeam_error().
lookup_doc_rev(#server{}=Conn, DbName, DocId) ->
    case do_fetch_rev(get_db(Conn, DbName), DocId) of
        ?NE_BINARY = Rev -> {'ok', Rev};
        {'error', _}=E -> E
    end.

-spec ensure_saved(server(), ne_binary(), wh_json:object(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          couchbeam_error().
ensure_saved(#server{}=Conn, DbName, Doc, Opts) ->
    Db = get_db(Conn, DbName),
    do_ensure_saved(Db, Doc, Opts).

-spec del_doc(server(), ne_binary(), wh_json:object() | ne_binary()) ->
                     {'ok', wh_json:objects()} |
                     couchbeam_error().
del_doc(#server{}=Conn, DbName, DocId) when is_binary(DocId) ->
    case lookup_doc_rev(Conn, DbName, DocId) of
        {'error', _}=Err -> Err;
        {'ok', Rev} ->
            del_doc(Conn, DbName, wh_json:from_list([{<<"_id">>, DocId}
                                                     ,{<<"_rev">>, Rev}
                                                    ]))
    end;
del_doc(#server{}=Conn, DbName, Doc) ->
    Db = get_db(Conn, DbName),
    do_delete_doc(Conn, Db, Doc).

-spec del_docs(server(), ne_binary(), wh_json:objects()) ->
                      {'ok', wh_json:objects()} |
                      couchbeam_error().
del_docs(#server{}=Conn, DbName, Doc) ->
    Db = get_db(Conn, DbName),
    do_delete_docs(Conn, Db, Doc).

%% Internal Doc functions
-spec do_delete_doc(server(), couchbeam_db(), wh_json:object() | ne_binary()) ->
                           {'ok', wh_json:objects()} |
                           couchbeam_error().
do_delete_doc(Conn, #db{}=Db, Doc) ->
    do_delete_docs(Conn, Db, [Doc]).

-spec do_delete_docs(server(), couchbeam_db(), wh_json:objects() | ne_binaries()) ->
                            {'ok', wh_json:objects()} |
                            couchbeam_error().
do_delete_docs(Conn, #db{}=Db, Docs) ->
    do_save_docs(Db, [prepare_doc_for_del(Conn, Db, Doc) || Doc <- Docs], []).

%% Save only the minimal amount of document needed, since couch can't fully
%% delete the doc off the node's disk
%% See https://wiki.apache.org/couchdb/FUQ, point 4 for Documents
%% and http://grokbase.com/t/couchdb/user/11cpvasem0/database-size-seems-off-even-after-compaction-runs

-spec prepare_doc_for_del(server(), couchbeam_db(), wh_json:object() | ne_binary()) ->
                                 wh_json:object().
prepare_doc_for_del(Conn, Db, <<_/binary>> = DocId) ->
    prepare_doc_for_del(Conn, Db, wh_json:from_list([{<<"_id">>, DocId}]));
prepare_doc_for_del(Conn, #db{name=DbName}, Doc) ->
    Id = wh_doc:id(Doc),
    DocRev = case wh_doc:revision(Doc) of
                 'undefined' ->
                     {'ok', Rev} = lookup_doc_rev(Conn, wh_util:to_binary(DbName), Id),
                     Rev;
                 Rev -> Rev
             end,
    wh_json:from_list(
      props:filter_undefined(
        [{<<"_id">>, Id}
         ,{<<"_rev">>, DocRev}
         ,{<<"_deleted">>, 'true'}
         | publish_fields(Doc)
        ])).

-spec do_ensure_saved(couchbeam_db(), wh_json:object(), wh_proplist()) ->
                             {'ok', wh_json:object()} |
                             couchbeam_error().
do_ensure_saved(#db{}=Db, Doc, Opts) ->
    case do_save_doc(Db, Doc, Opts) of
        {'ok', _}=Ok -> Ok;
        {'error', 'conflict'} ->
            case do_fetch_rev(Db, wh_doc:id(Doc)) of
                {'error', 'not_found'} ->
                    do_ensure_saved(Db, wh_doc:delete_revision(Doc), Opts);
                Rev ->
                    do_ensure_saved(Db, wh_doc:set_revision(Doc, Rev), Opts)
            end;
        {'error', _}=E -> E
    end.

-spec do_fetch_rev(couchbeam_db(), ne_binary()) ->
                          ne_binary() |
                          couchbeam_error().
do_fetch_rev(#db{}=Db, DocId) ->
    case wh_util:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> ?RETRY_504(couchbeam:lookup_doc_rev(Db, DocId))
    end.

-spec do_fetch_doc(couchbeam_db(), ne_binary(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          couchbeam_error().
do_fetch_doc(#db{}=Db, DocId, Options) ->
    case wh_util:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> ?RETRY_504(couchbeam:open_doc(Db, DocId, Options))
    end.

-spec do_save_doc(couchbeam_db(), wh_json:object() | wh_json:objects(), wh_proplist()) ->
                         {'ok', wh_json:object()} |
                         couchbeam_error().
do_save_doc(#db{}=Db, Docs, Options) when is_list(Docs) ->
    do_save_docs(Db, Docs, Options);
do_save_doc(#db{}=Db, Doc, Options) ->
    {PreparedDoc, PublishDoc} = prepare_doc_for_save(Db, Doc),
    case ?RETRY_504(couchbeam:save_doc(Db, PreparedDoc, Options)) of
        {'ok', JObj}=Ok ->
            _ = maybe_publish_doc(Db, PublishDoc, JObj),
            Ok;
        Else -> Else
    end.

-spec do_save_docs(couchbeam_db(), wh_json:objects(), wh_proplist()) ->
                          {'ok', wh_json:objects()} |
                          couchbeam_error().
do_save_docs(#db{}=Db, Docs, Options) ->
    do_save_docs(Db, Docs, Options, []).

-spec do_save_docs(couchbeam_db(), wh_json:objects(), wh_proplist(), wh_json:objects()) ->
                          {'ok', wh_json:objects()} |
                          couchbeam_error().
do_save_docs(#db{}=Db, Docs, Options, Acc) ->
    try lists:split(?MAX_BULK_INSERT, Docs) of
        {Save, Cont} ->
            case perform_save_docs(Db, Save, Options) of
                {'error', _}=E -> E;
                {'ok', JObjs} -> do_save_docs(Db, Cont, Options, JObjs ++ Acc)
            end
    catch
        'error':'badarg' ->
            case perform_save_docs(Db, Docs, Options) of
                {'ok', JObjs} -> {'ok', JObjs ++ Acc};
                {'error', _}=E -> E
            end
    end.

-spec perform_save_docs(couchbeam_db(), wh_json:objects(), wh_proplist()) ->
                               {'ok', wh_json:objects()} |
                               couchbeam_error().
perform_save_docs(Db, Docs, Options) ->
    {PreparedDocs, Publish} = lists:unzip([prepare_doc_for_save(Db, D) || D <- Docs]),
    case ?RETRY_504(couchbeam:save_docs(Db, PreparedDocs, Options)) of
        {'ok', JObjs} ->
            _ = maybe_publish_docs(Db, Publish, JObjs),
            {'ok', JObjs};
        {'error', _}=E -> E
    end.

-spec prepare_doc_for_save(couchbeam_db(), wh_json:object()) ->
                                  {wh_json:object(), wh_json:object()}.
-spec prepare_doc_for_save(couchbeam_db(), wh_json:object(), boolean()) ->
                                  {wh_json:object(), wh_json:object()}.
prepare_doc_for_save(Db, JObj) ->
    prepare_doc_for_save(Db, JObj, wh_util:is_empty(wh_doc:id(JObj))).
prepare_doc_for_save(_Db, JObj, 'true') ->
    prepare_publish(maybe_set_docid(JObj));
prepare_doc_for_save(Db, JObj, 'false') ->
    flush_cache_doc(Db, JObj),
    prepare_publish(JObj).

-spec prepare_publish(wh_json:object()) ->
                             {wh_json:object(), wh_json:object()}.
prepare_publish(JObj) ->
    {maybe_tombstone(JObj), wh_json:from_list(publish_fields(JObj))}.

-spec maybe_tombstone(wh_json:object()) -> wh_json:object().
-spec maybe_tombstone(wh_json:object(), boolean()) -> wh_json:object().
maybe_tombstone(JObj) ->
    maybe_tombstone(JObj, wh_json:is_true(<<"_deleted">>, JObj, 'false')).

maybe_tombstone(JObj, 'true') ->
    wh_json:delete_keys(?PUBLISH_FIELDS, JObj);
maybe_tombstone(JObj, 'false') -> JObj.

-spec maybe_set_docid(wh_json:object()) -> wh_json:object().
maybe_set_docid(Doc) ->
    case wh_doc:id(Doc) of
        'undefined' -> wh_doc:set_id(Doc, couch_mgr:get_uuid());
        _ -> Doc
    end.

%% Attachment-related functions ------------------------------------------------
-spec fetch_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                              {'ok', binary()} |
                              couchbeam_error().
fetch_attachment(#server{}=Conn, DbName, DocId, AName) ->
    Db = get_db(Conn, DbName),
    do_fetch_attachment(Db, DocId, AName).

-spec stream_attachment(server(), ne_binary(), ne_binary(), ne_binary(), pid()) ->
                               {'ok', reference()} |
                               couchbeam_error().
stream_attachment(#server{}=Conn, DbName, DocId, AName, Caller) ->
    do_stream_attachment(get_db(Conn, DbName), DocId, AName, Caller).

-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error().
-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents) ->
    put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, []).

put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, Options) ->
    Db = get_db(Conn, DbName),
    do_put_attachment(Db, DocId, AName, Contents, maybe_add_rev(Db, DocId, Options)).

-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
delete_attachment(#server{}=Conn, DbName, DocId, AName) ->
    delete_attachment(#server{}=Conn, DbName, DocId, AName, []).

delete_attachment(#server{}=Conn, DbName, DocId, AName, Options) ->
    Db = get_db(Conn, DbName),
    do_del_attachment(Db, DocId, AName,  maybe_add_rev(Db, DocId, Options)).

%% Internal Attachment-related functions ---------------------------------------
-spec do_fetch_attachment(couchbeam_db(), ne_binary(), ne_binary()) ->
                                 {'ok', binary()} |
                                 couchbeam_error().
do_fetch_attachment(#db{}=Db, DocId, AName) ->
    ?RETRY_504(couchbeam:fetch_attachment(Db, DocId, AName)).

-spec do_stream_attachment(couchbeam_db(), ne_binary(), ne_binary(), pid()) ->
                                  {'ok', reference()} |
                                  couchbeam_error().
do_stream_attachment(#db{}=Db, DocId, AName, Caller) ->
    couchbeam:stream_fetch_attachment(Db, DocId, AName, Caller).

-spec do_put_attachment(couchbeam_db(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
do_put_attachment(#db{}=Db, DocId, AName, Contents, Options) ->
    _ = flush_cache_doc(Db, DocId),
    case ?RETRY_504(couchbeam:put_attachment(Db, DocId, AName, Contents, Options)) of
        {'ok', JObj}=Ok ->
            maybe_publish_doc(Db, wh_json:from_list([{<<"_id">>, DocId}]), maybe_add_pvt_type(Db, DocId, JObj)),
            Ok;
        Else -> Else
    end.

-spec do_del_attachment(couchbeam_db(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
do_del_attachment(#db{}=Db, DocId, AName, Options) ->
    Doc = wh_util:to_binary(http_uri:encode(wh_util:to_list(DocId))),
    _ = flush_cache_doc(Db, DocId),
    case ?RETRY_504(couchbeam:delete_attachment(Db, Doc, AName, Options)) of
        {'ok', JObj}=Ok ->
            maybe_publish_doc(Db, wh_json:from_list([{<<"_id">>, DocId}]), maybe_add_pvt_type(Db, DocId, JObj)),
            Ok;
        Else -> Else
    end.

%% Helpers for getting Couchbeam records ---------------------------------------

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% returns the #db{} record
%% @end
%%------------------------------------------------------------------------------
-spec get_db(server(), ne_binary()) -> db().
get_db(#server{}=Conn, DbName) ->
    {'ok', Db} = couchbeam:open_db(Conn, DbName),
    Db.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_rev(couchbeam_db(), ne_binary(), wh_proplist()) -> wh_proplist().
maybe_add_rev(#db{name=_Name}=Db, DocId, Options) ->
    case props:get_value('rev', Options) =:= 'undefined'
        andalso do_fetch_rev(Db, DocId)
    of
        <<_/binary>> = Rev ->
            lager:debug("adding rev ~s to options", [Rev]),
            [{'rev', Rev} | Options];
        'false' ->
            lager:debug("rev is in options list: ~p", [Options]),
            Options;
        {'error', 'not_found'} ->
            lager:debug("failed to find rev of ~s in ~p, not_found in db", [DocId, _Name]),
            Options;
        {'error', 'empty_doc_id'} ->
            lager:debug("failed to find doc id ~p", [DocId]),
            Options;
        _Else ->
            lager:debug("unknown rev format for ~p: ~p", [DocId, _Else]),
            Options
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_pvt_type(couchbeam_db(), ne_binary(), wh_json:object()) -> wh_json:object().
maybe_add_pvt_type(Db, DocId, JObj) ->
    case wh_doc:type(JObj) =:= 'undefined'
        andalso couchbeam:open_doc(Db, DocId)
    of
        {'error', R} ->
            lager:error("failed to open doc ~p in ~p : ~p", [DocId, Db, R]),
            JObj;
        {'ok', Doc} ->
            wh_json:set_values(publish_fields(Doc), JObj);
        _Else ->
            JObj
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Send the query function in an anon fun with arity 0; if it returns 504, retry
%% until 3 failed retries occur.
%% @end
%%------------------------------------------------------------------------------
-type retry504_ret() :: _.
%% 'ok' | ne_binary() |
%% {'ok', wh_json:object() | wh_json:objects() |
%%  binary() | ne_binaries() | boolean() | integer()
%% } |
%% couchbeam_error() |
%% {'error', 'timeout'}.

-spec retry504s(fun(() -> retry504_ret())) -> retry504_ret().
-spec retry504s(fun(() -> retry504_ret()), 0..3) -> retry504_ret().
retry504s(Fun) when is_function(Fun, 0) ->
    retry504s(Fun, 0).
retry504s(_Fun, 3) ->
    lager:debug("504 retry failed"),
    whistle_stats:increment_counter(<<"bigcouch-504-error">>),
    {'error', 'timeout'};
retry504s(Fun, Cnt) ->
    whistle_stats:increment_counter(<<"bigcouch-request">>),
    case catch Fun() of
        {'error', {'ok', "504", _, _}} ->
            whistle_stats:increment_counter(<<"bigcouch-504-error">>),
            timer:sleep(100 * (Cnt+1)),
            retry504s(Fun, Cnt+1);
        {'error', {'ok', ErrCode, _Hdrs, _Body}} ->
            whistle_stats:increment_counter(<<"bigcouch-other-error">>),
            {'error', wh_util:to_integer(ErrCode)};
        {'error', _Other}=E ->
            whistle_stats:increment_counter(<<"bigcouch-other-error">>),
            E;
        {'ok', _Other}=OK -> OK;
        {'EXIT', _E} ->
            ST = erlang:get_stacktrace(),
            lager:debug("exception running fun: ~p", [_E]),
            wh_util:log_stacktrace(ST),
            whistle_stats:increment_counter(<<"bigcouch-other-error">>),
            retry504s(Fun, Cnt+1);
        OK -> OK
    end.

-spec maybe_publish_docs(couchbeam_db(), wh_json:objects(), wh_json:objects()) -> 'ok'.
maybe_publish_docs(#db{}=Db, Docs, JObjs) ->
    case couch_mgr:change_notice() of
        'true' ->
            _ = wh_util:spawn(
                  fun() ->
                          [publish_doc(Db, Doc, JObj)
                           || {Doc, JObj} <- lists:zip(Docs, JObjs)
                                  , should_publish_doc(Doc)
                          ]
                  end),
            'ok';
        'false' -> 'ok'
    end.

-spec maybe_publish_doc(couchbeam_db(), wh_json:object(), wh_json:object()) -> 'ok'.
maybe_publish_doc(#db{}=Db, Doc, JObj) ->
    case couch_mgr:change_notice()
        andalso should_publish_doc(Doc)
    of
        'true' ->
            _ = wh_util:spawn(fun() -> publish_doc(Db, Doc, JObj) end),
            'ok';
        'false' -> 'ok'
    end.

-spec maybe_publish_db(ne_binary(), wapi_conf:action()) -> 'ok'.
maybe_publish_db(DbName, Action) ->
    case couch_mgr:change_notice() of
        'true' ->
            _ = wh_util:spawn(fun() -> publish_db(DbName, Action) end),
            'ok';
        'false' -> 'ok'
    end.

-spec should_publish_doc(wh_json:object()) -> boolean().
should_publish_doc(Doc) ->
    case wh_doc:id(Doc) of
        <<"_design/", _/binary>> = _D -> 'false';
        _Else -> 'true'
    end.

-spec publish_doc(couchbeam_db(), wh_json:object(), wh_json:object()) -> 'ok'.
publish_doc(#db{name=DbName}, Doc, JObj) ->
    case wh_doc:is_soft_deleted(Doc)
        orelse wh_json:is_true(<<"_deleted">>, Doc)
    of
        'true' ->
            publish('deleted', wh_util:to_binary(DbName), publish_fields(Doc, JObj));
        'false' ->
            case wh_doc:revision(JObj) of
                <<"1-", _/binary>> ->
                    publish('created', wh_util:to_binary(DbName), publish_fields(Doc, JObj));
                _Else ->
                    publish('edited', wh_util:to_binary(DbName), publish_fields(Doc, JObj))
            end
    end.

-spec publish_db(ne_binary(), wapi_conf:action()) -> 'ok'.
publish_db(DbName, Action) ->
    Props =
        [{<<"Type">>, 'database'}
         ,{<<"ID">>, DbName}
         ,{<<"Database">>, DbName}
         | wh_api:default_headers(<<"configuration">>
                                  ,<<"db_", (wh_util:to_binary(Action))/binary>>
                                  ,?CONFIG_CAT
                                  ,<<"1.0.0">>
                                 )
        ],
    Fun = fun(P) -> wapi_conf:publish_db_update(Action, DbName, P) end,
    whapps_util:amqp_pool_send(Props, Fun).

-spec publish_fields(wh_json:object()) -> wh_proplist().
-spec publish_fields(wh_json:object(), wh_json:object()) -> wh_json:object().
publish_fields(Doc) ->
    [{Key, V} ||
        Key <- ?PUBLISH_FIELDS,
        wh_util:is_not_empty(V = wh_json:get_value(Key, Doc))
    ].

publish_fields(Doc, JObj) ->
    wh_json:set_values(publish_fields(Doc), JObj).

-spec publish(wapi_conf:action(), ne_binary(), wh_json:object()) -> 'ok'.
publish(Action, Db, Doc) ->
    Type = wh_doc:type(Doc),
    Id = wh_doc:id(Doc),

    IsSoftDeleted = wh_doc:is_soft_deleted(Doc),
    EventName = doc_change_event_name(Action, IsSoftDeleted),

    Props =
        [{<<"ID">>, Id}
         ,{<<"Type">>, Type}
         ,{<<"Database">>, Db}
         ,{<<"Rev">>, wh_doc:revision(Doc)}
         ,{<<"Account-ID">>, doc_acct_id(Db, Doc)}
         ,{<<"Date-Modified">>, wh_doc:created(Doc)}
         ,{<<"Date-Created">>, wh_doc:modified(Doc)}
         ,{<<"Is-Soft-Deleted">>, IsSoftDeleted}
         | wh_api:default_headers(<<"configuration">>
                                  ,EventName
                                  ,?CONFIG_CAT
                                  ,<<"1.0.0">>
                                 )
        ],
    Fun = fun(P) -> wapi_conf:publish_doc_update(Action, Db, Type, Id, P) end,
    whapps_util:amqp_pool_send(Props, Fun).

-spec doc_change_event_name(wapi_conf:action(), boolean()) -> ne_binary().
doc_change_event_name(_Action, 'true') ->
    ?DOC_DELETED;
doc_change_event_name(Action, 'false') ->
    <<"doc_", (wh_util:to_binary(Action))/binary>>.

-spec doc_acct_id(ne_binary(), wh_json:object()) -> ne_binary().
doc_acct_id(Db, Doc) ->
    wh_doc:account_id(Doc, wh_util:format_account_id(Db, 'raw')).


-spec default_copy_function(boolean()) -> copy_function().
default_copy_function('true') -> fun ensure_saved/4;
default_copy_function('false') -> fun save_doc/4.

-spec copy_doc(server(), copy_doc(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      couchbeam_error().
copy_doc(#server{}=Conn, #wh_copy_doc{source_dbname = SourceDb
                                      ,dest_dbname='undefined'
                                     }=CopySpec, Options) ->
    copy_doc(Conn, CopySpec#wh_copy_doc{dest_dbname=SourceDb
                                        ,dest_doc_id=wh_util:rand_hex_binary(16)
                                       }, Options);
copy_doc(#server{}=Conn, #wh_copy_doc{dest_doc_id='undefined'}=CopySpec, Options) ->
    copy_doc(Conn, CopySpec#wh_copy_doc{dest_doc_id=wh_util:rand_hex_binary(16)}, Options);
copy_doc(#server{}=Conn, CopySpec, Options) ->
    SaveFun = default_copy_function(props:is_defined(?COPY_DOC_OVERRIDE_PROPERTY, Options)),
    copy_doc(Conn, CopySpec, SaveFun, props:delete(?COPY_DOC_OVERRIDE_PROPERTY, Options)).


-spec copy_doc(server(), copy_doc(), copy_function(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      couchbeam_error().
copy_doc(#server{}=Conn, CopySpec, CopyFun, Options) ->
    #wh_copy_doc{source_dbname = SourceDbName
                 ,source_doc_id = SourceDocId
                 ,dest_dbname = DestDbName
                 ,dest_doc_id = DestDocId
                } = CopySpec,
    case open_doc(Conn, SourceDbName, SourceDocId, Options) of
        {'ok', SourceDoc} ->
            Props = [{<<"_id">>, DestDocId}],
            DestinationDoc = wh_json:set_values(Props, wh_json:delete_keys(?DELETE_KEYS, SourceDoc)),
            case CopyFun(Conn, DestDbName, DestinationDoc, Options) of
                {'ok', _JObj} ->
                    Attachments = wh_doc:attachments(SourceDoc, wh_json:new()),
                    copy_attachments(Conn, CopySpec, wh_json:get_values(Attachments));
                Error -> Error
            end;
        Error -> Error
    end.

-spec copy_attachments(server(), copy_doc(), {wh_json:json_terms(), wh_json:keys()}) ->
                              {'ok', ne_binary()} |
                              {'error', any()}.
copy_attachments(#server{}=Conn, CopySpec, {[], []}) ->
    #wh_copy_doc{dest_dbname = DestDbName
                 ,dest_doc_id = DestDocId
                } = CopySpec,
    open_doc(Conn, DestDbName, DestDocId, []);
copy_attachments(#server{}=Conn, CopySpec, {[JObj | JObjs], [Key | Keys]}) ->
    #wh_copy_doc{source_dbname = SourceDbName
                 ,source_doc_id = SourceDocId
                 ,dest_dbname = DestDbName
                 ,dest_doc_id = DestDocId
                } = CopySpec,
    case fetch_attachment(Conn, SourceDbName, SourceDocId, Key) of
        {'ok', Contents} ->
            ContentType = wh_json:get_value([<<"content_type">>], JObj),
            Opts = [{'headers', [{'content_type', wh_util:to_list(ContentType)}]}],
            case put_attachment(Conn, DestDbName, DestDocId, Key, Contents, Opts) of
                {'ok', _AttachmentDoc} ->
                    copy_attachments(Conn, CopySpec, {JObjs, Keys});
                Error -> Error
            end;
        Error -> Error
    end.

-spec move_doc(server(), copy_doc(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      couchbeam_error().
move_doc(Conn, CopySpec, Options) ->
    #wh_copy_doc{source_dbname = SourceDbName
                 ,source_doc_id = SourceDocId
                } = CopySpec,
    case copy_doc(Conn, CopySpec, Options) of
        {'ok', JObj} ->
            _ = del_doc(Conn, SourceDbName, SourceDocId),
            {'ok', JObj};
        Error -> Error
     end.
