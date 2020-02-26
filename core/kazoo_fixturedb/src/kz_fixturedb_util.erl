%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fixturedb_util).

%% Driver callbacks
-export([format_error/1]).

%% API
-export([open_json/2, doc_path/2
        ,open_attachment/3, att_path/3
        ,open_view/3, view_path/3

        ,docs_dir/1, views_dir/1

        ,update_doc/1, update_revision/1

        ,encode_query_filename/2

        ,get_default_fixtures_db/1

         %% utilities for calling from shell
        ,get_doc_path/2, get_view_path/3, get_att_path/3
        ,get_doc_path/3, get_view_path/4, get_att_path/4

        ,add_view_to_index/3, add_att_to_index/3
        ,add_view_to_index/4, add_att_to_index/4

        ,update_pvt_doc_hash/0, update_pvt_doc_hash/1

        ,start_me/0, start_me/1, stop_me/1

        ,view_index_to_disk/3
        ]).

-include("kz_fixturedb.hrl").

-type binary_or_error() :: {'ok', binary()} | {'error', 'not_found' | any()}.

%%%=============================================================================
%%% Driver callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_error(any()) -> any().
format_error('timeout') -> 'timeout';
format_error('conflict') -> 'conflict';
format_error('not_found') -> 'not_found';
format_error('db_not_found') -> 'db_not_found';
format_error(Other) -> Other.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec open_json(db_map(), kz_term:ne_binary()) -> doc_resp().
open_json(Db, DocId) ->
    read_json(doc_path(Db, DocId)).

-spec open_attachment(db_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> binary_or_error().
open_attachment(Db, DocId, AName) ->
    read_file(att_path(Db, DocId, AName)).

-spec open_view(db_map(), kz_term:ne_binary(), kz_data:options()) -> docs_resp().
open_view(Db, Design, Options) ->
    read_json(view_path(Db, Design, Options)).

-spec doc_path(db_map(), kz_term:ne_binary()) -> file:filename_all().
doc_path(#{server := #{url := Url}, name := DbName}, DocId) ->
    filename:join(kz_term:to_list(Url) ++ "/" ++ kz_term:to_list(DbName)
                 ,["docs/", http_uri:encode(kz_term:to_list(DocId)), ".json"]
                 ).

-spec att_path(db_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> file:filename_all().
att_path(#{server := #{url := Url}, name := DbName}, DocId, AName) ->
    AttName = kz_binary:hexencode(crypto:hash('md5', <<DocId/binary, AName/binary>>)),
    filename:join(kz_term:to_list(Url) ++ "/" ++ kz_term:to_list(DbName)
                 ,["docs/", kz_term:to_list(AttName), ".att"]
                 ).

-spec view_path(db_map(), kz_term:ne_binary(), kz_data:options()) -> file:filename_all().
view_path(#{server := #{url := Url}, name := DbName}, Design, Options) ->
    filename:join(kz_term:to_list(Url) ++ "/" ++ kz_term:to_list(DbName)
                 ,["views/", encode_query_filename(Design, Options)]
                 ).

%% @doc The idea is to encode file name based on view options so you can
%% write JSON file specifically for each of your view queries
-spec encode_query_filename(kz_term:ne_binary(), kz_data:options()) -> kz_term:text().
encode_query_filename(Design, Options) ->
    encode_query_options(Design, ?DANGEROUS_VIEW_OPTS, Options, []).

-spec docs_dir(db_map()) -> kz_term:text().
docs_dir(#{server := #{url := Url}, name := DbName}) ->
    kz_term:to_list(filename:join([Url, DbName, "docs"])).

-spec views_dir(db_map()) -> kz_term:text().
views_dir(#{server := #{url := Url}, name := DbName}) ->
    kz_term:to_list(filename:join([Url, DbName, "views"])).

-spec update_doc(kz_json:object()) -> kz_json:object().
update_doc(JObj) ->
    Funs = [
            %% fun update_revision/1
            fun(J) -> kz_doc:set_document_hash(J, kz_doc:calculate_document_hash(J)) end
           ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Funs).

-spec update_revision(kz_json:object()) -> kz_json:object().
update_revision(JObj) ->
    case kz_doc:revision(JObj) of
        'undefined' ->
            kz_doc:set_revision(JObj, <<"1-", (kz_binary:rand_hex(16))/binary>>);
        Rev ->
            [RevPos|_] = binary:split(Rev, <<"-">>),
            Rev2 = kz_term:to_integer(RevPos) + 1,
            kz_doc:set_revision(JObj, <<(kz_term:to_binary(Rev2))/binary, "-", (kz_binary:rand_hex(16))/binary>>)
    end.

%%%=============================================================================
%%% Handy functions to use from shell to managing files
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_me() -> pid().
start_me() ->
    start_me('false').

-spec start_me(boolean()) -> pid().
start_me(SilentLager) ->
    ?LOG_DEBUG(":: Starting up Kazoo FixtureDB"),

    {'ok', _Started} = application:ensure_all_started('kazoo_config'),

    ?LOG_DEBUG("started all apps ~p", [_Started]),

    Pid = case kazoo_data_link_sup:start_link() of
              {'ok', P} -> P;
              {'error', {'already_started', P}} -> P
          end,

    'ignore' = kazoo_data_bootstrap:start_link(),

    _ = case SilentLager of
            'true' ->
                _ = lager:set_loglevel('lager_console_backend', 'none'),
                _ = lager:set_loglevel('lager_file_backend', 'none'),
                lager:set_loglevel('lager_syslog_backend', 'none');
            'false' -> 'ok'
        end,
    Pid.

-spec stop_me(pid()) -> 'ok'.
stop_me(Pid) ->
    _ = erlang:exit(Pid, 'normal'),
    _ = application:stop('kazoo_config'),
    ?LOG_DEBUG(":: Stopped Kazoo FixtureDB").

-spec get_doc_path(kz_term:ne_binary(), kz_term:ne_binary()) -> file:filename_all().
get_doc_path(DbName, DocId) ->
    get_doc_path(kz_fixturedb_maintenance:new_connection(), DbName, DocId).

-spec get_doc_path(server_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> file:filename_all().
get_doc_path(Conn, DbName, DocId) ->
    doc_path(kz_fixturedb_server:get_db(Conn, DbName), DocId).

-spec get_att_path(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> file:filename_all().
get_att_path(DbName, DocId, AName) ->
    get_att_path(kz_fixturedb_maintenance:new_connection(), DbName, DocId, AName).

-spec get_att_path(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> file:filename_all().
get_att_path(Conn, DbName, DocId, AName) ->
    att_path(kz_fixturedb_server:get_db(Conn, DbName), DocId, AName).

-spec get_view_path(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> file:filename_all().
get_view_path(DbName, Design, Options) ->
    get_view_path(kz_fixturedb_maintenance:new_connection(), DbName, Design, Options).

-spec get_view_path(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> file:filename_all().
get_view_path(Conn, DbName, Design, Options) ->
    view_path(kz_fixturedb_server:get_db(Conn, DbName), Design, Options).

-spec add_att_to_index(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | {'error', any()}.
add_att_to_index(DbName, DocId, AName) ->
    add_att_to_index(kz_fixturedb_maintenance:new_connection(), DbName, DocId, AName).

-spec add_att_to_index(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> binary_or_error().
add_att_to_index(Conn, DbName, DocId, AName) ->
    #{server := #{url := Url}} = Db = kz_fixturedb_server:get_db(Conn, DbName),
    AttPath = att_path(Db, DocId, AName),
    Row = kz_term:to_binary(io_lib:format("~s, ~s, ~s", [DocId, AName, filename:basename(AttPath)])),
    Header = <<"doc_id, attachment_name, attachment_file_name">>,

    IndexPath = index_file_path("attachment", Url, DbName),
    case update_index_file(IndexPath, Header, Row) of
        {'ok', _} ->
            maybe_symlink_att_file(AName, AttPath),
            {'ok', Row};
        {'error', _}=Error -> Error
    end.

-spec maybe_symlink_att_file(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_symlink_att_file(AName, AttPath) ->
    case filelib:is_regular(filename:join([code:priv_dir('kazoo_fixturedb'), "media_files/", AName])) of
        'true' ->
            case file:make_symlink(<<"../../../media_files/", AName/binary>>, AttPath) of
                'ok' ->
                    ?DEV_LOG("created a sym-link for ~s to kazoo_fixturedb/priv/media_files/~s", [filename:basename(AttPath), AName]);
                {'error', 'enotsup'} ->
                    ?DEV_LOG("creating sym-link is not supported by your platform");
                {'error', _Reason} ->
                    ?DEV_LOG("Existing ~p~nAttPath ~p", [<<"../../../media_files/", AName/binary>>, AttPath]),
                    ?DEV_LOG("failed to create sym-link for attachment to kazoo_fixturedb/priv/media_files/~s: ~p", [AName, _Reason]),
                    ?DEV_LOG("you have to create the attachment manually.")
            end;
        'false' ->
            ?DEV_LOG("attchment ~s file doesn't exists", [AName])
    end.

-spec add_view_to_index(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> binary_or_error().
add_view_to_index(DbName, Design, Options) ->
    add_view_to_index(kz_fixturedb_maintenance:new_connection(), DbName, Design, Options).

-spec add_view_to_index(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> binary_or_error().
add_view_to_index(Conn, DbName, Design, Options) ->
    #{server := #{url := Url}} = Db = kz_fixturedb_server:get_db(Conn, DbName),
    ViewPath = view_path(Db, Design, Options),
    Row = kz_term:to_binary(io_lib:format("~s, ~1000p, ~s", [Design, Options, filename:basename(ViewPath)])),
    Header = <<"view_name, view_options, view_file_name">>,

    IndexPath = index_file_path("view", Url, DbName),
    case update_index_file(IndexPath, Header, Row) of
        {'ok', _} -> {'ok', Row};
        {'error', _}=Error -> Error
    end.

-spec index_file_path(kz_term:text(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:text().
index_file_path(Mode, Url, DbName) ->
    kz_term:to_list(Url) ++ "/" ++ kz_term:to_list(DbName) ++ "/" ++ Mode ++ "-index.csv".

-spec update_pvt_doc_hash() -> 'ok'.
update_pvt_doc_hash() ->
    Paths = filelib:wildcard(code:priv_dir('kazoo_fixturedb') ++ "/dbs/*/docs/*.json"),
    _ = [update_pvt_doc_hash(Path) || Path <- Paths],
    'ok'.

-spec update_pvt_doc_hash(kz_term:text() | kz_term:ne_binary()) -> 'ok' | {'error', any()}.
update_pvt_doc_hash(Path) ->
    case read_json(Path) of
        {'ok', JObj} ->
            NewJObj = kz_doc:set_document_hash(JObj, kz_doc:calculate_document_hash(JObj)),
            file:write_file(Path, kz_json:encode(NewJObj, ['pretty']));
        {'error', _}=Error -> Error
    end.

-spec get_default_fixtures_db(kz_term:ne_binary()) -> db_map().
get_default_fixtures_db(DbName) ->
    {'ok', Server} = kz_fixturedb_server:new_connection(#{}),
    kz_fixturedb_server:get_db(Server, DbName).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec read_json(file:filename_all()) -> {'ok', kz_json:object() | kz_json:objects()} | {'error', 'not_found'}.
read_json(Path) ->
    process_read_json(read_file(Path)).

process_read_json({'ok', Bin}) -> {'ok', kz_json:decode(Bin)};
process_read_json({'error', _}) -> {'error', 'not_found'}.

-spec read_file(file:filename_all()) -> binary_or_error().
read_file(Path) ->
    process_read_file(file:read_file(Path)).

process_read_file({'ok', _}=OK) -> OK;
process_read_file({'error', _}) -> {'error', 'not_found'}.

-spec update_index_file(file:filename_all(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          binary_or_error().
update_index_file(Path, Header, NewLine) ->
    write_index_file(Path, NewLine, read_index_file(Path, Header, NewLine)).

-spec write_index_file(file:filename_all(), kz_term:ne_binary(), binary_or_error()) ->
          binary_or_error().
write_index_file(_, _, {'error', _}=Error) ->
    Error;
write_index_file(Path, NewLine, {'ok', IndexLines}) ->
    [Header|Lines] = binary:split(IndexLines, <<"\n">>, ['global']),
    ToWrite = [<<Header/binary, "\n">>
                   | [<<L/binary, "\n">>
                          || L <- lists:usort(Lines),
                             kz_term:is_not_empty(L)
                     ]
              ],
    case file:write_file(Path, ToWrite) of
        'ok' -> {'ok', NewLine};
        {'error', _Reason}=Error ->
            ?DEV_LOG("failed to write index file ~s: ~p", [Path, _Reason]),
            Error
    end.

-spec read_index_file(file:filename_all(), kz_term:ne_binary(), kz_term:ne_binary()) -> binary_or_error().
read_index_file(Path, Header, NewLine) ->
    HSize = size(Header),
    case file:read_file(Path) of
        {'ok', <<>>} ->                              {'ok', <<Header/binary  , "\n", NewLine/binary>>};
        {'ok', <<Header:HSize/binary, "\n">>=H} ->   {'ok', <<H/binary       ,       NewLine/binary>>};
        {'ok', Header} ->                            {'ok', <<Header/binary  , "\n", NewLine/binary>>};
        {'ok', IndexBin} ->                          {'ok', <<IndexBin/binary, "\n", NewLine/binary>>};
        {'error', 'enoent'} ->                       {'ok', <<Header/binary  , "\n", NewLine/binary>>};
        {'error', _Reason}=Error ->
            ?DEV_LOG("failed to open index file ~s: ~p", [Path, _Reason]),
            Error
    end.

-spec encode_query_options(kz_term:ne_binary(), list(), kz_data:options(), list()) -> kz_term:text().
encode_query_options(Design, [], _, []) ->
    DesignView = design_view(Design),
    kz_term:to_list(<<DesignView/binary, ".json">>);
encode_query_options(Design, [], _, Acc) ->
    DesignView = design_view(Design),
    QueryHash = kz_binary:hexencode(crypto:hash('md5', erlang:term_to_binary(Acc))),
    kz_term:to_list(<<DesignView/binary, "-", QueryHash/binary, ".json">>);
encode_query_options(Design, [Key|Keys], Options, Acc) ->
    case props:get_value(Key, Options, 'undefined') of
        'undefined' -> encode_query_options(Design, Keys, Options, Acc);
        Value -> encode_query_options(Design, Keys, Options, ["&", Key, "=", Value | Acc])
    end.

-spec design_view(kz_term:ne_binary()) -> kz_term:ne_binary().
design_view(Design) ->
    case binary:split(Design, <<"/">>) of
        [DesignName] -> DesignName;
        [DesignName, ViewName|_] -> <<DesignName/binary, "+", ViewName/binary>>
    end.

-spec view_index_to_disk(kz_term:ne_binary(), kz_term:ne_binary(), kz_datamgr:view_options()) -> 'ok'.
view_index_to_disk(Database, ViewName, Options) ->
    Path = get_view_path(Database, ViewName, Options),
    'ok' = filelib:ensure_dir(Path),

    {'ok', Results} = kz_datamgr:get_results(Database, ViewName, Options),
    lager:info(" persisting view index ~s/~s to ~s", [Database, ViewName, Path]),
    'ok' = file:write_file(Path, kz_json:encode(Results, ['pretty'])).
