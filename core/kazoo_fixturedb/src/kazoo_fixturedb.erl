%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_fixturedb).

-export([start/0, start/1]).

%% Driver callbacks
-export([new_connection/1
        ,format_error/1
        ]).

%% Server callbacks
-export([server_info/1
        ,server_url/1
        ,get_db/2
        ,db_url/2
        ]).


%% DB operations
-export([db_create/3
        ,db_delete/2
        ,db_view_cleanup/2
        ,db_info/1, db_info/2
        ,db_exists/2
        ,db_archive/3
        ,db_list/2
        ]).

%% Document operations
-export([open_doc/4
        ,lookup_doc_rev/3
        ,save_doc/4
        ,save_docs/4
        ,del_doc/4
        ,del_docs/4
        ,ensure_saved/4
        ]).

%% Attachment-related
-export([fetch_attachment/4
        ,stream_attachment/5
        ,put_attachment/6
        ,delete_attachment/5
        ,attachment_url/5
        ]).

%% View-related
-export([design_info/3
        ,all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,all_docs/3
        ]).

-include("kz_fixturedb.hrl").

%%%=============================================================================
%%% Driver callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new_connection(map()) -> {'ok', server_map()}.
new_connection(Map) ->
    kz_fixturedb_server:new_connection(Map).

-spec format_error(any()) -> any().
format_error(Error) ->
    kz_fixturedb_util:format_error(Error).

%%%=============================================================================
%%% Connection operations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_db(server_map(), kz_term:ne_binary()) -> map().
get_db(Server, DbName) ->
    kz_fixturedb_server:get_db(ensure_server(Server), DbName).

-spec server_url(server_map()) -> kz_term:ne_binary().
server_url(Server) ->
    kz_fixturedb_server:server_url(ensure_server(Server)).

-spec db_url(server_map(), kz_term:ne_binary()) -> kz_term:ne_binary().
db_url(Server, DbName) ->
    kz_fixturedb_server:db_url(ensure_server(Server), DbName).

-spec server_info(server_map()) -> doc_resp().
server_info(Server) ->
    kz_fixturedb_server:server_info(ensure_server(Server)).

%%%=============================================================================
%%% DB operations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_create(server_map(), kz_term:ne_binary(), kz_data:options()) -> boolean().
db_create(Server, DbName, Options) ->
    kz_fixturedb_db:db_create(ensure_server(Server), DbName, Options).

-spec db_delete(server_map(), kz_term:ne_binary()) -> boolean().
db_delete(Server, DbName) ->
    kz_fixturedb_db:db_delete(ensure_server(Server), DbName).

-spec db_view_cleanup(server_map(), kz_term:ne_binary()) -> boolean().
db_view_cleanup(Server, DbName) ->
    kz_fixturedb_db:db_view_cleanup(ensure_server(Server), DbName).

-spec db_info(server_map()) -> {ok, kz_term:ne_binaries()}.
db_info(Server) ->
    kz_fixturedb_db:db_info(ensure_server(Server)).

-spec db_info(server_map(), kz_term:ne_binary()) -> docs_resp().
db_info(Server, DbName) ->
    kz_fixturedb_db:db_info(ensure_server(Server), DbName).

-spec db_exists(server_map(), kz_term:ne_binary()) -> boolean().
db_exists(Server, DbName) ->
    kz_fixturedb_db:db_exists(ensure_server(Server), DbName).

-spec db_archive(server_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> ok.
db_archive(_, _, _) ->
    ok.

-spec db_list(server_map(), kz_data:options()) -> {'ok', kz_term:ne_binaries()}.
db_list(Server, Options) ->
    kz_fixturedb_db:db_list(ensure_server(Server), Options).

%%%=============================================================================
%%% Document operations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec open_doc(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> doc_resp().
open_doc(Server, DbName, DocId, Options) ->
    kz_fixturedb_doc:open_doc(ensure_server(Server), DbName, DocId, Options).

-spec lookup_doc_rev(server_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> {ok, kz_term:ne_binary()} | fixture_error().
lookup_doc_rev(Server, DbName, DocId) ->
    kz_fixturedb_doc:lookup_doc_rev(ensure_server(Server), DbName, DocId).

-spec save_doc(server_map(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> doc_resp().
save_doc(Server, DbName, Doc, Options) ->
    kz_fixturedb_doc:save_doc(ensure_server(Server), DbName, Doc, Options).

-spec save_docs(server_map(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) -> docs_resp().
save_docs(Server, DbName, Docs, Options) ->
    kz_fixturedb_doc:save_docs(ensure_server(Server), DbName, Docs, Options).

-spec del_doc(server_map(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> docs_resp().
del_doc(Server, DbName, Doc, Options) ->
    kz_fixturedb_doc:del_doc(ensure_server(Server), DbName, Doc, Options).

-spec del_docs(server_map(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) -> docs_resp().
del_docs(Server, DbName, Docs, Options) ->
    kz_fixturedb_doc:del_docs(ensure_server(Server), DbName, Docs, Options).

-spec ensure_saved(server_map(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> doc_resp().
ensure_saved(Server, DbName, Doc, Options) ->
    kz_fixturedb_doc:ensure_saved(ensure_server(Server), DbName, Doc, Options).

%%%=============================================================================
%%% Attachment-related
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_attachment(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> {ok, binary()} | fixture_error().
fetch_attachment(Server, DbName, DocId, AName) ->
    kz_fixturedb_attachments:fetch_attachment(ensure_server(Server), DbName, DocId, AName).

-spec stream_attachment(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) -> {ok, reference()} | fixture_error().
stream_attachment(Server, DbName, DocId, AName, Caller) ->
    kz_fixturedb_attachments:stream_attachment(ensure_server(Server), DbName, DocId, AName, Caller).

-spec put_attachment(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> doc_resp().
put_attachment(Server, DbName, DocId, AName, Contents, Options) ->
    kz_fixturedb_attachments:put_attachment(ensure_server(Server), DbName, DocId, AName, Contents, Options).

-spec delete_attachment(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> doc_resp().
delete_attachment(Server, DbName, DocId, AName, Options) ->
    kz_fixturedb_attachments:delete_attachment(ensure_server(Server), DbName, DocId, AName, Options).

-spec attachment_url(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> kz_term:ne_binary().
attachment_url(Server, DbName, DocId, AName, Options) ->
    kz_fixturedb_attachments:attachment_url(ensure_server(Server), DbName, DocId, AName, Options).

%%%=============================================================================
%%% View-related
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec design_info(server_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> doc_resp().
design_info(Server, DbName, Design) ->
    kz_fixturedb_view:design_info(ensure_server(Server), DbName, Design).

-spec all_design_docs(server_map(), kz_term:ne_binary(), kz_data:options()) -> docs_resp().
all_design_docs(Server, DbName, Options) ->
    kz_fixturedb_view:all_design_docs(ensure_server(Server), DbName, Options).

-spec get_results(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> get_results().
get_results(Server, DbName, Design, Options) ->
    kz_fixturedb_view:get_results(ensure_server(Server), DbName, Design, Options).

-spec get_results_count(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
          {'ok', non_neg_integer()} | no_return().
get_results_count(Server, DbName, Design, Options) ->
    kz_fixturedb_view:get_results_count(ensure_server(Server), DbName, Design, Options).

-spec all_docs(server_map(), kz_term:ne_binary(), kz_data:options()) -> get_results().
all_docs(Server, DbName, Options) ->
    kz_fixturedb_view:all_docs(ensure_server(Server), DbName, Options).

%%%=============================================================================
%%% Utilities
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start() -> pid().
start() ->
    start('false').

-spec start(boolean()) -> pid().
start(SilentLager) ->
    kz_fixturedb_util:start_me(SilentLager).

-spec ensure_server(server_map()) -> server_map().
ensure_server(#{options := _, url := _}=ServerConn) ->
    ServerConn;
ensure_server(_) ->
    kz_fixturedb_maintenance:new_connection().
