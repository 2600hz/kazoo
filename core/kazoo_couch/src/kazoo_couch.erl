%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_couch).
-behaviour(kz_data).

%% Driver callbacks
-export([new_connection/1
        ,format_error/1
        ]).

%% Server callbacks
-export([server_info/1
        ,server_url/1
        ,server_version/1
        ,get_db/2
        ,get_admin_dbs/0, get_admin_dbs/1
        ,get_admin_nodes/0, get_admin_nodes/1
        ,db_url/2
        ]).


%% DB operations
-export([db_create/3
        ,db_delete/2
        ,db_view_cleanup/2
        ,db_info/1, db_info/2
        ,db_exists/2
        ,db_archive/3
        ,db_import/3
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
        ,copy_doc/3
        ,move_doc/3
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
        ,design_compact/3
        ,all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,show/5
        ,all_docs/3
        ]).

-include("kz_couch.hrl").

-export_type([couch_version/0]).

%% Server operations
-spec new_connection(map()) -> kz_data:connection() |
          {'error', 'timeout' | 'ehostunreach' | _}.
new_connection(Map) ->
    kz_couch_util:new_connection(Map).

-spec format_error(any()) -> any().
format_error(Error) ->
    kz_couch_util:format_error(Error).

%% Connection operations
-spec get_db(server(), kz_term:ne_binary()) -> db().
get_db(Server, DbName) ->
    kz_couch_util:get_db(Server, DbName).

-spec get_admin_dbs() -> kz_term:ne_binary().
get_admin_dbs() ->
    #{server := {_App, #server{}=Conn}} = kzs_plan:plan(),
    get_admin_dbs(Conn).

-spec get_admin_dbs(couch_version() | kz_data:connection()) -> kz_term:ne_binary().
get_admin_dbs(#server{}=Server) ->
    get_admin_dbs(server_version(Server));
get_admin_dbs('bigcouch') -> <<"dbs">>;
get_admin_dbs(_Driver) -> <<"_dbs">>.

-spec get_admin_nodes() -> kz_term:ne_binary().
get_admin_nodes() ->
    #{server := {_App, #server{}=Conn}} = kzs_plan:plan(),
    get_admin_nodes(Conn).

-spec get_admin_nodes(couch_version() | kz_data:connection()) -> kz_term:ne_binary().
get_admin_nodes(#server{}=Server) ->
    get_admin_nodes(server_version(Server));
get_admin_nodes('bigcouch') -> <<"nodes">>;
get_admin_nodes(_Driver) -> <<"_nodes">>.

-spec server_url(kz_data:connection()) -> kz_term:ne_binary().
server_url(Server) ->
    kz_couch_util:server_url(Server).

-spec db_url(kz_data:connection(), kz_term:ne_binary()) -> kz_term:ne_binary().
db_url(Server, DbName) ->
    kz_couch_util:db_url(Server, DbName).

-spec server_info(kz_data:connection()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
server_info(Server) ->
    kz_couch_util:server_info(Server).

%% DB operations
-spec db_create(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) -> boolean().
db_create(Server, DbName, Options) ->
    kz_couch_db:db_create(Server, DbName, Options).

-spec db_delete(kz_data:connection(), kz_term:ne_binary()) -> boolean().
db_delete(Server, DbName) ->
    kz_couch_db:db_delete(Server, DbName).

-spec db_view_cleanup(kz_data:connection(), kz_term:ne_binary()) -> boolean().
db_view_cleanup(Server, DbName) ->
    kz_couch_db:db_view_cleanup(Server, DbName).

-spec db_info(kz_data:connection()) -> any().
db_info(Server) ->
    kz_couch_db:db_info(Server).

-spec db_info(kz_data:connection(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
db_info(Server, DbName) ->
    kz_couch_db:db_info(Server, DbName).

-spec db_exists(kz_data:connection(), kz_term:ne_binary()) -> boolean().
db_exists(Server, DbName) ->
    kz_couch_db:db_exists(Server, DbName).

-spec db_archive(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' |
          couchbeam_error().
db_archive(Server, DbName, Filename) ->
    kz_couch_db:db_archive(Server, DbName, Filename).

-spec db_import(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok' |
          couchbeam_error().
db_import(Server, DbName, Filename) ->
    kz_couch_db:db_import(Server, DbName, Filename).

-spec db_list(kz_data:connection(), kz_data:options()) ->
          {'ok', kz_json:objects() | kz_term:ne_binaries()} | couchbeam_error().
db_list(Server, Options) ->
    db_list(server_version(Server), Server, Options).

%%
%% db specific
%%
db_list('couchdb_2', Server, Options) ->
    kz_couch_db:db_list(Server, Options);
db_list('bigcouch', Server, Options) ->
    {'ok', List} = kz_couch_db:db_list(Server, Options),
    {'ok', db_local_filter(List, Options)};
db_list('couchdb_1_6', Server, Options) ->
    {'ok', List} = kz_couch_db:db_list(Server, Options),
    {'ok', db_local_filter(List, Options)}.

%% Document operations
-spec open_doc(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
open_doc(Server, DbName, DocId, Options) ->
    kz_couch_doc:open_doc(Server, DbName, DocId, Options).

-spec lookup_doc_rev(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          couchbeam_error().
lookup_doc_rev(Server, DbName, DocId) ->
    kz_couch_doc:lookup_doc_rev(Server, DbName, DocId).

-spec save_doc(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
save_doc(Server, DbName, Doc, Options) ->
    kz_couch_doc:save_doc(Server, DbName, Doc, Options).

-spec save_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) ->
          {'ok', kz_json:objects()} |
          couchbeam_error().
save_docs(Server, DbName, Docs, Options) ->
    kz_couch_doc:save_docs(Server, DbName, Docs, Options).

-spec del_doc(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
del_doc(Server, DbName, Doc, Options) ->
    kz_couch_doc:del_doc(Server, DbName, Doc, Options).

-spec del_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) ->
          {'ok', kz_json:objects()} |
          couchbeam_error().
del_docs(Server, DbName, Docs, Options) ->
    kz_couch_doc:del_docs(Server, DbName, Docs, Options).

-spec ensure_saved(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
ensure_saved(Server, DbName, DocId, Options) ->
    kz_couch_doc:ensure_saved(Server, DbName, DocId, Options).

-spec copy_doc(kz_data:connection(), copy_doc(), kz_data:options()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
copy_doc(Server, CopySpec, Options) ->
    kz_couch_doc:copy_doc(Server, CopySpec, Options).

-spec move_doc(kz_data:connection(), copy_doc(), kz_data:options()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
move_doc(Server, CopySpec, Options) ->
    kz_couch_doc:move_doc(Server, CopySpec, Options).

%% Attachment-related
-spec fetch_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', binary()} |
          couchbeam_error().
fetch_attachment(Server, DbName, DocId, AName) ->
    kz_couch_attachments:fetch_attachment(Server, DbName, DocId, AName).

-spec stream_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) ->
          {'ok', doc()} |
          {'error', any()}.
stream_attachment(Server, DbName, DocId, AName, Caller) ->
    kz_couch_attachments:stream_attachment(Server, DbName, DocId, AName, Caller).

-spec put_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), iodata(), kz_data:options()) -> any().
put_attachment(Server, DbName, DocId, AName, Contents, Options) ->
    kz_couch_attachments:put_attachment(Server, DbName, DocId, AName, Contents, Options).

-spec delete_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
delete_attachment(Server, DbName, DocId, AName, Options) ->
    kz_couch_attachments:delete_attachment(Server, DbName, DocId, AName, Options).

-spec attachment_url(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
          kz_term:ne_binary() |
          {'proxy', tuple()}.
attachment_url(Server, DbName, DocId, AName, Options) ->
    kz_couch_attachments:attachment_url(Server, DbName, DocId, AName, Options).

%% View-related
-spec design_info(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
design_info(Server, DBName, Design) ->
    kz_couch_view:design_info(Server, DBName, Design).

-spec design_compact(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
design_compact(Server, DbName, Design) ->
    kz_couch_view:design_compact(Server, DbName, Design).

-spec all_design_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) ->
          {'ok', kz_json:objects()} |
          couchbeam_error().
all_design_docs(#server{}=Server, ?NE_BINARY = DBName, Options) ->
    kz_couch_view:all_design_docs(Server, DBName, Options).

-spec get_results(kz_data:connection(), 'all_docs' | kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
          {'ok', kz_json:objects() | kz_json:path()} |
          couchbeam_error().
get_results(Server, DbName, DesignDoc, ViewOptions) ->
    kz_couch_view:get_results(Server, DbName, DesignDoc, ViewOptions).

-spec get_results_count(kz_data:connection(), 'all_docs' | kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
          {'ok', kz_term:api_integer()} |
          couchbeam_error().
get_results_count(Server, DbName, DesignDoc, ViewOptions) ->
    kz_couch_view:get_results_count(Server, DbName, DesignDoc, ViewOptions).

-spec show(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          couchbeam_error().
show(Server, DbName, DesignDoc, DocId, Options) ->
    kz_couch_view:show(Server, DbName, DesignDoc, DocId, Options).

-spec all_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) ->
          {'ok', kz_json:objects()} |
          couchbeam_error().
all_docs(Server, DbName, Options) ->
    kz_couch_view:all_docs(Server, DbName, Options).

-spec server_version(server()) -> couch_version().
server_version(#server{options=Options}) ->
    props:get_value('driver_version', Options, 'bigcouch').

-spec db_local_filter(kz_term:ne_binaries(), kz_data:options()) -> kz_term:ne_binaries().
db_local_filter(List, Options) ->
    [DB || DB <- List,
           all_valid_options(Options, DB)
    ].

-spec all_valid_options(kz_data:options(), kz_term:ne_binary()) -> boolean().
all_valid_options(Options, DB) ->
    lists:all(fun(Option) ->
                      db_local_filter_option(Option, DB)
              end
             ,Options
             ).

-spec db_local_filter_option(kz_data:option(), kz_term:ne_binary()) -> boolean().
db_local_filter_option({'start_key', Value}, DB) ->
    DB >= Value;
db_local_filter_option({'startkey', Value}, DB) ->
    DB >= Value;
db_local_filter_option({'end_key', Value}, DB) ->
    DB =< Value;
db_local_filter_option({'endkey', Value}, DB) ->
    DB =< Value;
db_local_filter_option(_Option, _DB) -> 'true'.
