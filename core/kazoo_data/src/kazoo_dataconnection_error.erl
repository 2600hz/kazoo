%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% data connection with error
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kazoo_dataconnection_error).
-behaviour(kz_data).

-include("kz_data.hrl").

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

-type conn_error(Type) :: {'error', Type}.
-type conn_error() :: conn_error('resource_not_available').

%% Server operations
-spec new_connection(map()) -> conn_error().
new_connection(_Map) -> conn_error().

-spec format_error(conn_error(A)) -> conn_error(A).
format_error(Error) -> Error.

%% Connection operations
-spec get_db(kz_data:connection(), ne_binary()) -> conn_error().
get_db(_, _) -> conn_error().

-spec server_url(kz_data:connection()) -> conn_error().
server_url(_Server) -> conn_error().

-spec db_url(kz_data:connection(), ne_binary()) -> conn_error().
db_url(_Server, _DbName) -> conn_error().

-spec server_info(kz_data:connection()) -> conn_error().
server_info(_Server) -> conn_error().

%% DB operations
-spec db_create(kz_data:connection(), ne_binary(), kz_data:options()) -> conn_error().
db_create(_Server, _DbName, _Options) -> conn_error().

-spec db_delete(kz_data:connection(), ne_binary()) -> conn_error().
db_delete(_Server, _DbName) -> conn_error().

-spec db_view_cleanup(kz_data:connection(), ne_binary()) -> conn_error().
db_view_cleanup(_Server, _DbName) -> conn_error().

-spec db_info(kz_data:connection()) -> conn_error().
db_info(_Server) -> conn_error().

-spec db_info(kz_data:connection(), ne_binary()) -> conn_error().
db_info(_Server, _DbName) -> conn_error().

-spec db_exists(kz_data:connection(), ne_binary()) -> conn_error().
db_exists(_Server, _DbName) -> conn_error().

-spec db_archive(kz_data:connection(), ne_binary(), ne_binary()) -> conn_error().
db_archive(_Server, _DbName, _Filename) -> conn_error().

-spec db_list(kz_data:connection(), kz_data:options()) -> conn_error().
db_list(_Server, _Options) -> conn_error().

%% Document operations
-spec open_doc(kz_data:connection(), ne_binary(), ne_binary(), kz_data:options()) -> conn_error().
open_doc(_Server, _DbName, _DocId, _Options) -> conn_error().

-spec lookup_doc_rev(kz_data:connection(), ne_binary(), ne_binary()) -> conn_error().
lookup_doc_rev(_Server, _DbName, _DocId) -> conn_error().

-spec save_doc(kz_data:connection(), ne_binary(), kz_data:document(), kz_data:options()) -> conn_error().
save_doc(_Server, _DbName, _Doc, _Options) -> conn_error().

-spec save_docs(kz_data:connection(), ne_binary(), kz_data:documents(), kz_data:options()) -> conn_error().
save_docs(_Server, _DbName, _Docs, _Options) -> conn_error().

-spec del_doc(kz_data:connection(), ne_binary(), kz_data:document(), kz_data:options()) -> conn_error().
del_doc(_Server, _DbName, _Doc, _Options) -> conn_error().

-spec del_docs(kz_data:connection(), ne_binary(), kz_data:documents(), kz_data:options()) -> conn_error().
del_docs(_Server, _DbName, _Docs, _Options) -> conn_error().

-spec ensure_saved(kz_data:connection(), ne_binary(), kz_data:document(), kz_data:options()) -> conn_error().
ensure_saved(_Server, _DbName, _Doc, _Options) -> conn_error().

%% Attachment-related
-spec fetch_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary()) -> conn_error().
fetch_attachment(_Server, _DbName, _DocId, _AName) -> conn_error().

-spec stream_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), pid()) -> conn_error().
stream_attachment(_Server, _DbName, _DocId, _AName, _Caller) -> conn_error().

-spec put_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> conn_error().
put_attachment(_Server, _DbName, _DocId, _AName, _Contents, _Options) -> conn_error().

-spec delete_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> conn_error().
delete_attachment(_Server, _DbName, _DocId, _AName, _Options) -> conn_error().

-spec attachment_url(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> conn_error().
attachment_url(_Server, _DbName, _DocId, _AName, _Options) -> conn_error().

%% View-related
-spec design_info(kz_data:connection(), ne_binary(), ne_binary()) -> conn_error().
design_info(_Server, _DBName, _Design) -> conn_error().

-spec all_design_docs(kz_data:connection(), ne_binary(), kz_data:options()) -> conn_error().
all_design_docs(_Server, _DBName, _Options) -> conn_error().

-spec get_results(kz_data:connection(), ne_binary(), ne_binary(), kz_data:options()) -> conn_error().
get_results(_Server, _DbName, _DesignDoc, _ViewOptions) -> conn_error().

-spec get_results_count(kz_data:connection(), ne_binary(), ne_binary(), kz_data:options()) -> conn_error().
get_results_count(_Server, _DbName, _DesignDoc, _ViewOptions) -> conn_error().

-spec all_docs(kz_data:connection(), ne_binary(), kz_data:options()) -> conn_error().
all_docs(_Server, _DbName, _Options) -> conn_error().


-spec conn_error() -> conn_error().
conn_error() -> {error, resource_not_available}.
