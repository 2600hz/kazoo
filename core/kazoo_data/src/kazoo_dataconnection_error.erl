%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc data connection with error
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
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

-type error(Type) :: {'error', Type}.
-type error() :: error('resource_not_available').

%% Server operations
-spec new_connection(map()) -> error().
new_connection(_Map) ->
    {'error', 'resource_not_available'}.

-spec format_error(error(A)) -> error(A).
format_error(Error) ->
    Error.

%% Connection operations
-spec get_db(kz_data:connection(), kz_term:ne_binary()) -> error().
get_db(_, _) ->
    {'error', 'resource_not_available'}.

-spec server_url(kz_data:connection()) -> error().
server_url(_Server) ->
    {'error', 'resource_not_available'}.

-spec db_url(kz_data:connection(), kz_term:ne_binary()) -> error().
db_url(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

-spec server_info(kz_data:connection()) -> error().
server_info(_Server) ->
    {'error', 'resource_not_available'}.

%% DB operations
-spec db_create(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) -> error().
db_create(_Server, _DbName, _Options) ->
    {'error', 'resource_not_available'}.

-spec db_delete(kz_data:connection(), kz_term:ne_binary()) -> error().
db_delete(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

-spec db_view_cleanup(kz_data:connection(), kz_term:ne_binary()) -> error().
db_view_cleanup(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

-spec db_info(kz_data:connection()) -> error().
db_info(_Server) ->
    {'error', 'resource_not_available'}.

-spec db_info(kz_data:connection(), kz_term:ne_binary()) -> error().
db_info(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

-spec db_exists(kz_data:connection(), kz_term:ne_binary()) -> error().
db_exists(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

-spec db_archive(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> error().
db_archive(_Server, _DbName, _Filename) ->
    {'error', 'resource_not_available'}.

-spec db_list(kz_data:connection(), kz_data:options()) -> error().
db_list(_Server, _Options) ->
    {'error', 'resource_not_available'}.

%% Document operations
-spec open_doc(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> error().
open_doc(_Server, _DbName, _DocId, _Options) ->
    {'error', 'resource_not_available'}.

-spec lookup_doc_rev(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> error().
lookup_doc_rev(_Server, _DbName, _DocId) ->
    {'error', 'resource_not_available'}.

-spec save_doc(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> error().
save_doc(_Server, _DbName, _Doc, _Options) ->
    {'error', 'resource_not_available'}.

-spec save_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) -> error().
save_docs(_Server, _DbName, _Docs, _Options) ->
    {'error', 'resource_not_available'}.

-spec del_doc(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> error().
del_doc(_Server, _DbName, _Doc, _Options) ->
    {'error', 'resource_not_available'}.

-spec del_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) -> error().
del_docs(_Server, _DbName, _Docs, _Options) ->
    {'error', 'resource_not_available'}.

-spec ensure_saved(kz_data:connection(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> error().
ensure_saved(_Server, _DbName, _Doc, _Options) ->
    {'error', 'resource_not_available'}.

%% Attachment-related
-spec fetch_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> error().
fetch_attachment(_Server, _DbName, _DocId, _AName) ->
    {'error', 'resource_not_available'}.

-spec stream_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) -> error().
stream_attachment(_Server, _DbName, _DocId, _AName, _Caller) ->
    {'error', 'resource_not_available'}.

-spec put_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> error().
put_attachment(_Server, _DbName, _DocId, _AName, _Contents, _Options) ->
    {'error', 'resource_not_available'}.

-spec delete_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> error().
delete_attachment(_Server, _DbName, _DocId, _AName, _Options) ->
    {'error', 'resource_not_available'}.

-spec attachment_url(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> error().
attachment_url(_Server, _DbName, _DocId, _AName, _Options) ->
    {'error', 'resource_not_available'}.

%% View-related
-spec design_info(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> error().
design_info(_Server, _DBName, _Design) ->
    {'error', 'resource_not_available'}.

-spec all_design_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) -> error().
all_design_docs(_Server, _DBName, _Options) ->
    {'error', 'resource_not_available'}.

-spec get_results(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> error().
get_results(_Server, _DbName, _DesignDoc, _ViewOptions) ->
    {'error', 'resource_not_available'}.

-spec get_results_count(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> error().
get_results_count(_Server, _DbName, _DesignDoc, _ViewOptions) ->
    {'error', 'resource_not_available'}.

-spec all_docs(kz_data:connection(), kz_term:ne_binary(), kz_data:options()) -> error().
all_docs(_Server, _DbName, _Options) ->
    {'error', 'resource_not_available'}.
