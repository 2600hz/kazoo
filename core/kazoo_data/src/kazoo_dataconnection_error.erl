%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data connection with error
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kazoo_dataconnection_error).

-behaviour(kz_data).

-include_lib("kazoo_data/src/kz_data.hrl").

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

%% Server operations
new_connection(_Map) ->
    {'error', 'resource_not_available'}.
format_error(Error) ->
    Error.

%% Connection operations
get_db(_, _) ->
    {'error', 'resource_not_available'}.

server_url(_Server) ->
    {'error', 'resource_not_available'}.

db_url(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

server_info(_Server) ->
    {'error', 'resource_not_available'}.

%% DB operations
db_create(_Server, _DbName, _Options) ->
    {'error', 'resource_not_available'}.

db_delete(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

db_view_cleanup(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

db_info(_Server) ->
    {'error', 'resource_not_available'}.

db_info(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

db_exists(_Server, _DbName) ->
    {'error', 'resource_not_available'}.

db_archive(_Server, _DbName, _Filename) ->
    {'error', 'resource_not_available'}.

db_list(_Server, _Options) ->
    {'error', 'resource_not_available'}.

%% Document operations
open_doc(_Server, _DbName, _DocId, _Options) ->
    {'error', 'resource_not_available'}.

lookup_doc_rev(_Server, _DbName, _DocId) ->
    {'error', 'resource_not_available'}.

save_doc(_Server, _DbName, _DocId, _Options) ->
    {'error', 'resource_not_available'}.

save_docs(_Server, _DbName, _Docs, _Options) ->
    {'error', 'resource_not_available'}.

del_doc(_Server, _DbName, _Doc, _Options) ->
    {'error', 'resource_not_available'}.

del_docs(_Server, _DbName, _Docs, _Options) ->
    {'error', 'resource_not_available'}.

ensure_saved(_Server, _DbName, _DocId, _Options) ->
    {'error', 'resource_not_available'}.

%% Attachment-related
fetch_attachment(_Server, _DbName, _DocId, _AName) ->
    {'error', 'resource_not_available'}.

stream_attachment(_Server, _DbName, _DocId, _AName, _Caller) ->
    {'error', 'resource_not_available'}.

put_attachment(_Server, _DbName, _DocId, _AName, _Contents, _Options) ->
    {'error', 'resource_not_available'}.

delete_attachment(_Server, _DbName, _DocId, _AName, _Options) ->
    {'error', 'resource_not_available'}.

attachment_url(_Server, _DbName, _DocId, _AName, _Options) ->
    {'error', 'resource_not_available'}.

%% View-related
design_info(_Server, _DBName, _Design) ->
    {'error', 'resource_not_available'}.

all_design_docs(_Server, _DBName, _Options) ->
    {'error', 'resource_not_available'}.

get_results(_Server, _DbName, _DesignDoc, _ViewOptions) ->
    {'error', 'resource_not_available'}.

get_results_count(_Server, _DbName, _DesignDoc, _ViewOptions) ->
    {'error', 'resource_not_available'}.

all_docs(_Server, _DbName, _Options) ->
    {'error', 'resource_not_available'}.
