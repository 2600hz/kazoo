-module(kazoo_couch).

-behaviour(application).
-behaviour(kz_data).

-include("kz_couch.hrl").

%% Application callbacks
-export([start/2, stop/1]).

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
         ,all_design_docs/3
         ,get_results/4
         ,get_results_count/4
         ,all_docs/3
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_couch_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.

%% Server operations
new_connection(Map) ->
    kz_couch_util:new_connection(Map).
format_error(Error) ->
    kz_couch_util:format_error(Error).

%% Connection operations
get_db(Server, DbName) ->
    kz_couch_util:get_db(Server, DbName).

server_url(Server) ->
    kz_couch_util:server_url(Server).

db_url(Server, DbName) ->
    kz_couch_util:db_url(Server, DbName).

server_info(Server) ->
    kz_couch_util:server_info(Server).

%% DB operations
db_create(Server, DbName, Options) ->
    kz_couch_db:db_create(Server, DbName, Options).

db_delete(Server, DbName) ->
    kz_couch_db:db_delete(Server, DbName).

db_view_cleanup(Server, DbName) ->
    kz_couch_db:db_view_cleanup(Server, DbName).

db_info(Server) ->
    kz_couch_db:db_info(Server).

db_info(Server, DbName) ->
    kz_couch_db:db_info(Server, DbName).

db_exists(Server, DbName) ->
    kz_couch_db:db_exists(Server, DbName).

db_archive(Server, DbName, Filename) ->
    kz_couch_db:db_archive(Server, DbName, Filename).

db_list(Server, Options) ->
    kz_couch_db:db_list(Server, Options).

%% Document operations
open_doc(Server, DbName, DocId, Options) ->
    kz_couch_doc:open_doc(Server, DbName, DocId, Options).

lookup_doc_rev(Server, DbName, DocId) ->
    kz_couch_doc:lookup_doc_rev(Server, DbName, DocId).

save_doc(Server, DbName, DocId, Options) ->
    kz_couch_doc:save_doc(Server, DbName, DocId, Options).

save_docs(Server, DbName, Docs, Options) ->
    kz_couch_doc:save_doc(Server, DbName, Docs, Options).

del_doc(Server, DbName, Doc, Options) ->
    kz_couch_doc:del_doc(Server, DbName, Doc, Options).

del_docs(Server, DbName, Docs, Options) ->
    kz_couch_doc:del_docs(Server, DbName, Docs, Options).

ensure_saved(Server, DbName, DocId, Options) ->
    kz_couch_doc:ensure_saved(Server, DbName, DocId, Options).

copy_doc(Server, CopySpec, Options) ->
    kz_couch_doc:copy_doc(Server, CopySpec, Options).

move_doc(Server, CopySpec, Options) ->
    kz_couch_doc:move_doc(Server, CopySpec, Options).

%% Attachment-related
fetch_attachment(Server, DbName, DocId, AName) ->
    kz_couch_attachments:fetch_attachment(Server, DbName, DocId, AName).

stream_attachment(Server, DbName, DocId, AName, Caller) ->
    kz_couch_attachments:stream_attachment(Server, DbName, DocId, AName, Caller).

put_attachment(Server, DbName, DocId, AName, Contents, Options) ->
    kz_couch_attachments:put_attachment(Server, DbName, DocId, AName, Contents, Options).

delete_attachment(Server, DbName, DocId, AName, Options) ->
    kz_couch_attachments:delete_attachment(Server, DbName, DocId, AName, Options).

attachment_url(Server, DbName, DocId, AName, Options) ->
    kz_couch_attachments:attachment_url(Server, DbName, DocId, AName, Options).

%% View-related
design_info(Server, DBName, Design) ->
    kz_couch_view:design_info(Server, DBName, Design).

all_design_docs(Server, DBName, Options) ->
    kz_couch_view:all_design_docs(Server, DBName, Options).

get_results(Server, DbName, DesignDoc, ViewOptions) ->
    kz_couch_view:get_results(Server, DbName, DesignDoc, ViewOptions).

get_results_count(Server, DbName, DesignDoc, ViewOptions) ->
    kz_couch_view:get_results_count(Server, DbName, DesignDoc, ViewOptions).

all_docs(Server, DbName, Options) ->
    kz_couch_view:all_docs(Server, DbName, Options).
