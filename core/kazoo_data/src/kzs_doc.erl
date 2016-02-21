%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_doc).

%% Doc related
-export([open_doc/4
         ,lookup_doc_rev/3
         ,save_doc/4
         ,save_docs/4
         ,del_doc/3
         ,del_docs/3
         ,ensure_saved/4
         ,copy_doc/3
         ,move_doc/3
        ]).


-include("kz_data.hrl").

-type copy_function() :: fun((server(), ne_binary(), wh_json:object(), wh_proplist()) ->
                              {'ok', wh_json:object()} | data_error()).
-export_type([copy_function/0]).
-define(COPY_DOC_OVERRIDE_PROPERTY, 'override_existing_document').


%% Document related functions --------------------------------------------------

-spec open_doc(server(), ne_binary(), ne_binary(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      data_error().
open_doc({App, Conn}, DbName, DocId, Options) ->
    App:open_doc(Conn, DbName, DocId, Options).

-spec save_doc(server(), ne_binary(), wh_json:object(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      data_error().
save_doc({App, Conn}, DbName, Doc, Options) ->
    App:save_doc(Conn, DbName, Doc, Options).

-spec save_docs(server(), ne_binary(), wh_json:objects(), wh_proplist()) ->
                       {'ok', wh_json:objects()} |
                       data_error().
save_docs({App, Conn}, DbName, Docs, Options) ->
    App:save_docs(Conn, DbName, Docs, Options).

-spec lookup_doc_rev(server(), ne_binary(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            data_error().
lookup_doc_rev({App, Conn}, DbName, DocId) ->
    App:lookup_doc_rev(Conn, DbName, DocId).

-spec ensure_saved(server(), ne_binary(), wh_json:object(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          data_error().
ensure_saved({App, Conn}, DbName, Doc, Opts) ->
    App:ensure_saved(Conn, DbName, Doc, Opts).

-spec del_doc(server(), ne_binary(), wh_json:object() | ne_binary()) ->
                     {'ok', wh_json:objects()} |
                     data_error().
del_doc({App, Conn}, DbName, Doc) ->
    App:del_doc(Conn, DbName, Doc).

-spec del_docs(server(), ne_binary(), wh_json:objects()) ->
                      {'ok', wh_json:objects()} |
                      data_error().
del_docs({App, Conn}, DbName, Doc) ->
    App:del_docs(Conn, DbName, Doc).


-spec copy_doc(server(), copy_doc(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      data_error().
copy_doc(Server, #copy_doc{source_dbname = SourceDb
                                      ,dest_dbname='undefined'
                                     }=CopySpec, Options) ->
    copy_doc(Server, CopySpec#copy_doc{dest_dbname=SourceDb
                                        ,dest_doc_id=wh_util:rand_hex_binary(16)
                                       }, Options);
copy_doc(Server, #copy_doc{dest_doc_id='undefined'}=CopySpec, Options) ->
    copy_doc(Server, CopySpec#copy_doc{dest_doc_id=wh_util:rand_hex_binary(16)}, Options);
copy_doc({App, Conn}, CopySpec, Options) ->
    App:copy_doc(Conn, CopySpec, Options).

-spec move_doc(server(), copy_doc(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      data_error().
move_doc({App, Conn}, CopySpec, Options) ->
    App:move_doc(Conn, CopySpec, Options).
