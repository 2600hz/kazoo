%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% Util functions used by whistle_couch
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(kz_couch_attachments).

%% Attachment-related
-export([fetch_attachment/4
         ,stream_attachment/5
         ,put_attachment/5
         ,put_attachment/6
         ,delete_attachment/4
         ,delete_attachment/5
        ]).

-include("kz_couch.hrl").

%% Attachment-related functions ------------------------------------------------
-spec fetch_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                              {'ok', binary()} |
                              couchbeam_error().
fetch_attachment(#server{}=Conn, DbName, DocId, AName) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_fetch_attachment(Db, DocId, AName).

-spec stream_attachment(server(), ne_binary(), ne_binary(), ne_binary(), pid()) ->
                               {'ok', reference()} |
                               couchbeam_error().
stream_attachment(#server{}=Conn, DbName, DocId, AName, Caller) ->
    do_stream_attachment(kz_couch_util:get_db(Conn, DbName), DocId, AName, Caller).

-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error().
-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents) ->
    put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, []).

put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_put_attachment(Db, DocId, AName, Contents, kz_couch_util:maybe_add_rev(Db, DocId, Options)).

-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
delete_attachment(#server{}=Conn, DbName, DocId, AName) ->
    delete_attachment(#server{}=Conn, DbName, DocId, AName, []).

delete_attachment(#server{}=Conn, DbName, DocId, AName, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_del_attachment(Db, DocId, AName,  kz_couch_util:maybe_add_rev(Db, DocId, Options)).

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
    ?RETRY_504(couchbeam:put_attachment(Db, DocId, AName, Contents, Options)).

-spec do_del_attachment(couchbeam_db(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
do_del_attachment(#db{}=Db, DocId, AName, Options) ->
    Doc = wh_util:to_binary(http_uri:encode(wh_util:to_list(DocId))),
    ?RETRY_504(couchbeam:delete_attachment(Db, Doc, AName, Options)).
