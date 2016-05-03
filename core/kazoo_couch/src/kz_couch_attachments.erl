%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% Util functions used by kazoo_couch
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
         ,attachment_url/5
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
                            {'ok', kz_json:object()} |
                            couchbeam_error().
-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) ->
                            {'ok', kz_json:object()} |
                            couchbeam_error().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents) ->
    put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, []).

put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_put_attachment(Db, DocId, AName, Contents, Options).

-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', kz_json:object()} |
                               couchbeam_error().
-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) ->
                               {'ok', kz_json:object()} |
                               couchbeam_error().
delete_attachment(#server{}=Conn, DbName, DocId, AName) ->
    delete_attachment(#server{}=Conn, DbName, DocId, AName, []).

delete_attachment(#server{}=Conn, DbName, DocId, AName, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_del_attachment(Db, DocId, AName,  kz_couch_util:maybe_add_rev(Db, DocId, Options)).

-spec attachment_url(server(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) ->
          {'ok', ne_binary()} | {'proxy', tuple()}.
attachment_url(#server{}=Conn, DbName, DocId, AName, Options) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_bigcouch_direct">>, 'true') of
        'true' ->
            list_to_binary([kz_couch_util:db_url(Conn, DbName)
                           ,"/", kz_http_util:urlencode(DocId)
                           ,"/", kz_http_util:urlencode(AName)
                           , maybe_add_revision(Options)
                           ]);
        'false' ->
            {'proxy', {DbName, DocId, AName, Options}}
    end.

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
    case couchbeam:fetch_attachment(Db, DocId, AName, [{stream, true},{async,true}]) of
        {'ok', Ref}=Ret ->
            Msg = couchbeam:stream_attachment(Ref),
            St = get(Ref),
            kz_util:spawn(fun relay_stream_attachment/4, [Caller, Ref, Msg, St]),
            Ret;
        Else -> Else
    end.

relay_stream_attachment(Caller, Ref, Msg, St) ->
    put(Ref, St),
    relay_stream_attachment(Caller, Ref, Msg).

relay_stream_attachment(Caller, Ref, {'error', _}=Msg) ->
    Caller ! {Ref, Msg};
relay_stream_attachment(Caller, Ref, 'done'=Msg) ->
    Caller ! {Ref, Msg};
relay_stream_attachment(Caller, Ref, Msg) ->
    Caller ! {Ref, Msg},
    relay_stream_attachment(Caller, Ref, couchbeam:stream_attachment(Ref)).

-spec do_put_attachment(couchbeam_db(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) ->
                               {'ok', kz_json:object()} |
                               couchbeam_error().
do_put_attachment(#db{}=Db, DocId, AName, Contents, Options) ->
    ?RETRY_504(couchbeam:put_attachment(Db, DocId, AName, Contents, Options)).

-spec do_del_attachment(couchbeam_db(), ne_binary(), ne_binary(), kz_proplist()) ->
                               {'ok', kz_json:object()} |
                               couchbeam_error().
do_del_attachment(#db{}=Db, DocId, AName, Options) ->
    Doc = kz_util:to_binary(http_uri:encode(kz_util:to_list(DocId))),
    ?RETRY_504(couchbeam:delete_attachment(Db, Doc, AName, Options)).

-spec maybe_add_revision(kz_proplist()) -> binary().
maybe_add_revision(Options) ->
    case props:get_value('rev', Options) of
        'undefined' -> <<>>;
        Rev -> <<"?rev=", Rev/binary>>
    end.

