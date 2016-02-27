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
                            {'ok', wh_json:object()} |
                            couchbeam_error().
-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents) ->
    put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, []).

put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
%    do_put_attachment(Db, DocId, AName, Contents, kz_couch_util:maybe_add_rev(Db, DocId, Options)).
    do_put_attachment(Db, DocId, AName, Contents, Options).

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

-spec attachment_url(server(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> ne_binary().
attachment_url(#server{}=Conn, DbName, DocId, AName, Options) ->
    DocName = maybe_add_extension(AName, Options),
    list_to_binary([kz_couch_util:db_url(Conn, DbName)
                    ,"/", DocId
                    ,"/", DocName
                    , maybe_add_revision(Options)
                   ]).

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
    case couchbeam:fetch_attachment(Db, DocId, AName, [{stream, true}]) of
        {'ok', Ref}=Ret -> wh_util:spawn(fun relay_stream_attachment/2, [Caller, Ref]),
                           Ret;
        Else -> Else
    end.
                       
relay_stream_attachment(Caller, Ref) ->
    relay_stream_attachment(Caller, Ref, couchbeam:stream_attachment(Ref)).

relay_stream_attachment(Caller, Ref, {'error', _}=Msg) ->
    Caller ! {Ref, Msg};
relay_stream_attachment(Caller, Ref, Msg) ->
    Caller ! {Ref, Msg},
    relay_stream_attachment(Caller, Ref, couchbeam:stream_attachment(Ref)).

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

-spec maybe_add_extension(ne_binary(), wh_proplist()) -> ne_binary().
maybe_add_extension(AName, Options) ->
    case {props:get_value('content_type', Options), filename:extension(AName)} of
        {'undefined', _} -> AName;
        {CT, []} -> Ext = kz_mime:to_extension(CT),
                    <<AName/binary, ".", Ext/binary>>;
        _ -> AName
    end.

-spec maybe_add_revision(wh_proplist()) -> binary().
maybe_add_revision(Options) ->
    case props:get_value('revision', Options) of
        'undefined' -> <<>>;
        Rev -> <<"?rev=", Rev/binary>>
    end.

