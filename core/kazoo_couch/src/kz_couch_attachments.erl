%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Util functions used by kazoo_couch
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
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
-spec fetch_attachment(server(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', binary()} |
                              couchbeam_error().
fetch_attachment(#server{}=Conn, DbName, DocId, AName) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_fetch_attachment(Db, DocId, AName).

-spec stream_attachment(server(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) ->
                               {'ok', reference()} |
                               couchbeam_error().
stream_attachment(#server{}=Conn, DbName, DocId, AName, Caller) ->
    do_stream_attachment(kz_couch_util:get_db(Conn, DbName), DocId, AName, Caller).

-spec put_attachment(server(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {'ok', kz_json:object()} |
                            couchbeam_error().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents) ->
    put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, []).

-spec put_attachment(server(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                            {'ok', kz_json:object()} |
                            couchbeam_error().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_put_attachment(Db, DocId, AName, Contents, Options).

-spec delete_attachment(server(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                               {'ok', kz_json:object()} |
                               couchbeam_error().
delete_attachment(#server{}=Conn, DbName, DocId, AName) ->
    delete_attachment(#server{}=Conn, DbName, DocId, AName, []).

-spec delete_attachment(server(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                               {'ok', kz_json:object()} |
                               couchbeam_error().
delete_attachment(#server{}=Conn, DbName, DocId, AName, Options) ->
    Db = kz_couch_util:get_db(Conn, DbName),
    do_del_attachment(Db, DocId, AName,  kz_couch_util:maybe_add_rev(Db, DocId, Options)).

-spec attachment_url(server(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                            kz_term:ne_binary() |
                            {'proxy', tuple()}.
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
-spec do_fetch_attachment(couchbeam_db(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                 {'ok', binary()} |
                                 couchbeam_error().
do_fetch_attachment(#db{}=Db, DocId, AName) ->
    ?RETRY_504(couchbeam:fetch_attachment(Db, DocId, AName)).

-spec do_stream_attachment(couchbeam_db(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) ->
                                  {'ok', reference() | atom()} |
                                  couchbeam_error().
do_stream_attachment(#db{}=Db, DocId, AName, Caller) ->
    case couchbeam:fetch_attachment(Db, DocId, AName, [{'stream', 'true'}
                                                      ,{'async', 'true'}
                                                      ]
                                   )
    of
        {'ok', Ref}=Ret ->
            Msg = couchbeam:stream_attachment(Ref),
            St = get(Ref),
            kz_process:spawn(fun relay_stream_attachment/4, [Caller, Ref, Msg, St]),
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

-spec do_put_attachment(couchbeam_db(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                               {'ok', kz_json:object()} |
                               couchbeam_error().
do_put_attachment(#db{}=Db, DocId, AttName, Contents, Options0) ->
    %% At the time of this change, couchbeam is striping "/" from attachment name when put_attachment only.
    %% Fetch and delete are encoding properly
    AName = kz_http_util:urlencode(AttName),
    Options = case props:get_value('content_type', Options0) of
                  'undefined' -> [{'content_type', kz_mime:from_filename(AName)} | Options0];
                  _CT -> Options0
              end,
    ?RETRY_504(couchbeam:put_attachment(Db, DocId, AName, Contents, Options)).

-spec do_del_attachment(couchbeam_db(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                               {'ok', kz_json:object()} |
                               couchbeam_error().
do_del_attachment(#db{}=Db, DocId, AName, Options) ->
    Doc = kz_term:to_binary(http_uri:encode(kz_term:to_list(DocId))),
    ?RETRY_504(couchbeam:delete_attachment(Db, Doc, AName, Options)).

-spec maybe_add_revision(kz_term:proplist()) -> binary().
maybe_add_revision(Options) ->
    case props:get_value('rev', Options) of
        'undefined' -> <<>>;
        Rev -> <<"?rev=", Rev/binary>>
    end.
