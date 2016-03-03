%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_attachments).



%% Attachment-related
-export([fetch_attachment/4
         ,stream_attachment/5
         ,put_attachment/6
         ,delete_attachment/5
         ,attachment_url/5
        ]).


-include("kz_data.hrl").



%% Attachment-related functions ------------------------------------------------
-spec fetch_attachment(map(), ne_binary(), ne_binary(), ne_binary()) ->
                              {'ok', binary()} |
                              data_error().
fetch_attachment(#{server := {App, Conn}}, DbName, DocId, AName) ->
    %% OPEN CACHE DOC
    %% CHECK STUB
    %% MAYBE PROXY
    App:fetch_attachment(Conn, DbName, DocId, AName).

-spec stream_attachment(map(), ne_binary(), ne_binary(), ne_binary(), pid()) ->
                               {'ok', reference()} |
                               data_error().
stream_attachment(#{server := {App, Conn}}, DbName, DocId, AName, Caller) ->
    %% OPEN CACHE DOC
    %% CHECK STUB
    %% MAYBE PROXY
    App:stream_attachment(Conn, DbName, DocId, AName, Caller).

-spec put_attachment(map(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            data_error().
put_attachment(#{server := {App, Conn}
                ,att_handler := {Handler, Params}
                }, DbName, DocId, AName, Contents, Options) ->
    kzs_cache:flush_cache_doc(DbName, DocId),
    Handler:put_attachment(Params, DbName, DocId, AName, Contents, Options),
    App:put_attachment(Conn, DbName, DocId, AName, Contents, Options);
%%     kzs_cache:flush_cache_doc(DbName, DocId),
%%     App:put_attachment(Conn, DbName, DocId, AName, Contents, Options);
%%     case Handler:put_attachment(Params, DbName, DocId, AName, Contents, Options) of
%%         {'stub', Id} ->

put_attachment(#{server := {App, Conn}}, DbName, DocId, AName, Contents, Options) ->
    kzs_cache:flush_cache_doc(DbName, DocId),
    App:put_attachment(Conn, DbName, DocId, AName, Contents, Options).

-spec delete_attachment(map(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               data_error().
delete_attachment(#{server := {App, Conn}}, DbName, DocId, AName, Options) ->
    App:delete_attachment(Conn, DbName, DocId, AName, Options).

attachment_url(#{att_proxy := 'true'}, DbName, DocId, AttachmentId, Options) ->
    wh_media_url:store(DbName, DocId, AttachmentId, Options);
attachment_url(#{server := {App, Conn}}, DbName, DocId, AttachmentId, Options) ->
    App:attachment_url(Conn, DbName, DocId, AttachmentId, Options).
