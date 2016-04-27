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
         ,attachment_url/6
        ]).


-include("kz_data.hrl").

-define(KEY_STUB_ATTACHMENTS, <<"pvt_attachments">>).


%% Attachment-related functions ------------------------------------------------
-spec fetch_attachment(map(), ne_binary(), ne_binary(), ne_binary()) ->
                              {'ok', binary()} |
                              data_error().
fetch_attachment(#{}=Server, DbName, DocId, AName) ->
    case kzs_cache:open_cache_doc(Server, DbName, DocId, []) of
        {'ok', Doc} ->
            case wh_doc:attachment(Doc, AName) of
                'undefined' -> {'error', 'not_found'};
                Att -> do_fetch_attachment(Server, DbName, DocId, AName, Att)
            end;
        {'error', _}=E -> E
    end.

do_fetch_attachment(#{server := {App, Conn}}, DbName, DocId, AName, Att) ->
    case wh_json:get_value(<<"handler">>, Att) of
        'undefined' -> App:fetch_attachment(Conn, DbName, DocId, AName);
        Handler -> do_fetch_attachment_from_handler(wh_json:to_proplist(Handler), DbName, DocId, AName)
    end.

do_fetch_attachment_from_handler([{Handler, Props}], DbName, DocId, AName) ->
    Module = wh_util:to_atom(Handler, 'true'),
    Module:fetch_attachment(Props, DbName, DocId, AName).

-spec stream_attachment(map(), ne_binary(), ne_binary(), ne_binary(), pid()) ->
                               {'ok', reference()} |
                               data_error().
stream_attachment(#{server := {App, Conn}}, DbName, DocId, AName, Caller) ->
    App:stream_attachment(Conn, DbName, DocId, AName, Caller).

-spec put_attachment(map(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            data_error().
put_attachment(#{att_handler := {Handler, Params}}=Map
              ,DbName, DocId, AName, Contents, Options) ->
    case Handler:put_attachment(Params, DbName, DocId, AName, Contents, Options) of
        {'ok', Props} ->
            CT = props:get_value('content_type', Options, kz_mime:from_filename(AName)),
            Size = size(Contents),
            Att = attachment_from_handler(AName, attachment_handler_jobj(Handler, Props), Size, CT),
            handle_put_attachment(Map, Att, DbName, DocId, AName, Contents, Options, Props);
        {'error', _} = E -> E
    end;
put_attachment(#{server := {App, Conn}}, DbName, DocId, AName, Contents, Options) ->
    kzs_cache:flush_cache_doc(DbName, DocId),
    App:put_attachment(Conn, DbName, DocId, AName, Contents, Options).


attachment_from_handler(AName, AttHandler, Size, CT) ->
    Props = [{<<"content_type">>, wh_util:to_binary(CT)}
            ,{<<"length">>, Size}
            ,{<<"stub">>, false}
            ,{<<"handler">>, AttHandler}
            ],
    wh_json:set_value(AName, wh_json:from_list(Props), wh_json:new()).

attachment_handler_jobj(Handler, Props) ->
    JObj = wh_json:from_list(props:get_value('attachment', Props, [])),
    wh_json:set_value(wh_util:to_binary(Handler), JObj, wh_json:new()).

-spec handle_put_attachment(map(), wh_json:object(), ne_binary(), ne_binary(), ne_binary(), ne_binary()
                           , wh_proplist(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            data_error().

handle_put_attachment(#{att_post_handler := 'stub'
                       ,server := {App, Conn}
                       }, _Att, DbName, DocId, AName, Contents, Options, _Props) ->
    kzs_cache:flush_cache_doc(DbName, DocId),
    App:put_attachment(Conn, DbName, DocId, AName, Contents, Options);

handle_put_attachment(#{att_post_handler := 'external'}, Att, DbName, DocId, _AName, _Contents, Options, Props) ->
    case kz_datamgr:open_cache_doc(DbName, DocId, Options) of
        {'ok', JObj} -> external_attachment(DbName, JObj, Att, Props);
        {'error', _}=E -> E
    end.

external_attachment(DbName, JObj, Att, Props) ->
    Atts = wh_json:merge_jobjs(Att, wh_json:get_value(?KEY_STUB_ATTACHMENTS, JObj, wh_json:new())),
    kz_datamgr:save_doc(DbName, wh_json:set_values([{?KEY_STUB_ATTACHMENTS, Atts}
                                                    | props:get_value('document', Props, [])
                                                   ], JObj)).

-spec delete_attachment(map(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               data_error().
delete_attachment(#{server := {App, Conn}}, DbName, DocId, AName, Options) ->
    kzs_cache:flush_cache_doc(DbName, DocId),
    App:delete_attachment(Conn, DbName, DocId, AName, Options).

attachment_url(#{att_proxy := 'true'}, DbName, DocId, AttachmentId, 'undefined', Options) ->
    {'proxy', {DbName, DocId, AttachmentId, Options}};
attachment_url(#{server := {App, Conn}}, DbName, DocId, AttachmentId, 'undefined', Options) ->
    App:attachment_url(Conn, DbName, DocId, AttachmentId, Options);
attachment_url(_, DbName, DocId, AttachmentId, Handler, Options) ->
    {'proxy', {DbName, DocId, AttachmentId, [{'handler', Handler} | Options]}}.
