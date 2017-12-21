%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
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
-spec fetch_attachment(map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', binary()} |
                              data_error() |
                              gen_attachment:error_response().
fetch_attachment(#{}=Server, DbName, DocId, AName) ->
    case kzs_cache:open_cache_doc(Server, DbName, DocId, []) of
        {'ok', Doc} ->
            case kz_doc:attachment(Doc, AName) of
                'undefined' -> {'error', 'not_found'};
                Att -> do_fetch_attachment(Server, DbName, DocId, AName, Att)
            end;
        {'error', _}=E -> E
    end.

do_fetch_attachment(#{server := {App, Conn}}=Server, DbName, DocId, AName, Att) ->
    AttHandler = maps:get('att_handler', Server, 'undefined'),
    case kz_json:get_value(<<"handler">>, Att) of
        'undefined' -> App:fetch_attachment(Conn, DbName, DocId, AName);
        Handler ->
            do_fetch_attachment_from_handler(kz_json:to_proplist(Handler), AttHandler, DbName, DocId, AName)
    end.

do_fetch_attachment_from_handler([{Handler, Props}], 'undefined', DbName, DocId, AName) ->
    Module = kz_term:to_atom(Handler, 'true'),
    Module:fetch_attachment(Props, DbName, DocId, AName);
do_fetch_attachment_from_handler([{Handler, HandlerProps}], {Module, ModuleProps}, DbName, DocId, AName) ->
    Props = kz_json:set_value(<<"handler_props">>, ModuleProps, HandlerProps),
    case kz_term:to_atom(Handler, 'true') of
        Module -> Module:fetch_attachment(Props, DbName, DocId, AName);
        DiffModule -> DiffModule:fetch_attachment(HandlerProps, DbName, DocId, AName)
    end.

-spec stream_attachment(map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) ->
                               {'ok', reference()} |
                               data_error().
stream_attachment(#{}=Server, DbName, DocId, AName, Caller) ->
    case kzs_cache:open_cache_doc(Server, DbName, DocId, []) of
        {'ok', Doc} ->
            case kz_doc:attachment(Doc, AName) of
                'undefined' -> {'error', 'not_found'};
                Att -> do_stream_attachment(Server, DbName, DocId, AName, Att, Caller)
            end;
        {'error', _}=E -> E
    end.

do_stream_attachment(#{server := {App, Conn}}=Server, DbName, DocId, AName, Att, Caller) ->
    AttHandler = maps:get('att_handler', Server, 'undefined'),
    case kz_json:get_value(<<"handler">>, Att) of
        'undefined' -> App:stream_attachment(Conn, DbName, DocId, AName, Caller);
        Handler -> do_stream_attachment_from_handler(kz_json:to_proplist(Handler), AttHandler, DbName, DocId, AName, Caller)
    end.

do_stream_attachment_from_handler([{Handler, Props}], 'undefined', DbName, DocId, AName, Caller) ->
    Module = kz_term:to_atom(Handler, 'true'),
    Ref = make_ref(),
    kz_util:spawn(fun relay_stream_attachment/7, [Caller, Ref, Module, Props, DbName, DocId, AName]),
    {'ok', Ref};
do_stream_attachment_from_handler([{Handler, HandlerProps}], {Module, ModuleProps}, DbName, DocId, AName, Caller) ->
    case kz_term:to_atom(Handler, 'true') of
        Module ->
            FinalModule = Module,
            Props = kz_json:set_value(<<"handler_props">>, ModuleProps, HandlerProps);
        DiffModule ->
            FinalModule = DiffModule,
            Props = HandlerProps
    end,
    Ref = make_ref(),
    kz_util:spawn(fun relay_stream_attachment/7, [Caller, Ref, FinalModule, Props, DbName, DocId, AName]),
    {'ok', Ref}.

relay_stream_attachment(Caller, Ref, Module, Props, DbName, DocId, AName) ->
    case Module:fetch_attachment(Props, DbName, DocId, AName) of
        {'ok', Bin} -> relay_stream_attachment(Caller, Ref, Bin);
        {'error', _} = Error -> Caller ! {Ref, Error}
    end.

-define(CHUNK_SIZE, 8192).

relay_stream_attachment(Caller, Ref, <<>>) ->
    Caller ! {Ref, 'done'};
relay_stream_attachment(Caller, Ref, <<Bin:?CHUNK_SIZE/binary, Rest/binary>>) ->
    Caller ! {Ref, {'ok', Bin}},
    relay_stream_attachment(Caller, Ref, Rest);
relay_stream_attachment(Caller, Ref, Bin) ->
    Caller ! {Ref, {'ok', Bin}},
    relay_stream_attachment(Caller, Ref, <<>>).


-type att_map() :: #{'att_handler':={_,_}
                    ,'att_post_handler':='external', _=>_
                    ,'server' := {module(), db()}
                    }.

-spec put_attachment(att_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                            {'ok', kz_json:object()} |
                            {'ok', kz_json:object(), kz_term:proplist()} |
                            data_error() |
                            gen_attachment:error_response().

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
    Props = [{<<"content_type">>, kz_term:to_binary(CT)}
            ,{<<"length">>, Size}
            ,{<<"stub">>, 'false'}
            ,{<<"handler">>, AttHandler}
            ],
    kz_json:set_value(AName, kz_json:from_list(Props), kz_json:new()).

attachment_handler_jobj(Handler, Props) ->
    JObj = kz_json:from_list(props:get_value('attachment', Props, [])),
    kz_json:set_value(kz_term:to_binary(Handler), JObj, kz_json:new()).

-spec handle_put_attachment(att_map(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
                           , kz_term:proplist(), kz_term:proplist()) ->
                                   {'ok', kz_json:object()} |
                                   {'ok', kz_json:object(), kz_term:proplist()} |
                                   data_error().

handle_put_attachment(#{att_post_handler := 'stub'
                       ,server := {App, Conn}
                       }, _Att, DbName, DocId, AName, Contents, Options, _Props) ->
    kzs_cache:flush_cache_doc(DbName, DocId),
    App:put_attachment(Conn, DbName, DocId, AName, Contents, Options);

handle_put_attachment(#{att_post_handler := 'external'}=Map, Att, DbName, DocId, _AName, _Contents, _Options, Props) ->
    case kzs_doc:open_doc(Map, DbName, DocId, []) of
        {'ok', JObj} -> external_attachment(Map, DbName, JObj, Att, Props);
        {'error', _}=E -> E
    end.

-spec external_attachment(att_map(), kz_term:ne_binary(), kz_json:object(), kz_json:object(), Props) ->
                                 {'ok', kz_json:object(), Props} |
                                 data_error().
external_attachment(Map, DbName, JObj, Att, Props) ->
    Atts = kz_json:merge_jobjs(Att, kz_json:get_value(?KEY_STUB_ATTACHMENTS, JObj, kz_json:new())),
    case kzs_doc:save_doc(Map, DbName, kz_json:set_values([{?KEY_STUB_ATTACHMENTS, Atts}], JObj), []) of
        {'ok', Doc} -> {'ok', Doc, Props};
        Error -> Error
    end.

-spec delete_attachment(map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                               {'ok', kz_json:object()} |
                               data_error().
delete_attachment(#{server := {App, Conn}}, DbName, DocId, AName, Options) ->
    kzs_cache:flush_cache_doc(DbName, DocId),
    App:delete_attachment(Conn, DbName, DocId, AName, Options).

-spec attachment_url(map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_atom(), kz_term:proplist()) ->
                            kz_term:ne_binary() |
                            {'proxy', tuple()}.
attachment_url(#{att_proxy := 'true'}, DbName, DocId, AttachmentId, 'undefined', Options) ->
    {'proxy', {DbName, DocId, AttachmentId, Options}};
attachment_url(#{server := {App, Conn}}, DbName, DocId, AttachmentId, 'undefined', Options) ->
    App:attachment_url(Conn, DbName, DocId, AttachmentId, Options);
attachment_url(_, DbName, DocId, AttachmentId, Handler, Options) ->
    {'proxy', {DbName, DocId, AttachmentId, [{'handler', Handler} | Options]}}.
