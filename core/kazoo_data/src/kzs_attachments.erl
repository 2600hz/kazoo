%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc data adapter behaviour
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_attachments).

%% Attachment-related
-export([fetch_attachment/4
        ,fetch_attachment/5
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
                              kz_att_error:error().
fetch_attachment(#{}=Server, DbName, DocId, AName) ->
    fetch_attachment(Server, DbName, DocId, AName, []).

-spec fetch_attachment(map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
                              {'ok', binary()} |
                              data_error() |
                              kz_att_error:error().
fetch_attachment(#{}=Server, DbName, DocId, AName, Options) ->
    case kzs_cache:open_cache_doc(Server, DbName, DocId, []) of
        {'ok', Doc} ->
            case kz_doc:attachment(Doc, AName) of
                'undefined' -> {'error', 'not_found'};
                Att -> do_fetch_attachment(Server, DbName, DocId, AName, Att, Options)
            end;
        {'error', _}=E -> E
    end.

do_fetch_attachment(#{server := {App, Conn}}=Server, DbName, DocId, AName, Att, Options) ->
    AttHandler = maps:get('att_handler', Server, 'undefined'),

    case kz_json:get_value(<<"handler">>, Att) of
        'undefined' ->
            App:fetch_attachment(Conn, DbName, DocId, AName);
        Handler ->
            HandlerPL = kz_json:to_proplist(Handler),
            case do_fetch_attachment_from_handler(HandlerPL, AttHandler, DbName, DocId, AName) of
                {'error', _Reason, _ExtendedError} = Error ->
                    handle_attachment_handler_error(Error, Options);
                Resp ->
                    Resp
            end
    end.

do_fetch_attachment_from_handler([{Handler, Props}], 'undefined', DbName, DocId, AName) ->
    Module = kz_term:to_atom(Handler, 'true'),
    Module:fetch_attachment(Props, DbName, DocId, AName);
do_fetch_attachment_from_handler([{Handler, HandlerProps}], {Module, ModuleProps}, DbName, DocId, AName) ->
    Props = kz_json:set_value(<<"handler_props">>, ModuleProps, HandlerProps),
    case kz_term:to_atom(Handler, 'true') of
        Module ->
            Module:fetch_attachment(Props, DbName, DocId, AName);
        DiffModule ->
            DiffModule:fetch_attachment(HandlerProps, DbName, DocId, AName)
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
    kz_process:spawn(fun relay_stream_attachment/7, [Caller, Ref, Module, Props, DbName, DocId, AName]),
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
    kz_process:spawn(fun relay_stream_attachment/7, [Caller, Ref, FinalModule, Props, DbName, DocId, AName]),
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
                            kz_att_error:error().

put_attachment(#{att_handler := {Handler, Params}}=Map
              ,DbName, DocId, AName, Contents, Options
              ) ->
    lager:info("using handler ~s to store ~s/~s/~s", [Handler, DbName, DocId, AName]),

    case Handler:put_attachment(Params, DbName, DocId, AName, Contents, Options) of
        {'ok', Props} ->
            CT = props:get_value('content_type', Options, kz_mime:from_filename(AName)),
            Size = size(Contents),
            Att = attachment_from_handler(AName, attachment_handler_jobj(Handler, Props), Size, CT),
            handle_put_attachment(Map, Att, DbName, DocId, AName, Contents, Options, Props);
        {'error', _Reason, _ExtendedError} = AttHandlerError ->
            case props:get_value('save_error', Options, 'true') of
                'true' ->
                    _Pid = kz_process:spawn(fun save_attachment_handler_error/3
                                           ,[Map, DbName, AttHandlerError]
                                           ),
                    lager:debug("saving attachment handler error in ~p", [_Pid]);
                'false' ->
                    lager:debug("skipping error save because save_error is set to false")
            end,
            handle_attachment_handler_error(AttHandlerError, Options)
    end;
put_attachment(Map, DbName, DocId, AName, Contents, Options) ->
    handle_put_attachment(Map, 'undefined', DbName, DocId, AName, Contents, Options, []).

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

-spec handle_put_attachment(att_map(), kz_term:api_object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
                           , kz_term:proplist(), kz_term:proplist()) ->
                                   {'ok', kz_json:object()} |
                                   {'ok', kz_json:object(), kz_term:proplist()} |
                                   data_error().

handle_put_attachment(#{att_post_handler := 'external'}=Map, Att, DbName, DocId, _AName, _Contents, _Options, Props) ->
    case kzs_doc:open_doc(Map, DbName, DocId, []) of
        {'ok', JObj} ->
            external_attachment(Map, DbName, JObj, Att, Props);
        {'error', _}=E -> E
    end;
handle_put_attachment(#{server := {App, Conn}}=Map, _Att, DbName, DocId, AName, Contents, Options, _Props) ->
    case App:put_attachment(Conn, DbName, DocId, AName, Contents, Options) of
        {'ok', _JObj}=Result ->
            publish_doc(Map, DbName, DocId),
            kzs_cache:flush_cache_doc(DbName, DocId),
            Result;
        Result -> Result
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
delete_attachment(#{server := {App, Conn}}=Map, DbName, DocId, AName, Options) ->
    case App:delete_attachment(Conn, DbName, DocId, AName, Options) of
        {'ok', _}=Result ->
            publish_doc(Map, DbName, DocId),
            kzs_cache:flush_cache_doc(DbName, DocId),
            Result;
        Result -> Result
    end.

-spec publish_doc(map(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_doc(Plan, DbName, DocId) ->
    case kzs_doc:open_doc(Plan, DbName, DocId, []) of
        {'ok', JObj} -> kzs_publish:maybe_publish_doc(DbName, JObj, JObj);
        {'error', Error} ->
            lager:debug("failed to publish doc update after attachment store for ~s/~s (~p)", [DbName, DocId, Error])
    end.

-spec attachment_url(map(), DbName, DocId, AttachmentId, Handler, Options) ->
                            kz_term:ne_binary() |
                            {'proxy', {DbName, DocId, AttachmentId, [{'handler', Handler}] | Options}}
                                when DbName :: kz_term:ne_binary()
                                     ,DocId :: kz_term:ne_binary()
                                     ,AttachmentId :: kz_term:ne_binary()
                                     ,Handler :: kz_term:api_atom()
                                     ,Options :: kz_term:proplist().
attachment_url(#{att_proxy := 'true'}, DbName, DocId, AttachmentId, 'undefined', Options) ->
    {'proxy', {DbName, DocId, AttachmentId, Options}};
attachment_url(#{server := {App, Conn}}, DbName, DocId, AttachmentId, 'undefined', Options) ->
    App:attachment_url(Conn, DbName, DocId, AttachmentId, Options);
attachment_url(_, DbName, DocId, AttachmentId, Handler, Options) ->
    {'proxy', {DbName, DocId, AttachmentId, [{'handler', Handler} | Options]}}.

-spec save_attachment_handler_error(att_map()
                                   ,kz_term:ne_binary()
                                   ,kz_att_error:error()
                                   ) -> 'ok'.
save_attachment_handler_error(Map
                             ,DbName
                             ,{'error', Reason, ExtendedError}
                             ) ->
    %% Workaround for `kzc_recording` and `kz_endpoint_recording`.
    AttUUID = maps:get('att_handler_id', Map, <<"overridden">>),
    NewValues = [{<<"_id">>, kazoo_modb_util:modb_id()}
                ,{<<"reason">>, Reason}
                ,{<<"handler_id">>, AttUUID}
                ,{<<"pvt_type">>, <<"attachment_handler_error">>}
                ],
    ErrorJSON = kz_json:set_values(NewValues, kz_att_error:to_json(ExtendedError)),
    UpdatedErrorJSON = kz_doc:update_pvt_parameters(ErrorJSON, DbName),
    {'ok', SavedJObj} = kazoo_modb:save_doc(DbName, UpdatedErrorJSON),
    lager:debug("attachment handler error stored with id: ~p", [kz_doc:id(SavedJObj)]).

-spec handle_attachment_handler_error(kz_att_error:error(), kz_data:options()) ->
                                             kz_att_error:error() | kz_datamgr:data_error().
handle_attachment_handler_error({'error', Reason, _ExtendedError}, []) ->
    {'error', Reason};
handle_attachment_handler_error({'error', Reason, ExtendedError}, Options) ->
    case props:get_value('error_verbosity', Options) of
        'undefined' ->
            {'error', Reason};
        'verbose' ->
            {'error', Reason, ExtendedError}
    end.
