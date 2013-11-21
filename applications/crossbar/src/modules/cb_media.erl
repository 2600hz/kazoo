%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Account module
%%%
%%% Store/retrieve media files
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_media).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,content_types_provided/3
         ,content_types_accepted/3
         ,get/3
         ,put/1
         ,post/2, post/3
         ,delete/2, delete/3
        ]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).
-define(BIN_DATA, <<"raw">>).

-define(MEDIA_MIME_TYPES, [{<<"audio">>, <<"x-wav">>}
                           ,{<<"audio">>, <<"wav">>}
                           ,{<<"audio">>, <<"mpeg">>}
                           ,{<<"audio">>, <<"mp3">>}
                           ,{<<"audio">>, <<"ogg">>}
                           ,{<<"application">>, <<"base64">>}
                           ,{<<"application">>, <<"x-base64">>}
                          ]).

-define(CB_LIST, <<"media/crossbar_listing">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".media">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.content_types_provided.media">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.media">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.media">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.media">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.media">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.media">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.media">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.media">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.media">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_MediaID) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_MediaID, ?BIN_DATA) ->
    [?HTTP_GET, ?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?BIN_DATA) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(#cb_context{req_verb = ?HTTP_GET}=Context, MediaId, ?BIN_DATA) ->
    case load_media_meta(MediaId, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj, [])) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], JObj),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
            end
    end;
content_types_provided(Context, _, _) -> Context.

-spec content_types_accepted(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(#cb_context{req_verb = ?HTTP_POST}=Context, _MediaID, ?BIN_DATA) ->
    CTA = [{'from_binary', ?MEDIA_MIME_TYPES}],
    Context#cb_context{content_types_accepted=CTA};
content_types_accepted(Context, _, _) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_media_summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_request('undefined', Context).
validate(#cb_context{req_verb = ?HTTP_GET}=Context, MediaId) ->
    load_media_meta(MediaId, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, MediaId) ->
    validate_request(MediaId, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE, req_data=_Data}=Context, MediaID) ->
    load_media_meta(MediaID, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, MediaId, ?BIN_DATA) ->
    lager:debug("fetch media contents"),
    load_media_binary(MediaId, Context);
validate(#cb_context{req_verb = ?HTTP_POST, req_files=[]}=Context, _MediaID, ?BIN_DATA) ->
    Message = <<"please provide an media file">>,
    cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context);
validate(#cb_context{req_verb = ?HTTP_POST, req_files=[{_Filename, FileObj}]}=Context, MediaId, ?BIN_DATA) ->
    case load_media_meta(MediaId, Context) of
        #cb_context{resp_status='success', doc=JObj}=C ->
            CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj, <<"application/octet-stream">>),
            Size = wh_json:get_integer_value([<<"headers">>, <<"content_length">>]
                                             ,FileObj
                                             ,byte_size(wh_json:get_value(<<"contents">>, FileObj, <<>>))),

            Props = [{<<"content_type">>, CT}
                     ,{<<"content_length">>, Size}
                     ,{<<"media_source">>, <<"recording">>}
                    ],
            validate_request(MediaId, C#cb_context{req_data=wh_json:set_values(Props, JObj)});
        Else -> Else
    end;
validate(#cb_context{req_verb = ?HTTP_POST}=Context, _, ?BIN_DATA) ->
    Message = <<"please provide a single media file">>,
    cb_context:add_validation_error(<<"file">>, <<"maxItems">>, Message, Context).

-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
get(Context, _MediaID, ?BIN_DATA) ->
    Context#cb_context{resp_headers = [{<<"Content-Type">>
                                            ,wh_json:get_value(<<"content-type">>, Context#cb_context.doc, <<"application/octet-stream">>)}
                                       ,{<<"Content-Length">>
                                             ,wh_util:to_binary(binary:referenced_byte_size(Context#cb_context.resp_data))}
                                       | Context#cb_context.resp_headers]}.

-spec put(cb_context:context()) -> cb_context:context().
put(#cb_context{doc=JObj}=Context) ->
    Text = wh_json:get_value([<<"tts">>, <<"text">>], JObj),
    PrevText = wh_json:get_value(<<"pvt_previous_tts">>, JObj),
    TTS = not (wh_util:is_empty(Text) orelse Text =:= PrevText),
    Routines = [fun(C) -> crossbar_doc:save(C) end
                ,fun(#cb_context{resp_status='success', doc=J}=C) when TTS ->
                         Voice = wh_json:get_value([<<"tts">>, <<"voice">>], J, <<"female/en-US">>),
                         case whapps_speech:create(Text, Voice) of
                             {'error', R} ->
                                crossbar_doc:delete(C),
                                crossbar_util:response('error', wh_util:to_binary(R), C);
                             {'ok', ContentType, Content} ->
                                 MediaId = wh_json:get_value(<<"_id">>, J),
                                 Headers = wh_json:from_list([{<<"content_type">>, ContentType}
                                                              ,{<<"content_length">>, byte_size(Content)}
                                                             ]),
                                 FileJObj = wh_json:from_list([{<<"headers">>, Headers}
                                                               ,{<<"contents">>, Content}
                                                              ]),
                                 FileName = <<"text_to_speech_"
                                              ,(wh_util:to_binary(wh_util:current_tstamp()))/binary
                                              ,".wav">>,
                                 _ = update_media_binary(MediaId, C#cb_context{req_files=[{FileName, FileJObj}]
                                                                               ,resp_status='error'
                                                                              }),
                                 crossbar_doc:load(MediaId, C)
                         end;
                    (C) -> C
                 end
                ,fun(#cb_context{resp_status='success', doc=J1}=C) when TTS ->
                         J2 = wh_json:set_value(<<"media_source">>, <<"tts">>, J1),
                         crossbar_doc:save(C#cb_context{doc=wh_json:set_value(<<"pvt_previous_tts">>, Text, J2)});
                    (C) -> C
                 end
                ],
    lists:foldl(fun(F, C) -> F(C) end, Context, Routines).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().

post(#cb_context{doc=JObj}=Context, MediaId) ->
    Text = wh_json:get_value([<<"tts">>, <<"text">>], JObj),
    PrevText = wh_json:get_value(<<"pvt_previous_tts">>, JObj),
    TTS = not (wh_util:is_empty(Text) orelse Text =:= PrevText),
    Routines = [fun(#cb_context{resp_status='success', doc=J}=C) when TTS ->
                        Voice = wh_json:get_value([<<"tts">>, <<"voice">>], J, <<"female/en-US">>),
                        case whapps_speech:create(Text, Voice) of
                            {'error', R} -> crossbar_util:response('error', wh_util:to_binary(R), C);
                            {'ok', ContentType, Content} ->
                                Headers = wh_json:from_list([{<<"content_type">>, ContentType}
                                                             ,{<<"content_length">>, byte_size(Content)}
                                                            ]),
                                FileJObj = wh_json:from_list([{<<"headers">>, Headers}
                                                              ,{<<"contents">>, Content}
                                                              ]),
                                FileName = <<"text_to_speech_"
                                             ,(wh_util:to_binary(wh_util:current_tstamp()))/binary
                                             ,".wav">>,
                                _ = update_media_binary(MediaId, C#cb_context{req_files=[{FileName, FileJObj}]
                                                                              ,resp_status='error'
                                                                             }),
                                crossbar_doc:load_merge(MediaId, wh_json:public_fields(JObj), Context)
                         end;
                   (C) -> C
                end
                ,fun(#cb_context{resp_status='success', doc=J1}=C) when TTS ->
                         J2 = wh_json:set_value(<<"media_source">>, <<"tts">>, J1),
                         crossbar_doc:save(C#cb_context{doc=wh_json:set_value(<<"pvt_previous_tts">>, Text, J2)});
                    (#cb_context{resp_status='success'}=C) -> 
                         crossbar_doc:save(C);
                    (C) -> C
                 end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Context, Routines).

post(Context, MediaID, ?BIN_DATA) ->
    update_media_binary(MediaID, Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().

delete(Context, _MediaID) ->
    crossbar_doc:delete(Context).
delete(Context, MediaID, ?BIN_DATA) ->
    delete_media_binary(MediaID, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized list of media
%% @end
%%--------------------------------------------------------------------
-spec load_media_summary(cb_context:context()) -> cb_context:context().
load_media_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a media document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_media_meta(ne_binary(), cb_context:context()) -> cb_context:context().
load_media_meta(MediaId, Context) ->
    crossbar_doc:load(MediaId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(MediaId, Context) ->
    check_media_schema(MediaId, Context).

check_media_schema(MediaId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(MediaId, C) end,
    cb_context:validate_request_data(<<"media">>, Context, OnSuccess).

on_successful_validation('undefined', #cb_context{doc=Doc}=Context) ->
    Props = [{<<"pvt_type">>, <<"media">>}
             ,{<<"media_source">>, <<"upload">>}
            ],
    Context#cb_context{doc=wh_json:set_values(Props, Doc)};
on_successful_validation(MediaId, Context) -> 
    crossbar_doc:load_merge(MediaId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) ->
                                    wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
-spec load_media_binary(path_token(), cb_context:context()) -> cb_context:context().
load_media_binary(MediaId, Context) ->
    case load_media_meta(MediaId, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            MediaMeta = wh_json:get_value([<<"_attachments">>], JObj, []),

            case wh_json:get_keys(MediaMeta) of
                [] -> crossbar_util:response_bad_identifier(MediaId, Context);
                [Attachment|_] ->
                    lists:foldl(fun({K, V}, C) ->
                                        cb_context:add_resp_header(K, V, C)
                                end
                                ,crossbar_doc:load_attachment(JObj, Attachment, Context)
                                ,[{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
                                  ,{<<"Content-Type">>, wh_json:get_value([Attachment, <<"content_type">>], MediaMeta)}
                                  ,{<<"Content-Length">>, wh_json:get_value([Attachment, <<"length">>], MediaMeta)}
                                 ])
            end;
        Context1 -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
-spec update_media_binary(path_token(), cb_context:context()) ->
                                 cb_context:context().
update_media_binary(MediaID, #cb_context{doc=JObj
                                         ,req_files=[{Filename, FileObj}]
                                         ,db_name=Db
                                        }=Context) ->
    Contents = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    lager:debug("file content type: ~s", [CT]),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
    OldAttachments = wh_json:get_value(<<"_attachments">>, JObj, wh_json:new()),
    Id = wh_json:get_value(<<"_id">>, JObj),
    _ = [couch_mgr:delete_attachment(Db, Id, Attachment)
         || Attachment <- wh_json:get_keys(OldAttachments)
        ],
    crossbar_doc:save_attachment(MediaID, attachment_name(Filename, CT), Contents, Context, Opts).
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Delete the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
-spec delete_media_binary(path_token(), cb_context:context()) -> cb_context:context().
delete_media_binary(MediaID, Context) ->
    case crossbar_doc:load(MediaID, Context) of
        #cb_context{resp_status='success', doc=MediaMeta} ->
            case wh_json:get_value([<<"_attachments">>, 1], MediaMeta) of
                'undefined' -> crossbar_util:response_bad_identifier(MediaID, Context);
                AttachMeta ->
                    [AttachmentID] = wh_json:get_keys(AttachMeta),
                    crossbar_doc:delete_attachment(MediaID, AttachmentID, Context)
            end;
        Context1 -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%--------------------------------------------------------------------
-spec attachment_name(ne_binary(), ne_binary()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun(A) ->
                          case wh_util:is_empty(A) of
                              true -> wh_util:to_hex_binary(crypto:rand_bytes(16));
                              false -> A
                          end
                  end
                  ,fun(A) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               false -> A;
                               true ->
                                   <<A/binary, ".", (content_type_to_extension(CT))/binary>>
                           end
                   end
                 ],
    lists:foldr(fun(F, A) -> F(A) end, Filename, Generators).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert known media types to extensions
%% @end
%%--------------------------------------------------------------------
-spec content_type_to_extension(ne_binary()) -> ne_binary().
content_type_to_extension(<<"audio/wav">>) -> <<"wav">>;
content_type_to_extension(<<"audio/x-wav">>) -> <<"wav">>;
content_type_to_extension(<<"audio/mpeg">>) -> <<"mp3">>;
content_type_to_extension(<<"audio/mpeg3">>) -> <<"mp3">>;
content_type_to_extension(<<"audio/mp3">>) -> <<"mp3">>;
content_type_to_extension(<<"audio/ogg">>) -> <<"ogg">>.
