%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
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
-define(DEFAULT_VOICE, whapps_config:get(<<"speech">>, <<"tts_default_voice">>, <<"female/en-US">>)).

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
-spec content_types_provided(cb_context:context(), path_token(), path_token(), http_method()) ->
                                    cb_context:context().
content_types_provided(Context, MediaId, ?BIN_DATA) ->
    content_types_provided(Context, MediaId, ?BIN_DATA, cb_context:req_verb(Context)).

content_types_provided(Context, MediaId, ?BIN_DATA, ?HTTP_GET) ->
    Context1 = load_media_meta(MediaId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj, [])) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], JObj),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    cb_context:set_content_types_provided(Context, [{'to_binary', [{Type, SubType}]}])
            end
    end;
content_types_provided(Context, _MediaId, ?BIN_DATA, _Verb) ->
    Context.

-spec content_types_accepted(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_accepted_for_upload(cb_context:context(), http_method()) ->
                                               cb_context:context().
content_types_accepted(Context, _MediaId, ?BIN_DATA) ->
    content_types_accepted_for_upload(Context, cb_context:req_verb(Context)).

content_types_accepted_for_upload(Context, ?HTTP_POST) ->
    CTA = [{'from_binary', ?MEDIA_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted_for_upload(Context, _Verb) ->
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
validate(Context) ->
    validate_media_docs(Context, cb_context:req_verb(Context)).

validate(Context, MediaId) ->
    validate_media_doc(Context, MediaId, cb_context:req_verb(Context)).

validate(Context, MediaId, ?BIN_DATA) ->
    validate_media_binary(Context, MediaId, cb_context:req_verb(Context), cb_context:req_files(Context)).

validate_media_docs(Context, ?HTTP_GET) ->
    load_media_summary(Context);
validate_media_docs(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

validate_media_doc(Context, MediaId, ?HTTP_GET) ->
    load_media_meta(MediaId, Context);
validate_media_doc(Context, MediaId, ?HTTP_POST) ->
    validate_request(MediaId, Context);
validate_media_doc(Context, MediaId, ?HTTP_DELETE) ->
    load_media_meta(MediaId, Context).

validate_media_binary(Context, MediaId, ?HTTP_GET, _Files) ->
    lager:debug("fetch media contents"),
    load_media_binary(MediaId, Context);
validate_media_binary(Context, _MediaId, ?HTTP_POST, []) ->
    Message = <<"please provide an media file">>,
    cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context);
validate_media_binary(Context, MediaId, ?HTTP_POST, [{_Filename, FileObj}]) ->
    Context1 = load_media_meta(MediaId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj, <<"application/octet-stream">>),
            Size = wh_json:get_integer_value([<<"headers">>, <<"content_length">>]
                                             ,FileObj
                                             ,byte_size(wh_json:get_value(<<"contents">>, FileObj, <<>>))
                                            ),

            Props = [{<<"content_type">>, CT}
                     ,{<<"content_length">>, Size}
                     ,{<<"media_source">>, <<"recording">>}
                    ],
            validate_request(MediaId
                             ,cb_context:set_req_data(Context1
                                                      ,wh_json:set_values(Props, cb_context:doc(Context1))
                                                     )
                            );
        _Status -> Context1
    end;
validate_media_binary(Context, _MediaId, ?HTTP_POST, _Files) ->
    Message = <<"please provide a single media file">>,
    cb_context:add_validation_error(<<"file">>, <<"maxItems">>, Message, Context).

-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
get(Context, _MediaID, ?BIN_DATA) ->
    cb_context:add_resp_headers(Context
                                ,[{<<"Content-Type">>
                                   ,wh_json:get_value(<<"content-type">>, cb_context:doc(Context), <<"application/octet-stream">>)
                                  }
                                  ,{<<"Content-Length">>
                                    ,binary:referenced_byte_size(cb_context:resp_data(Context))
                                   }
                                 ]).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    JObj = cb_context:doc(Context),
    TTS = is_tts(JObj),

    Routines = [fun crossbar_doc:save/1
                ,fun(C) when TTS ->
                         Text = wh_json:get_value([<<"tts">>, <<"text">>], JObj),
                         Voice = wh_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),

                         maybe_update_tts(C, Text, Voice, cb_context:resp_status(C));
                    (C) -> C
                 end
                ,fun(C) when TTS ->
                         Text = wh_json:get_value([<<"tts">>, <<"text">>], JObj),
                         Voice = wh_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),

                         maybe_save_tts(C, Text, Voice, cb_context:resp_status(C));
                    (C) -> C
                 end
                ],
    lists:foldl(fun(F, C) -> F(C) end, Context, Routines).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().

post(Context, MediaId) ->
    JObj = cb_context:doc(Context),
    Text = wh_json:get_value([<<"tts">>, <<"text">>], JObj),
    Voice = wh_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),
    TTS = is_tts(JObj),

    Routines = [fun(C) when TTS ->
                        maybe_merge_tts(C, MediaId, Text, Voice, cb_context:resp_status(C));
                   (C) -> C
                end
                ,fun(C) when TTS ->
                         maybe_save_tts(C, Text, Voice, cb_context:resp_status(Context));
                    (C) ->
                         case cb_context:resp_status(C) of
                             'success' -> crossbar_doc:save(C);
                             _Status -> Context
                         end
                 end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Context, Routines).

post(Context, MediaID, ?BIN_DATA) ->
    update_media_binary(MediaID, Context).

-spec maybe_save_tts(cb_context:context(), ne_binary(), ne_binary(), crossbar_status()) ->
                            cb_context:context().
maybe_save_tts(Context, Text, Voice, 'success') ->
    JObj = wh_json:set_value(<<"media_source">>, <<"tts">>, cb_context:doc(Context)),
    crossbar_doc:save(
      cb_context:set_doc(Context
                         ,wh_json:set_values([{<<"pvt_previous_tts">>, Text}
                                              ,{<<"pvt_previous_voice">>, Voice}
                                             ], JObj)
                        )
     );
maybe_save_tts(Context, _Text, _Voice, _Status) ->
    Context.

-spec maybe_update_tts(cb_context:context(), ne_binary(), ne_binary(), crossbar_status()) ->
                        cb_context:context().
maybe_update_tts(Context, Text, Voice, 'success') ->
    JObj = cb_context:doc(Context),
    Voice = wh_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),
    case whapps_speech:create(Text, Voice) of
        {'error', R} ->
            crossbar_doc:delete(Context),
            crossbar_util:response('error', wh_util:to_binary(R), Context);
        {'ok', ContentType, Content} ->
            MediaId = wh_json:get_value(<<"_id">>, JObj),
            Headers = wh_json:from_list([{<<"content_type">>, ContentType}
                                         ,{<<"content_length">>, byte_size(Content)}
                                        ]),
            FileJObj = wh_json:from_list([{<<"headers">>, Headers}
                                          ,{<<"contents">>, Content}
                                         ]),
            FileName = <<"text_to_speech_"
                         ,(wh_util:to_binary(wh_util:current_tstamp()))/binary
                         ,".wav"
                       >>,
            _ = update_media_binary(MediaId
                                    ,cb_context:set_resp_status(
                                       cb_context:set_req_files(Context, [{FileName, FileJObj}])
                                       ,'error'
                                      )),
            crossbar_doc:load(MediaId, Context)
    end;
maybe_update_tts(Context, _Text, _Voice, _Status) -> Context.

-spec maybe_merge_tts(cb_context:context(), ne_binary(), ne_binary(), ne_binary(), crossbar_status()) ->
                             cb_context:context().
maybe_merge_tts(Context, MediaId, Text, Voice, 'success') ->
    JObj = cb_context:doc(Context),

    case whapps_speech:create(Text, Voice) of
        {'error', R} -> crossbar_util:response('error', wh_util:to_binary(R), Context);
        {'ok', ContentType, Content} ->
            Headers = wh_json:from_list([{<<"content_type">>, ContentType}
                                         ,{<<"content_length">>, byte_size(Content)}
                                        ]),
            FileJObj = wh_json:from_list([{<<"headers">>, Headers}
                                          ,{<<"contents">>, Content}
                                         ]),
            FileName = <<"text_to_speech_"
                         ,(wh_util:to_binary(wh_util:current_tstamp()))/binary
                         ,".wav"
                       >>,

            _ = update_media_binary(MediaId
                                    ,cb_context:set_resp_status(
                                       cb_context:set_req_files(Context
                                                                ,[{FileName, FileJObj}]
                                                               )
                                       ,'error'
                                      )),
            crossbar_doc:load_merge(MediaId, wh_json:public_fields(JObj), Context)
    end;
maybe_merge_tts(Context, _MediaId, _Text, _Voice, _Status) ->
    Context.

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

on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"media">>}
             ,{<<"media_source">>, <<"upload">>}
            ],
    cb_context:set_doc(Context, wh_json:set_values(Props, cb_context:doc(Context)));
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
    Context1 = load_media_meta(MediaId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            MediaMeta = wh_json:get_value([<<"_attachments">>], cb_context:doc(Context1), []),

            case wh_json:get_keys(MediaMeta) of
                [] -> crossbar_util:response_bad_identifier(MediaId, Context);
                [Attachment|_] ->
                    cb_context:add_resp_headers(
                      crossbar_doc:load_attachment(cb_context:doc(Context1), Attachment, Context1)
                      ,[{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
                        ,{<<"Content-Type">>, wh_json:get_value([Attachment, <<"content_type">>], MediaMeta)}
                        ,{<<"Content-Length">>, wh_json:get_value([Attachment, <<"length">>], MediaMeta)}
                       ])
            end;
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
-spec update_media_binary(path_token(), cb_context:context()) ->
                                 cb_context:context().
update_media_binary(MediaId, Context) ->
    [{Filename, FileObj}] = cb_context:req_files(Context),

    Contents = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    lager:debug("file content type: ~s", [CT]),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],

    Context1 = maybe_remove_old_attachments(Context),

    crossbar_doc:save_attachment(MediaId, cb_modules_util:attachment_name(Filename, CT)
                                 ,Contents, Context1, Opts
                                ).

maybe_remove_old_attachments(Context) ->
    MediaJObj = cb_context:doc(Context),
    maybe_remove_old_attachments(Context, MediaJObj
                                 ,wh_json:get_value(<<"_attachments">>, MediaJObj)
                                ).
maybe_remove_old_attachments(Context, _MediaJObj, 'undefined') ->
    Context;
maybe_remove_old_attachments(Context, MediaJObj, _Attachments) ->
    lager:debug("removing old attachments"),
    crossbar_doc:save(cb_context:set_doc(Context
                                         ,wh_json:delete_key(<<"_attachments">>, MediaJObj)
                                        )).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Delete the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
-spec delete_media_binary(path_token(), cb_context:context()) -> cb_context:context().
delete_media_binary(MediaID, Context) ->
    Context1 = crossbar_doc:load(MediaID, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            case wh_json:get_value([<<"_attachments">>, 1], cb_context:doc(Context1)) of
                'undefined' -> crossbar_util:response_bad_identifier(MediaID, Context);
                AttachMeta ->
                    [AttachmentID] = wh_json:get_keys(AttachMeta),
                    crossbar_doc:delete_attachment(MediaID, AttachmentID, Context)
            end;
        _Status -> Context1
    end.

-spec is_tts(wh_json:object()) -> boolean().
-spec is_tts(wh_json:object(), api_binary()) -> boolean().
is_tts(JObj) ->
    is_tts(JObj, wh_json:get_ne_value([<<"tts">>, <<"text">>], JObj)).

is_tts(_JObj, 'undefined') -> 'false';
is_tts(JObj, Text) ->
    is_tts_text_changed(JObj, Text =:= wh_json:get_value(<<"pvt_previous_tts">>, JObj)).

-spec is_tts_text_changed(wh_json:object(), boolean()) -> boolean().
is_tts_text_changed(JObj, 'true') ->
    Voice = wh_json:get_value([<<"tts">>, <<"voice">>], JObj),
    PrevVoice = wh_json:get_value(<<"pvt_previous_voice">>, JObj),
    not (Voice =:= PrevVoice);
is_tts_text_changed(_JObj, 'false') -> 'true'.
