%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
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
        ,authorize/1
        ,validate/1, validate/2, validate/3
        ,content_types_provided/3
        ,content_types_accepted/3
        ,languages_provided/1, languages_provided/2, languages_provided/3
        ,get/3
        ,put/1
        ,post/2, post/3
        ,delete/2, delete/3

        ,acceptable_content_types/0
        ]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).
-define(BIN_DATA, <<"raw">>).
-define(LANGUAGES, <<"languages">>).
-define(PROMPTS, <<"prompts">>).

-define(MEDIA_MIME_TYPES
       ,?AUDIO_CONTENT_TYPES
        ++ ?VIDEO_CONTENT_TYPES
        ++ ?BASE64_CONTENT_TYPES
       ).

-define(CB_LIST, <<"media/crossbar_listing">>).
-define(CB_LIST_BY_LANG, <<"media/listing_by_language">>).
-define(CB_LIST_BY_PROMPT, <<"media/listing_by_prompt">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".media">>).

-define(DEFAULT_VOICE, kapps_config:get(<<"speech">>, <<"tts_default_voice">>, <<"female/en-US">>)).
-define(NORMALIZATION_FORMAT, kapps_config:get(?MOD_CONFIG_CAT, <<"normalization_format">>, <<"mp3">>)).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    {'ok', _} = application:ensure_all_started('kazoo_media'),

    _ = crossbar_bindings:bind(<<"*.content_types_provided.media">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.media">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.media">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.media">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.languages_provided.media">>, ?MODULE, 'languages_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.media">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.media">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.media">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.media">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.media">>, ?MODULE, 'delete'),
    ok.

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

allowed_methods(?LANGUAGES) ->
    [?HTTP_GET];
allowed_methods(?PROMPTS) ->
    [?HTTP_GET];
allowed_methods(_MediaId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?LANGUAGES, _Language) ->
    [?HTTP_GET];
allowed_methods(?PROMPTS, _PromptId) ->
    [?HTTP_GET];
allowed_methods(_MediaId, ?BIN_DATA) ->
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
resource_exists(?LANGUAGES, _Language) -> 'true';
resource_exists(?PROMPTS, _PromptId) -> 'true';
resource_exists(_, ?BIN_DATA) -> 'true'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_media(Context, cb_context:req_nouns(Context), cb_context:account_id(Context)).

-spec authorize_media(cb_context:context(), req_nouns(), api_binary()) -> boolean().
authorize_media(_Context, [{<<"media">>, [?PROMPTS]}], 'undefined') ->
    lager:debug("allowing system prompts request"),
    'true';
authorize_media(_Context, [{<<"media">>, [?LANGUAGES]}], 'undefined') ->
    lager:debug("allowing system languages request"),
    'true';

authorize_media(_Context, [{<<"media">>, [?PROMPTS, _PromptId]}], 'undefined') ->
    lager:debug("allowing system prompt request for ~s", [_PromptId]),
    'true';
authorize_media(_Context, [{<<"media">>, [?LANGUAGES, _Language]}], 'undefined') ->
    lager:debug("allowing system language request for ~s", [_Language]),
    'true';

authorize_media(Context, [{<<"media">>, _}|_], 'undefined') ->
    IsAuthenticated = cb_context:is_authenticated(Context),
    IsSuperDuperAdmin = cb_context:is_superduper_admin(Context),
    IsReqVerbGet = cb_context:req_verb(Context) =:= ?HTTP_GET,
    case IsAuthenticated
        andalso (IsSuperDuperAdmin
                 orelse IsReqVerbGet
                )
    of
        'true' -> 'true';
        'false' -> {'halt', cb_context:add_system_error('forbidden', Context)}
    end;
authorize_media(Context, [{<<"media">>, _}, {<<"accounts">>, [AccountId]}], AccountId) ->
    cb_simple_authz:authorize(Context);
authorize_media(_Context, _Nouns, _AccountId) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec acceptable_content_types() -> kz_proplist().
acceptable_content_types() ->
    ?MEDIA_MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided_for_media(cb_context:context(), path_token(), path_token(), http_method()) ->
                                              cb_context:context().
content_types_provided(Context, MediaId, ?BIN_DATA) ->
    content_types_provided_for_media(Context, kz_http_util:urlencode(MediaId), ?BIN_DATA, cb_context:req_verb(Context)).

content_types_provided_for_media(Context, MediaId, ?BIN_DATA, ?HTTP_GET) ->
    Context1 = load_media_meta(Context, MediaId),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            case kz_doc:attachment_names(JObj) of
                [] -> Context1;
                [Attachment|_] ->
                    CT = kz_doc:attachment_content_type(JObj, Attachment),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    cb_context:set_content_types_provided(Context, [{'to_binary', [{Type, SubType}]}])
            end;
        _Status -> Context1
    end;
content_types_provided_for_media(Context, _MediaId, ?BIN_DATA, _Verb) ->
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
%% @public
%% @doc
%% If you provide alternative languages, return a list of languages and optional
%% quality value:
%% [<<"en">>, <<"en-gb;q=0.7">>, <<"da;q=0.5">>]
%% @end
%%--------------------------------------------------------------------
-spec languages_provided(cb_context:context()) -> cb_context:context().
-spec languages_provided(cb_context:context(), path_token()) -> cb_context:context().
-spec languages_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
languages_provided(Context) ->
    Context.
languages_provided(Context, _Id) ->
    Context.
languages_provided(Context, _Id, _Path) ->
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

validate(Context, ?LANGUAGES) ->
    load_available_languages(Context);
validate(Context, ?PROMPTS) ->
    load_available_prompts(Context);
validate(Context, MediaId) ->
    validate_media_doc(Context, kz_http_util:urlencode(MediaId), cb_context:req_verb(Context)).

validate(Context, ?LANGUAGES, Language) ->
    load_media_docs_by_language(Context, kz_util:to_lower_binary(Language));
validate(Context, ?PROMPTS, PromptId) ->
    load_media_docs_by_prompt(Context, PromptId);
validate(Context, MediaId, ?BIN_DATA) ->
    lager:debug("uploading binary data to '~s'", [MediaId]),
    validate_media_binary(Context, kz_http_util:urlencode(MediaId), cb_context:req_verb(Context), cb_context:req_files(Context)).

-spec validate_media_docs(cb_context:context(), http_method()) -> cb_context:context().
validate_media_docs(Context, ?HTTP_GET) ->
    load_media_summary(Context);
validate_media_docs(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

-spec validate_media_doc(cb_context:context(), ne_binary(), http_method()) -> cb_context:context().
validate_media_doc(Context, MediaId, ?HTTP_GET) ->
    load_media_meta(Context, MediaId);
validate_media_doc(Context, MediaId, ?HTTP_POST) ->
    validate_request(MediaId, Context);
validate_media_doc(Context, MediaId, ?HTTP_DELETE) ->
    load_media_meta(Context, MediaId).

-spec validate_media_binary(cb_context:context(), ne_binary(), http_method(), kz_proplist()) -> cb_context:context().
validate_media_binary(Context, MediaId, ?HTTP_GET, _Files) ->
    lager:debug("fetch media contents for '~s'", [MediaId]),
    load_media_binary(Context, MediaId);
validate_media_binary(Context, _MediaId, ?HTTP_POST, []) ->
    cb_context:add_validation_error(
      <<"file">>
                                   ,<<"required">>
                                   ,kz_json:from_list([
                                                       {<<"message">>, <<"Please provide an media file">>}
                                                      ])
                                   ,Context
     );
validate_media_binary(Context, MediaId, ?HTTP_POST, [{_Filename, FileObj}]) ->
    Context1 = load_media_meta(Context, MediaId),
    lager:debug("loaded media meta for '~s'", [MediaId]),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_normalize_upload(Context1, MediaId, FileObj);
        _Status -> Context1
    end;
validate_media_binary(Context, _MediaId, ?HTTP_POST, _Files) ->
    cb_context:add_validation_error(
      <<"file">>
                                   ,<<"maxItems">>
                                   ,kz_json:from_list([
                                                       {<<"message">>, <<"Please provide a single media file">>}
                                                      ])
                                   ,Context
     ).

-spec maybe_normalize_upload(cb_context:context(), ne_binary(), kz_json:object()) -> cb_context:context().
maybe_normalize_upload(Context, MediaId, FileJObj) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"normalize_media">>, 'false') of
        'true' ->
            lager:debug("normalizing uploaded media"),
            normalize_upload(Context, MediaId, FileJObj);
        'false' ->
            lager:debug("normalization not enabled, leaving upload as-is"),
            validate_upload(Context, MediaId, FileJObj)
    end.

-spec normalize_upload(cb_context:context(), ne_binary(), kz_json:object()) ->
                              cb_context:context().
-spec normalize_upload(cb_context:context(), ne_binary(), kz_json:object(), api_binary()) ->
                              cb_context:context().
normalize_upload(Context, MediaId, FileJObj) ->
    normalize_upload(Context, MediaId, FileJObj, kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj)).

normalize_upload(Context, MediaId, FileJObj, UploadContentType) ->
    FromExt = kz_mime:to_extension(UploadContentType),
    ToExt =  ?NORMALIZATION_FORMAT,

    lager:info("upload is of type '~s', normalizing from ~s to ~s"
              ,[UploadContentType, FromExt, ToExt]
              ),

    case kz_media_util:normalize_media(FromExt
                                      ,ToExt
                                      ,kz_json:get_value(<<"contents">>, FileJObj)
                                      )
    of
        {'ok', Contents} ->
            lager:debug("successfully converted to ~s", [ToExt]),
            {Major, Minor, _} = cow_mimetypes:all(<<"foo.", (ToExt)/binary>>),

            NewFileJObj = kz_json:set_values([{[<<"headers">>, <<"content_type">>], <<Major/binary, "/", Minor/binary>>}
                                             ,{[<<"headers">>, <<"content_length">>], iolist_size(Contents)}
                                             ,{<<"contents">>, Contents}
                                             ], FileJObj),

            validate_upload(
              cb_context:setters(Context
                                ,[{fun cb_context:set_req_files/2, [{<<"original_media">>, FileJObj}
                                                                   ,{<<"normalized_media">>, NewFileJObj}
                                                                   ]
                                  }
                                 ,{fun cb_context:set_doc/2, kz_json:delete_key(<<"normalization_error">>, cb_context:doc(Context))}
                                 ]
                                )
                           ,MediaId
                           ,NewFileJObj
             );
        {'error', _R} ->
            lager:warning("failed to convert to ~s: ~p", [ToExt, _R]),
            Reason = <<"failed to communicate with conversion utility">>,
            validate_upload(cb_context:set_doc(Context
                                              ,kz_json:set_value(<<"normalization_error">>, Reason, cb_context:doc(Context))
                                              )
                           ,MediaId
                           ,FileJObj
                           )
    end.

-spec validate_upload(cb_context:context(), ne_binary(), kz_json:object()) -> cb_context:context().
validate_upload(Context, MediaId, FileJObj) ->
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj, <<"application/octet-stream">>),
    Size = kz_json:get_integer_value([<<"headers">>, <<"content_length">>]
                                    ,FileJObj
                                    ,iolist_size(kz_json:get_value(<<"contents">>, FileJObj, <<>>))
                                    ),

    Props = [{<<"content_type">>, CT}
            ,{<<"content_length">>, Size}
            ,{<<"media_source">>, <<"upload">>}
            ],
    validate_request(MediaId
                    ,cb_context:set_req_data(Context
                                            ,kz_json:set_values(Props, cb_context:doc(Context))
                                            )
                    ).

-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
get(Context, _MediaId, ?BIN_DATA) ->
    cb_context:add_resp_headers(Context
                               ,[{<<"Content-Type">>
                                 ,kz_json:get_value(<<"content-type">>, cb_context:doc(Context), <<"application/octet-stream">>)
                                 }
                                ,{<<"Content-Length">>
                                 ,binary:referenced_byte_size(cb_context:resp_data(Context))
                                 }
                                ]).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    put_media(Context, cb_context:account_id(Context)).

-spec put_media(cb_context:context(), api_binary()) -> cb_context:context().
put_media(Context, 'undefined') ->
    put_media(cb_context:set_account_db(Context, ?KZ_MEDIA_DB), <<"ignore">>);
put_media(Context, _AccountId) ->
    JObj = cb_context:doc(Context),
    TTS = is_tts(JObj),

    Routines = [fun crossbar_doc:save/1
               ,fun(C) when TTS ->
                        Text = kz_json:get_value([<<"tts">>, <<"text">>], JObj),
                        Voice = kz_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),

                        maybe_update_tts(C, Text, Voice, cb_context:resp_status(C));
                   (C) -> C
                end
               ,fun(C) when TTS ->
                        Text = kz_json:get_value([<<"tts">>, <<"text">>], JObj),
                        Voice = kz_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),

                        maybe_save_tts(C, Text, Voice, cb_context:resp_status(C));
                   (C) -> C
                end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Context, Routines).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, MediaId) ->
    post_media_doc(Context, kz_http_util:urlencode(MediaId), cb_context:account_id(Context)).

-spec post_media_doc(cb_context:context(), ne_binary(), api_binary()) -> cb_context:context().
post_media_doc(Context, MediaId, 'undefined') ->
    post_media_doc(cb_context:set_account_db(Context, ?KZ_MEDIA_DB), MediaId, <<"ignore">>);
post_media_doc(Context, MediaId, _AccountId) ->
    JObj = cb_context:doc(Context),
    Text = kz_json:get_value([<<"tts">>, <<"text">>], JObj),
    Voice = kz_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),
    TTS = is_tts(JObj),

    Routines = [fun(C) when TTS ->
                        maybe_merge_tts(C, MediaId, Text, Voice, cb_context:resp_status(C));
                   (C) -> C
                end
               ,fun(C) when TTS ->
                        maybe_save_tts(C, Text, Voice, cb_context:resp_status(C));
                   (C) ->
                        case cb_context:resp_status(C) of
                            'success' -> crossbar_doc:save(remove_tts_keys(C));
                            _Status -> Context
                        end
                end
               ],
    lists:foldl(fun(F, C) -> F(C) end, Context, Routines).

post(Context, MediaId, ?BIN_DATA) ->
    post_media_binary(Context, kz_http_util:urlencode(MediaId), cb_context:account_id(Context)).

-spec remove_tts_keys(cb_context:context()) -> cb_context:context().
remove_tts_keys(C) ->
    JObj = cb_context:doc(C),
    cb_context:set_doc(C, kz_json:delete_keys([<<"pvt_previous_tts">>, <<"pvt_previous_voice">>], JObj)).

-spec post_media_binary(cb_context:context(), ne_binary(), api_binary()) -> cb_context:context().
post_media_binary(Context, MediaId, 'undefined') ->
    post_media_binary(cb_context:set_account_db(Context, ?KZ_MEDIA_DB), MediaId, <<"ignore">>);
post_media_binary(Context, MediaId, _AccountId) ->
    update_media_binary(Context, MediaId).

-spec maybe_save_tts(cb_context:context(), ne_binary(), ne_binary(), crossbar_status()) ->
                            cb_context:context().
maybe_save_tts(Context, Text, Voice, 'success') ->
    JObj = kz_json:set_value(<<"media_source">>, <<"tts">>, cb_context:doc(Context)),
    crossbar_doc:save(
      cb_context:set_doc(Context
                        ,kz_json:set_values([{<<"pvt_previous_tts">>, Text}
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
    Voice = kz_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),
    try kapps_speech:create(Text, Voice) of
        {'error', Reason} ->
            crossbar_doc:delete(Context),
            crossbar_util:response('error', kz_util:to_binary(Reason), Context);
        {'error', 'tts_provider_failure', Reason} ->
            crossbar_doc:delete(Context),
            crossbar_util:response('error', kz_util:to_binary(Reason), Context);
        {'ok', ContentType, Content} ->
            MediaId = kz_doc:id(JObj),
            Headers = kz_json:from_list([{<<"content_type">>, ContentType}
                                        ,{<<"content_length">>, iolist_size(Content)}
                                        ]),
            FileJObj = kz_json:from_list([{<<"headers">>, Headers}
                                         ,{<<"contents">>, Content}
                                         ]),
            FileName = <<"text_to_speech_"
                         ,(kz_util:to_binary(kz_util:current_tstamp()))/binary
                         ,".wav"
                       >>,
            _ = update_media_binary(cb_context:set_resp_status(cb_context:set_req_files(Context, [{FileName, FileJObj}])
                                                              ,'error'
                                                              )
                                   ,MediaId
                                   ),
            crossbar_doc:load(MediaId, Context, ?TYPE_CHECK_OPTION(kzd_media:type()))
    catch
        _E:_R ->
            lager:debug("creating tts excepted: ~s: ~p", [_E, _R]),
            crossbar_doc:delete(Context),
            crossbar_util:response('error', <<"creating TTS failed unexpectedly">>, Context)
    end;
maybe_update_tts(Context, _Text, _Voice, _Status) -> Context.

-spec maybe_merge_tts(cb_context:context(), ne_binary(), ne_binary(), ne_binary(), crossbar_status()) ->
                             cb_context:context().
maybe_merge_tts(Context, MediaId, Text, Voice, 'success') ->
    JObj = cb_context:doc(Context),

    case kapps_speech:create(Text, Voice) of
        {'error', R} ->
            crossbar_util:response('error', kz_util:to_binary(R), Context);
        {'error', 'tts_provider_failure', R} ->
            crossbar_util:response('error', kz_util:to_binary(R), Context);
        {'ok', ContentType, Content} ->
            Headers = kz_json:from_list([{<<"content_type">>, ContentType}
                                        ,{<<"content_length">>, iolist_size(Content)}
                                        ]),
            FileJObj = kz_json:from_list([{<<"headers">>, Headers}
                                         ,{<<"contents">>, Content}
                                         ]),
            FileName = <<"text_to_speech_"
                         ,(kz_util:to_binary(kz_util:current_tstamp()))/binary
                         ,".wav"
                       >>,

            _ = update_media_binary(cb_context:set_resp_status(cb_context:set_req_files(Context, [{FileName, FileJObj}])
                                                              ,'error'
                                                              )
                                   ,MediaId
                                   ),
            crossbar_doc:load_merge(MediaId, kz_json:public_fields(JObj), Context, ?TYPE_CHECK_OPTION(kzd_media:type()))
    end;
maybe_merge_tts(Context, _MediaId, _Text, _Voice, _Status) ->
    Context.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _MediaId) ->
    case kzd_media:is_prompt(cb_context:resp_data(Context)) of
        'true' -> crossbar_doc:delete(Context, 'permanent');
        'false' -> crossbar_doc:delete(Context)
    end.

delete(Context, MediaId, ?BIN_DATA) ->
    delete_media_binary(kz_http_util:urlencode(MediaId), Context, cb_context:account_id(Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized list of media
%% @end
%%--------------------------------------------------------------------
-spec load_media_summary(cb_context:context()) -> cb_context:context().
-spec load_media_summary(cb_context:context(), api_binary()) -> cb_context:context().
load_media_summary(Context) ->
    load_media_summary(Context, cb_context:account_id(Context)).

load_media_summary(Context, 'undefined') ->
    lager:debug("loading system_config media"),
    fix_start_keys(
      crossbar_doc:load_view(?CB_LIST
                            ,[{'startkey_fun', fun start_key/1}]
                            ,cb_context:set_account_db(Context, ?KZ_MEDIA_DB)
                            ,fun normalize_view_results/2
                            )
     );
load_media_summary(Context, _AccountId) ->
    fix_start_keys(
      crossbar_doc:load_view(?CB_LIST
                            ,[{'startkey_fun', fun start_key/1}]
                            ,Context
                            ,fun normalize_view_results/2
                            )
     ).

-spec start_key(cb_context:context()) -> crossbar_doc:startkey().
start_key(Context) ->
    case crossbar_doc:start_key(Context) of
        'undefined' -> 'undefined';
        StartKey -> kz_http_util:urlencode(StartKey)
    end.

-spec fix_start_keys(cb_context:context()) -> cb_context:context().
fix_start_keys(Context) ->
    cb_context:set_resp_envelope(Context
                                ,lists:foldl(fun fix_start_keys_fold/2
                                            ,cb_context:resp_envelope(Context)
                                            ,[<<"start_key">>, <<"next_start_key">>]
                                            )
                                ).

-spec fix_start_keys_fold(kz_json:path(), kz_json:object()) -> kz_json:object().
fix_start_keys_fold(Key, JObj) ->
    lager:debug("fix ~s: ~p", [Key, kz_json:get_value(Key, JObj)]),
    case kz_json:get_value(Key, JObj) of
        'undefined' -> JObj;
        <<_/binary>> -> JObj;
        [_Value] -> kz_json:delete_key(Key, JObj);
        ['null', Id] -> kz_json:set_value(Key, Id, JObj);
        [Lang, Id] -> kz_json:set_value(Key, kz_media_util:prompt_id(Id, Lang), JObj)
    end.

-spec load_available_languages(cb_context:context()) -> cb_context:context().
-spec load_available_languages(cb_context:context(), api_binary()) -> cb_context:context().
load_available_languages(Context) ->
    load_available_languages(Context, cb_context:account_id(Context)).

load_available_languages(Context, 'undefined') ->
    fix_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_LANG
                            ,[{'group_level', 1}]
                            ,cb_context:set_account_db(Context, ?KZ_MEDIA_DB)
                            ,fun normalize_count_results/2
                            )
     );
load_available_languages(Context, _AccountId) ->
    fix_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_LANG
                            ,[{'group_level', 1}]
                            ,Context
                            ,fun normalize_count_results/2
                            )
     ).

-spec normalize_count_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_count_results(JObj, []) ->
    normalize_count_results(JObj, [kz_json:new()]);
normalize_count_results(JObj, [Acc]) ->
    case kz_json:get_value(<<"key">>, JObj) of
        ['null'] ->
            [kz_json:set_value(<<"missing">>, kz_json:get_integer_value(<<"value">>, JObj), Acc)];
        [Lang] ->
            [kz_json:set_value(Lang, kz_json:get_integer_value(<<"value">>, JObj), Acc)]
    end.

-spec load_media_docs_by_language(cb_context:context(), ne_binary()) ->
                                         cb_context:context().
-spec load_media_docs_by_language(cb_context:context(), ne_binary() | 'null', api_binary()) ->
                                         cb_context:context().
load_media_docs_by_language(Context, <<"missing">>) ->
    lager:debug("loading media files missing a language"),
    load_media_docs_by_language(Context, 'null', cb_context:account_id(Context));
load_media_docs_by_language(Context, Language) ->
    lager:debug("loading media files in language ~p", [Language]),
    load_media_docs_by_language(Context, Language, cb_context:account_id(Context)).

load_media_docs_by_language(Context, Language, 'undefined') ->
    fix_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_LANG
                            ,[{'startkey_fun', fun(Ctx) -> language_start_key(Ctx, Language) end}
                             ,{'endkey', [Language, kz_json:new()]}
                             ,{'reduce', 'false'}
                             ,{'include_docs', 'false'}
                             ]
                            ,cb_context:set_account_db(Context, ?KZ_MEDIA_DB)
                            ,fun normalize_language_results/2
                            )
     );
load_media_docs_by_language(Context, Language, _AccountId) ->
    fix_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_LANG
                            ,[{'startkey_fun', fun(Ctx) -> language_start_key(Ctx, Language) end}
                             ,{'endkey', [Language, kz_json:new()]}
                             ,{'reduce', 'false'}
                             ,{'include_docs', 'false'}
                             ]
                            ,Context
                            ,fun normalize_language_results/2
                            )
     ).

-spec language_start_key(cb_context:context(), ne_binary()) -> ne_binaries().
-spec language_start_key(cb_context:context(), ne_binary(), ne_binaries()) -> ne_binaries().
language_start_key(Context, Language) ->
    case crossbar_doc:start_key(Context) of
        'undefined' -> [Language];
        Key -> language_start_key(Context, Language, binary:split(Key, <<"/">>))
    end.

language_start_key(_Context, Language, [Language, Id]) ->
    [Language, Id];
language_start_key(_Context, Language, _Key) ->
    [Language].

-spec normalize_language_results(kz_json:object(), ne_binaries()) -> ne_binaries().
normalize_language_results(JObj, Acc) ->
    [kz_doc:id(JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load prompt listing
%% @end
%%--------------------------------------------------------------------
-spec load_available_prompts(cb_context:context()) ->
                                    cb_context:context().
-spec load_available_prompts(cb_context:context(), api_binary()) ->
                                    cb_context:context().
load_available_prompts(Context) ->
    load_available_prompts(Context, cb_context:account_id(Context)).

load_available_prompts(Context, 'undefined') ->
    fix_prompt_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_PROMPT
                            ,[{'group_level', 1}
                             ,{'startkey_fun', fun prompt_start_key/1}
                             ]
                            ,cb_context:set_account_db(Context, ?KZ_MEDIA_DB)
                            ,fun normalize_count_results/2
                            )
     );
load_available_prompts(Context, _AccountId) ->
    fix_prompt_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_PROMPT
                            ,[{'group_level', 1}
                             ,{'startkey_fun', fun prompt_start_key/1}
                             ]
                            ,Context
                            ,fun normalize_count_results/2
                            )
     ).

-spec load_media_docs_by_prompt(cb_context:context(), ne_binary()) -> cb_context:context().
-spec load_media_docs_by_prompt(cb_context:context(), ne_binary(), api_binary()) -> cb_context:context().
load_media_docs_by_prompt(Context, PromptId) ->
    lager:debug("loading media files in prompt ~p", [PromptId]),
    load_media_docs_by_prompt(Context, PromptId, cb_context:account_id(Context)).

load_media_docs_by_prompt(Context, PromptId, 'undefined') ->
    fix_prompt_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_PROMPT
                            ,[{'startkey_fun', fun(Ctx) -> prompt_start_key(Ctx, PromptId) end}
                             ,{'endkey', [PromptId, kz_json:new()]}
                             ,{'reduce', 'false'}
                             ,{'include_docs', 'false'}
                             ]
                            ,cb_context:set_account_db(Context, ?KZ_MEDIA_DB)
                            ,fun normalize_prompt_results/2
                            )
     );
load_media_docs_by_prompt(Context, PromptId, _AccountId) ->
    fix_prompt_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_PROMPT
                            ,[{'startkey_fun', fun(Ctx) -> prompt_start_key(Ctx, PromptId) end}
                             ,{'endkey', [PromptId, kz_json:new()]}
                             ,{'reduce', 'false'}
                             ,{'include_docs', 'false'}
                             ]
                            ,Context
                            ,fun normalize_prompt_results/2
                            )
     ).


-spec prompt_start_key(cb_context:context()) ->
                              ne_binaries().
-spec prompt_start_key(cb_context:context(), api_binary()) ->
                              ne_binaries().
prompt_start_key(Context) ->
    prompt_start_key(Context, 'undefined').

prompt_start_key(Context, PromptId) ->
    case crossbar_doc:start_key(Context) of
        PromptId -> PromptId;
        'undefined' -> [PromptId];
        Key -> [Key]
    end.

-spec normalize_prompt_results(kz_json:object(), ne_binaries()) -> ne_binaries().
normalize_prompt_results(JObj, Acc) ->
    HasAttachments =
        case kz_doc:attachments(kz_json:get_value(<<"doc">>, JObj)) of
            'undefined' -> 'false';
            As -> not kz_json:is_empty(As)
        end,
    [kz_json:from_list(
       [{<<"id">>, kz_doc:id(JObj)}
       ,{<<"has_attachments">>, HasAttachments}
       ])
     | Acc
    ].

-spec fix_prompt_start_keys(cb_context:context()) -> cb_context:context().
fix_prompt_start_keys(Context) ->
    cb_context:set_resp_envelope(
      Context
                                ,lists:foldl(fun fix_prompt_start_keys_fold/2
                                            ,cb_context:resp_envelope(Context)
                                            ,[<<"start_key">>, <<"next_start_key">>]
                                            )
     ).

-spec fix_prompt_start_keys_fold(kz_json:path(), kz_json:object()) -> kz_json:object().
fix_prompt_start_keys_fold(Key, JObj) ->
    lager:debug("fix ~s: ~p", [Key, kz_json:get_value(Key, JObj)]),
    case kz_json:get_value(Key, JObj) of
        'undefined' -> JObj;
        <<_/binary>> -> JObj;
        [PromptId] -> kz_json:set_value(Key, PromptId, JObj);
        [PromptId, _Lang] ->
            lager:debug("removing ~s from start key ~s", [_Lang, PromptId]),
            kz_json:set_value(Key, PromptId, JObj)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a media document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_media_meta(cb_context:context(), ne_binary()) ->
                             cb_context:context().
-spec load_media_meta(cb_context:context(), ne_binary(), api_binary()) ->
                             cb_context:context().
load_media_meta(Context, MediaId) ->
    load_media_meta(Context, MediaId, cb_context:account_id(Context)).

load_media_meta(Context, MediaId, 'undefined') ->
    crossbar_doc:load(MediaId, cb_context:set_account_db(Context, ?KZ_MEDIA_DB), ?TYPE_CHECK_OPTION(kzd_media:type()));
load_media_meta(Context, MediaId, _AccountId) ->
    crossbar_doc:load(MediaId, Context, ?TYPE_CHECK_OPTION(kzd_media:type())).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(MediaId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(MediaId, C) end,
    cb_context:validate_request_data(<<"media">>, Context, OnSuccess).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, kzd_media:type()}
            ,{<<"media_source">>, <<"upload">>}
             | maybe_add_prompt_fields(Context)
            ],
    cb_context:set_doc(Context, kz_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(MediaId, Context) ->
    Context1 = crossbar_doc:load_merge(MediaId, Context, ?TYPE_CHECK_OPTION(kzd_media:type())),
    maybe_validate_prompt(MediaId, Context1, cb_context:resp_status(Context1)).

-spec maybe_validate_prompt(ne_binary(), cb_context:context(), crossbar_status()) ->
                                   cb_context:context().
maybe_validate_prompt(MediaId, Context, 'success') ->
    case kzd_media:prompt_id(cb_context:doc(Context)) of
        'undefined' -> Context;
        PromptId ->
            validate_prompt(MediaId, Context, PromptId)
    end;
maybe_validate_prompt(_MediaId, Context, _Status) ->
    Context.

-spec validate_prompt(ne_binary(), cb_context:context(), ne_binary()) ->
                             cb_context:context().
validate_prompt(MediaId, Context, PromptId) ->
    Language = kz_util:to_lower_binary(kzd_media:language(cb_context:doc(Context))),
    case kz_media_util:prompt_id(PromptId, Language) of
        MediaId -> Context;
        _OtherId ->
            lager:info("attempt to change prompt id '~s' is not allowed on existing media doc '~s'"
                      ,[PromptId, MediaId]
                      ),
            cb_context:add_validation_error(<<"prompt_id">>
                                           ,<<"invalid">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"Changing the prompt_id on an existing prompt is not allowed">>}
                                              ,{<<"cause">>, PromptId}
                                              ])
                                           ,Context
                                           )
    end.

-spec maybe_add_prompt_fields(cb_context:context()) -> kz_proplist().
maybe_add_prompt_fields(Context) ->
    JObj = cb_context:doc(Context),
    case kzd_media:prompt_id(JObj) of
        'undefined' -> [];
        PromptId ->
            Language = kz_util:to_lower_binary(kzd_media:language(JObj, kz_media_util:default_prompt_language())),
            ID = kz_media_util:prompt_id(PromptId, Language),

            lager:debug("creating properties for prompt ~s (~s)", [PromptId, Language]),

            [{<<"_id">>, ID}
            ,{<<"language">>, Language}
            ,{<<"name">>, kz_json:get_value(<<"name">>, JObj, ID)}
            ]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
-spec load_media_binary(cb_context:context(), path_token()) -> cb_context:context().
load_media_binary(Context, MediaId) ->
    Context1 = load_media_meta(Context, MediaId),
    case cb_context:resp_status(Context1) of
        'success' ->
            case kz_doc:attachment_names(cb_context:doc(Context1)) of
                [] -> crossbar_util:response_bad_identifier(MediaId, Context);
                [Attachment|_] ->
                    cb_context:add_resp_headers(
                      crossbar_doc:load_attachment(cb_context:doc(Context1)
                                                  ,Attachment
                                                  ,?TYPE_CHECK_OPTION(kzd_media:type())
                                                  ,Context1
                                                  )
                                               ,[{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
                                                ,{<<"Content-Type">>, kz_doc:attachment_content_type(cb_context:doc(Context1), Attachment)}
                                                ,{<<"Content-Length">>, kz_doc:attachment_length(cb_context:doc(Context1), Attachment)}
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
-spec update_media_binary(cb_context:context(), path_token()) ->
                                 cb_context:context().
-spec update_media_binary(cb_context:context(), path_token(), req_files()) ->
                                 cb_context:context().
update_media_binary(Context, MediaId) ->
    update_media_binary(crossbar_util:maybe_remove_attachments(Context)
                       ,MediaId
                       ,cb_context:req_files(Context)
                       ).

update_media_binary(Context, _MediaId, []) -> Context;
update_media_binary(Context, MediaId, [{Filename, FileObj}|Files]) ->
    Contents = kz_json:get_value(<<"contents">>, FileObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    lager:debug("file content type: ~s", [CT]),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(kzd_media:type())],

    update_media_binary(
      crossbar_doc:save_attachment(MediaId
                                  ,cb_modules_util:attachment_name(Filename, CT)
                                  ,Contents
                                  ,Context
                                  ,Opts
                                  )
                       ,MediaId
                       ,Files
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Delete the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
-spec delete_media_binary(path_token(), cb_context:context(), api_binary()) -> cb_context:context().
delete_media_binary(MediaId, Context, 'undefined') ->
    delete_media_binary(MediaId, cb_context:set_account_db(Context, ?KZ_MEDIA_DB), <<"ignore">>);
delete_media_binary(MediaId, Context, _AccountId) ->
    Context1 = crossbar_doc:load(MediaId, Context, ?TYPE_CHECK_OPTION(kzd_media:type())),
    case cb_context:resp_status(Context1) of
        'success' ->
            case kz_doc:attachment_names(cb_context:doc(Context1)) of
                [] -> crossbar_util:response_bad_identifier(MediaId, Context);
                [AttachmentId|_] ->
                    crossbar_doc:delete_attachment(MediaId, AttachmentId, Context)
            end;
        _Status -> Context1
    end.

-spec is_tts(kz_json:object()) -> boolean().
-spec is_tts(kz_json:object(), api_binary()) -> boolean().
is_tts(JObj) ->
    is_tts(JObj, kz_json:get_ne_value([<<"tts">>, <<"text">>], JObj)).

is_tts(_JObj, 'undefined') -> 'false';
is_tts(JObj, Text) ->
    is_tts_text_changed(JObj, Text =:= kz_json:get_value(<<"pvt_previous_tts">>, JObj)).

-spec is_tts_text_changed(kz_json:object(), boolean()) -> boolean().
is_tts_text_changed(JObj, 'true') ->
    Voice = kz_json:get_value([<<"tts">>, <<"voice">>], JObj),
    PrevVoice = kz_json:get_value(<<"pvt_previous_voice">>, JObj),
    not (Voice =:= PrevVoice);
is_tts_text_changed(_JObj, 'false') -> 'true'.
