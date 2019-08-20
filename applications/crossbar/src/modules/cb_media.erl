%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Account module
%%% Store/retrieve media files
%%%
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_media).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,authorize/1
        ,validate/1, validate/2, validate/3
        ,content_types_provided/2, content_types_provided/3
        ,content_types_accepted/2, content_types_accepted/3
        ,languages_provided/1, languages_provided/2, languages_provided/3
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

-define(DEFAULT_VOICE
       ,list_to_binary([kazoo_tts:default_voice(), $/, kazoo_tts:default_language()])
       ).

-define(NORMALIZATION_FORMAT
       ,kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"normalization_format">>, <<"mp3">>)
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    {'ok', _} = application:ensure_all_started('kazoo_media'),

    _ = crossbar_bindings:bind(<<"*.content_types_provided.media">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.media">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.media">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.media">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.languages_provided.media">>, ?MODULE, 'languages_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.media">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.media">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.media">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.media">>, ?MODULE, 'delete'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?LANGUAGES) ->
    [?HTTP_GET];
allowed_methods(?PROMPTS) ->
    [?HTTP_GET];
allowed_methods(_MediaId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?LANGUAGES, _Language) ->
    [?HTTP_GET];
allowed_methods(?PROMPTS, _PromptId) ->
    [?HTTP_GET];
allowed_methods(_MediaId, ?BIN_DATA) ->
    [?HTTP_GET, ?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?LANGUAGES, _Language) -> 'true';
resource_exists(?PROMPTS, _PromptId) -> 'true';
resource_exists(_, ?BIN_DATA) -> 'true'.

-spec authorize(cb_context:context()) -> boolean() |
                                         {'stop', cb_context:context()}.
authorize(Context) ->
    authorize_media(Context, cb_context:req_nouns(Context), cb_context:account_id(Context)).

-spec authorize_media(cb_context:context(), req_nouns(), kz_term:api_binary()) -> boolean().
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
        'false' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end;
authorize_media(Context, [{<<"media">>, _}, {<<"accounts">>, [AccountId]}], AccountId) ->
    cb_simple_authz:authorize(Context);
authorize_media(_Context, _Nouns, _AccountId) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc Add content types accepted and provided by this module
%% @end
%%------------------------------------------------------------------------------
-spec acceptable_content_types() -> [cowboy_content_type()].
acceptable_content_types() ->
    ?MEDIA_MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, MediaId) ->
    Verb = cb_context:req_verb(Context),
    ContentType = cb_context:req_header(Context, <<"accept">>),
    case ?HTTP_GET =:= Verb
        andalso api_util:content_type_matches(ContentType, acceptable_content_types())
    of
        'false' -> Context;
        'true' ->
            content_types_provided_for_media(Context, MediaId, ?BIN_DATA, ?HTTP_GET)
    end.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, MediaId, ?BIN_DATA) ->
    content_types_provided_for_media(Context, MediaId, ?BIN_DATA, cb_context:req_verb(Context)).

-spec content_types_provided_for_media(cb_context:context(), path_token(), path_token(), http_method()) ->
                                              cb_context:context().
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

-spec content_types_accepted(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
content_types_accepted(Context, _MediaId) ->
    Verb = cb_context:req_verb(Context),
    ContentType = cb_context:req_header(Context, <<"content-type">>),
    case ?HTTP_POST =:= Verb
        andalso api_util:content_type_matches(ContentType, acceptable_content_types())
    of
        'false' -> Context;
        'true' ->
            CTA = [{'from_binary', acceptable_content_types()}],
            cb_context:set_content_types_accepted(Context, CTA)
    end.

-spec content_types_accepted(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(Context, _MediaId, ?BIN_DATA) ->
    content_types_accepted_for_upload(Context, cb_context:req_verb(Context)).

-spec content_types_accepted_for_upload(cb_context:context(), http_method()) ->
                                               cb_context:context().
content_types_accepted_for_upload(Context, ?HTTP_POST) ->
    CTA = [{'from_binary', acceptable_content_types()}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted_for_upload(Context, _Verb) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If you provide alternative languages, return a list of languages and optional
%% quality value:
%% `[<<"en">>, <<"en-gb;q=0.7">>, <<"da;q=0.5">>]'
%% @end
%%------------------------------------------------------------------------------

-spec languages_provided(cb_context:context()) -> cb_context:context().
languages_provided(Context) ->
    Context.

-spec languages_provided(cb_context:context(), path_token()) -> cb_context:context().
languages_provided(Context, _Id) ->
    Context.

-spec languages_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
languages_provided(Context, _Id, _Path) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request.
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_media_docs(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?LANGUAGES) ->
    load_available_languages(Context);
validate(Context, ?PROMPTS) ->
    load_available_prompts(Context);
validate(Context, MediaId) ->
    validate_media_doc(Context, MediaId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?LANGUAGES, Language) ->
    load_media_docs_by_language(Context, kz_term:to_lower_binary(Language));
validate(Context, ?PROMPTS, PromptId) ->
    load_media_docs_by_prompt(Context, PromptId);
validate(Context, MediaId, ?BIN_DATA) ->
    lager:debug("uploading binary data to '~s'", [MediaId]),
    validate_media_binary(Context, MediaId, cb_context:req_verb(Context), cb_context:req_files(Context)).

-spec validate_media_docs(cb_context:context(), http_method()) -> cb_context:context().
validate_media_docs(Context, ?HTTP_GET) ->
    load_media_summary(Context);
validate_media_docs(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

-spec validate_media_doc(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
validate_media_doc(Context, MediaId, ?HTTP_GET) ->
    case api_util:content_type_matches(cb_context:req_header(Context, <<"accept">>)
                                      ,acceptable_content_types()
                                      )
    of
        'false' -> load_media_meta(Context, MediaId);
        'true' -> validate_media_binary(Context, MediaId, ?HTTP_GET, [])
    end;
validate_media_doc(Context, MediaId, ?HTTP_POST) ->
    validate_media_doc_update(Context, MediaId, cb_context:req_header(Context, <<"content-type">>));
validate_media_doc(Context, MediaId, ?HTTP_DELETE) ->
    load_media_meta(Context, MediaId).

-spec validate_media_doc_update(cb_context:context(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> cb_context:context().
validate_media_doc_update(Context, MediaId, ContentType) ->
    lager:debug("trying to update doc with content ~s", [ContentType]),
    case api_util:content_type_matches(ContentType, acceptable_content_types()) of
        'false' -> validate_request(MediaId, Context);
        'true' -> validate_media_binary(Context, MediaId, ?HTTP_POST, cb_context:req_files(Context))
    end.

-spec validate_media_binary(cb_context:context(), kz_term:ne_binary(), http_method(), kz_term:proplist()) -> cb_context:context().
validate_media_binary(Context, MediaId, ?HTTP_GET, _Files) ->
    lager:debug("fetch media contents for '~s'", [MediaId]),
    load_media_binary(Context, MediaId);
validate_media_binary(Context, _MediaId, ?HTTP_POST, []) ->
    error_missing_file(Context);
validate_media_binary(Context, MediaId, ?HTTP_POST, [{_Filename, FileObj}]) ->
    Context1 = load_media_meta(Context, MediaId),
    lager:debug("loaded media meta for '~s'", [MediaId]),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_normalize_upload(Context1, MediaId, FileObj);
        _Status -> Context1
    end;
validate_media_binary(Context, _MediaId, ?HTTP_POST, _Files) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"maxItems">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide a single media file">>}])
                                   ,Context
                                   ).

-spec error_missing_file(cb_context:context()) -> cb_context:context().
error_missing_file(Context) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide an media file">>}])
                                   ,Context
                                   ).

-spec maybe_normalize_upload(cb_context:context(), kz_term:ne_binary(), kz_json:object()) -> cb_context:context().
maybe_normalize_upload(Context, MediaId, FileJObj) ->
    case kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"normalize_media">>, 'false') of
        'true' ->
            lager:debug("normalizing uploaded media"),
            normalize_upload(Context, MediaId, FileJObj);
        'false' ->
            lager:debug("normalization not enabled, leaving upload as-is"),
            validate_upload(Context, MediaId, FileJObj)
    end.

-spec normalize_upload(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
                              cb_context:context().
normalize_upload(Context, MediaId, FileJObj) ->
    normalize_upload(Context, MediaId, FileJObj
                    ,kz_json:get_ne_binary_value([<<"headers">>, <<"content_type">>], FileJObj)
                    ).

-spec normalize_upload(cb_context:context(), kz_term:ne_binary(), kz_json:object(), kz_term:api_binary()) ->
                              cb_context:context().
normalize_upload(Context, MediaId, FileJObj, UploadContentType) ->
    FromExt = kz_mime:to_extension(UploadContentType),
    ToExt =  ?NORMALIZATION_FORMAT,

    lager:info("upload is of type '~s', normalizing from ~s to ~s"
              ,[UploadContentType, FromExt, ToExt]
              ),

    {UpdatedContext, UpdatedFileJObj}
        = cb_modules_util:normalize_media_upload(Context, FromExt, ToExt, FileJObj, []),
    validate_upload(UpdatedContext
                   ,MediaId
                   ,UpdatedFileJObj
                   ).

-spec validate_upload(cb_context:context(), kz_term:ne_binary(), kz_json:object()) -> cb_context:context().
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

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    put_media(Context, cb_context:account_id(Context)).

-spec put_media(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
put_media(Context, 'undefined') ->
    put_media(cb_context:set_account_db(Context, ?KZ_MEDIA_DB), <<"ignore">>);
put_media(Context, _AccountId) ->
    case is_tts(cb_context:doc(Context)) of
        'true' -> create_update_tts(Context, <<"create">>);
        'false' -> crossbar_doc:save(Context)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, MediaId) ->
    post_media_doc(Context, MediaId, cb_context:account_id(Context)).

-spec post_media_doc(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) -> cb_context:context().
post_media_doc(Context, MediaId, 'undefined') ->
    post_media_doc(cb_context:set_account_db(Context, ?KZ_MEDIA_DB), MediaId, <<"ignore">>);
post_media_doc(Context, MediaId, _AccountId) ->
    case is_tts(cb_context:doc(Context)) of
        'true' -> create_update_tts(Context, <<"update">>);
        'false' -> post_media_doc_or_binary(remove_tts_keys(Context), MediaId, cb_context:req_header(Context, <<"content-type">>))
    end.

-spec post_media_doc_or_binary(cb_context:context(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> cb_context:context().
post_media_doc_or_binary(Context, MediaId, ContentType) ->
    case api_util:content_type_matches(ContentType, acceptable_content_types()) of
        'false' -> crossbar_doc:save(Context);
        'true' -> post(Context, MediaId, ?BIN_DATA)
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, MediaId, ?BIN_DATA) ->
    post_media_binary(Context, MediaId, cb_context:account_id(Context)).

-spec remove_tts_keys(cb_context:context()) -> cb_context:context().
remove_tts_keys(Context) ->
    cb_context:set_doc(Context
                      ,kz_json:delete_keys([<<"pvt_previous_tts">>, <<"pvt_previous_voice">>]
                                          ,cb_context:doc(Context)
                                          )).

-spec post_media_binary(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) -> cb_context:context().
post_media_binary(Context, MediaId, 'undefined') ->
    post_media_binary(cb_context:set_account_db(Context, ?KZ_MEDIA_DB), MediaId, <<"ignore">>);
post_media_binary(Context, MediaId, _AccountId) ->
    update_media_binary(Context, MediaId).

create_update_tts(Context, <<"create">>) ->
    C1 = update_and_save_tts_doc(Context),
    maybe_update_media_file(C1, <<"create">>, 'true', cb_context:resp_status(C1));
create_update_tts(Context, <<"update">>) ->
    maybe_update_media_file(Context, <<"update">>, is_tts_changed(cb_context:doc(Context)), cb_context:resp_status(Context)).

-spec maybe_update_media_file(cb_context:context(), kz_term:ne_binary(), boolean(), crossbar_status()) ->
                                     cb_context:context().
maybe_update_media_file(Context, CreateOrUpdate, 'true', 'success') ->
    JObj = cb_context:doc(Context),
    Text = kz_json:get_value([<<"tts">>, <<"text">>], JObj),
    Voice = kz_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),

    try kazoo_tts:create(Text, Voice) of
        {'error', Reason} ->
            _ = maybe_delete_tts(Context, kz_term:to_binary(Reason), CreateOrUpdate),
            crossbar_util:response('error', kz_term:to_binary(Reason), Context);
        {'error', 'tts_provider_failure', Reason} ->
            _ = maybe_delete_tts(Context, kz_term:to_binary(Reason), CreateOrUpdate);
        {'ok', ContentType, Content} ->
            MediaId = kz_doc:id(JObj),
            Headers = kz_json:from_list([{<<"content_type">>, ContentType}
                                        ,{<<"content_length">>, iolist_size(Content)}
                                        ]),
            FileJObj = kz_json:from_list([{<<"headers">>, Headers}
                                         ,{<<"contents">>, Content}
                                         ]),
            FileName = list_to_binary(["text_to_speech_"
                                      ,kz_term:to_binary(kz_time:now_s())
                                      ,".wav"
                                      ]),
            C1 = update_media_binary(cb_context:set_req_files(Context, [{FileName, FileJObj}]), MediaId),
            case cb_context:resp_status(C1) =:= 'success'
                andalso CreateOrUpdate
            of
                'false' -> maybe_delete_tts(C1, <<"creating TTS failed unexpectedly">>, CreateOrUpdate);
                <<"create">> -> Context;
                <<"update">> ->
                    C2 = crossbar_doc:load_merge(MediaId, kz_doc:public_fields(JObj), Context, ?TYPE_CHECK_OPTION(kzd_media:type())),
                    case cb_context:resp_status(C2) of
                        'success' -> update_and_save_tts_doc(C2);
                        _ -> C2
                    end
            end
    catch
        _E:_R ->
            lager:debug("creating tts failed unexpectedly: ~s: ~p", [_E, _R]),
            maybe_delete_tts(Context, <<"creating TTS failed unexpectedly">>, CreateOrUpdate)
    end;
maybe_update_media_file(Context, <<"update">>, 'false', 'success') ->
    crossbar_doc:save(Context);
maybe_update_media_file(Context, _, _, _) ->
    Context.

-spec update_and_save_tts_doc(cb_context:context()) -> cb_context:context().
update_and_save_tts_doc(Context) ->
    JObj = cb_context:doc(Context),
    Text = kz_json:get_value([<<"tts">>, <<"text">>], JObj),
    Voice = kz_json:get_value([<<"tts">>, <<"voice">>], JObj, ?DEFAULT_VOICE),

    Doc = kz_json:set_values([{<<"pvt_previous_tts">>, Text}
                             ,{<<"pvt_previous_voice">>, Voice}
                             ], JObj
                            ),
    crossbar_doc:save(cb_context:set_doc(Context, Doc)).

-spec maybe_delete_tts(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
maybe_delete_tts(Context, Reason, <<"create">>) ->
    _ = crossbar_doc:delete(Context),
    crossbar_util:response('error', Reason, Context);
maybe_delete_tts(Context, _, <<"update">>) ->
    Context.

-spec delete_type(boolean() | cb_context:context()) -> ?HARD_DELETE | ?SOFT_DELETE.
delete_type('true') ->
    ?HARD_DELETE;

delete_type('false') ->
    ?SOFT_DELETE;

delete_type(Context) ->
    Prompt = kzd_media:is_prompt(cb_context:resp_data(Context)),
    Hard   = kz_json:is_true(<<"hard_delete">>, cb_context:req_data(Context)),

    delete_type(Prompt or Hard).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _MediaId) ->
    crossbar_doc:delete(Context, delete_type(Context)).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, MediaId, ?BIN_DATA) ->
    delete_media_binary(MediaId, Context, cb_context:account_id(Context)).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized list of media
%% @end
%%------------------------------------------------------------------------------

-spec load_media_summary(cb_context:context()) -> cb_context:context().
load_media_summary(Context) ->
    load_media_summary(Context, cb_context:account_id(Context)).

-spec load_media_summary(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
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
        StartKey -> StartKey
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
load_available_languages(Context) ->
    load_available_languages(Context, cb_context:account_id(Context)).

-spec load_available_languages(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
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

-spec load_media_docs_by_language(cb_context:context(), kz_term:ne_binary()) ->
                                         cb_context:context().
load_media_docs_by_language(Context, <<"missing">>) ->
    lager:debug("loading media files missing a language"),
    load_media_docs_by_language(Context, 'null', cb_context:account_id(Context));
load_media_docs_by_language(Context, Language) ->
    lager:debug("loading media files in language ~p", [Language]),
    load_media_docs_by_language(Context, Language, cb_context:account_id(Context)).

-spec load_media_docs_by_language(cb_context:context(), kz_term:ne_binary() | 'null', kz_term:api_binary()) ->
                                         cb_context:context().
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

-spec language_start_key(cb_context:context(), kz_term:ne_binary()) -> kz_term:ne_binaries().
language_start_key(Context, Language) ->
    case crossbar_doc:start_key(Context) of
        'undefined' -> [Language];
        Key -> language_start_key(Context, Language, binary:split(Key, <<"/">>))
    end.

-spec language_start_key(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
language_start_key(_Context, Language, [Language, Id]) ->
    [Language, Id];
language_start_key(_Context, Language, _Key) ->
    [Language].

-spec normalize_language_results(kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
normalize_language_results(JObj, Acc) ->
    [kz_doc:id(JObj) | Acc].

%%------------------------------------------------------------------------------
%% @doc Load prompt listing
%% @end
%%------------------------------------------------------------------------------
-spec load_available_prompts(cb_context:context()) ->
                                    cb_context:context().
load_available_prompts(Context) ->
    load_available_prompts(Context, cb_context:account_id(Context)).

-spec load_available_prompts(cb_context:context(), kz_term:api_binary()) ->
                                    cb_context:context().
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

-spec load_media_docs_by_prompt(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_media_docs_by_prompt(Context, PromptId) ->
    lager:debug("loading media files in prompt ~p", [PromptId]),
    load_media_docs_by_prompt(Context, PromptId, cb_context:account_id(Context)).

-spec load_media_docs_by_prompt(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) -> cb_context:context().
load_media_docs_by_prompt(Context, PromptId, 'undefined') ->
    fix_prompt_start_keys(
      crossbar_doc:load_view(?CB_LIST_BY_PROMPT
                            ,[{'startkey_fun', fun(Ctx) -> prompt_start_key(Ctx, PromptId) end}
                             ,{'endkey', [PromptId, kz_json:new()]}
                             ,{'reduce', 'false'}
                             ,'include_docs'
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
                             ,'include_docs'
                             ]
                            ,Context
                            ,fun normalize_prompt_results/2
                            )
     ).


-spec prompt_start_key(cb_context:context()) ->
                              kz_term:ne_binaries().
prompt_start_key(Context) ->
    prompt_start_key(Context, 'undefined').

-spec prompt_start_key(cb_context:context(), kz_term:api_binary()) ->
                              kz_term:ne_binaries().
prompt_start_key(Context, PromptId) ->
    case crossbar_doc:start_key(Context) of
        PromptId -> PromptId;
        'undefined' -> [PromptId];
        Key -> [Key]
    end.

-spec normalize_prompt_results(kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
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
    cb_context:set_resp_envelope(Context
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


%%------------------------------------------------------------------------------
%% @doc Load a media document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_media_meta(cb_context:context(), kz_term:ne_binary()) ->
                             cb_context:context().
load_media_meta(Context, MediaId) ->
    load_media_meta(Context, MediaId, cb_context:account_id(Context)).

-spec load_media_meta(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) ->
                             cb_context:context().
load_media_meta(Context, MediaId, 'undefined') ->
    crossbar_doc:load(MediaId, cb_context:set_account_db(Context, ?KZ_MEDIA_DB), ?TYPE_CHECK_OPTION(kzd_media:type()));
load_media_meta(Context, MediaId, _AccountId) ->
    crossbar_doc:load(MediaId, Context, ?TYPE_CHECK_OPTION(kzd_media:type())).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(MediaId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(MediaId, C) end,
    cb_context:validate_request_data(<<"media">>, Context, OnSuccess).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = cb_context:doc(Context),
    Props = [{<<"pvt_type">>, kzd_media:type()}
             | maybe_add_prompt_fields(Context)
            ],
    cb_context:set_doc(Context, kz_json:set_values(Props, Doc));
on_successful_validation(MediaId, Context) ->
    Context1 = crossbar_doc:load_merge(MediaId, Context, ?TYPE_CHECK_OPTION(kzd_media:type())),
    maybe_validate_prompt(MediaId, Context1, cb_context:resp_status(Context1)).

-spec maybe_validate_prompt(kz_term:ne_binary(), cb_context:context(), crossbar_status()) ->
                                   cb_context:context().
maybe_validate_prompt(MediaId, Context, 'success') ->
    case kzd_media:prompt_id(cb_context:doc(Context)) of
        'undefined' -> Context;
        PromptId ->
            validate_prompt(MediaId, Context, PromptId)
    end;
maybe_validate_prompt(_MediaId, Context, _Status) ->
    Context.

-spec validate_prompt(kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary()) ->
                             cb_context:context().
validate_prompt(MediaId, Context, PromptId) ->
    Language = kz_term:to_lower_binary(kzd_media:language(cb_context:doc(Context))),
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

-spec maybe_add_prompt_fields(cb_context:context()) -> kz_term:proplist().
maybe_add_prompt_fields(Context) ->
    JObj = cb_context:doc(Context),
    case kzd_media:prompt_id(JObj) of
        'undefined' -> [];
        PromptId ->
            Language = kz_term:to_lower_binary(kzd_media:language(JObj, kz_media_util:default_prompt_language())),
            ID = kz_media_util:prompt_id(PromptId, Language),

            lager:debug("creating properties for prompt ~s (~s)", [PromptId, Language]),

            [{<<"_id">>, ID}
            ,{<<"language">>, Language}
            ,{<<"name">>, kz_json:get_value(<<"name">>, JObj, ID)}
            ]
    end.

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc Load the binary attachment of a media doc
%% @end
%%------------------------------------------------------------------------------
-spec load_media_binary(cb_context:context(), path_token()) -> cb_context:context().
load_media_binary(Context, MediaId) ->
    Context1 = load_media_meta(Context, MediaId),
    case cb_context:resp_status(Context1) of
        'success' ->
            case kz_doc:attachment_names(cb_context:doc(Context1)) of
                [] -> crossbar_util:response_bad_identifier(MediaId, Context);
                [Attachment|_] ->
                    LoadedContext = crossbar_doc:load_attachment(cb_context:doc(Context1)
                                                                ,Attachment
                                                                ,?TYPE_CHECK_OPTION(kzd_media:type())
                                                                ,Context1
                                                                ),
                    cb_context:add_resp_headers(LoadedContext
                                               ,#{<<"content-disposition">> => <<"attachment; filename=", Attachment/binary>>
                                                 ,<<"content-type">> => kz_doc:attachment_content_type(cb_context:doc(Context1), Attachment)
                                                 }
                                               )
            end;
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Update the binary attachment of a media doc
%% @end
%%------------------------------------------------------------------------------
-spec update_media_binary(cb_context:context(), path_token()) ->
                                 cb_context:context().
update_media_binary(Context, MediaId) ->
    update_media_binary(crossbar_util:maybe_remove_attachments(Context)
                       ,MediaId
                       ,cb_context:req_files(Context)
                       ).

-spec update_media_binary(cb_context:context(), path_token(), req_files()) ->
                                 cb_context:context().
update_media_binary(Context, _MediaId, []) -> Context;
update_media_binary(Context, MediaId, [{Filename, FileObj}|Files]) ->
    Contents = kz_json:get_value(<<"contents">>, FileObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    lager:debug("file content type: ~s", [CT]),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(kzd_media:type())],

    AttachmentName = cb_modules_util:attachment_name(Filename, CT),
    Context1 = crossbar_doc:save_attachment(MediaId
                                           ,AttachmentName
                                           ,Contents
                                           ,Context
                                           ,Opts
                                           ),
    case cb_context:resp_status(Context1) of
        'success' -> update_media_binary(Context1, MediaId, Files);
        _Failure ->
            lager:info("failed to save attachment ~s to ~s", [AttachmentName, MediaId]),
            Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Delete the binary attachment of a media doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_media_binary(path_token(), cb_context:context(), kz_term:api_binary()) -> cb_context:context().
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
is_tts(JObj) ->
    kz_json:get_ne_binary_value(<<"media_source">>, JObj) =:= <<"tts">>.

-spec is_tts_changed(kz_json:object()) -> boolean().
is_tts_changed(JObj) ->
    Text = kz_json:get_ne_binary_value([<<"tts">>, <<"text">>], JObj),
    Voice = kz_json:get_ne_binary_value([<<"tts">>, <<"voice">>], JObj),
    PreText = kz_json:get_value(<<"pvt_previous_tts">>, JObj),
    PrevVoice = kz_json:get_ne_binary_value(<<"pvt_previous_voice">>, JObj),

    Text =/= PreText
        orelse Voice =/= PrevVoice.
