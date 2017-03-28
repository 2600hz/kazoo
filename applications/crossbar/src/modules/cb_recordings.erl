%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% Provides access to stored call recordings.
%%%
%%% @end
%%% @contributors:
%%%
%%%   Original pull request https://github.com/2600hz/kazoo/pull/1218
%%%     OnNet (Kirill Sysoev github.com/onnet)
%%%     Dinkor (Andrew Korniliv github.com/dinkor)
%%%
%%%   refactor
%%%     lazedo (Luis Azedo github.com/2600hz)
%%%
%%%-------------------------------------------------------------------
-module(cb_recordings).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_provided/2
        ,validate/1, validate/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"recordings/crossbar_listing">>).
-define(CB_LIST_BY_OWNERID, <<"recordings/listing_by_user">>).

-define(INLINE, <<"inline">>).
-define(MEDIA_MIME_TYPES, [{<<"audio">>, <<"mpeg">>}
                          ,{<<"audio">>, <<"mp3">>}
                          ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.recordings">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.recordings">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.recordings">>, ?MODULE, 'content_types_provided'),
    crossbar_bindings:bind(<<"*.validate.recordings">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().

allowed_methods() -> [?HTTP_GET].
allowed_methods(_RecordingId) -> [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.

resource_exists() -> 'true'.
resource_exists(_RecordingId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's Accept header)
%% Of the form {atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, _RecordingId) ->
    content_types_provided_for_download(Context, cb_context:req_verb(Context)).

-spec content_types_provided_for_download(cb_context:context(), http_method()) -> cb_context:context().
content_types_provided_for_download(Context, ?HTTP_GET) ->
    CTP = [{'to_json', ?JSON_CONTENT_TYPES}
          ,{'to_binary', ?MEDIA_MIME_TYPES}
          ],
    cb_context:set_content_types_provided(Context, CTP);
content_types_provided_for_download(Context, _Verb) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    recording_summary(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, RecordingId) ->
    case action_lookup(Context) of
        'read' ->
            load_recording_doc(Context, RecordingId);
        'download' ->
            load_recording_binary(Context, RecordingId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------

-spec recording_summary(cb_context:context()) -> cb_context:context().
recording_summary(Context) ->
    {View, PreFilter, PostFilter} = get_view_and_filter(Context),
    case cb_modules_util:range_modb_view_options(Context, PreFilter, PostFilter) of
        {'ok', ViewOptions} ->
            recording_summary(Context, View, ViewOptions);
        Ctx -> Ctx
    end.

-spec recording_summary(cb_context:context(), ne_binary(), crossbar_doc:view_options()) ->
                               cb_context:context().
recording_summary(Context, View, ViewOptions) ->
    LoadedContext = crossbar_doc:load_view(View
                                          ,['include_docs'
                                           ,{'startkey_fun', fun start_key_builder/2}
                                            | props:delete('endkey', ViewOptions)
                                           ]
                                          ,Context
                                          ,fun normalize_view_results/2
                                          ),
    case cb_context:resp_status(LoadedContext) of
        'success' -> update_summary_pagination(LoadedContext);
        _Status -> LoadedContext
    end.

-spec start_key_builder(kz_proplist(), cb_context:context()) -> kz_json:api_json_term().
start_key_builder(Options, Context) ->
    case crossbar_doc:start_key(Context) of
        'undefined' ->
            lager:debug("using default start key"),
            props:get_value('startkey', Options);
        CreatedBin ->
            override_start_key(Options, kz_term:to_integer(CreatedBin))
    end.

-spec override_start_key(kz_proplist(), gregorian_seconds()) ->
                                [ne_binary() | gregorian_seconds()].
override_start_key(Options, Created) ->
    case props:get_value('startkey', Options) of
        'undefined' ->
            lager:debug("using supplied start key ~p", [Created]),
            [Created];
        CreatedFrom when is_integer(CreatedFrom) ->
            lager:debug("overriding ~p with supplied ~p", [CreatedFrom, Created]),
            [Created];
        [_CreatedFrom, Id] ->
            lager:debug("overriding ~p with supplied ~p", [_CreatedFrom, Created]),
            [Created, Id];
        [OwnerId, _CreatedFrom, Id] ->
            lager:debug("overriding ~p with supplied ~p", [_CreatedFrom, Created]),
            [OwnerId, Created, Id]
    end.

-spec update_summary_pagination(cb_context:context()) -> cb_context:context().
update_summary_pagination(Context) ->
    cb_context:set_resp_envelope(Context
                                ,lists:foldl(fun fix_resp_start_keys_fold/2
                                            ,cb_context:resp_envelope(Context)
                                            ,[<<"start_key">>, <<"next_start_key">>]
                                            )
                                ).

-spec fix_resp_start_keys_fold(kz_json:key(), kz_json:object()) -> kz_json:object().
fix_resp_start_keys_fold(Key, ResponseEnvelope) ->
    case kz_json:get_value(Key, ResponseEnvelope) of
        'undefined' -> ResponseEnvelope;
        <<_/binary>> -> ResponseEnvelope;
        Created when is_integer(Created) -> ResponseEnvelope;
        [Created, _Id] ->
            lager:debug("setting ~s to ~p", [Key, Created]),
            kz_json:set_value(Key, Created, ResponseEnvelope);
        [_OwnerId, Created, _Id] ->
            lager:debug("setting ~s to ~p", [Key, Created]),
            kz_json:set_value(Key, Created, ResponseEnvelope)
    end.

-spec load_recording_doc(cb_context:context(), ne_binary()) -> cb_context:context().
load_recording_doc(Context, <<Year:4/binary, Month:2/binary, "-", _/binary>> = RecordingId) ->
    Ctx = cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    crossbar_doc:load({<<"call_recording">>, RecordingId}, Ctx, ?TYPE_CHECK_OPTION(<<"call_recording">>));
load_recording_doc(Context, Id) ->
    crossbar_util:response_bad_identifier(Id, Context).

-spec load_recording_binary(cb_context:context(), ne_binary()) -> cb_context:context().
load_recording_binary(Context, <<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId) ->
    do_load_recording_binary(cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month)), DocId).

-spec do_load_recording_binary(cb_context:context(), ne_binary()) -> cb_context:context().
do_load_recording_binary(Context, DocId) ->
    Context1 = crossbar_doc:load({<<"call_recording">>, DocId}, Context, ?TYPE_CHECK_OPTION(<<"call_recording">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            do_load_recording_binary_attachment(Context1, DocId);
        _Status -> Context1
    end.

-spec do_load_recording_binary_attachment(cb_context:context(), ne_binary()) ->
                                                 cb_context:context().
do_load_recording_binary_attachment(Context, DocId) ->
    case kz_doc:attachment_names(cb_context:doc(Context)) of
        [] ->
            cb_context:add_system_error('bad_identifier'
                                       ,kz_json:from_list([{<<"details">>, DocId}])
                                       ,Context
                                       );
        [AName | _] ->
            LoadedContext = crossbar_doc:load_attachment({<<"call_recording">>, DocId}
                                                        ,AName
                                                        ,?TYPE_CHECK_OPTION(<<"call_recording">>)
                                                        ,Context
                                                        ),

            set_resp_headers(LoadedContext, AName)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------

-spec set_resp_headers(cb_context:context(), ne_binary()) -> cb_context:context().
set_resp_headers(Context, AName) ->
    Headers = [{<<"Content-Disposition">>, get_disposition(AName, Context)}],
    cb_context:set_resp_headers(Context, Headers).

-spec get_disposition(ne_binary(), cb_context:context()) -> ne_binary().
get_disposition(MediaName, Context) ->
    case kz_json:is_true(<<"inline">>, cb_context:query_string(Context), 'false') of
        'false' -> <<"attachment; filename=", MediaName/binary>>;
        'true' -> <<"inline; filename=", MediaName/binary>>
    end.

-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_doc:public_fields(kz_json:get_value(<<"doc">>, JObj))|Acc].

-spec get_view_and_filter(cb_context:context()) -> {ne_binary(), ne_binaries(), [<<>>]}.
get_view_and_filter(Context) ->
    case cb_context:user_id(Context) of
        'undefined' -> {?CB_LIST, [], [<<>>]};
        UserId -> {?CB_LIST_BY_OWNERID, [UserId], [<<>>]}
    end.

-spec action_lookup(cb_context:context()) -> atom().
-spec action_lookup(kz_proplist(), media_values()) -> atom().
action_lookup(Context) ->
    Acceptable = acceptable_content_types(Context),
    action_lookup(Acceptable, accept_values(Context)).

action_lookup(_, [?MEDIA_VALUE(<<"application">>, <<"json">>, _, _, _)|_]) ->
    'read';
action_lookup(_, [?MEDIA_VALUE(<<"application">>, <<"x-json">>, _, _, _)|_]) ->
    'read';
action_lookup(_, [?MEDIA_VALUE(<<"*">>, <<"*">>, _, _, _)|_]) ->
    lager:debug("catch-all accept header, using json"),
    'read';
action_lookup(Acceptable, [?MEDIA_VALUE(Type, SubType, _, _, _)|Accepts]) ->
    case is_acceptable_accept(Acceptable, Type, SubType) of
        'false' ->
            lager:debug("unknown accept header: ~s/~s", [Type, SubType]),
            action_lookup(Acceptable, Accepts);
        'true' ->
            lager:debug("accept header: ~s/~s", [Type, SubType]),
            'download'
    end;
action_lookup(_, []) ->
    lager:debug("no accept headers, using json"),
    'read'.

-spec accept_values(cb_context:context()) -> media_values().
accept_values(Context) ->
    AcceptValue = cb_context:req_header(Context, <<"accept">>),
    Tunneled = cb_context:req_value(Context, <<"accept">>),
    media_values(AcceptValue, Tunneled).

-spec media_values(api_binary(), api_binary()) -> media_values().
media_values('undefined', 'undefined') ->
    lager:debug("no accept headers, assuming JSON"),
    [?MEDIA_VALUE(<<"application">>, <<"json">>)];
media_values(AcceptValue, 'undefined') ->
    case cb_modules_util:parse_media_type(AcceptValue) of
        {'error', 'badarg'} -> media_values('undefined', 'undefined');
        AcceptValues -> lists:reverse(lists:keysort(2, AcceptValues))
    end;
media_values(AcceptValue, Tunneled) ->
    case cb_modules_util:parse_media_type(Tunneled) of
        {'error', 'badarg'} -> media_values(AcceptValue, 'undefined');
        TunneledValues ->
            lager:debug("using tunneled accept value ~s", [Tunneled]),
            lists:reverse(lists:keysort(2, TunneledValues))
    end.

-spec acceptable_content_types(cb_context:context()) -> kz_proplist().
acceptable_content_types(Context) ->
    props:get_value('to_binary', cb_context:content_types_provided(Context), []).

-spec is_acceptable_accept(kz_proplist(), ne_binary(), ne_binary()) -> boolean().
is_acceptable_accept(Acceptable, Type, SubType) ->
    lists:member({Type,SubType}, Acceptable).
