%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Provides access to stored call recordings.
%%%
%%% @author OnNet (Kirill Sysoev [github.com/onnet])
%%% @author Dinkor (Andrew Korniliv [github.com/dinkor])
%%% @author Lazedo (Luis Azedo [github.com/2600hz])
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_recordings).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_provided/2
        ,validate/1, validate/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"recordings/crossbar_listing">>).
-define(CB_LIST_BY_OWNERID, <<"recordings/listing_by_user">>).

-define(MEDIA_MIME_TYPES, [{<<"audio">>, <<"mpeg">>}
                          ,{<<"audio">>, <<"mp3">>}
                          ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"*.allowed_methods.recordings">>, 'allowed_methods'}
               ,{<<"*.resource_exists.recordings">>, 'resource_exists'}
               ,{<<"*.content_types_provided.recordings">>, 'content_types_provided'}
               ,{<<"*.validate.recordings">>, 'validate'}
               ,{<<"*.execute.delete.recordings">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_RecordingId) -> [?HTTP_GET, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_RecordingId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be using to respond (matched against
%% client's Accept header).
%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    recording_summary(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, RecordingId) ->
    validate_recording(Context, RecordingId, cb_context:req_verb(Context)).

validate_recording(Context, RecordingId, ?HTTP_GET) ->
    case action_lookup(Context) of
        'read' ->
            load_recording_doc(Context, RecordingId);
        'download' ->
            load_recording_binary(Context, RecordingId)
    end;
validate_recording(Context, RecordingId, ?HTTP_DELETE) ->
    load_recording_doc(Context, RecordingId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _RecordingId) ->
    crossbar_doc:delete(Context).

-spec recording_summary(cb_context:context()) -> cb_context:context().
recording_summary(Context) ->
    UserId = cb_context:user_id(Context),
    ViewName = get_view_name(UserId),
    Options = [{'mapper', fun summary_doc_fun/2}
              ,{'range_start_keymap', [UserId]}
              ,{'range_end_keymap', fun(Ts) -> build_end_key(Ts, UserId) end}
              ,'include_docs'
              ],
    crossbar_view:load_modb(Context, ViewName, Options).

-spec summary_doc_fun(kz_json:object(), kz_json:objects()) -> kz_json:objects().
summary_doc_fun(View, Acc) ->
    Recording = kz_json:get_json_value(<<"doc">>, View),
    Attachments = kz_doc:attachments(Recording, kz_json:new()),
    ContentTypes = kz_json:foldl(fun attachment_content_type/3, [], Attachments),

    [kz_json:set_value([<<"_read_only">>, <<"content_types">>]
                      ,lists:usort(ContentTypes)
                      ,Recording
                      )
     | Acc
    ].

-spec attachment_content_type(kz_json:key(), kz_json:object(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
attachment_content_type(_Name, Meta, CTs) ->
    [kz_json:get_ne_binary_value(<<"content_type">>, Meta) | CTs].

-spec build_end_key(kz_time:gregorian_seconds(), kz_term:api_ne_binary()) -> kazoo_data:range_key().
build_end_key(Timestamp, 'undefined') -> [Timestamp, kz_json:new()];
build_end_key(Timestamp, UserId) -> [UserId, Timestamp, kz_json:new()].

-spec get_view_name(kz_term:api_ne_binary()) -> kz_term:ne_binary().
get_view_name('undefined') -> ?CB_LIST;
get_view_name(_) -> ?CB_LIST_BY_OWNERID.

-spec load_recording_doc(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_recording_doc(Context, ?MATCH_MODB_PREFIX(Year, Month, _) = RecordingId) ->
    Ctx = cb_context:set_db_name(Context
                                ,kzs_util:format_account_id(cb_context:account_id(Context)
                                                           ,kz_term:to_integer(Year)
                                                           ,kz_term:to_integer(Month)
                                                           )
                                ),
    crossbar_doc:load({<<"call_recording">>, RecordingId}, Ctx, ?TYPE_CHECK_OPTION(<<"call_recording">>));
load_recording_doc(Context, Id) ->
    crossbar_util:response_bad_identifier(Id, Context).

-spec load_recording_binary(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_recording_binary(Context, ?MATCH_MODB_PREFIX(Year, Month, _) = DocId) ->
    do_load_recording_binary(cb_context:set_db_name(Context, kzs_util:format_account_id(cb_context:account_id(Context), kz_term:to_integer(Year), kz_term:to_integer(Month))), DocId).

-spec do_load_recording_binary(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
do_load_recording_binary(Context, DocId) ->
    Context1 = crossbar_doc:load({<<"call_recording">>, DocId}, Context, ?TYPE_CHECK_OPTION(<<"call_recording">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            do_load_recording_binary_attachment(Context1, DocId);
        _Status -> Context1
    end.

-spec do_load_recording_binary_attachment(cb_context:context(), kz_term:ne_binary()) ->
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

            set_resp_headers(LoadedContext
                            ,AName
                            ,kz_doc:attachment(cb_context:doc(Context), AName)
                            )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_resp_headers(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
          cb_context:context().
set_resp_headers(Context, AName, Attachment) ->
    Headers = #{<<"content-disposition">> => get_disposition(AName, Context)
               ,<<"content-type">> => kz_json:get_ne_binary_value(<<"content_type">>, Attachment)
               },
    cb_context:add_resp_headers(Context, Headers).

-spec get_disposition(kz_term:ne_binary(), cb_context:context()) -> kz_term:ne_binary().
get_disposition(MediaName, Context) ->
    case kz_json:is_true(<<"inline">>, cb_context:query_string(Context), 'false') of
        'false' -> <<"attachment; filename=", MediaName/binary>>;
        'true' -> <<"inline; filename=", MediaName/binary>>
    end.

-spec action_lookup(cb_context:context()) -> atom().
action_lookup(Context) ->
    Acceptable = acceptable_content_types(Context),
    action_lookup(Acceptable, accept_values(Context)).

-spec action_lookup(kz_term:proplist(), media_values()) -> atom().
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

-spec media_values(kz_term:api_binary(), kz_term:api_binary()) -> media_values().
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

-spec acceptable_content_types(cb_context:context()) -> kz_term:proplist().
acceptable_content_types(Context) ->
    props:get_value('to_binary', cb_context:content_types_provided(Context), []).

-spec is_acceptable_accept(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_acceptable_accept(Acceptable, Type, SubType) ->
    lists:member({Type,SubType}, Acceptable).
