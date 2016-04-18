%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% Provides access to stored call recordings:
%%%
%%% To play in browser:
%%% https://my_crossbar.tld:8443/v2/accounts/33ca...1452/recordings/201412-6471-8c79ae@192.1.1.2/attachment?inline=true&auth_token=05c1...7a075
%%%
%%% To download:
%%% https://my_crossbar.tld:8443/v2/accounts/33ca...1452/recordings/201412-6471-8c79ae@192.1.1.2/attachment?auth_token=05c1...7a075
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,content_types_provided/3
         ,validate/1, validate/2, validate/3
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"recordings/crossbar_listing">>).
-define(CB_LIST_BY_OWNERID, <<"recordings/listing_by_user">>).

-define(ATTACHMENT, <<"attachment">>).
-define(INLINE, <<"inline">>).
-define(MEDIA_MIME_TYPES, [{<<"audio">>, <<"mpeg">>}]).

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
-spec allowed_methods(path_token(), path_token()) -> http_methods().

allowed_methods() -> [?HTTP_GET].
allowed_methods(_RecordingId ) -> [?HTTP_GET].
allowed_methods(_RecordingId, ?ATTACHMENT) -> [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.

resource_exists() -> 'true'.
resource_exists(_RecordingId) -> 'true'.
resource_exists(_RecordingId, ?ATTACHMENT) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, _, _) ->
    content_types_provided_for_download(Context, cb_context:req_verb(Context)).

-spec content_types_provided_for_download(cb_context:context(), http_method()) -> cb_context:context().
content_types_provided_for_download(Context, ?HTTP_GET) ->
    CTP = [{'to_binary', ?MEDIA_MIME_TYPES}],
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
validate(Context, <<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId) ->
    Ctx = cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    crossbar_doc:load({<<"call_recording">>, DocId}, Ctx, ?TYPE_CHECK_OPTION(<<"call_recording">>)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, DocId, ?ATTACHMENT) ->
    load_recording_binary(DocId, Context).



-spec recording_summary(cb_context:context()) -> cb_context:context().
recording_summary(Context) ->
    {View, PreFilter, PostFilter} = get_view_and_filter(Context),
    case cb_modules_util:range_modb_view_options(Context, PreFilter, PostFilter) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(View
                                   ,['include_docs' | ViewOptions]
                                   ,Context
                                   ,fun normalize_view_results/2
                                  );
        Ctx -> Ctx
    end.

-spec normalize_view_results(wh_json:object(), wh_json:objects()) ->
                                             wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:public_fields(wh_json:get_value(<<"doc">>, JObj))|Acc].

-spec get_view_and_filter(cb_context:context()) -> {ne_binary(), api_binaries(), api_binaries()}.
get_view_and_filter(Context) ->
    case cb_context:user_id(Context) of
        'undefined' -> {?CB_LIST, [], [wh_json:new()]};
        UserId -> {?CB_LIST_BY_OWNERID, [UserId], 'undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get recording name from cdr by provided cdr record id in request and load file with record from third party BigCouch
%% @end
%%--------------------------------------------------------------------
load_recording_binary(<<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId, Context) ->
    do_load_recording_binary(DocId, cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month))).

-spec do_load_recording_binary(ne_binary(), cb_context:context()) -> cb_context:context().
do_load_recording_binary(DocId, Context) ->
    Context1 = crossbar_doc:load({<<"call_recording">>, DocId}, Context, ?TYPE_CHECK_OPTION(<<"call_recording">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            case wh_doc:attachment_names(cb_context:doc(Context1)) of
                [] -> cb_context:add_system_error('bad_identifier', [{'details', DocId}], Context1);
                [AName | _] ->
                    Ctx = crossbar_doc:load_attachment({<<"call_recording">>, DocId}, AName, ?TYPE_CHECK_OPTION(<<"call_recording">>), Context),
                    set_resp_headers(Ctx, AName)
            end;
        _Status -> Context1
    end.

-spec set_resp_headers(cb_context:context(), ne_binary()) -> cb_context:context().
set_resp_headers(Context, AName) ->
    Headers = [{<<"Content-Disposition">>, get_disposition(AName, Context)}],
    cb_context:set_resp_headers(Context, Headers).

-spec get_disposition(ne_binary(), cb_context:context()) -> ne_binary().
get_disposition(MediaName, Context) ->
    case wh_json:is_true(<<"inline">>, cb_context:query_string(Context), 'false') of
        'false' -> <<"attachment; filename=", MediaName/binary>>;
        'true' -> <<"inline; filename=", MediaName/binary>>
    end.
