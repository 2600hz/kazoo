%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% Provides access to third party bigcouch stored call recordings:
%%%
%%% To play in browser:
%%% https://my_crossbar.tld:8443/v1/accounts/33ca...1452/third_party_couch/call_recordings/201412-6471-8c79ae@192.1.1.2/attachment/inline?auth_token=05c1...7a075
%%%
%%% To download:
%%% https://my_crossbar.tld:8443/v1/accounts/33ca...1452/third_party_couch/call_recordings/201412-6471-8c79ae@192.1.1.2/attachment?auth_token=05c1...7a075
%%%
%%% @end
%%% @contributors:
%%% 
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%   Dinkor (Andrew Korniliv github.com/dinkor)
%%% 
%%%-------------------------------------------------------------------
-module(cb_third_party_couch).

-export([init/0
         ,allowed_methods/3, allowed_methods/4
         ,resource_exists/3, resource_exists/4
         ,content_types_provided/4, content_types_provided/5
         ,validate/4, validate/5
        ]).

-include("../crossbar.hrl").

-define(CALL_RECORDINGS, <<"call_recordings">>).
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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.third_party_couch">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.third_party_couch">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.third_party_couch">>, ?MODULE, 'content_types_provided'),
    crossbar_bindings:bind(<<"*.validate.third_party_couch">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token(), path_token()) -> http_methods().

allowed_methods(?CALL_RECORDINGS, _Id, ?ATTACHMENT) ->
    [?HTTP_GET].

allowed_methods(?CALL_RECORDINGS, _Id, ?ATTACHMENT, ?INLINE) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% 
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.

resource_exists(?CALL_RECORDINGS, _Id, ?ATTACHMENT) -> 'true'.
resource_exists(?CALL_RECORDINGS, _Id, ?ATTACHMENT, ?INLINE) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, ?CALL_RECORDINGS, <<Year:4/binary, Month:2/binary, "-", _/binary>>, ?ATTACHMENT) ->
    Ctx = cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    content_types_provided_for_download(Ctx, cb_context:req_verb(Context));
content_types_provided(Context, _, _, _) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, ?CALL_RECORDINGS, <<Year:4/binary, Month:2/binary, "-", _/binary>>, ?ATTACHMENT, ?INLINE) ->
    Ctx = cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    content_types_provided_for_download(Ctx, cb_context:req_verb(Context));
content_types_provided(Context, _, _, _, _) ->
    Context.

-spec content_types_provided_for_download(cb_context:context(), path_token()) -> cb_context:context().
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
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?CALL_RECORDINGS, CdrId, ?ATTACHMENT) ->
    load_recording_binary(CdrId, Context).

-spec validate(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?CALL_RECORDINGS, CdrId, ?ATTACHMENT, ?INLINE) ->
    load_recording_binary(CdrId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get recording name from cdr by provided cdr record id in request and load file with record from third party BigCouch
%% @end
%%--------------------------------------------------------------------
-spec load_recording_binary(path_token(), cb_context:context()) -> cb_context:context().
load_recording_binary(<<Year:4/binary, Month:2/binary, "-", _/binary>> = CdrId, Context) ->
    do_load_recording_binary(CdrId, cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month))).

-spec do_load_recording_binary(path_token(), cb_context:context()) -> cb_context:context().
do_load_recording_binary(CdrId, Context) ->
    Context1 = crossbar_doc:load(CdrId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Doc = cb_context:doc(Context1),
            case wh_json:get_value([<<"custom_channel_vars">>, <<"media_name">>], Doc) of
                'undefined' -> cb_context:add_system_error('bad_identifier', [{'details', CdrId}], Context1);
                MediaName ->
                    {'ok', Attachment} = get_recording(MediaName, Context),
                    cb_context:set_resp_etag(
                      cb_context:set_resp_headers(cb_context:setters(Context1
                                                     ,[{fun cb_context:set_resp_data/2, Attachment}
                                                     ,{fun cb_context:set_resp_etag/2, 'undefined'}
                                                   ])

                                                  ,[{<<"Content-Disposition">>, get_disposition(MediaName, Context1)}
                                                    ,{<<"Content-Type">>, <<"audio/mpeg">>}
                                                    ,{<<"Content-Length">>, byte_size(Attachment)}
                                                    | cb_context:resp_headers(Context)
                                                   ])
                      ,'undefined'
                                            )
            end;
        _Status -> Context1
    end.

-spec get_recording(ne_binary(), cb_context:context()) -> {'ok', ne_binary()} | 'ok'.
get_recording(MediaName, Context) ->
    case whapps_config:get_ne_binary(<<"media">>, <<"third_party_bigcouch_host">>) of
        'undefined' -> lager:debug("no URL for call recording provided, third_party_bigcouch_host undefined");
        BCHost -> get_attachment_from_third_party_bigcouch(MediaName, BCHost, Context)
    end.

-spec get_attachment_from_third_party_bigcouch(ne_binary(), ne_binary(), cb_context:context()) -> {'ok', ne_binary()}.
get_attachment_from_third_party_bigcouch(MediaName, BCHost, Context) ->
    S = couchbeam:server_connection(BCHost, wh_util:to_list(whapps_config:get(<<"media">>, <<"third_party_bigcouch_port">>, <<"5984">>))),
    {'ok', Db} = couchbeam:open_or_create_db(S, cb_context:account_db(Context), []),
    couchbeam:fetch_attachment(Db, re:replace(MediaName, "\\.mp3$|\\.wav$", "", [{'return', 'binary'}]), MediaName).

-spec get_disposition(ne_binary(), cb_context:context()) -> ne_binary().
get_disposition(MediaName, Context) ->
    case lists:member(<<"inline">>, cb_context:path_tokens(Context)) of
        'true' -> <<"inline; filename=", MediaName/binary>>;
        'false' -> <<"attachment; filename=", MediaName/binary>>
    end.
