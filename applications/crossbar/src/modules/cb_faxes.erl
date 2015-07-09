%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_faxes).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_provided/4
         ,content_types_accepted/1, content_types_accepted/2
         ,validate/1, validate/2, validate/3, validate/4
         ,put/1, put/2
         ,post/1, post/3
         ,patch/1, patch/3
         ,delete/3, delete/4
        ]).

-include("../crossbar.hrl").

-define(OUTGOING, <<"outgoing">>).
-define(INCOMING, <<"incoming">>).
-define(SMTP_LOG, <<"smtplog">>).

-define(ATTACHMENT, <<"attachment">>).

-define(CB_LIST_ALL, <<"faxes/crossbar_listing">>).
-define(CB_LIST_BY_FAXBOX, <<"faxes/list_by_faxbox">>).
-define(CB_LIST_BY_OWNERID, <<"faxes/list_by_ownerid">>).
-define(CB_LIST_BY_ACCOUNT, <<"faxes/list_by_account">>).
-define(CB_LIST_SMTP_LOG, <<"faxes/smtp_log">>).

-define(FAX_FILE_TYPE, <<"tiff">>).

-define(ACCEPTED_MIME_TYPES, [{<<"application">>, <<"json">>}
                              ,{<<"application">>, <<"pdf">>}
                              ,{<<"image">>, <<"tiff">>}
                              | ?MULTIPART_CONTENT_TYPES
                             ]).
-define(ACCEPTED_TYPES, [{'from_binary', ?ACCEPTED_MIME_TYPES}]).


-define(OUTGOING_FAX_DOC_MAP, [{<<"created">>, <<"pvt_created">>}
                               ,{<<"delivered">>, fun get_delivered_date/1}
                               ,{<<"status">>, fun get_execution_status/2}
                              ]).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".fax">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.faxes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.faxes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.faxes">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.faxes">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.faxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.faxes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.faxes">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.faxes">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.faxes">>, ?MODULE, 'delete').

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
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().

allowed_methods() ->
    [?HTTP_PUT].

allowed_methods(?SMTP_LOG) ->
    [?HTTP_GET];
allowed_methods(?INCOMING) ->
    [?HTTP_GET];
allowed_methods(?OUTGOING) ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?SMTP_LOG, _Id) ->
    [?HTTP_GET];
allowed_methods(?INCOMING, _Id) ->
    [?HTTP_GET, ?HTTP_DELETE];
allowed_methods(?OUTGOING, _Id) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

allowed_methods(?INCOMING, _Id, ?ATTACHMENT) ->
    [?HTTP_GET, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /faxes => []
%%    /faxes/foo => [<<"foo">>]
%%    /faxes/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.

resource_exists() -> 'true'.

resource_exists(?SMTP_LOG) -> 'true';
resource_exists(?INCOMING) -> 'true';
resource_exists(?OUTGOING) -> 'true'.

resource_exists(?SMTP_LOG, _Id) -> 'true';
resource_exists(?INCOMING, _Id) -> 'true';
resource_exists(?OUTGOING, _Id) -> 'true'.

resource_exists(?INCOMING, _Id, ?ATTACHMENT) -> 'true'.

-spec content_types_accepted(cb_context:context()) -> cb_context:context().
content_types_accepted(Context) ->
    maybe_add_types_accepted(Context, cb_context:req_verb(Context)).

-spec content_types_accepted(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_accepted(Context, ?OUTGOING) ->
    maybe_add_types_accepted(Context, cb_context:req_verb(Context));
content_types_accepted(Context, _) -> Context.

-spec maybe_add_types_accepted(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_add_types_accepted(Context, ?HTTP_PUT) ->
    cb_context:add_content_types_accepted(Context, ?ACCEPTED_TYPES);
maybe_add_types_accepted(Context, _) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, ?INCOMING, <<Year:4/binary, Month:2/binary, "-", _/binary>> = FaxId, ?ATTACHMENT) ->
    Ctx = cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    content_types_provided_for_fax(Ctx, FaxId, cb_context:req_verb(Context));
content_types_provided(Context, ?INCOMING, FaxId, ?ATTACHMENT) ->
    content_types_provided_for_fax(Context, FaxId, cb_context:req_verb(Context));
content_types_provided(Context, _, _, _) ->
    Context.

-spec content_types_provided_for_fax(cb_context:context(), ne_binary(), http_method()) ->
                                            cb_context:context().
content_types_provided_for_fax(Context, FaxId, ?HTTP_GET) ->
    Context1 = load_fax_meta(FaxId, Context),
    case cb_context:resp_status(Context1) of
        'success' -> content_types_provided_for_fax(Context1);
        _Status -> Context
    end;
content_types_provided_for_fax(Context, _FaxId, _Verb) ->
    Context.

-spec content_types_provided_for_fax(cb_context:context()) -> cb_context:context().
content_types_provided_for_fax(Context) ->
    case wh_doc:attachment_names(cb_context:doc(Context)) of
        [] -> Context;
        [AttachmentId|_] ->
            CT = wh_doc:attachment_content_type(cb_context:doc(Context), AttachmentId),
            [Type, SubType] = binary:split(CT, <<"/">>),
            cb_context:set_content_types_provided(Context, [{'to_binary', [{Type, SubType}]}])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /faxes mights load a list of fax objects
%% /faxes/123 might load the fax object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().

validate(Context) ->
    create(cb_context:set_account_db(Context, ?WH_FAXES_DB)).

validate(Context, ?OUTGOING) ->
    validate_outgoing_fax(Context, cb_context:req_verb(Context));
validate(Context, ?INCOMING) ->
    incoming_summary(Context);
validate(Context, ?SMTP_LOG) ->
    smtp_summary(Context).

validate_outgoing_fax(Context, ?HTTP_GET) ->
    outgoing_summary(cb_context:set_account_db(Context, ?WH_FAXES_DB));
validate_outgoing_fax(Context, ?HTTP_PUT) ->
    create(cb_context:set_account_db(Context, ?WH_FAXES_DB)).

validate(Context, ?SMTP_LOG, Id) ->
    load_smtp_log_doc(Id, Context);
validate(Context, ?INCOMING, Id) ->
    load_incoming_fax_doc(Id, Context);
validate(Context, ?OUTGOING, Id) ->
    validate_outgoing_fax(Context, Id, cb_context:req_verb(Context)).

validate_outgoing_fax(Context, Id, ?HTTP_GET) ->
    load_outgoing_fax_doc(Id, cb_context:set_account_db(Context, ?WH_FAXES_DB));
validate_outgoing_fax(Context, Id, ?HTTP_POST) ->
    update(Id, cb_context:set_account_db(Context, ?WH_FAXES_DB));
validate_outgoing_fax(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, cb_context:set_account_db(Context, ?WH_FAXES_DB));
validate_outgoing_fax(Context, Id, ?HTTP_DELETE) ->
    read(Id, cb_context:set_account_db(Context, ?WH_FAXES_DB)).

validate(Context, ?INCOMING, Id, ?ATTACHMENT) ->
    validate_incoming_fax_attachment(Context, Id, cb_context:req_verb(Context)).

validate_incoming_fax_attachment(Context, Id, ?HTTP_GET) ->
    load_fax_binary(Id, Context);
validate_incoming_fax_attachment(Context, Id, ?HTTP_DELETE) ->
    load_incoming_fax_doc(Id, Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    maybe_save_attachment(crossbar_doc:save(Context)).
put(Context, ?OUTGOING) ->
    maybe_save_attachment(crossbar_doc:save(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(Context).
post(Context, ?OUTGOING, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PATCH, execute the actual action, usually a db save
%% (after a merge).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context()) -> cb_context:context().
-spec patch(cb_context:context(), path_token(), path_token()) -> cb_context:context().
patch(Context) ->
    crossbar_doc:save(Context).
patch(Context, ?OUTGOING, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?OUTGOING, _Id) ->
    crossbar_doc:delete(Context);
delete(Context, ?INCOMING, _Id) ->
    crossbar_doc:delete(Context).

-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?INCOMING, Id, ?ATTACHMENT) ->
    ANames = wh_doc:attachment_names(cb_context:doc(Context)),
    lists:foldl(fun(AName, Ctx) ->
                        crossbar_doc:delete_attachment(Id, AName, Ctx)
                end
               , Context, ANames).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"faxes">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(<<Year:4/binary, Month:2/binary, "-", _/binary>> = Id, Context) ->
    crossbar_doc:load(Id, cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)));
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

-spec load_incoming_fax_doc(ne_binary(), cb_context:context()) -> cb_context:context().
load_incoming_fax_doc(Id, Context) ->
    read(Id, Context).

-spec load_smtp_log_doc(ne_binary(), cb_context:context()) -> cb_context:context().
load_smtp_log_doc(Id, Context) ->
    read(Id, Context).

-spec load_outgoing_fax_doc(ne_binary(), cb_context:context()) -> cb_context:context().
load_outgoing_fax_doc(Id, Context) ->
    Ctx = read(Id, Context),
    crossbar_util:apply_response_map(Ctx, ?OUTGOING_FAX_DOC_MAP).

-spec get_delivered_date(wh_json:object()) -> api_integer().
get_delivered_date(JObj) ->
    case wh_json:get_value(<<"pvt_delivered_date">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"completed">> -> wh_doc:modified(JObj);
                _ -> 'undefined'
            end;
        Date -> Date
    end.

-spec get_execution_status(ne_binary(), wh_json:object()) -> api_binary().
get_execution_status(Id, JObj) ->
    case wh_json:get_value(<<"pvt_job_status">>, JObj) of
        <<"processing">> = S ->
            case wh_json:get_value(<<"pvt_queue">>, JObj) of
                'undefined' -> S;
                Q -> get_fax_running_status(Id, Q)
            end;
        Status -> Status
    end.

-spec get_fax_running_status(ne_binary(), ne_binary()) -> ne_binary().
get_fax_running_status(Id, Q) ->
    Api = [{<<"Job-ID">>, Id} | wh_api:default_headers(?APP_NAME, ?APP_VERSION)],
    case whapps_util:amqp_pool_request(Api
                                       ,fun(A) -> wapi_fax:publish_query_status(Q, A) end
                                       ,fun wapi_fax:status_v/1
                                      )
    of
        {'ok', JObj } -> wh_json:get_value(<<"Status">>, JObj);
        _ -> <<"not available">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a fax document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_fax_meta(ne_binary(), cb_context:context()) -> cb_context:context().
load_fax_meta(FaxId, Context) ->
    crossbar_doc:load(FaxId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"faxes">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing instance partially with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = cb_context:account_db(Context),
    ResellerId = cb_context:reseller_id(Context),
    AuthDoc = cb_context:auth_doc(Context),
    OwnerId = wh_json:get_value(<<"owner_id">>, AuthDoc),
    Timezone = crossbar_util:get_user_timezone(AccountId, OwnerId),
    JobStatus = initial_job_status(cb_context:req_files(Context)),

    cb_context:set_doc(Context
                       ,wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                                            ,{<<"pvt_job_status">>, JobStatus}
                                            ,{<<"attempts">>, 0}
                                            ,{<<"pvt_account_id">>, AccountId}
                                            ,{<<"pvt_account_db">>, AccountDb}
                                            ,{<<"pvt_reseller_id">>, ResellerId}
                                            ,{<<"fax_timezone">>, Timezone}
                                           ]
                                           ,cb_context:doc(Context)
                                          )
                       );
on_successful_validation(DocId, Context) ->
    maybe_reset_job(crossbar_doc:load_merge(DocId, Context), cb_context:resp_status(Context)).

-spec initial_job_status(req_files()) -> ne_binary().
initial_job_status([]) -> <<"pending">>;
initial_job_status(_) -> <<"attaching_docs">>.

-spec maybe_reset_job(cb_context:context(), crossbar_status()) -> cb_context:context().
maybe_reset_job(Context, 'success') ->
    JObj = cb_context:doc(Context),
    case wh_json:get_value(<<"pvt_job_status">>, JObj) of
        'undefined' -> Context;
        _Else ->
            cb_context:set_doc(Context
                               ,wh_json:set_values([{<<"pvt_job_status">>, <<"pending">>}
                                                    ,{<<"attempts">>, 0}
                                                   ]
                                                   ,JObj
                                                  ))
    end;
maybe_reset_job(Context, _Status) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec incoming_summary(cb_context:context()) -> cb_context:context().
incoming_summary(Context) ->
    JObj = cb_context:doc(Context),
    {View, PreFilter, PostFilter} = get_incoming_view_and_filter(JObj),
    case cb_modules_util:range_modb_view_options(Context, PreFilter, PostFilter) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(View
                                   ,['include_docs' | ViewOptions]
                                   ,Context
                                   ,fun normalize_incoming_view_results/2
                                  );
        Ctx -> Ctx
    end.

-spec get_incoming_view_and_filter(wh_json:object()) ->
          {ne_binary(), api_binaries(), api_binaries()}.
get_incoming_view_and_filter(JObj) ->
    Id = wh_doc:id(JObj),
    case wh_doc:type(JObj) of
        <<"faxbox">> -> {?CB_LIST_BY_FAXBOX, [Id], 'undefined'};
        <<"user">> -> {?CB_LIST_BY_OWNERID, [Id], 'undefined'};
        _Else -> {?CB_LIST_ALL, 'undefined', [wh_json:new()]}
    end.

-spec smtp_summary(cb_context:context()) -> cb_context:context().
smtp_summary(Context) ->
    case cb_modules_util:range_modb_view_options(Context) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(?CB_LIST_SMTP_LOG
                                   ,['include_docs' | ViewOptions]
                                   ,Context
                                   ,fun normalize_view_results/2
                                  );
        Ctx -> Ctx
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a fax doc
%% @end
%%--------------------------------------------------------------------
-spec load_fax_binary(path_token(), cb_context:context()) -> cb_context:context().
load_fax_binary(<<Year:4/binary, Month:2/binary, "-", _/binary>> = FaxId, Context) ->
    do_load_fax_binary(FaxId, cb_context:set_account_modb(Context, wh_util:to_integer(Year), wh_util:to_integer(Month)));
load_fax_binary(FaxId, Context) ->
    do_load_fax_binary(FaxId, Context).

-spec do_load_fax_binary(path_token(), cb_context:context()) -> cb_context:context().
do_load_fax_binary(FaxId, Context) ->
    Context1 = load_fax_meta(FaxId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            case wh_doc:attachment_names(cb_context:doc(Context1)) of
                [] -> cb_context:add_system_error('bad_identifier', wh_json:from_list([{<<"cause">>, FaxId}]), Context1);
                [AttachmentId|_] ->
                    set_fax_binary(Context1, AttachmentId)
            end;
        _Status -> Context1
    end.

-spec set_fax_binary(cb_context:context(), ne_binary()) -> cb_context:context().
set_fax_binary(Context, AttachmentId) ->
    cb_context:setters(crossbar_doc:load_attachment(cb_context:doc(Context), AttachmentId, Context)
                       ,[{fun cb_context:set_resp_etag/2, 'undefined'}
                         ,{fun cb_context:add_resp_headers/2
                           ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
                             ,{<<"Content-Type">>, wh_doc:attachment_content_type(cb_context:doc(Context), AttachmentId)}
                             ,{<<"Content-Length">>, wh_doc:attachment_length(cb_context:doc(Context), AttachmentId)}
                            ]
                          }
                        ]
                      ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec outgoing_summary(cb_context:context()) -> cb_context:context().
outgoing_summary(Context) ->
    JObj = cb_context:doc(Context),
    {View, ViewOptions} =
        case wh_doc:type(JObj) of
            <<"faxbox">> ->
                {?CB_LIST_BY_FAXBOX
                 ,[{'key', wh_doc:id(JObj)}
                   ,'include_docs'
                  ]};
            _Else ->
                {?CB_LIST_BY_ACCOUNT
                 ,[{'startkey', [cb_context:account_id(Context)]}
                   ,{'endkey', [cb_context:account_id(Context), wh_json:new()]}
                   ,'include_docs'
                  ]}
        end,

    crossbar_doc:load_view(View
                           ,ViewOptions
                           ,Context
                           ,fun normalize_view_results/2
                          ).

-spec normalize_view_results(wh_json:object(), wh_json:objects()) ->
                                    wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_incoming_view_results(wh_json:object(), wh_json:objects()) ->
                                             wh_json:objects().
normalize_incoming_view_results(JObj, Acc) ->
    [wh_json:public_fields(wh_json:get_value(<<"doc">>, JObj))|Acc].

-spec maybe_save_attachment(cb_context:context()) -> cb_context:context().
maybe_save_attachment(Context) ->
    maybe_save_attachment(Context, cb_context:req_files(Context)).

-spec maybe_save_attachment(cb_context:context(), req_files()) -> cb_context:context().
maybe_save_attachment(Context, []) -> Context;
maybe_save_attachment(Context, [{Filename, FileJObj} | _Others]) ->
    save_attachment(Context, Filename, FileJObj).

-spec save_attachment(cb_context:context(), binary(), wh_json:object()) -> cb_context:context().
save_attachment(Context, Filename, FileJObj) ->
    JObj = cb_context:doc(Context),
    DocId = wh_doc:id(JObj),
    Contents = wh_json:get_value(<<"contents">>, FileJObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}
            ,{'rev', wh_doc:revision(JObj)}
           ],
    set_pending(crossbar_doc:save_attachment(DocId
                                             ,cb_modules_util:attachment_name(Filename, CT)
                                             ,Contents
                                             ,Context
                                             ,Opts
                                            )
                ,DocId
               ).

-spec set_pending(cb_context:context(), binary()) -> cb_context:context().
set_pending(Context, DocId) ->
    Ctx1 = crossbar_doc:load(DocId, Context),
    KVs = [{<<"pvt_job_status">>, <<"pending">>}],
    crossbar_doc:save(cb_context:set_doc(Ctx1, wh_json:set_values(KVs, cb_context:doc(Ctx1)))).
