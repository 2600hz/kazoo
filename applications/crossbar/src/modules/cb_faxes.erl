%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
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
        ,put/1, put/2, put/3
        ,delete/3, delete/4

        ,acceptable_content_types/0
        ]).

-include("crossbar.hrl").

-define(OUTGOING, <<"outgoing">>).
-define(INCOMING, <<"incoming">>).
-define(INBOX, <<"inbox">>).
-define(OUTBOX, <<"outbox">>).
-define(SMTP_LOG, <<"smtplog">>).

-define(ATTACHMENT, <<"attachment">>).

-define(CB_LIST_ALL, <<"faxes/crossbar_listing">>).
-define(CB_LIST_BY_FAXBOX, <<"faxes/list_by_faxbox">>).
-define(CB_LIST_BY_OWNERID, <<"faxes/list_by_ownerid">>).
-define(CB_LIST_BY_FOLDER, <<"faxes/list_by_folder">>).
-define(CB_LIST_BY_ACCOUNT, <<"faxes/list_by_account">>).
-define(CB_LIST_SMTP_LOG, <<"faxes/smtp_log">>).

-define(FAX_FILE_TYPE, <<"tiff">>).

-define(ACCEPTED_MIME_TYPES, [{<<"application">>, <<"json">>}
                             ,{<<"image">>, <<"tiff">>}
                              | ?MULTIPART_CONTENT_TYPES
                              ++ ?PDF_CONTENT_TYPES
                             ]).
-define(ACCEPTED_TYPES, [{'from_binary', ?ACCEPTED_MIME_TYPES}]).

-define(OUTGOING_FAX_DOC_MAP, [{<<"created">>, <<"pvt_created">>}
                              ,{<<"delivered">>, fun get_delivered_date/1}
                              ,{<<"status">>, fun get_execution_status/2}
                              ]).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".fax">>).

-define(FAX_TYPE, <<"fax">>).
-define(SMTP_TYPE, <<"fax_smtp_log">>).

-define(OUTBOX_ACTION_RESUBMIT, <<"resubmit">>).
-define(OUTBOX_ACTIONS, [?OUTBOX_ACTION_RESUBMIT]).

-define(INBOX_ACTION_FORWARD, <<"forward">>).
-define(INBOX_ACTIONS, [?INBOX_ACTION_FORWARD]).

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
    _ = crossbar_bindings:bind(<<"*.execute.delete.faxes">>, ?MODULE, 'delete').

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
allowed_methods(?INBOX) ->
    [?HTTP_GET];
allowed_methods(?INCOMING) ->
    [?HTTP_GET];
allowed_methods(?OUTBOX) ->
    [?HTTP_GET];
allowed_methods(?OUTGOING) ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?SMTP_LOG, _AttemptId) ->
    [?HTTP_GET];
allowed_methods(?INCOMING, _FaxId) ->
    [?HTTP_GET];
allowed_methods(?INBOX, _FaxId) ->
    [?HTTP_GET, ?HTTP_DELETE, ?HTTP_PUT];
allowed_methods(?OUTBOX, _FaxId) ->
    [?HTTP_GET, ?HTTP_DELETE, ?HTTP_PUT];
allowed_methods(?OUTGOING, _FaxJobId) ->
    [?HTTP_GET].

allowed_methods(?INBOX, _FaxId, ?ATTACHMENT) ->
    [?HTTP_GET, ?HTTP_DELETE];
allowed_methods(?OUTBOX, _FaxId, ?ATTACHMENT) ->
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
resource_exists(?INBOX) -> 'true';
resource_exists(?OUTBOX) -> 'true';
resource_exists(?OUTGOING) -> 'true'.

resource_exists(?SMTP_LOG, _Id) -> 'true';
resource_exists(?INCOMING, _Id) -> 'true';
resource_exists(?INBOX, _Id) -> 'true';
resource_exists(?OUTBOX, _Id) -> 'true';
resource_exists(?OUTGOING, _Id) -> 'true'.

resource_exists(?INBOX, _Id, ?ATTACHMENT) -> 'true';
resource_exists(?OUTBOX, _Id, ?ATTACHMENT) -> 'true'.

-spec acceptable_content_types() -> kz_proplist().
acceptable_content_types() ->
    ?ACCEPTED_MIME_TYPES.

-spec content_types_accepted(cb_context:context()) -> cb_context:context().
-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(Context) ->
    maybe_add_types_accepted(Context, cb_context:req_verb(Context)).

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
content_types_provided(Context, ?OUTBOX, ?MATCH_MODB_PREFIX(YYYY,MM,_) = FaxId, ?ATTACHMENT) ->
    Year  = kz_term:to_integer(YYYY),
    Month = kz_term:to_integer(MM),
    Ctx = cb_context:set_account_modb(Context, Year, Month),
    content_types_provided_for_fax(Ctx, FaxId, ?OUTBOX, cb_context:req_verb(Context));
content_types_provided(Context, ?INBOX, ?MATCH_MODB_PREFIX(YYYY,MM,_) = FaxId, ?ATTACHMENT) ->
    Year  = kz_term:to_integer(YYYY),
    Month = kz_term:to_integer(MM),
    Ctx = cb_context:set_account_modb(Context, Year, Month),
    content_types_provided_for_fax(Ctx, FaxId, ?INBOX, cb_context:req_verb(Context));
content_types_provided(Context, ?INBOX, FaxId, ?ATTACHMENT) ->
    content_types_provided_for_fax(Context, FaxId, ?INBOX, cb_context:req_verb(Context));
content_types_provided(Context, ?OUTBOX, FaxId, ?ATTACHMENT) ->
    content_types_provided_for_fax(Context, FaxId, ?OUTBOX, cb_context:req_verb(Context));
content_types_provided(Context, _, _, _) ->
    Context.

-spec content_types_provided_for_fax(cb_context:context(), ne_binary(), ne_binary(), http_method()) ->
                                            cb_context:context().
content_types_provided_for_fax(Context, FaxId, Folder, ?HTTP_GET) ->
    Context1 = load_fax_meta(FaxId, Folder, Context),
    case cb_context:resp_status(Context1) of
        'success' -> content_types_provided_for_fax(Context1);
        _Status -> Context
    end;
content_types_provided_for_fax(Context, _FaxId, _Folder, _Verb) ->
    Context.

-spec content_types_provided_for_fax(cb_context:context()) -> cb_context:context().
content_types_provided_for_fax(Context) ->
    case kz_doc:attachment_names(cb_context:doc(Context)) of
        [] -> Context;
        [AttachmentId|_] ->
            CT = kz_doc:attachment_content_type(cb_context:doc(Context), AttachmentId),
            [Type, SubType] = binary:split(CT, <<"/">>),
            lager:debug("found attachement of content type: ~s/~s~n", [Type, SubType]),
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
    create(cb_context:set_account_db(Context, ?KZ_FAXES_DB)).

validate(Context, ?OUTGOING) ->
    validate_outgoing_fax(Context, cb_context:req_verb(Context));
validate(Context, ?INCOMING) ->
    inbox_summary(Context);
validate(Context, ?INBOX) ->
    inbox_summary(Context);
validate(Context, ?OUTBOX) ->
    outbox_summary(Context);
validate(Context, ?SMTP_LOG) ->
    load_smtp_log(Context).

validate(Context, ?SMTP_LOG, Id) ->
    load_smtp_log_doc(Id, Context);
validate(Context, ?INCOMING, Id) ->
    load_modb_fax_doc(Id, ?INCOMING, Context);
validate(Context, ?INBOX, Id) ->
    validate_inbox_fax(Context, Id, cb_context:req_verb(Context));
validate(Context, ?OUTBOX, Id) ->
    validate_outbox_fax(Context, Id, cb_context:req_verb(Context));
validate(Context, ?OUTGOING, Id) ->
    validate_outgoing_fax(Context, Id, cb_context:req_verb(Context)).

validate(Context, ?INBOX, Id, ?ATTACHMENT) ->
    validate_modb_fax_attachment(Context, Id, ?INBOX, cb_context:req_verb(Context));
validate(Context, ?OUTBOX, Id, ?ATTACHMENT) ->
    validate_modb_fax_attachment(Context, Id, ?OUTBOX, cb_context:req_verb(Context)).

-spec validate_outgoing_fax(cb_context:context(), http_method()) -> cb_context:context().
validate_outgoing_fax(Context, ?HTTP_GET) ->
    outgoing_summary(cb_context:set_account_db(Context, ?KZ_FAXES_DB));
validate_outgoing_fax(Context, ?HTTP_PUT) ->
    create(cb_context:set_account_db(Context, ?KZ_FAXES_DB)).

-spec validate_outgoing_fax(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_outgoing_fax(Context, Id, ?HTTP_GET) ->
    load_outgoing_fax_doc(Id, cb_context:set_account_db(Context, ?KZ_FAXES_DB)).

-spec validate_outbox_fax(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_outbox_fax(Context, Id, ?HTTP_PUT) ->
    Action = kz_json:get_value(<<"action">>, cb_context:req_json(Context)),
    validate_outbox_fax_action(Action, Id, Context);
validate_outbox_fax(Context, Id, _) ->
    load_modb_fax_doc(Id, ?OUTBOX, Context).

-spec validate_inbox_fax(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_inbox_fax(Context, Id, ?HTTP_PUT) ->
    Action = kz_json:get_value(<<"action">>, cb_context:req_json(Context)),
    validate_inbox_fax_action(Action, Id, Context);
validate_inbox_fax(Context, Id, _) ->
    load_modb_fax_doc(Id, ?INBOX, Context).

-spec validate_modb_fax_attachment(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_modb_fax_attachment(Context, Id, Folder, ?HTTP_GET) ->
    load_fax_binary(Id, Folder, Context);
validate_modb_fax_attachment(Context, Id, Folder, ?HTTP_DELETE) ->
    load_modb_fax_doc(Id, Folder, Context).

-spec validate_outbox_fax_action(api_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
validate_outbox_fax_action('undefined', _Id, Context) ->
    cb_context:add_system_error(<<"action required">>, Context);
validate_outbox_fax_action(Action, Id, Context) ->
    case lists:member(Action, ?OUTBOX_ACTIONS) of
        'true' -> load_modb_fax_doc(Id, ?OUTBOX, Context);
        'false' -> cb_context:add_system_error(<<"invalid action">>, Context)
    end.

-spec validate_inbox_fax_action(api_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
validate_inbox_fax_action('undefined', _Id, Context) ->
    cb_context:add_system_error(<<"action required">>, Context);
validate_inbox_fax_action(Action, Id, Context) ->
    case lists:member(Action, ?INBOX_ACTIONS) of
        'true' -> load_modb_fax_doc(Id, ?INBOX, Context);
        'false' -> cb_context:add_system_error(<<"invalid action">>, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context) ->
    maybe_save_attachment(crossbar_doc:save(Context)).
put(Context, ?OUTGOING) ->
    maybe_save_attachment(crossbar_doc:save(Context)).
put(Context, ?OUTBOX, Id) ->
    Action = kz_json:get_value(<<"action">>, cb_context:req_json(Context)),
    do_put_action(Context, ?OUTBOX, Action, Id);
put(Context, ?INBOX, Id) ->
    Action = kz_json:get_value(<<"action">>, cb_context:req_json(Context)),
    do_put_action(Context, ?INBOX, Action, Id).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _, _Id) ->
    crossbar_doc:delete(Context).

-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, _, Id, ?ATTACHMENT) ->
    ANames = kz_doc:attachment_names(cb_context:doc(Context)),
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
    FaxBoxDoc = cb_context:fetch(Context, <<"faxbox">>),
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"faxes">>, maybe_add_faxbox_data(FaxBoxDoc, Context), OnSuccess).

maybe_add_faxbox_data('undefined', Context) -> Context;
maybe_add_faxbox_data(FaxBoxDoc, Context) ->
    Props = props:filter_undefined(
              [{<<"from_name">>, kz_json:get_value(<<"caller_name">>, FaxBoxDoc)}
              ,{<<"fax_identity_name">>, kz_json:get_value(<<"fax_header">>, FaxBoxDoc)}
              ,{<<"from_number">>, kz_json:get_value(<<"caller_id">>, FaxBoxDoc)}
              ,{<<"fax_identity_number">>, kz_json:get_value(<<"fax_identity">>, FaxBoxDoc)}
              ,{<<"fax_timezone">>, kzd_fax_box:timezone(FaxBoxDoc)}
              ,{<<"retries">>, kzd_fax_box:retries(FaxBoxDoc, 3)}
              ,{<<"faxbox_id">>, kz_doc:id(FaxBoxDoc)}
              ]),
    ReqData = lists:foldl(fun({K,V}, Acc) ->
                                  case kz_json:get_value(K, Acc) of
                                      'undefined' -> kz_json:set_value(K, V, Acc);
                                      _ -> Acc
                                  end
                          end
                         ,cb_context:req_data(Context)
                         ,Props),
    cb_context:set_req_data(Context, ReqData).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
read(?MATCH_MODB_PREFIX(YYYY,MM,_) = Id, Type, Context) ->
    Year  = kz_term:to_integer(YYYY),
    Month = kz_term:to_integer(MM),
    crossbar_doc:load({Type, Id}, cb_context:set_account_modb(Context, Year, Month), ?TYPE_CHECK_OPTION(Type));
read(Id, Type, Context) ->
    crossbar_doc:load({Type, Id}, Context, ?TYPE_CHECK_OPTION(Type)).

-spec load_modb_fax_doc(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
load_modb_fax_doc(Id, Folder, Context) ->
    validate_fax_doc_folder(Folder, read(Id, ?FAX_TYPE, Context)).

-spec validate_fax_doc_folder(ne_binary(), cb_context:context()) -> cb_context:context().
validate_fax_doc_folder(Folder, Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            DocFolder = kz_json:get_value(<<"folder">>, cb_context:doc(Context)),
            validate_fax_doc_folder(Folder, DocFolder, Context);
        _ ->
            Context
    end.

-spec validate_fax_doc_folder(ne_binary(), api_binary(), cb_context:context()) -> cb_context:context().
validate_fax_doc_folder(Folder, Folder, Context) ->
    Context;
validate_fax_doc_folder(Folder, _DocFolder, Context) ->
    cb_context:add_system_error(<<"document is not in folder ", Folder/binary>>, Context).

-spec load_smtp_log_doc(ne_binary(), cb_context:context()) -> cb_context:context().
load_smtp_log_doc(Id, Context) ->
    read(Id, ?SMTP_TYPE, Context).

-spec load_outgoing_fax_doc(ne_binary(), cb_context:context()) -> cb_context:context().
load_outgoing_fax_doc(Id, Context) ->
    Ctx = read(Id, ?FAX_TYPE, Context),
    crossbar_util:apply_response_map(Ctx, ?OUTGOING_FAX_DOC_MAP).

-spec get_delivered_date(kz_json:object()) -> api_integer().
get_delivered_date(JObj) ->
    case kz_json:get_value(<<"pvt_delivered_date">>, JObj) of
        'undefined' ->
            case kz_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"completed">> -> kz_doc:modified(JObj);
                _ -> 'undefined'
            end;
        Date -> Date
    end.

-spec get_execution_status(ne_binary(), kz_json:object()) -> api_binary().
get_execution_status(Id, JObj) ->
    case kz_json:get_value(<<"pvt_job_status">>, JObj) of
        <<"processing">> = S ->
            case kz_json:get_value(<<"pvt_queue">>, JObj) of
                'undefined' -> S;
                Q -> get_fax_running_status(Id, Q)
            end;
        Status -> Status
    end.

-spec get_fax_running_status(ne_binary(), ne_binary()) -> ne_binary().
get_fax_running_status(Id, Q) ->
    Api = [{<<"Job-ID">>, Id} | kz_api:default_headers(?APP_NAME, ?APP_VERSION)],
    case kapps_util:amqp_pool_request(Api
                                     ,fun(A) -> kapi_fax:publish_query_status(Q, A) end
                                     ,fun kapi_fax:status_v/1
                                     )
    of
        {'ok', JObj } -> kz_json:get_value(<<"Status">>, JObj);
        _ -> <<"not available">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a fax document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_fax_meta(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
load_fax_meta(FaxId, Folder, Context) ->
    validate_fax_doc_folder(Folder, crossbar_doc:load({<<"fax">>, FaxId}, Context, ?TYPE_CHECK_OPTION(<<"fax">>))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    case cb_context:req_files(Context) =:= []
        andalso kzd_fax:document_url(cb_context:doc(Context)) =:= 'undefined'
    of
        'true' ->
            Property = [<<"document">>,<<"url">>],
            Code = <<"required">>,
            Message = <<"add document url or upload a document">>,
            cb_context:add_validation_error(Property, Code, Message, Context);
        'false' -> verify_number(Context)
    end.

-spec verify_number(cb_context:context()) -> cb_context:context().
verify_number(Context) ->
    AccountId = cb_context:account_id(Context),
    Number = kzd_fax:to_number(cb_context:doc(Context)),
    case knm_converters:is_reconcilable(Number, AccountId) of
        'true' ->
            NormalizedNumber = knm_converters:normalize(Number, AccountId),
            Doc = kz_json:set_value(<<"to_number">>, NormalizedNumber, cb_context:doc(Context)),
            on_success(cb_context:set_doc(Context, Doc));
        'false' ->
            Property = [<<"document">>,<<"to_number">>],
            Code = <<"required">>,
            Message = <<"add a valid number to send the fax to">>,
            cb_context:add_validation_error(Property, Code, Message, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_success(cb_context:context()) -> cb_context:context().
on_success(Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = cb_context:account_db(Context),
    ResellerId = cb_context:reseller_id(Context),
    AuthDoc = cb_context:auth_doc(Context),
    OwnerId = kz_json:get_value(<<"owner_id">>, AuthDoc),
    Timezone = crossbar_util:get_user_timezone(AccountId, OwnerId),
    JobStatus = initial_job_status(cb_context:req_files(Context)),

    cb_context:set_doc(Context
                      ,kz_json:set_values([{<<"pvt_type">>, <<"fax">>}
                                          ,{<<"pvt_job_status">>, JobStatus}
                                          ,{<<"pvt_created">>, kz_time:current_tstamp()}
                                          ,{<<"pvt_modified">>, kz_time:current_tstamp()}
                                          ,{<<"attempts">>, 0}
                                          ,{<<"pvt_account_id">>, AccountId}
                                          ,{<<"pvt_account_db">>, AccountDb}
                                          ,{<<"pvt_reseller_id">>, ResellerId}
                                          ,{<<"fax_timezone">>, Timezone}
                                          ]
                                         ,cb_context:doc(Context)
                                         )
                      ).

-spec initial_job_status(req_files()) -> ne_binary().
initial_job_status([]) -> <<"pending">>;
initial_job_status(_) -> <<"attaching_files">>.

-spec get_filter_doc(cb_context:context()) -> kz_json:object().
get_filter_doc(Context) ->
    case cb_context:fetch(Context, <<"faxbox">>) of
        'undefined' -> maybe_user_filter_doc(Context);
        JObj -> JObj
    end.

-spec maybe_user_filter_doc(cb_context:context()) -> kz_json:object().
maybe_user_filter_doc(Context) ->
    case cb_context:user_id(Context) of
        'undefined' -> kz_json:new();
        UserId ->
            Options = [{'id', UserId}
                      ,{'type', <<"user">>}
                      ],
            kz_doc:update_pvt_parameters(kz_json:new(), 'undefined', Options)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec fax_modb_summary(cb_context:context(), ne_binary()) -> cb_context:context().
fax_modb_summary(Context, Folder) ->
    JObj = get_filter_doc(Context),
    {View, PreFilter, PostFilter} = get_incoming_view_and_filter(JObj, Folder),
    case cb_modules_util:range_modb_view_options(Context, PreFilter, PostFilter) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(View
                                  ,['include_docs' | ViewOptions]
                                  ,cb_context:set_resp_status(Context, 'success')
                                  ,fun normalize_modb_view_results/2
                                  );
        Ctx -> Ctx
    end.

-spec inbox_summary(cb_context:context()) -> cb_context:context().
inbox_summary(Context) -> fax_modb_summary(Context, ?INBOX).

-spec outbox_summary(cb_context:context()) -> cb_context:context().
outbox_summary(Context) -> fax_modb_summary(Context, ?OUTBOX).

-spec get_incoming_view_and_filter(kz_json:object(), ne_binary()) ->
                                          {ne_binary(), api_binaries(), api_binaries()}.
get_incoming_view_and_filter(JObj, Folder) ->
    Id = kz_doc:id(JObj),
    case kz_doc:type(JObj) of
        <<"faxbox">> -> {?CB_LIST_BY_FAXBOX, [Id, Folder], 'undefined'};
        <<"user">> -> {?CB_LIST_BY_OWNERID, [Id, Folder], 'undefined'};
        _Else -> {?CB_LIST_BY_FOLDER, [Folder], [kz_json:new()]}
    end.

-spec load_smtp_log(cb_context:context()) -> cb_context:context().
load_smtp_log(Context) ->
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
-spec load_fax_binary(path_token(), path_token(), cb_context:context()) -> cb_context:context().
load_fax_binary(?MATCH_MODB_PREFIX(Year,Month,_) = FaxId, Folder, Context) ->
    do_load_fax_binary(FaxId, Folder, cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month)));
load_fax_binary(FaxId, Folder, Context) ->
    do_load_fax_binary(FaxId, Folder, Context).

-spec do_load_fax_binary(path_token(), path_token(), cb_context:context()) -> cb_context:context().
do_load_fax_binary(FaxId, Folder, Context) ->
    Context1 = load_fax_meta(FaxId, Folder, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            case kz_doc:attachment_names(cb_context:doc(Context1)) of
                [] -> cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, FaxId}]), Context1);
                [AttachmentId|_] ->
                    set_fax_binary(Context1, AttachmentId)
            end;
        _Status -> Context1
    end.

-spec set_fax_binary(cb_context:context(), ne_binary()) -> cb_context:context().
set_fax_binary(Context, AttachmentId) ->
    Disposition = cb_context:req_param(Context, <<"disposition">>, <<"attachment">>),
    cb_context:setters(crossbar_doc:load_attachment(cb_context:doc(Context), AttachmentId, ?TYPE_CHECK_OPTION(<<"fax">>), Context)
                      ,[{fun cb_context:set_resp_etag/2, 'undefined'}
                       ,{fun cb_context:add_resp_headers/2
                        ,[{<<"Content-Disposition">>, <<Disposition/binary, "; filename=", AttachmentId/binary>>}
                         ,{<<"Content-Type">>, kz_doc:attachment_content_type(cb_context:doc(Context), AttachmentId)}
                         ,{<<"Content-Length">>, kz_doc:attachment_length(cb_context:doc(Context), AttachmentId)}
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
    JObj = cb_context:fetch(Context, <<"faxbox">>, kz_json:new()),
    {View, ViewOptions} =
        case kz_doc:type(JObj) of
            <<"faxbox">> ->
                {?CB_LIST_BY_FAXBOX
                ,[{'key', kz_doc:id(JObj)}
                 ,'include_docs'
                 ]};
            _Else ->
                {?CB_LIST_BY_ACCOUNT
                ,[{'startkey', [cb_context:account_id(Context)]}
                 ,{'endkey', [cb_context:account_id(Context), kz_json:new()]}
                 ,'include_docs'
                 ]}
        end,

    crossbar_doc:load_view(View
                          ,ViewOptions
                          ,Context
                          ,fun normalize_view_results/2
                          ).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_modb_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_modb_view_results(JObj, Acc) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    View = kz_json:get_value(<<"value">>, JObj),
    [kz_doc:public_fields(kz_json:merge_jobjs(View, Doc))|Acc].

-spec maybe_save_attachment(cb_context:context()) -> cb_context:context().
maybe_save_attachment(Context) ->
    maybe_save_attachment(Context, cb_context:req_files(Context)).

-spec maybe_save_attachment(cb_context:context(), req_files()) -> cb_context:context().
maybe_save_attachment(Context, []) -> Context;
maybe_save_attachment(Context, [{Filename, FileJObj} | _Others]) ->
    save_attachment(Context, Filename, FileJObj).

-spec save_attachment(cb_context:context(), binary(), kz_json:object()) -> cb_context:context().
save_attachment(Context, Filename, FileJObj) ->
    JObj = cb_context:doc(Context),
    DocId = kz_doc:id(JObj),
    Contents = kz_json:get_value(<<"contents">>, FileJObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    Opts = [{'content_type', CT}
           ,{'rev', kz_doc:revision(JObj)}
            | ?TYPE_CHECK_OPTION(<<"fax">>)
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
    KVs = [{<<"pvt_job_status">>, <<"pending">>}
          ,{<<"pvt_modified">>, kz_time:current_tstamp()}
          ],
    crossbar_doc:save(cb_context:set_doc(Ctx1, kz_json:set_values(KVs, cb_context:doc(Ctx1)))).

-spec do_put_action(cb_context:context(), ne_binary(), ne_binary(), ne_binary()) -> cb_context:context().
do_put_action(Context, ?OUTBOX, ?OUTBOX_ACTION_RESUBMIT, Id) ->
    ReqData = kz_doc:public_fields(cb_context:req_data(Context)),
    Fun = fun(_Source, Target) -> set_resubmit_data(kz_json:merge_jobjs(ReqData, Target)) end,
    Options = [{'transform', Fun}],
    FromDB = cb_context:account_db(Context),
    NewId = kz_binary:rand_hex(16),
    lager:debug("copying ~s/~s to ~s/~s", [FromDB, Id, ?KZ_FAXES_DB, NewId]),
    case kz_datamgr:copy_doc(FromDB, {?FAX_TYPE, Id}, ?KZ_FAXES_DB, NewId, Options) of
        {'ok', _Doc} ->
            Updates = [{<<"pvt_job_status">>, <<"pending">>}],
            {'ok', UpdatedDoc} = kz_datamgr:update_doc(?KZ_FAXES_DB, NewId, Updates),
            cb_context:set_resp_data(Context, kz_doc:public_fields(UpdatedDoc));
        {'error', Error} ->
            lager:error("error resubmitting fax : ~p", [Error]),
            cb_context:add_system_error(<<"error when resubmitting fax">>, Context)
    end;
do_put_action(Context, ?INBOX, ?INBOX_ACTION_FORWARD, Id) ->
    ReqData = kz_doc:public_fields(cb_context:req_data(Context)),
    Fun = fun(_Source, Target) -> set_forward_data(kz_json:merge_jobjs(ReqData, Target)) end,
    Options = [{'transform', Fun}],
    FromDB = cb_context:account_db(Context),
    NewId = kz_binary:rand_hex(16),
    lager:debug("copying ~s/~s to ~s/~s", [FromDB, Id, ?KZ_FAXES_DB, NewId]),
    case kz_datamgr:copy_doc(FromDB, {?FAX_TYPE, Id}, ?KZ_FAXES_DB, NewId, Options) of
        {'ok', _Doc} ->
            Updates = [{<<"pvt_job_status">>, <<"pending">>}],
            {'ok', UpdatedDoc} = kz_datamgr:update_doc(?KZ_FAXES_DB, NewId, Updates),
            cb_context:set_resp_data(Context, kz_doc:public_fields(UpdatedDoc));
        {'error', Error} ->
            lager:error("error resubmitting fax : ~p", [Error]),
            cb_context:add_system_error(<<"error when forwarding fax">>, Context)
    end.

-spec set_resubmit_data(kz_json:object()) -> kz_json:object().
set_resubmit_data(TargetDoc) ->
    Keys = [<<"tx_result">>
           ,<<"retry_after">>
           ,<<"pvt_job_node">>
           ],
    Values = [{<<"pvt_created">>, kz_time:current_tstamp()}
             ,{<<"pvt_modified">>, kz_time:current_tstamp()}
             ,{<<"pvt_job_status">>, <<"resubmitting">>}
             ,{<<"attempts">>, 0}
             ],
    kz_json:set_values(Values, kz_json:delete_keys(Keys, TargetDoc)).

-spec set_forward_data(kz_json:object()) -> kz_json:object().
set_forward_data(TargetDoc) ->
    Keys = [<<"tx_result">>
           ,<<"rx_result">>
           ,<<"retry_after">>
           ,<<"pvt_job_node">>
           ],
    Values = [{<<"pvt_created">>, kz_time:current_tstamp()}
             ,{<<"pvt_modified">>, kz_time:current_tstamp()}
             ,{<<"pvt_job_status">>, <<"resubmitting">>}
             ,{<<"attempts">>, 0}
             ],
    kz_json:set_values(Values, kz_json:delete_keys(Keys, TargetDoc)).
