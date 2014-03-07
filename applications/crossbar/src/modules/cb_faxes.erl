%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
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
         ,validate/1, validate/2, validate/3, validate/4
         ,put/1, put/2
         ,post/1, post/3
         ,delete/3
        ,get_delivered_date/1
        ]).

-include("../crossbar.hrl").

-define(OUTGOING, <<"outgoing">>).
-define(INCOMING, <<"incoming">>).

-define(ATTACHMENT, <<"attachment">>).

-define(CB_LIST, <<"media/listing_private_media">>).
-define(FAX_FILE_TYPE, <<"tiff">>).

-define(OUTGOING_FAX_DOC_MAP, [{<<"created">>,<<"pvt_created">>},
                               {<<"delivered">>, fun get_delivered_date/1}
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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.faxes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.faxes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.faxes">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.faxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.faxes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.faxes">>, ?MODULE, 'post'),
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

allowed_methods(?INCOMING) ->
    [?HTTP_GET];
allowed_methods(?OUTGOING) ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?INCOMING, _Id) ->
    [?HTTP_GET];
allowed_methods(?OUTGOING, _Id) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?INCOMING, _Id, ?ATTACHMENT) ->
    [?HTTP_GET].

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
resource_exists(?INCOMING) -> 'true';
resource_exists(?OUTGOING) -> 'true'.
resource_exists(?INCOMING, _Id) -> 'true';
resource_exists(?OUTGOING, _Id) -> 'true'.
resource_exists(?INCOMING, _Id, ?ATTACHMENT) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, ?INCOMING, FaxId, ?ATTACHMENT) ->
    content_types_provided_for_fax(Context, FaxId, cb_context:req_verb(Context));
content_types_provided(Context, _, _, _) ->
    Context.

content_types_provided_for_fax(Context, FaxId, ?HTTP_GET) ->
    Context1 = load_fax_meta(FaxId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], cb_context:doc(Context1))) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], cb_context:doc(Context1)),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    cb_context:set_content_types_provided(Context, [{'to_binary', [{Type, SubType}]}])
            end;
        _Status -> Context
    end;
content_types_provided_for_fax(Context, _FaxId, _Verb) ->
    Context.

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
    create(cb_context:set_account_db(Context, ?WH_FAXES)).

validate(Context, ?OUTGOING) ->
    validate_outgoing_fax(Context, cb_context:req_verb(Context));
validate(Context, ?INCOMING) ->
    incoming_summary(Context).

validate_outgoing_fax(Context, ?HTTP_GET) ->
    outgoing_summary(cb_context:set_account_db(Context, ?WH_FAXES));
validate_outgoing_fax(Context, ?HTTP_PUT) ->
    create(cb_context:set_account_db(Context, ?WH_FAXES)).

validate(Context, ?INCOMING, Id) ->
    load_incoming_fax_doc(Id, Context);
validate(Context, ?OUTGOING, Id) ->
    validate_outgoing_fax(Context, Id, cb_context:req_verb(Context)).

validate_outgoing_fax(Context, Id, ?HTTP_GET) ->
    load_outgoing_fax_doc(Id, cb_context:set_account_db(Context, ?WH_FAXES));
validate_outgoing_fax(Context, Id, ?HTTP_POST) ->
    update(Id, cb_context:set_account_db(Context, ?WH_FAXES));
validate_outgoing_fax(Context, Id, ?HTTP_DELETE) ->
    read(Id, cb_context:set_account_db(Context, ?WH_FAXES)).

validate(Context, ?INCOMING, Id, ?ATTACHMENT) ->
    load_fax_binary(Id, Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).
put(Context, ?OUTGOING) ->
    crossbar_doc:save(Context).

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
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, ?OUTGOING, _Id) ->
    crossbar_doc:delete(Context).

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
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

-spec load_incoming_fax_doc(ne_binary(), cb_context:context()) -> cb_context:context().
load_incoming_fax_doc(Id, Context) ->
    read(Id, Context).

-spec load_outgoing_fax_doc(ne_binary(), cb_context:context()) -> cb_context:context().
load_outgoing_fax_doc(Id, Context) ->
    Ctx = read(Id, Context),
    crossbar_util:apply_response_map(Ctx, ?OUTGOING_FAX_DOC_MAP).


-spec get_delivered_date(wh_json:object()) -> any().
get_delivered_date(JObj) ->
    case wh_json:get_value(<<"pvt_delivered_date">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"completed">> -> wh_json:get_value(<<"pvt_modified">>, JObj);
                _ -> 'undefined'
            end;
        Date -> Date
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
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    AuthDoc = cb_context:auth_doc(Context),
    OwnerId = wh_json:get_value(<<"owner_id">>, AuthDoc),
    Timezone = crossbar_util:get_user_timezone(AccountId, OwnerId),
			
    cb_context:set_doc(Context
                       ,wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                                            ,{<<"pvt_job_status">>, <<"pending">>}
                                            ,{<<"attempts">>, 0}
                                            ,{<<"pvt_account_id">>, AccountId}
                                            ,{<<"pvt_account_db">>, AccountDb}
                                            ,{<<"fax_timezone">>, Timezone}
                                           ], cb_context:doc(Context))
                       );
on_successful_validation(DocId, Context) ->
    maybe_reset_job(crossbar_doc:load_merge(DocId, Context), cb_context:resp_status(Context)).

maybe_reset_job(Context, 'success') ->
    JObj = cb_context:doc(Context),
    case wh_json:get_value(<<"pvt_job_status">>, JObj) of
        'undefined' -> Context;
        _Else ->
            cb_context:set_doc(Context
                               ,wh_json:set_values([{<<"pvt_job_status">>, <<"pending">>}
                                                    ,{<<"attempts">>, 0}
                                                   ], JObj))
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
    crossbar_doc:load_view(?CB_LIST
                           ,[{'startkey', [?FAX_FILE_TYPE]}
                             ,{'endkey', [?FAX_FILE_TYPE, wh_json:new()]}
                             ,'include_docs'
                            ]
                           ,Context
                           ,fun normalize_incoming_view_results/2
                          ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a fax doc
%% @end
%%--------------------------------------------------------------------
-spec load_fax_binary(path_token(), cb_context:context()) -> cb_context:context().
load_fax_binary(FaxId, Context) ->
    Context1 = load_fax_meta(FaxId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            FaxMeta = wh_json:get_value([<<"_attachments">>], JObj),
            case wh_json:get_keys(FaxMeta) of
                [] -> cb_context:add_system_error('bad_identifier', [{'details', FaxId}], Context1);
                [Attachment|_] ->
                    cb_context:set_resp_etag(
                      cb_context:set_resp_headers(crossbar_doc:load_attachment(JObj, Attachment, Context1)
                                                  ,[{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
                                                    ,{<<"Content-Type">>, wh_json:get_value([Attachment, <<"content_type">>], FaxMeta)}
                                                    ,{<<"Content-Length">>, wh_json:get_value([Attachment, <<"length">>], FaxMeta)}
                                                    | cb_context:resp_headers(Context)
                                                   ])
                      ,'undefined'
                     )
            end;
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec outgoing_summary(cb_context:context()) -> cb_context:context().
outgoing_summary(Context) ->
    ViewOptions=[{'key', cb_context:account_id(Context)}
                 ,'include_docs'
                ],
    crossbar_doc:load_view(<<"faxes/crossbar_listing">>
                           ,ViewOptions
                           ,Context
                           ,fun normalize_view_results/2
                          ).

-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_incoming_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_incoming_view_results(JObj, Acc) ->
    [wh_json:public_fields(wh_json:get_value(<<"doc">>, JObj))|Acc].
