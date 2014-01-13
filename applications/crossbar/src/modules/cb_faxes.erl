%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3, allowed_methods/4, allowed_methods/5
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3, resource_exists/4, resource_exists/5
         ,content_types_provided/4, content_types_accepted/1, content_types_accepted/2
         ,validate/1, validate/2, validate/3, validate/4
         ,get/1, get/2, get/3, get/4
         ,put/1, put/2
         ,post/1, post/3
         ,delete/3
		,content_type_to_extension/1 
        ]).

-include("../crossbar.hrl").

-define(OUTGOING, <<"outgoing">>).
-define(INCOMING, <<"incoming">>).

-define(BOXES, <<"boxes">>).
-define(INBOX, <<"inbox">>).
-define(OUTBOX, <<"outbox">>).
-define(SENT, <<"sent">>).

-define(ATTACHMENT, <<"attachment">>).

-define(CB_LIST, <<"media/listing_private_media">>).
-define(FAX_FILE_TYPE, <<"tiff">>).

-define(UPLOAD_MIME_TYPES, [{<<"application">>, <<"json">>}
                           ,{<<"application">>, <<"pdf">>}
                           ,{<<"image">>, <<"tiff">>}
                           ,{<<"multipart">>, <<"form-data">>}
                           ,{<<"multipart">>, <<"mixed">>}
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
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.faxes">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.faxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.faxes">>, ?MODULE, 'get'),
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
-spec allowed_methods(path_token(), path_token(), path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token(), path_token(), path_token()) -> http_methods().

allowed_methods() ->
    [?HTTP_PUT].

allowed_methods(?BOXES) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?INCOMING) ->
    [?HTTP_GET];
allowed_methods(?OUTGOING) ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(?BOXES, _Id) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?INCOMING, _Id) ->
    [?HTTP_GET];
allowed_methods(?OUTGOING, _Id) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?INCOMING, _Id, ?ATTACHMENT) ->
    [?HTTP_GET];
allowed_methods(?BOXES, _Id, ?INBOX) ->
    [?HTTP_GET];
allowed_methods(?BOXES, _Id, ?OUTBOX) ->
    [?HTTP_GET, ?HTTP_PUT];
allowed_methods(?BOXES, _Id, ?SENT) ->
    [?HTTP_GET].

allowed_methods(?BOXES, _Id, ?INBOX, _FaxId) ->
    [?HTTP_GET, ?HTTP_DELETE];
allowed_methods(?BOXES, _Id, ?OUTBOX, _FaxId) ->
    [?HTTP_GET, ?HTTP_DELETE];
allowed_methods(?BOXES, _Id, ?SENT, _FaxId) ->
    [?HTTP_GET, ?HTTP_DELETE].

allowed_methods(?BOXES, _Id, ?INBOX, _FaxId, ?ATTACHMENT) ->
    [?HTTP_GET];
allowed_methods(?BOXES, _Id, ?OUTBOX, _FaxId, ?ATTACHMENT) ->
    [?HTTP_GET];
allowed_methods(?BOXES, _Id, ?SENT, _FaxId, ?ATTACHMENT) ->
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
-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token(), path_token(), path_token()) -> 'true'.

resource_exists() -> 'true'.
resource_exists(?BOXES) -> 'true';
resource_exists(?INCOMING) -> 'true';
resource_exists(?OUTGOING) -> 'true'.
resource_exists(?BOXES, _Id) -> 'true';
resource_exists(?INCOMING, _Id) -> 'true';
resource_exists(?OUTGOING, _Id) -> 'true'.

resource_exists(?INCOMING, _Id, ?ATTACHMENT) -> 'true';
resource_exists(?BOXES, _Id, ?INBOX) -> 'true';
resource_exists(?BOXES, _Id, ?OUTBOX) -> 'true';
resource_exists(?BOXES, _Id, ?SENT) -> 'true'.

resource_exists(?BOXES, _Id, ?INBOX, _FaxId) -> 'true';
resource_exists(?BOXES, _Id, ?OUTBOX, _FaxId) -> 'true';
resource_exists(?BOXES, _Id, ?SENT, _FaxId) -> 'true'.

resource_exists(?BOXES, _Id, ?INBOX, _FaxId, ?ATTACHMENT) -> 'true';
resource_exists(?BOXES, _Id, ?OUTBOX, _FaxId, ?ATTACHMENT) -> 'true';
resource_exists(?BOXES, _Id, ?SENT, _FaxId, ?ATTACHMENT) -> 'true'.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_provided(#cb_context{req_verb = ?HTTP_GET}=Context, ?INCOMING, FaxId, ?ATTACHMENT) ->
    case load_fax_meta(FaxId, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj)) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], JObj),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
            end
    end;
content_types_provided(Context, _, _, _) ->
    Context.


-spec content_types_accepted(#cb_context{}) -> #cb_context{}.
content_types_accepted(#cb_context{}=Context) ->
    Context#cb_context{content_types_accepted = [{'from_binary', ?UPLOAD_MIME_TYPES}]}.

-spec content_types_accepted(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_accepted(#cb_context{}=Context, _MediaID) ->
    Context#cb_context{content_types_accepted = [{'from_binary', ?UPLOAD_MIME_TYPES}]}.

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
-spec validate(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().

validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(Context#cb_context{db_name=?WH_FAXES}).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES) ->
    faxbox_listing(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?OUTGOING) ->
    outgoing_summary(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, ?OUTGOING) ->
    create(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, ?BOXES) ->
    create_faxbox(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?INCOMING) ->
    incoming_summary(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?INCOMING, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?OUTGOING, Id) ->
    read(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES, Id) ->
    read(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_POST}=Context, ?BOXES, Id) ->
    update_faxbox(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, ?BOXES, Id) ->
    read(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_POST}=Context, ?OUTGOING, Id) ->
    update(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, ?OUTGOING, Id) ->
    read(Id, Context#cb_context{db_name=?WH_FAXES}).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?INCOMING, Id, ?ATTACHMENT) ->
    load_fax_binary(Id, Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, ?BOXES, _BoxId, ?OUTBOX) ->
    create(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES,_BoxId, ?OUTBOX) ->
    outgoing_summary(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES, _BoxId, ?INBOX) ->
    incoming_summary(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES, _BoxId, ?SENT) ->
    incoming_summary(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES,_BoxId, ?OUTBOX, _FaxId) ->
    outgoing_summary(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES, _BoxId, ?INBOX, _FaxId) ->
    incoming_summary(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES, _BoxId, ?SENT, _FaxId) ->
    incoming_summary(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES,_BoxId, ?OUTBOX, _FaxId, ?ATTACHMENT) ->
    outgoing_summary(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES, _BoxId, ?INBOX, _FaxId, ?ATTACHMENT) ->
    incoming_summary(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?BOXES, _BoxId, ?SENT, _FaxId, ?ATTACHMENT) ->
    incoming_summary(Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get(cb_context:context()) -> cb_context:context().
-spec get(cb_context:context(), path_token()) -> cb_context:context().
-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec get(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
-spec get(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
-spec get(cb_context:context(), path_token(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().

get(#cb_context{}=Context) ->
    Context.
get(#cb_context{}=Context, _) ->
    Context.
get(#cb_context{}=Context, _, _) ->
    Context.
get(#cb_context{}=Context, _, _, _) ->
    Context.
get(#cb_context{}=Context, _, _, _, _) ->
    Context.
get(#cb_context{}=Context, _, _, _, _, _) ->
    Context.


save_attachment(#cb_context{doc=JObj,req_files=[{Filename, FileObj}],db_name=Db}=Context) ->
    Contents = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    lager:debug("file content type: ~s", [CT]),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
    OldAttachments = wh_json:get_value(<<"_attachments">>, JObj, wh_json:new()),
    Id = wh_json:get_value(<<"_id">>, JObj),
    _ = [couch_mgr:delete_attachment(Db, Id, Attachment)
         || Attachment <- wh_json:get_keys(OldAttachments)
        ],
    crossbar_doc:save_attachment(Id, attachment_name(Filename, CT), Contents, Context, Opts).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%--------------------------------------------------------------------
-spec attachment_name(ne_binary(), ne_binary()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun(A) ->
                          case wh_util:is_empty(A) of
                              true -> wh_util:to_hex_binary(crypto:rand_bytes(16));
                              false -> A
                          end
                  end
                  ,fun(A) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               false -> A;
                               true ->
                                   <<A/binary, ".", (content_type_to_extension(CT))/binary>>
                           end
                   end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, Filename, Generators).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert known media types to extensions
%% @end
%%--------------------------------------------------------------------
-spec content_type_to_extension(ne_binary()) -> ne_binary().
content_type_to_extension(<<"application/pdf">>) -> <<"pdf">>;
content_type_to_extension(<<"image/tiff">>) -> <<"tiff">>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).
put(#cb_context{}=Context, ?OUTGOING) ->
	%lager:debug("conext ~p",[Context]),
    C = crossbar_doc:save(Context),
	save_attachment(C);
put(#cb_context{}=Context, ?BOXES) ->
	crossbar_doc:save(Context).
put(#cb_context{}=Context, ?BOXES, BoxId) ->
	C = crossbar_doc:save(Context),
	save_attachment(C).
put(#cb_context{}=Context, ?BOXES, BoxId, ?OUTBOX) ->
	C = crossbar_doc:save(Context),
	save_attachment(C).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(#cb_context{}=Context) ->
    crossbar_doc:save(Context).
post(#cb_context{}=Context, ?OUTGOING, _) ->
    crossbar_doc:save(Context);
post(#cb_context{}=Context, ?BOXES, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(#cb_context{}=Context, ?OUTGOING, _Id) ->
    crossbar_doc:delete(Context);
delete(#cb_context{}=Context,?BOXES, _BoxId) ->
    crossbar_doc:delete(Context).
delete(#cb_context{}=Context,?BOXES, _BoxId, _Folder, _Id) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"faxes">>, Context, OnSuccess).


-spec create_faxbox(cb_context:context()) -> cb_context:context().
create_faxbox(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_faxbox_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

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
-spec update_faxbox(ne_binary(), cb_context:context()) -> cb_context:context().
update_faxbox(Id, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_faxbox_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_faxbox_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_faxbox_successful_validation('undefined', #cb_context{doc=JObj
                                                ,account_id=AccountId
                                               }=Context) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Context#cb_context{doc=wh_json:set_values([{<<"pvt_type">>, <<"faxbox">>}
                                               ,{<<"pvt_account_id">>, AccountId}
                                               ,{<<"pvt_account_db">>, AccountDb}
                                              ], JObj)};
on_faxbox_successful_validation(DocId, #cb_context{}=Context) ->
    crossbar_doc:load_merge(DocId, Context).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, #cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"faxes">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', #cb_context{doc=JObj
                                                ,account_id=AccountId
                                               }=Context) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Context#cb_context{doc=wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                                               ,{<<"pvt_job_status">>, <<"pending">>}
                                               ,{<<"attempts">>, 0}
                                               ,{<<"pvt_account_id">>, AccountId}
                                               ,{<<"pvt_account_db">>, AccountDb}
                                              ], JObj)};
on_successful_validation(DocId, #cb_context{}=Context) ->
    maybe_reset_job(crossbar_doc:load_merge(DocId, Context)).

maybe_reset_job(#cb_context{resp_status='success'
                            ,doc=JObj
                           }=Context) ->
    case wh_json:get_value(<<"pvt_job_status">>, JObj) of
        'undefined' -> Context;
        _Else ->
            J = wh_json:set_values([{<<"pvt_job_status">>, <<"pending">>}
                                    ,{<<"attempts">>, 0}
                                   ], JObj),
            Context#cb_context{doc=J}
    end;
maybe_reset_job(Context) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec incoming_summary(cb_context:context()) -> cb_context:context().
incoming_summary(#cb_context{}=Context) ->
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
load_fax_binary(FaxId, #cb_context{resp_headers=RespHeaders}=Context) ->
    case load_fax_meta(FaxId, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            FaxMeta = wh_json:get_value([<<"_attachments">>], JObj),
            case wh_json:get_keys(FaxMeta) of
                [] -> cb_context:add_system_error('bad_identifier', [{'details', FaxId}], Context);
                [Attachment|_] ->
                    Context1 = crossbar_doc:load_attachment(JObj, Attachment, Context),
                    Context1#cb_context{resp_headers = [{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
                                                        ,{<<"Content-Type">>, wh_json:get_value([Attachment, <<"content_type">>], FaxMeta)}
                                                        ,{<<"Content-Length">>, wh_json:get_value([Attachment, <<"length">>], FaxMeta)}
                                                        | RespHeaders
                                                       ]
                                        ,resp_etag='undefined'
                                       }
            end;
        Context1 -> Context1
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec faxbox_listing(cb_context:context()) -> cb_context:context().
faxbox_listing(#cb_context{account_id=AccountId}=Context) ->
    ViewOptions=[{'key', AccountId}
                 ,'include_docs'
                ],
    crossbar_doc:load_view(<<"faxboxes/crossbar_listing">>
                           ,ViewOptions
                           ,Context
                           ,fun normalize_view_results/2
                          ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec outgoing_summary(cb_context:context()) -> cb_context:context().
outgoing_summary(#cb_context{account_id=AccountId}=Context) ->
    ViewOptions=[{'key', AccountId}
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
