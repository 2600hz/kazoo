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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_provided/4
         ,validate/1, validate/2, validate/3, validate/4
         ,get/1, get/2, get/3, get/4
         ,put/1, put/2
         ,post/1, post/3
         ,delete/3
        ]).

-include("../crossbar.hrl").

-define(OUTGOING, <<"outgoing">>).
-define(INCOMING, <<"incoming">>).

-define(ATTACHMENT, <<"attachment">>).

-define(CB_LIST, <<"media/listing_private_media">>).
-define(FAX_FILE_TYPE, <<"tiff">>).

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

validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    create(Context#cb_context{db_name=?WH_FAXES}).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?OUTGOING) ->
    outgoing_summary(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, ?OUTGOING) ->
    create(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?INCOMING) ->
    incoming_summary(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?INCOMING, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?OUTGOING, Id) ->
    read(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_POST}=Context, ?OUTGOING, Id) ->
    update(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, ?OUTGOING, Id) ->
    read(Id, Context#cb_context{db_name=?WH_FAXES}).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?INCOMING, Id, ?ATTACHMENT) ->
    load_fax_binary(Id, Context).

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

get(#cb_context{}=Context) ->
    Context.
get(#cb_context{}=Context, _) ->
    Context.
get(#cb_context{}=Context, _, _) ->
    Context.
get(#cb_context{}=Context, _, _, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).
put(#cb_context{}=Context, ?OUTGOING) ->
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
post(#cb_context{}=Context) ->
    crossbar_doc:save(Context).
post(#cb_context{}=Context, ?OUTGOING, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(#cb_context{}=Context, ?OUTGOING, _Id) ->
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
