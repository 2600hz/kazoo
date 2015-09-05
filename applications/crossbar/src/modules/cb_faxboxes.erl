 %%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% Fax Box API
%%%
%%% @end
%%% @contributors:
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cb_faxboxes).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,patch/2
         ,delete/2
        ]).

-include("../crossbar.hrl").
-include_lib("kazoo_oauth/include/kazoo_oauth_types.hrl").

-define(CB_LIST, <<"faxbox/crossbar_listing">>).

-define(GPC_URL, "https://www.google.com/cloudprint/").
-define(GPC_URL_REGISTER, <<?GPC_URL,"register">>).
-define(GPC_PROXY, <<"kazoo-cloud-fax-printer-proxy">>).
-define(GPC_PROXY_HEADER,{"X-CloudPrint-Proxy","kazoo-cloud-fax-printer-proxy"}).
-define(DEFAULT_FAX_SMTP_DOMAIN, <<"fax.kazoo.io">>).

-define(CLOUD_STATE_FIELD, <<"pvt_cloud_state">>).
-define(CLOUD_CLAIM_URL_FIELD, <<"pvt_cloud_connector_claim_url">>).
-define(CLOUD_PRINTER_ID_FIELD, <<"pvt_cloud_printer_id">>).
-define(SMTP_EMAIL_FIELD, <<"pvt_smtp_email_address">>).

-define(LEAKED_FIELDS, [?CLOUD_STATE_FIELD
                        ,?CLOUD_CLAIM_URL_FIELD
                        ,?CLOUD_PRINTER_ID_FIELD
                        ,?SMTP_EMAIL_FIELD
                       ]).

-define(CLOUD_PROPERTIES, [<<"printer">>
                           ,<<"default_display_name">>
                           ,<<"manufacturer">>
                           ,<<"model">>
                           ,<<"setup_url">>
                           ,<<"support_url">>
                           ,<<"update_url">>
                          ]).

-type fax_field_name() :: ne_binary().
-type fax_file_name() :: ne_binary().
-type fax_file_content() :: binary() | iolist().
-type fax_content_type() :: ne_binary().

-type fax_file() :: {fax_field_name(), fax_file_name(), fax_file_content(), fax_content_type()}.
-type fax_files() :: [fax_file(),...] | [].

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.faxboxes">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.faxboxes">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.faxboxes">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.faxboxes">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.faxboxes">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.faxboxes">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.faxboxes">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().

allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_BoxId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

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

resource_exists() -> 'true'.
resource_exists(_BoxId) -> 'true'.

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

validate(Context) ->
    validate_faxboxes(Context, cb_context:req_verb(Context)).

validate_faxboxes(Context, ?HTTP_PUT) ->
    validate_email_address(create_faxbox(remove_private_fields(Context)));
validate_faxboxes(Context, ?HTTP_GET) ->
    faxbox_listing(Context).

validate(Context, Id) ->
    validate_faxbox(Context, Id, cb_context:req_verb(Context)).

validate_faxbox(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_faxbox(Context, Id, ?HTTP_POST) ->
    validate_email_address(update_faxbox(Id, remove_private_fields(Context)));
validate_faxbox(Context, Id, ?HTTP_PATCH) ->
    validate_patch(update_faxbox(Id, remove_private_fields(Context)));
validate_faxbox(Context, Id, ?HTTP_DELETE) ->
    delete_faxbox(Id, Context).

-spec validate_email_address(cb_context:context()) -> cb_context:context().
validate_email_address(Context) ->
    Email = wh_json:get_value(<<"custom_smtp_email_address">>, cb_context:doc(Context)),
    IsValid =
        case Email of
            'undefined' -> 'true';
            Email -> is_faxbox_email_global_unique(Email, wh_doc:id(cb_context:doc(Context)))
        end,
    case IsValid of
        'true' -> Context;
        'false' ->
            cb_context:add_validation_error(
              <<"custom_smtp_email_address">>
              ,<<"unique">>
              ,wh_json:from_list(
                 [{<<"message">>, <<"email address must be unique">>}
                  ,{<<"cause">>, Email}
                 ])
              ,Context
             )
    end.

-spec validate_patch(cb_context:context()) -> cb_context:context().
validate_patch(Context) ->
    DocId = wh_doc:id(cb_context:doc(Context)),
    IsValid = case wh_json:get_value(<<"custom_smtp_email_address">>, cb_context:doc(Context)) of
                  'undefined' -> 'true';
                  CustomEmail -> is_faxbox_email_global_unique(CustomEmail, DocId)
              end,
    case IsValid of
        'true' ->
            Context1 = crossbar_doc:load(DocId, Context),
            case cb_context:resp_status(Context1) of
                'success' ->
                    PatchJObj = cb_context:req_data(Context),
                    ConfigsJObj = wh_json:merge_jobjs(PatchJObj, cb_context:doc(Context1)),
                    cb_context:set_doc(Context, ConfigsJObj);
                _Status ->
                    Context1
            end;
        'false' ->
            cb_context:add_validation_error(<<"custom_smtp_email_address">>
                                            ,<<"unique">>
                                            ,<<"email address must be unique">>
                                            ,Context
                                           )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = faxbox_doc_save(maybe_register_cloud_printer(Context)),
    case cb_context:resp_status(Context1) of
        'success' ->
            RespData = wh_json:public_fields(leak_private_fields(cb_context:doc(Context1))),
            cb_context:set_resp_data(Context1, RespData);
        _ -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _Id) ->
    Context1 = faxbox_doc_save(maybe_register_cloud_printer(Context)),
    case cb_context:resp_status(Context1) of
        'success' ->
            RespData = wh_json:public_fields(leak_private_fields(cb_context:doc(Context1))),
            cb_context:set_resp_data(Context1, RespData);
        _ -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge).
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    faxbox_doc_delete(Context, Id).

-spec create_faxbox(cb_context:context()) -> cb_context:context().
create_faxbox(Context) ->
    OnSuccess = fun(C) -> on_faxbox_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

-spec delete_faxbox(ne_binary(), cb_context:context()) -> cb_context:context().
delete_faxbox(Id, Context) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Ctx1 = crossbar_doc:load(Id, Context),
    case cb_context:doc(Ctx1) of
        'undefined' -> Ctx1;
        Doc -> maybe_oauth_req(Doc, cb_context:req_value(Context, <<"oauth">>), Ctx1)
    end.

-spec leak_private_fields(wh_json:object()) -> wh_json:object().
leak_private_fields(JObj) ->
    J = wh_json:set_value(<<"id">>, wh_doc:id(JObj), JObj),
    lists:foldl(fun leak_private_field/2, J, ?LEAKED_FIELDS).

-spec leak_private_field(ne_binary(), wh_json:object()) -> wh_json:object().
leak_private_field(<<"pvt_", K1/binary>> = K, Acc) ->
    case wh_json:get_value(K, Acc) of
        'undefined' -> Acc;
        Value -> leak_private_field_value(K, K1, Value, Acc)
    end;
leak_private_field(_K, Acc) -> Acc.

-spec leak_private_field_value(ne_binary(), ne_binary(), wh_json:json_term(), wh_json:object()) ->
                                      wh_json:object().
leak_private_field_value(?CLOUD_CLAIM_URL_FIELD, K1, V, Acc) ->
    case wh_json:get_value(?CLOUD_STATE_FIELD, Acc) of
        <<"registered">> ->  wh_json:set_value(K1, V, Acc);
        _ -> Acc
    end;
leak_private_field_value(_K, K1, V, Acc) ->
    wh_json:set_value(K1, V, Acc).

-spec remove_private_fields(cb_context:context()) -> cb_context:context().
remove_private_fields(Context) ->
    JObj1 = lists:foldl(fun remove_private_fields_fold/2
                        ,cb_context:req_data(Context)
                        ,?LEAKED_FIELDS
                       ),
    cb_context:set_req_data(Context, JObj1).

-spec remove_private_fields_fold(ne_binary(), wh_json:object()) -> wh_json:object().
remove_private_fields_fold(<<"pvt_", K1/binary>>, Acc) ->
    case wh_json:get_value(K1, Acc) of
        'undefined' -> Acc;
        _Value -> wh_json:delete_key(K1, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_faxbox(ne_binary(), cb_context:context()) -> cb_context:context().
update_faxbox(Id, Context) ->
    OnSuccess = fun(C) -> on_faxbox_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_faxbox_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_faxbox_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_values([{<<"pvt_type">>, <<"faxbox">>}
                                            ,{<<"pvt_account_id">>, cb_context:account_id(Context)}
                                            ,{<<"pvt_account_db">>, cb_context:account_db(Context)}
                                            ,{<<"pvt_reseller_id">>, cb_context:reseller_id(Context)}
                                            ,{<<"_id">>, wh_util:rand_hex_binary(16)}
                                            ,{<<"pvt_smtp_email_address">>, generate_email_address(Context)}
                                           ]
                                           ,cb_context:doc(Context)
                                          )
                      );
on_faxbox_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context).

-spec generate_email_address(cb_context:context()) -> ne_binary().
generate_email_address(Context) ->
    ResellerId =  cb_context:reseller_id(Context),
    Domain = whapps_account_config:get_global(ResellerId, <<"fax">>, <<"default_smtp_domain">>, ?DEFAULT_FAX_SMTP_DOMAIN),
    New = wh_util:rand_hex_binary(4),
    <<New/binary, ".", Domain/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec faxbox_listing(cb_context:context()) -> cb_context:context().
faxbox_listing(Context) ->
    ViewOptions = ['include_docs'],
    crossbar_doc:load_view(<<"faxbox/crossbar_listing">>
                           ,ViewOptions
                           ,Context
                           ,fun normalize_view_results/2
                          ).

-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [leak_private_fields(wh_json:get_value(<<"doc">>, JObj)) | Acc].

-spec is_faxbox_email_global_unique(ne_binary(), ne_binary()) -> boolean().
is_faxbox_email_global_unique(Email, FaxBoxId) ->
    ViewOptions = [{'key', wh_util:to_lower_binary(Email)}],
    case couch_mgr:get_results(?WH_FAXES_DB, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> wh_doc:id(JObj) =:= FaxBoxId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

-spec maybe_reregister_cloud_printer(cb_context:context()) -> cb_context:context().
-spec maybe_reregister_cloud_printer(api_binary(), cb_context:context()) -> cb_context:context().
maybe_reregister_cloud_printer(Context) ->
    CurrentState = wh_json:get_value(<<"pvt_cloud_state">>, cb_context:doc(Context)),
    Ctx = maybe_reregister_cloud_printer(CurrentState, Context),
    Ctx1 = case wh_json:get_value(<<"pvt_cloud_state">>, cb_context:doc(Ctx)) of
               CurrentState -> cb_context:set_resp_status(Context, 'success');
               _ -> faxbox_doc_save(Ctx)
           end,
    case cb_context:resp_status(Ctx1) of
        'success' ->
            cb_context:set_resp_data(Ctx1, wh_doc:public_fields(leak_private_fields(cb_context:doc(Ctx1))));
        _ -> Ctx1
    end.

maybe_reregister_cloud_printer('undefined', Context) ->
    maybe_register_cloud_printer(Context);
maybe_reregister_cloud_printer(<<"expired">>, Context) ->
    maybe_register_cloud_printer(Context);
maybe_reregister_cloud_printer(_, Context) -> Context.

-spec maybe_register_cloud_printer(cb_context:context()) -> cb_context:context().
maybe_register_cloud_printer(Context) ->
    ResellerId =  cb_context:reseller_id(Context),
    CloudConnectorEnable = whapps_account_config:get(ResellerId, <<"fax">>, <<"enable_cloud_connector">>, 'false'),
    case wh_util:is_true(CloudConnectorEnable) of
        'true' -> maybe_register_cloud_printer(Context, cb_context:doc(Context));
        'false' -> Context
    end.

-spec maybe_register_cloud_printer(cb_context:context(), wh_json:object()) -> cb_context:context().
maybe_register_cloud_printer(Context, JObj) ->
    case wh_json:get_value(<<"pvt_cloud_printer_id">>, JObj) of
        'undefined' ->
            DocId = wh_doc:id(JObj),
            NewDoc = wh_json:set_values(register_cloud_printer(Context, DocId), JObj),
            cb_context:set_doc(Context, NewDoc);
        _PrinterId -> Context
    end.

-spec register_cloud_printer(cb_context:context(), ne_binary()) -> wh_proplist().
register_cloud_printer(Context, FaxboxId) ->
    ResellerId =  cb_context:reseller_id(Context),
    Boundary = <<"------", (wh_util:rand_hex_binary(16))/binary>>,
    Body = register_body(ResellerId, FaxboxId, Boundary),
    ContentType = wh_util:to_list(<<"multipart/form-data; boundary=", Boundary/binary>>),
    ContentLength = length(Body),
    Options = [{'content_type', ContentType}
               ,{'content_length', ContentLength}
              ],
    Headers = [?GPC_PROXY_HEADER, {"Content-Type",ContentType}],
    Url = wh_util:to_list(?GPC_URL_REGISTER),
    case ibrowse:send_req(Url, Headers, 'post', Body, Options) of
        {'ok', "200", _RespHeaders, RespJSON} ->
            JObj = wh_json:decode(RespJSON),
            case wh_json:is_true(<<"success">>, JObj, 'false') of
                'true' ->
                    get_cloud_registered_properties(JObj);
                'false' ->
                    lager:info("request was not successful: ~s", [RespJSON]),
                    []
            end;
        {'ok', _RespCode, _RespHeaders, _RespJSON} ->
            lager:info("unexpected resp ~s: ~s", [_RespCode, _RespJSON]),
            [];
        {'error', _R} ->
            lager:info("error querying: ~p", [_R]),
            []
    end.

-spec get_cloud_registered_properties(wh_json:object()) -> wh_proplist().
get_cloud_registered_properties(JObj) ->
    [PrinterDoc] = wh_json:get_value(<<"printers">>, JObj),
    [{<<"pvt_cloud_printer_id">>, wh_doc:id(PrinterDoc)}
     ,{<<"pvt_cloud_proxy">>, wh_json:get_value(<<"proxy">>, PrinterDoc)}
     ,{<<"pvt_cloud_created_time">>, wh_json:get_integer_value(<<"createTime">>, PrinterDoc)}
     ,{<<"pvt_cloud_registration_token">>, wh_json:get_value(<<"registration_token">>, JObj)}
     ,{<<"pvt_cloud_token_duration">>, wh_json:get_integer_value(<<"token_duration">>, JObj)}
     ,{<<"pvt_cloud_polling_url">>, wh_json:get_value(<<"polling_url">>, JObj)}
     ,{<<"pvt_cloud_connector_claim_url">>, wh_json:get_value(<<"complete_invite_url">>, JObj)}
     ,{<<"pvt_cloud_state">>, <<"registered">>}
     ,{<<"pvt_cloud_oauth_scope">>, wh_json:get_value(<<"oauth_scope">>, JObj)}
    ].

-spec register_body(ne_binary(), ne_binary(), ne_binary()) -> iolist().
register_body(ResellerId, FaxboxId, Boundary) ->
    {'ok', DefaultFields} = file:consult(
                       [filename:join(
                          [code:priv_dir('fax'), "cloud/register.props"])
                       ]),
    OverrideFields = whapps_account_config:get(ResellerId, <<"fax">>, <<"cloud_properties">>, []),
    Fields = lists:foldl(fun({<<"tag">>, _}=P, Acc) ->
                                 [P | Acc];
                            ({K, V}, Acc) ->
                                 case lists:member(K, ?CLOUD_PROPERTIES) of
                                     'true' -> props:set_value(K, V, props:delete(K, Acc));
                                     'false' -> Acc
                                 end
                         end, DefaultFields, OverrideFields),
    {'ok', PrinterDef} = file:read_file(
                           [filename:join(
                              [code:priv_dir('fax'), "cloud/printer.json"])
                           ]),
    Files = [{<<"capabilities">>
              ,<<"capabilities">>
              ,PrinterDef
              ,<<"application/json">>
             }],
    format_multipart_formdata(Boundary
                              ,[{<<"uuid">>, FaxboxId}
                                ,{<<"proxy">> , ?GPC_PROXY}
                                | Fields
                               ]
                              ,Files
                             ).

-spec format_multipart_formdata(ne_binary(), wh_proplist(), fax_files()) -> iolist().
format_multipart_formdata(Boundary, Fields, Files) ->
    EndingParts = [<<"--", Boundary/binary, "--">>, <<"">>],
    FileParts = build_file_parts(Boundary, Files, EndingParts),
    FieldParts = build_field_parts(Boundary, Fields, FileParts),
    lists:foldr(fun join_formdata_fold/2, [], FieldParts).

-spec build_field_parts(ne_binary(), wh_proplist(), iolist()) -> iolist().
build_field_parts(Boundary, Fields, Acc0) ->
    lists:foldr(fun({FieldName, FieldContent}, Acc) ->
                        [<<"--", Boundary/binary>>
                         ,<<"Content-Disposition: form-data; name=\"",FieldName/binary,"\"">>
                         ,<<>>
                         ,FieldContent
                         | Acc
                        ]
                end
                ,Acc0
                ,Fields
               ).

-spec build_file_parts(ne_binary(), fax_files(), iolist()) -> iolist().
build_file_parts(Boundary, Files, Acc0) ->
    lists:foldr(fun({FieldName, FileName, FileContent, FileContentType}, Acc) ->
                        [<<"--", Boundary/binary>>
                         ,<<"Content-Disposition: format-data; name=\"",FieldName/binary,"\"; filename=\"",FileName/binary,"\"">>
                         ,<<"Content-Type: ", FileContentType/binary>>
                         ,<<>>
                         ,FileContent
                         | Acc
                        ]
                end
                ,Acc0
                ,Files
               ).

-spec join_formdata_fold(ne_binary(), iolist()) -> iolist().
join_formdata_fold(Bin, Acc) ->
    string:join([binary_to_list(Bin), Acc], "\r\n").

-spec maybe_oauth_req(wh_json:object(), api_binary(), cb_context:context()) -> cb_context:context().
maybe_oauth_req(_Doc, 'undefined', Context) ->
    maybe_reregister_cloud_printer(Context);
maybe_oauth_req(Doc, _, Context) ->
    oauth_req(Doc, wh_json:get_value(<<"pvt_cloud_refresh_token">>, Doc), Context).

-spec oauth_req(wh_json:object(), api_binary(), cb_context:context()) -> cb_context:context().
oauth_req(Doc, 'undefined', Context) ->
    cb_context:set_resp_data(Context, wh_doc:public_fields(leak_private_fields(Doc)));
oauth_req(Doc, OAuthRefresh, Context) ->
    {'ok',App} = kazoo_oauth_util:get_oauth_app(wh_json:get_value(<<"pvt_cloud_oauth_app">>, Doc)),
    RefreshToken = #oauth_refresh_token{token = OAuthRefresh},
    {'ok', #oauth_token{expires=Expires}=Token} = kazoo_oauth_util:token(App, RefreshToken),
    TokenString = kazoo_oauth_util:authorization_header(Token),
    cb_context:set_resp_data(Context, wh_json:set_values([{<<"expires">>, Expires}
                                                          ,{<<"token">>, TokenString}
                                                         ], wh_json:new())).

-spec faxbox_doc_save(cb_context:context()) -> cb_context:context().
faxbox_doc_save(Context) ->
    Ctx2 = crossbar_doc:save(Context),
    Ctx3 = crossbar_doc:ensure_saved(
             cb_context:set_doc(
               cb_context:set_account_db(Ctx2, ?WH_FAXES_DB),
               wh_doc:delete_revision(cb_context:doc(Ctx2))
              )),
    case cb_context:resp_status(Ctx3) of
        'success' -> Ctx2;
        _ -> Ctx3
    end.

-spec faxbox_doc_delete(cb_context:context(), ne_binary()) -> cb_context:context().
faxbox_doc_delete(Context, Id) ->
    Ctx2 = crossbar_doc:delete(Context),
    _ = crossbar_doc:delete(
          read(Id, cb_context:set_account_db(Context, ?WH_FAXES_DB))
         ),
    Ctx2.
