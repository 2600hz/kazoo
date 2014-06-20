%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
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
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"faxbox/crossbar_listing">>).

-define(GPC_URL, "https://www.google.com/cloudprint/").
-define(GPC_URL_REGISTER, <<?GPC_URL,"register">>).
-define(GPC_PROXY, <<"kazoo-cloud-fax-printer-proxy">>).
-define(GPC_PROXY_HEADER,{"X-CloudPrint-Proxy","kazoo-cloud-fax-printer-proxy"}).
-define(DEFAULT_FAX_SMTP_DOMAIN, <<"fax.kazoo.io">>).

-define(LEAKED_FIELDS, [<<"pvt_smtp_email_address">>, <<"pvt_cloud_state">>
                        ,<<"pvt_cloud_printer_id">>, <<"pvt_cloud_connector_claim_url">>]).

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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

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
validate_faxbox(Context, Id, ?HTTP_DELETE) ->
    delete_faxbox(Id, Context).

-spec validate_email_address(cb_context:context()) -> cb_context:context().
validate_email_address(Context) ->
    DocId = wh_json:get_value(<<"_id">>, cb_context:doc(Context)),
    IsValid = case wh_json:get_value(<<"custom_smtp_email_address">>, cb_context:doc(Context)) of
                  'undefined' -> 'true';
                  CustomEmail -> is_faxbox_email_global_unique(CustomEmail, DocId)
              end,
    case IsValid of
        'true' -> Context;
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
    Ctx = maybe_register_cloud_printer(Context),
    Ctx2 = crossbar_doc:save(Ctx),
    _ = crossbar_doc:save(
          cb_context:set_doc(
            cb_context:set_account_db(Ctx2, ?WH_FAXES),
            wh_json:delete_key(<<"_rev">>, cb_context:doc(Ctx2))
           )),
    cb_context:set_resp_data(Ctx2, wh_json:public_fields(leak_private_fields(cb_context:doc(Ctx2)))).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _Id) ->
    Ctx = maybe_register_cloud_printer(Context),
    Ctx2 = crossbar_doc:save(Ctx),
    _ = crossbar_doc:ensure_saved(
          cb_context:set_doc(
            cb_context:set_account_db(Ctx2, ?WH_FAXES),
            wh_json:delete_key(<<"_rev">>, cb_context:doc(Ctx2))
           )),
    cb_context:set_resp_data(Ctx2, wh_json:public_fields(leak_private_fields(cb_context:doc(Ctx2)))).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    Ctx2 = crossbar_doc:delete(Context),
    _ = crossbar_doc:delete(
          read(Id,
               cb_context:set_account_db(Context, ?WH_FAXES))),
    Ctx2.

-spec create_faxbox(cb_context:context()) -> cb_context:context().
create_faxbox(Context) ->
    OnSuccess = fun(C) -> on_faxbox_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

-spec delete_faxbox(ne_binary(), cb_context:context()) -> cb_context:context().
delete_faxbox(Id, Context) ->
    % should not allow the deletion of default faxbox
    OnSuccess = fun(C) -> on_faxbox_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Ctx1 = crossbar_doc:load(Id, Context),
    cb_context:set_resp_data(Ctx1, wh_doc:public_fields(leak_private_fields(cb_context:doc(Ctx1)))).

-spec leak_private_fields(wh_json:object()) -> wh_json:object().
leak_private_fields(JObj) ->
    lists:foldl(fun(<<"pvt_", K1/binary>> = K, Acc) ->
                        case wh_json:get_value(K, Acc) of
                            'undefined' -> Acc;
                            Value -> wh_json:set_value(K1, Value , Acc)
                        end
                end, JObj, ?LEAKED_FIELDS).

-spec remove_private_fields(cb_context:context()) -> cb_context:context().
remove_private_fields(Context) ->
    JObj = cb_context:req_data(Context),
    JObj1 = lists:foldl(fun(<<"pvt_", K1/binary>>, Acc) ->
                                case wh_json:get_value(K1, Acc) of
                                    'undefined' -> Acc;
                                    _Value -> wh_json:delete_key(K1, Acc)
                                end
                        end, JObj, ?LEAKED_FIELDS),
    cb_context:set_req_data(Context, JObj1).

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
                                            ,{<<"_id">>, wh_util:rand_hex_binary(16)}
                                            ,{<<"pvt_smtp_email_address">>, generate_email_address()}
                                           ]
                                           ,cb_context:doc(Context)
                                          )
                      );
on_faxbox_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context).

-spec generate_email_address() -> ne_binary().
generate_email_address() ->
    Domain = whapps_config:get_binary(<<"fax">>, <<"default_smtp_domain">>, ?DEFAULT_FAX_SMTP_DOMAIN),
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
    case couch_mgr:get_results(?WH_FAXES, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= FaxBoxId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

-spec maybe_register_cloud_printer(cb_context:context()) -> cb_context:context().
maybe_register_cloud_printer(Context) ->
    case whapps_config:get_is_true(<<"fax">>, <<"enable_cloud_connector">>, 'true') of
        'true' ->
            maybe_register_cloud_printer(Context, cb_context:doc(Context));
        'false' -> Context
    end.

-spec maybe_register_cloud_printer(cb_context:context(), wh_json:object()) -> cb_context:context().
maybe_register_cloud_printer(Context, JObj) ->
    case wh_json:get_value(<<"pvt_cloud_printer_id">>, JObj) of
        'undefined' ->
            DocId = wh_json:get_value(<<"_id">>, JObj),
            NewDoc = wh_json:set_values(register_cloud_printer(DocId), JObj),
            cb_context:set_doc(Context, NewDoc);
        _PrinterId -> Context
    end.

-spec register_cloud_printer(ne_binary()) -> wh_proplist().
register_cloud_printer(FaxboxId) ->
    Boundary = <<"------", (wh_util:rand_hex_binary(16))/binary>>,
    Body = register_body(FaxboxId, Boundary),
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

    [{<<"pvt_cloud_printer_id">>, wh_json:get_value(<<"id">>, PrinterDoc)}
     ,{<<"pvt_cloud_proxy">>, wh_json:get_value(<<"proxy">>, PrinterDoc)}
     ,{<<"pvt_cloud_created_time">>, wh_json:get_integer_value(<<"createTime">>, PrinterDoc)}
     ,{<<"pvt_cloud_registration_token">>, wh_json:get_value(<<"registration_token">>, JObj)}
     ,{<<"pvt_cloud_token_duration">>, wh_json:get_integer_value(<<"token_duration">>, JObj)}
     ,{<<"pvt_cloud_polling_url">>, wh_json:get_value(<<"polling_url">>, JObj)}
     ,{<<"pvt_cloud_connector_claim_url">>, wh_json:get_value(<<"complete_invite_url">>, JObj)}
     ,{<<"pvt_cloud_state">>, <<"registered">>}
     ,{<<"pvt_cloud_oauth_scope">>, wh_json:get_value(<<"oauth_scope">>, JObj)}
    ].

-spec register_body(ne_binary(), ne_binary()) -> strings().
register_body(FaxboxId, Boundary) ->
    {'ok', Fields} = file:consult(
                       [filename:join(
                          [code:priv_dir('fax'), "cloud/register.props"])
                       ]),
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

-spec format_multipart_formdata(ne_binary(), wh_proplist(), [tuple(),...]) -> strings().
format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = [[<<"--", Boundary/binary>>
                   ,<<"Content-Disposition: form-data; name=\"",FieldName/binary,"\"">>
                   ,<<"">>
                   ,FieldContent
                  ]
                  || {FieldName, FieldContent} <- Fields
                 ],

    FieldParts2 = lists:append(FieldParts),

    FileParts = [[<<"--", Boundary/binary>>
                  ,<<"Content-Disposition: format-data; name=\"",FieldName/binary,"\"; filename=\"",FileName/binary,"\"">>
                  ,<<"Content-Type: ", FileContentType/binary>>
                  ,<<"">>
                  ,FileContent
                 ]
                 || {FieldName, FileName, FileContent, FileContentType} <- Files
                ],

    FileParts2 = lists:append(FileParts),

    EndingParts = [<<"--", Boundary/binary, "--">>, <<"">>],

    Parts = lists:append([FieldParts2, FileParts2, EndingParts]),

    lists:foldr(fun(A,B) -> string:join([binary_to_list(A), B], "\r\n") end, [], Parts).
