%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% Fax Box API
%%%
%%% @end
%%% @contributors:
%%%   
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
-define(MULTIPART_BOUNDARY,<<"------a450glvjfEoqerAc1p431paQlfDac152cadADfd">>).
-define(DEFAULT_FAX_SMTP_DOMAIN, <<"fax.kazoo.io">>).

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

allowed_methods(BoxId) ->
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
resource_exists(_) -> 'true'.
 

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

validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->    
    validate_email_address(create_faxbox(Context));
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    faxbox_listing(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    validate_email_address(update_faxbox(Id, Context));
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    delete_faxbox(Id, Context).




-spec validate_email_address(cb_context:context()) -> cb_context:context().
validate_email_address(#cb_context{doc=JObj}=Context) ->
    DocId = wh_json:get_value(<<"_id">>, JObj),
    IsValid = case wh_json:get_value(<<"custom_smtp_email_address">>, JObj ) of
                  'undefined' -> 'true';
                  CustomEmail -> is_faxbox_email_global_unique(CustomEmail, DocId)
              end,
    case IsValid of
        'true' -> Context;
        'false' -> cb_context:add_validation_error(<<"custom_smtp_email_address">>
                                                            ,<<"unique">>
                                                            ,<<"email address must be unique">>
                                                            ,Context)
    end.
    


%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(#cb_context{}=Context) ->
    Ctx = maybe_register_cloud_printer(Context),
    couch_mgr:ensure_saved(?WH_FAXES, wh_json:delete_key(<<"_rev">>, cb_context:doc(Ctx))),
    crossbar_doc:save(Ctx).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(#cb_context{}=Context, _Id) ->
    Ctx = maybe_register_cloud_printer(Context),
    couch_mgr:ensure_saved(?WH_FAXES, wh_json:delete_key(<<"_rev">>, cb_context:doc(Ctx))),
    crossbar_doc:save(Ctx).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(#cb_context{}=Context, _Id) ->
    crossbar_doc:delete(Context),
    couch_mgr:del_doc(?WH_FAXES, wh_json:delete_key(<<"_rev">>, cb_context:doc(Context))).
    


-spec create_faxbox(cb_context:context()) -> cb_context:context().
create_faxbox(#cb_context{}=Context) ->
    OnSuccess = fun(C) -> on_faxbox_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"faxbox">>, Context, OnSuccess).

-spec delete_faxbox(ne_binary(), cb_context:context()) -> cb_context:context().
delete_faxbox(Id, #cb_context{}=Context) ->
    % should not allow the deletion of default faxbox
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
                                               ,{<<"_id">>, wh_util:rand_hex_binary(16)}
                                               ,{<<"pvt_smtp_email_address">>, generate_email_address()}
                                              ], JObj)};
on_faxbox_successful_validation(DocId, #cb_context{doc=JObj}=Context) ->
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
faxbox_listing(#cb_context{account_id=AccountId}=Context) ->
    ViewOptions=[{'key',AccountId},'include_docs' ],
    crossbar_doc:load_view(<<"faxbox/crossbar_listing">>
                           ,ViewOptions
                           ,Context
                           ,fun normalize_view_results/2
                          ).


-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].


-spec is_faxbox_email_global_unique(ne_binary(), ne_binary()) -> boolean().
is_faxbox_email_global_unique(Email, FaxBoxId) ->
    ViewOptions = [{<<"key">>, wh_util:to_lower_binary(Email)}],
    case couch_mgr:get_results(?WH_FAXES, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= FaxBoxId;
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

maybe_register_cloud_printer(#cb_context{doc=JObj}=Context) ->
    case whapps_config:get_is_true(<<"fax">>, <<"enable_cloud_connector">>, 'true') of
        'true' -> case wh_json:get_value(<<"pvt_cloud_printer_id">>, JObj) of
                      'undefined' ->
                          DocId = wh_json:get_value(<<"_id">>, JObj),
                          NewDoc = wh_json:set_values(
                                     register_cloud_printer(DocId),JObj),
                          Context#cb_context{doc=NewDoc};                          
                      PrinterId -> Context
                  end;
        'false' -> Context
    end.    

-spec register_cloud_printer(ne_binary()) -> wh_proplist().
register_cloud_printer(FaxboxId) ->
    Body = register_body(FaxboxId),
    ContentType = wh_util:to_list(<<"multipart/form-data; boundary=", ?MULTIPART_BOUNDARY/binary>>),
    ContentLength = length(Body),
    Options = [
              {content_type, ContentType}
              ,{content_length, ContentLength}
               ],
    Headers = [?GPC_PROXY_HEADER, {"Content-Type",ContentType}],    
    Url = wh_util:to_list(?GPC_URL_REGISTER),
    case ibrowse:send_req(Url, Headers, 'post', Body, Options) of
        {'ok', "200", _RespHeaders, RespXML} ->
            JObj = wh_json:decode(RespXML),
            case wh_json:get_value(<<"success">>, JObj, 'false') of
                'true' ->
                    get_cloud_registered_properties(JObj);
                Other -> 
                    lager:info("Response is ~s",[Other]),
                    []
            end;
        {'ok', _RespCode, _RespHeaders, RespXML} ->
            lager:info("unexpected recv ~s: ~s", [_RespCode, RespXML]),
            [];
        {'error', _R} ->
            lager:info("error querying: ~p", [_R]),
            []
    end.

-spec get_cloud_registered_properties(wh_json:object()) -> wh_proplist().
get_cloud_registered_properties(JObj) ->
    [PrinterDoc] = wh_json:get_value(<<"printers">>, JObj),   
    TokenDuration = wh_json:get_integer_value(<<"token_duration">>, JObj),
    RegistrationToken = wh_json:get_value(<<"registration_token">>, JObj),
    InviteUrl = wh_json:get_value(<<"complete_invite_url">>, JObj),
    PrinterId = wh_json:get_value(<<"id">>, PrinterDoc),
    CreatedTime = wh_json:get_integer_value(<<"createTime">>, PrinterDoc),
    
    [{<<"pvt_cloud_printer_id">>, wh_json:get_value(<<"id">>, PrinterDoc)}
    ,{<<"pvt_cloud_proxy">>, wh_json:get_value(<<"proxy">>, PrinterDoc)}
    ,{<<"pvt_cloud_created_time">>, CreatedTime}
    ,{<<"pvt_cloud_registration_token">>, RegistrationToken}
    ,{<<"pvt_cloud_token_duration">>, TokenDuration}
    ,{<<"pvt_cloud_polling_url">>, wh_json:get_value(<<"polling_url">>, JObj)}
    ,{<<"cloud_connector_claim_url">>, InviteUrl}
    ,{<<"pvt_cloud_state">>, <<"registered">>}
    ,{<<"pvt_cloud_oauth_scope">>, wh_json:get_value(<<"oauth_scope">>, JObj)}
    ].
    

-spec register_body(ne_binary()) -> [string(),...].
register_body(FaxboxId) ->
    {'ok', Fields} = file:consult(
                       [filename:join(
                          [code:priv_dir('fax'), "cloud/register.props"])
                       ]),
    {'ok', PrinterDef} = file:read_file(
                               [filename:join(
                                  [code:priv_dir('fax'), "cloud/printer.json"])
                               ]), 
    Files = [
             {<<"capabilities">>,<<"capabilities">>
             ,PrinterDef
             ,<<"application/json">>}
             ],
    format_multipart_formdata(?MULTIPART_BOUNDARY
                              ,[{<<"uuid">>, FaxboxId}
                               ,{<<"proxy">> , ?GPC_PROXY}
                               | Fields], Files).

-spec format_multipart_formdata(ne_binary(), wh_proplist(), [tuple(),...]) -> [string(),...].
format_multipart_formdata(Boundary, Fields, Files) ->
    FieldParts = lists:map(fun({FieldName, FieldContent}) ->
                                   [<<"--", Boundary/binary>>,
                                    <<"Content-Disposition: form-data; name=\"",FieldName/binary,"\"">>,
                                    <<"">>,
                                    FieldContent]
                           end, Fields),
    FieldParts2 = lists:append(FieldParts),
    FileParts = lists:map(fun({FieldName, FileName, FileContent, FileContentType}) ->
                                  [<<"--", Boundary/binary>>,
                                   <<"Content-Disposition: format-data; name=\"",FieldName/binary,"\"; filename=\"",FileName/binary,"\"">>,
                                   <<"Content-Type: ", FileContentType/binary>>,
                                   <<"">>,
                                   FileContent]
                          end, Files),
    FileParts2 = lists:append(FileParts),
    EndingParts = [<<"--", Boundary/binary, "--">>, <<"">>],
    Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
    lists:foldr(fun(A,B) -> string:join([binary_to_list(A), B], "\r\n") end, [], Parts).
