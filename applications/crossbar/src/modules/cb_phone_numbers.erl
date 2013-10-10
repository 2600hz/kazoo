%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_phone_numbers).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_accepted/4
         ,validate/1, validate/2, validate/3, validate/4
         ,authorize/1
         ,authenticate/1
         ,put/2, put/3, put/4
         ,post/2, post/4
         ,delete/2, delete/4
         ,populate_phone_numbers/1
        ]).

-include("../crossbar.hrl").

-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-define(PORT_DOCS, <<"docs">>).
-define(PORT, <<"port">>).
-define(ACTIVATE, <<"activate">>).
-define(RESERVE, <<"reserve">>).
-define(CLASSIFIERS, <<"classifiers">>).
-define(IDENTIFY, <<"identify">>).

-define(FIND_NUMBER_SCHEMA, "{\"$schema\": \"http://json-schema.org/draft-03/schema#\", \"id\": \"http://json-schema.org/draft-03/schema#\", \"properties\": {\"prefix\": {\"required\": \"true\", \"type\": \"string\", \"minLength\": 3, \"maxLength\": 8}, \"quantity\": {\"default\": 1, \"type\": \"integer\", \"minimum\": 1}}}").

-define(MIME_TYPES, [{<<"application">>, <<"pdf">>}
                     ,{<<"application">>, <<"x-gzip">>}
                     ,{<<"application">>, <<"zip">>}
                     ,{<<"application">>, <<"x-rar-compressed">>}
                     ,{<<"application">>, <<"x-tar">>}
                     ,{<<"image">>, <<"*">>}
                     ,{<<"text">>, <<"plain">>}
                     ,{<<"application">>, <<"base64">>}
                     ,{<<"application">>, <<"x-base64">>}
                    ]).

-define(DEFAULT_COUNTRY, <<"US">>).
-define(PHONE_NUMBERS_CONFIG_CAT, <<"crossbar.phone_numbers">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"account.created">>, ?MODULE, 'populate_phone_numbers'),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.phone_numbers">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.phone_numbers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.phone_numbers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.phone_numbers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.phone_numbers">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.phone_numbers">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.phone_numbers">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec populate_phone_numbers(cb_context:context()) -> 'ok'.
populate_phone_numbers(Context) ->
    AccountDb = cb_context:account_db(Context),
    AccountId = cb_context:account_id(Context),

    PVTs = [{<<"_id">>, ?WNM_PHONE_NUMBER_DOC}
            ,{<<"pvt_account_db">>, AccountDb}
            ,{<<"pvt_account_id">>, AccountId}
            ,{<<"pvt_vsn">>, <<"1">>}
            ,{<<"pvt_type">>, ?WNM_PHONE_NUMBER_DOC}
            ,{<<"pvt_modified">>, wh_util:current_tstamp()}
            ,{<<"pvt_created">>, wh_util:current_tstamp()}
           ],
    _ = couch_mgr:save_doc(AccountDb, wh_json:from_list(PVTs)),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

allowed_methods(?CLASSIFIERS) ->
    [?HTTP_GET];
allowed_methods(<<"collection">>) ->
    [?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_, ?ACTIVATE) ->
    [?HTTP_PUT];
allowed_methods(_, ?RESERVE) ->
    [?HTTP_PUT];
allowed_methods(_, ?PORT) ->
    [?HTTP_PUT];
allowed_methods(_, ?PORT_DOCS) ->
    [?HTTP_GET];
allowed_methods(_, ?IDENTIFY) ->
    [?HTTP_GET];
allowed_methods(<<"collection">>, ?ACTIVATE) ->
    [?HTTP_PUT].

allowed_methods(_, ?PORT_DOCS, _) ->
    [?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> boolean().
-spec resource_exists(path_token(), path_token(), path_token()) -> boolean().
resource_exists() -> 'true'.

resource_exists(_) -> 'true'.

resource_exists(_, ?ACTIVATE) -> 'true';
resource_exists(_, ?RESERVE) -> 'true';
resource_exists(_, ?PORT) -> 'true';
resource_exists(_, ?PORT_DOCS) -> 'true';
resource_exists(_, ?IDENTIFY) -> 'true';
resource_exists(_, _) -> 'false'.

resource_exists(_, ?PORT_DOCS, _) -> 'true';
resource_exists(_, _, _) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(#cb_context{req_verb = ?HTTP_PUT}=Context, _Number, ?PORT_DOCS, _Name) ->
    Context#cb_context{content_types_accepted=[{'from_binary', ?MIME_TYPES}]};
content_types_accepted(#cb_context{req_verb = ?HTTP_POST}=Context, _Number, ?PORT_DOCS, _Name) ->
    Context#cb_context{content_types_accepted=[{'from_binary', ?MIME_TYPES}]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'true'.
authenticate(#cb_context{req_nouns=[{<<"phone_numbers">>,[<<"prefix">>]}]
                         ,req_verb = ?HTTP_GET
                        }) ->
    'true';
authenticate(#cb_context{req_nouns=[{<<"phone_numbers">>, []}]
                         ,req_verb = ?HTTP_GET
                        }) ->
    'true'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'true'.
authorize(#cb_context{req_nouns=[{<<"phone_numbers">>,[<<"prefix">>]}]
                      ,req_verb = ?HTTP_GET
                     }) ->
    'true';
authorize(#cb_context{req_nouns=[{<<"phone_numbers">>,[]}]
                      ,req_verb = ?HTTP_GET
                     }) ->
    'true'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().

validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id='undefined'
                    }=Context) ->
    find_numbers(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context).

validate(#cb_context{req_verb = ?HTTP_PUT}=Context, <<"collection">>) ->
    validate_request(Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, <<"collection">>) ->
    validate_request(Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, <<"collection">>) ->
    validate_delete(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?CLASSIFIERS) ->
    cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                             ,wnm_util:available_classifiers()
                            );
validate(#cb_context{req_verb = ?HTTP_GET}=Context, <<"prefix">>) ->
    find_prefix(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Number) ->
    read(Number, Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Number) ->
    validate_request(Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, _Number) ->
    validate_request(Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, _Number) ->
    validate_delete(Context).

validate(#cb_context{req_verb = ?HTTP_PUT}=Context, <<"collection">>, ?ACTIVATE) ->
    validate_request(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Number, ?ACTIVATE) ->
    validate_request(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Number, ?RESERVE) ->
    validate_request(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, _Number, ?PORT) ->
    validate_request(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Number, ?PORT_DOCS) ->
    list_attachments(Number, Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Number, ?IDENTIFY) ->
    identify(Context, Number).

validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Number, ?PORT_DOCS, _) ->
    read(Number, Context);
validate(#cb_context{req_files=[]}=Context, _, ?PORT_DOCS, _) ->
    lager:debug("No files in request to save attachment"),
    Message = <<"please provide an port document">>,
    cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context);
validate(#cb_context{req_files=[{_, FileObj}]}=Context, Number, ?PORT_DOCS, Name) ->
    FileName = wh_util:to_binary(http_uri:encode(wh_util:to_list(Name))),
    read(Number, Context#cb_context{req_files=[{FileName, FileObj}]});
validate(Context, _, ?PORT_DOCS, _) ->
    lager:debug("Multiple files in request to save attachment"),
    Message = <<"please provide a single port document per request">>,
    cb_context:add_validation_error(<<"file">>, <<"maxItems">>, Message, Context).

-spec post(cb_context:context(), path_token()) ->
                  cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) ->
                  cb_context:context().
post(Context, <<"collection">>) ->
    Results = collection_process(Context),
    set_response(Results, <<>>, Context);
post(Context, Number) ->
    Result = wh_number_manager:set_public_fields(Number
                                                 ,cb_context:doc(Context)
                                                 ,cb_context:auth_account_id(Context)
                                                ),
    set_response(Result, Number, Context).

post(Context, Number, ?PORT_DOCS, _) ->
    put_attachments(Number, Context, cb_context:req_files(Context)).

-spec put(cb_context:context(), path_token()) ->
                 cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) ->
                 cb_context:context().
-spec put(cb_context:context(), path_token(), path_token(), path_token()) ->
                 cb_context:context().
put(Context, <<"collection">>) ->
    Results = collection_process(Context),
    set_response(Results, <<>>, Context);
put(Context, Number) ->
    Result = wh_number_manager:create_number(Number
                                             ,cb_context:account_id(Context)
                                             ,cb_context:auth_account_id(Context)
                                             ,cb_context:doc(Context)
                                            ),
    set_response(Result, Number, Context).

put(Context, <<"collection">>, ?ACTIVATE) ->
    Results = collection_process(Context, ?ACTIVATE),
    set_response(Results, <<>>, Context);
put(Context, Number, ?PORT) ->
    Result = wh_number_manager:port_in(Number
                                       ,cb_context:account_id(Context)
                                       ,cb_context:auth_account_id(Context)
                                       ,cb_context:doc(Context)
                                      ),
    set_response(Result, Number, Context);
put(Context, Number, ?ACTIVATE) ->
    Result = wh_number_manager:assign_number_to_account(Number
                                                        ,cb_context:account_id(Context)
                                                        ,cb_context:auth_account_id(Context)
                                                        ,cb_context:doc(Context)
                                                       ),
    set_response(Result, Number, Context);
put(Context, Number, ?RESERVE) ->
    Result = wh_number_manager:reserve_number(Number
                                              ,cb_context:account_id(Context)
                                              ,cb_context:auth_account_id(Context)
                                              ,cb_context:doc(Context)
                                             ),
    set_response(Result, Number, Context);
put(Context, Number, ?PORT_DOCS) ->
    put_attachments(Number, Context, cb_context:req_files(Context)).

put(Context, Number, ?PORT_DOCS, _) ->
    put_attachments(Number, Context, cb_context:req_files(Context)).

-spec delete(cb_context:context(), path_token()) ->
                    cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), path_token()) ->
                    cb_context:context().
delete(Context, <<"collection">>) ->
    Results = collection_process(Context),
    set_response(Results, <<>>, Context);
delete(Context, Number) ->
    Result = wh_number_manager:release_number(Number, cb_context:auth_account_id(Context)),
    set_response(Result, Number, Context).

delete(Context, Number, ?PORT_DOCS, Name) ->
    FileName = wh_util:to_binary(http_uri:encode(wh_util:to_list(Name))),
    Result = wh_number_manager:delete_attachment(Number, FileName, cb_context:auth_account_id(Context)),
    set_response(Result, Number, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(cb_context:context()) -> cb_context:context().
find_numbers(Context) ->
    JObj = cb_context:query_string(Context),
    OnSuccess = fun(C) ->
                        Prefix = wh_json:get_ne_value(<<"prefix">>, JObj),
                        Quantity = wh_json:get_ne_value(<<"quantity">>, JObj, 1),
                        cb_context:set_resp_data(
                          cb_context:set_resp_status(C, 'success')
                          ,wh_number_manager:find(Prefix, Quantity, wh_json:to_proplist(JObj))
                         )
                end,
    Schema = wh_json:decode(?FIND_NUMBER_SCHEMA),
    cb_context:validate_request_data(Schema
                                     ,cb_context:set_req_data(Context, JObj)
                                     ,OnSuccess
                                    ).

-spec find_prefix(cb_context:context()) -> cb_context:context().
find_prefix(Context) ->
    QS = cb_context:query_string(Context),
    case wh_json:get_ne_value(<<"city">>, QS) of
        'undefined' -> cb_context:add_system_error('bad_identifier', Context);
        City ->
            case get_prefix(City) of
                {'ok', Data} ->
                    cb_context:set_resp_data(
                        cb_context:set_resp_status(Context, 'success')
                        ,Data
                    );
                {'error', Error} ->
                    lager:error("error while prefix for city: ~p : ~p", [City, Error]),
                    cb_context:set_resp_data(
                        cb_context:set_resp_status(Context, 'error')
                        ,Error
                    )
            end
    end.

-spec get_prefix(ne_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
get_prefix(City) ->
    Country = whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    case whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, <<"url">>) of
        'undefined' ->
            {'error', <<"Unable to acquire numbers missing carrier url">>};
        Url ->
            ReqParam  = wh_util:uri_encode(binary:bin_to_list(City)),
            Req = binary:bin_to_list(<<Url/binary, Country/binary, "/city?pattern=">>),
            Uri = lists:append(Req, ReqParam),
            case ibrowse:send_req(Uri, [], 'get') of
                {'error', Reason} ->
                    {'error', Reason};
                {'ok', "200", _Headers, Body} ->
                    JObj =  wh_json:decode(Body),
                    case wh_json:get_value(<<"data">>, JObj) of
                        'undefined' -> {'error ', JObj};
                        Data -> {'ok', Data}
                    end;
                {'ok', _Status, _Headers, Body} ->
                    {'error', Body}
            end
    end.
            


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case crossbar_doc:load(?WNM_PHONE_NUMBER_DOC, Context) of
        #cb_context{resp_error_code=404}=C ->
            crossbar_util:response(wh_json:new(), C);
        Else ->
            cb_context:set_resp_data(Else, clean_summary(Else))
    end.

-spec clean_summary(cb_context:context()) -> wh_json:object().
clean_summary(Context) ->
    JObj = cb_context:resp_data(Context),
    AccountId = cb_context:account_id(Context),

    Routines = [fun(J) ->
                        wh_json:delete_key(<<"id">>, J)
                end
                ,fun(J) ->
                         wh_json:set_value(<<"numbers">>, J, wh_json:new())
                 end
                ,fun(J) ->
                         Service =  wh_services:fetch(AccountId),
                         Quantity = wh_services:cascade_category_quantity(<<"phone_numbers">>, [], Service),
                         wh_json:set_value(<<"casquade_quantity">>, Quantity, J)
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec identify(cb_context:context(), ne_binary()) -> cb_context:context().
identify(Context, Number) ->
    case wh_number_manager:lookup_account_by_number(Number) of
        {'error', 'not_reconcilable'} ->
            cb_context:add_system_error('bad_identifier', [{'details', Number}], Context);
        {'error', E} ->
            set_response({wh_util:to_binary(E), <<>>}, Number, Context);
        {'ok', AccountId, Options} ->
            JObj = wh_json:set_values([{<<"account_id">>, AccountId}
                                       ,{<<"number">>, props:get_value('number', Options)}
                                      ]
                                      ,wh_json:new()
                                     ),
            set_response({'ok', JObj}, Number, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Number, Context) ->
    Result = wh_number_manager:get_public_fields(Number, cb_context:auth_account_id(Context)),
    set_response(Result, Number, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(cb_context:context()) -> cb_context:context().
validate_request(Context) ->
    maybe_add_porting_email(Context).

maybe_add_porting_email(Context) ->
    JObj = cb_context:req_data(Context),
    case wh_json:get_ne_value(<<"port">>, JObj) =/= 'undefined'
        andalso wh_json:get_ne_value([<<"port">>, <<"email">>], JObj) =:= 'undefined'
    of
        'false' -> check_phone_number_schema(Context);
        'true' -> add_porting_email(Context)
    end.

add_porting_email(Context) ->
    JObj = cb_context:req_data(Context),
    case get_auth_user_email(Context) of
        'undefined' -> check_phone_number_schema(Context);
        Email ->
            J = wh_json:set_value([<<"port">>, <<"email">>], Email, JObj),
            check_phone_number_schema(cb_context:set_req_data(Context, J))
    end.

check_phone_number_schema(Context) ->
    cb_context:validate_request_data(<<"phone_numbers">>, Context).

get_auth_user_email(Context) ->
    JObj = cb_context:auth_doc(Context),
    AccountId = cb_context:auth_account_id(Context),

    case wh_json:get_value(<<"owner_id">>, JObj) of
        'undefined' -> 'undefined';
        UserId ->
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            case couch_mgr:open_doc(AccountDb, UserId) of
                {'ok', User} -> wh_json:get_value(<<"email">>, User);
                {'error', _} -> 'undefined'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec validate_delete(cb_context:context()) -> cb_context:context().
validate_delete(Context) ->
    cb_context:set_doc(
      cb_context:set_resp_status(Context, 'success')
      ,'undefined'
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec list_attachments(ne_binary(), cb_context:context()) -> cb_context:context().
list_attachments(Number, Context) ->
    Result = wh_number_manager:list_attachments(Number, cb_context:auth_account_id(Context)),
    set_response(Result, Number, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec put_attachments(ne_binary(), cb_context:context(), wh_proplist()) ->
                             cb_context:context().
put_attachments(_, Context, []) ->
    cb_context:set_resp_status(Context, 'success');
put_attachments(Number, Context, [{Filename, FileObj}|Files]) ->
    AuthBy = cb_context:auth_account_id(Context),
    HeadersJObj = wh_json:get_value(<<"headers">>, FileObj),
    Content = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value(<<"content_type">>, HeadersJObj, <<"application/octet-stream">>),
    Options = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
    lager:debug("setting Content-Type to ~s", [CT]),
    case wh_number_manager:put_attachment(Number, Filename, Content, Options, AuthBy) of
        {'ok', NewDoc} ->
            put_attachments(Number, cb_context:set_resp_data(Context, NewDoc), Files);
        Result ->
            set_response(Result, Number, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_response(operation_return(), binary(), cb_context:context()) ->
                          cb_context:context().
set_response({'ok', Doc}, _, Context) ->
    crossbar_util:response(Doc, Context);
set_response({Error, Reason}, _, Context) ->
    crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context);
set_response(_Else, _, Context) ->
    lager:debug("unexpected response: ~p", [_Else]),
    cb_context:add_system_error('unspecified_fault', Context).

-spec collection_process(cb_context:context()) ->
                                operation_return().
-spec collection_process(cb_context:context(), ne_binary() | ne_binaries()) ->
                                operation_return().
collection_process(Context) ->
    Numbers = wh_json:get_value(<<"numbers">>, cb_context:req_data(Context), []),
    Result = collection_process(Context, Numbers),
    {'ok', Result}.

collection_process(Context, ?ACTIVATE) ->
    Numbers = wh_json:get_value(<<"numbers">>, cb_context:req_data(Context), []),
    Result = collection_process(Context, Numbers, ?ACTIVATE),
    {'ok', Result};
collection_process(Context, Numbers) ->
    Temp = wh_json:set_values([{<<"success">>, wh_json:new()}
                               ,{<<"error">>, wh_json:new()}
                              ], wh_json:new()),
    lists:foldl(
      fun(Number, Acc) ->
              case collection_action(Context, Number) of
                  {'ok', JObj} ->
                      wh_json:set_value([<<"success">>, Number], JObj, Acc);
                  {State, _} ->
                      JObj = wh_json:set_value(<<"reason">>, State, wh_json:new()),
                      wh_json:set_value([<<"error">>, Number], JObj, Acc)
              end
      end
      ,Temp
      ,Numbers
     ).

collection_process(Context, Numbers, Action) ->
    Temp = wh_json:set_values([{<<"success">>, wh_json:new()}
                               ,{<<"error">>, wh_json:new()}
                              ], wh_json:new()),
    lists:foldl(
      fun(Number, Acc) ->
              case collection_action(Context, Number, Action) of
                  {'ok', JObj} ->
                      wh_json:set_value([<<"success">>, Number], JObj, Acc);
                  {State, _} ->
                      JObj = wh_json:set_value(<<"reason">>, State, wh_json:new()),
                      wh_json:set_value([<<"error">>, Number], JObj, Acc)
              end
      end
      ,Temp
      ,Numbers
     ).

-spec collection_action(cb_context:context(), ne_binary()) ->
                               operation_return().
-spec collection_action(cb_context:context(), ne_binary(), ne_binary()) ->
                               operation_return().
collection_action(#cb_context{account_id=AssignTo
                              ,auth_account_id=AuthBy
                              ,doc=JObj
                              ,req_verb = ?HTTP_PUT
                             }, Number) ->
    wh_number_manager:create_number(Number, AssignTo, AuthBy, wh_json:delete_key(<<"numbers">>, JObj));
collection_action(#cb_context{auth_account_id=AuthBy
                              ,doc=Doc
                              ,req_verb = ?HTTP_POST
                             }, Number) ->
    case wh_number_manager:get_public_fields(Number, AuthBy) of
        {'ok', JObj} ->
            Doc1 = wh_json:delete_key(<<"numbers">>, Doc),
            wh_number_manager:set_public_fields(Number, wh_json:merge_jobjs(JObj, Doc1), AuthBy);
        {State, Error} ->
            lager:error("error while fetching number ~p : ~p", [Number, Error]),
            {State, Error}
    end;
collection_action(#cb_context{auth_account_id=AuthBy
                              ,req_verb = ?HTTP_DELETE
                             }, Number) ->
    wh_number_manager:release_number(Number, AuthBy).

collection_action(#cb_context{account_id=AssignTo
                              ,auth_account_id=AuthBy
                              ,doc=JObj
                              ,req_verb = ?HTTP_PUT
                             }, Number, ?ACTIVATE) ->
    case wh_number_manager:assign_number_to_account(Number, AssignTo, AuthBy, JObj) of
        {'ok', RJObj} ->
            {'ok', wh_json:delete_key(<<"numbers">>, RJObj)};
        Else -> Else
    end.
