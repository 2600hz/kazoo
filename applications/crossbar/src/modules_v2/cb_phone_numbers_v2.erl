%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_phone_numbers_v2).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,billing/1
         ,content_types_accepted/4
         ,validate/1 ,validate/2, validate/3, validate/4
         ,validate_request/1
         ,authorize/1
         ,authenticate/1
         ,put/2, put/3, put/4
         ,post/2, post/4
         ,delete/2, delete/4
         ,summary/1
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
-define(COLLECTION, <<"collection">>).
-define(FIX, <<"fix">>).
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

-define(PHONE_NUMBERS_CONFIG_CAT, <<"crossbar.phone_numbers">>).

-define(FIND_NUMBER_PREFIX
        ,wh_json:from_list([{<<"required">>, 'true'}
                            ,{<<"type">>, <<"string">>}
                            ,{<<"minLength">>, 3}
                            ,{<<"maxLength">>, 10}
                           ])
       ).
-define(FIND_NUMBER_QUANTITY
        ,wh_json:from_list([{<<"default">>, 1}
                            ,{<<"type">>, <<"integer">>}
                            ,{<<"minimum">>, 1}
                           ])
       ).

-define(FIND_NUMBER_PROPERTIES
        ,wh_json:from_list([{<<"prefix">>, ?FIND_NUMBER_PREFIX}
                            ,{<<"quantity">>, ?FIND_NUMBER_QUANTITY}
                           ])
       ).

-define(FIND_NUMBER_SCHEMA
        ,wh_json:from_list([{<<"$schema">>, <<"http://json-schema.org/draft-03/schema#">>}
                            ,{<<"id">>, <<"find_number">>}
                            ,{<<"properties">>, ?FIND_NUMBER_PROPERTIES}
                           ])
       ).

-define(DEFAULT_COUNTRY, <<"US">>).
-define(FREE_URL, <<"phonebook_url">>).
-define(PAYED_URL, <<"phonebook_url_premium">>).
-define(PREFIX, <<"prefix">>).
-define(LOCALITY, <<"locality">>).
-define(CHECK, <<"check">>).

-define(MAX_TOKENS, whapps_config:get_integer(?PHONE_NUMBERS_CONFIG_CAT, <<"activations_per_day">>, 100)).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"account.created">>, ?MODULE, 'populate_phone_numbers'),
    _ = crossbar_bindings:bind(<<"v2_resource.content_types_accepted.phone_numbers">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"v2_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v2_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v2_resource.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.phone_numbers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.phone_numbers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.phone_numbers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.put.phone_numbers">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.post.phone_numbers">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"v2_resource.execute.delete.phone_numbers">>, ?MODULE, 'delete').

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
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    maybe_authenticate(cb_context:req_verb(Context)
                       ,cb_context:req_nouns(Context)
                      ).

-spec maybe_authenticate(http_method(), req_nouns()) -> boolean().
maybe_authenticate(?HTTP_GET, [{?WNM_PHONE_NUMBER_DOC, []}]) ->
    'true';
maybe_authenticate(?HTTP_GET, [{?WNM_PHONE_NUMBER_DOC, [?PREFIX]}]) ->
    'true';
maybe_authenticate(_Verb, _Nouns) ->
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    maybe_authorize(cb_context:req_verb(Context)
                    ,cb_context:req_nouns(Context)
                   ).

maybe_authorize(?HTTP_GET, [{?WNM_PHONE_NUMBER_DOC,[]}]) ->
    'true';
maybe_authorize(?HTTP_GET, [{?WNM_PHONE_NUMBER_DOC, [?PREFIX]}]) ->
    'true';
maybe_authorize(_Verb, _Nouns) ->
    'false'.

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

allowed_methods(?FIX) ->
    [?HTTP_POST];
allowed_methods(?CLASSIFIERS) ->
    [?HTTP_GET];
allowed_methods(?COLLECTION) ->
    [?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(?PREFIX) ->
    [?HTTP_GET];
allowed_methods(?LOCALITY) ->
    [?HTTP_POST];
allowed_methods(?CHECK) ->
    [?HTTP_POST];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?COLLECTION, ?ACTIVATE) ->
    [?HTTP_PUT];
allowed_methods(?CLASSIFIERS, _PhoneNumber) ->
    [?HTTP_GET];
allowed_methods(_PhoneNumber, ?ACTIVATE) ->
    [?HTTP_PUT];
allowed_methods(_, ?RESERVE) ->
    [?HTTP_PUT];
allowed_methods(_, ?PORT) ->
    [?HTTP_PUT];
allowed_methods(_, ?PORT_DOCS) ->
    [?HTTP_GET];
allowed_methods(_, ?IDENTIFY) ->
    [?HTTP_GET].

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

resource_exists(?FIX) -> 'true';
resource_exists(?PREFIX) -> 'true';
resource_exists(?LOCALITY) -> 'true';
resource_exists(?CHECK) -> 'true';
resource_exists(?CLASSIFIERS) -> 'true';
resource_exists(_) -> 'true'.

resource_exists(_, ?ACTIVATE) -> 'true';
resource_exists(_, ?RESERVE) -> 'true';
resource_exists(_, ?PORT) -> 'true';
resource_exists(_, ?PORT_DOCS) -> 'true';
resource_exists(_, ?IDENTIFY) -> 'true';
resource_exists(?CLASSIFIERS, _) -> 'true';
resource_exists(_, _) -> 'false'.

resource_exists(_, ?PORT_DOCS, _) -> 'true';
resource_exists(_, _, _) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure we will be able to bill for phone_numbers
%% @end
%%--------------------------------------------------------------------
-spec billing(cb_context:context()) -> cb_context:context().
billing(Context) ->
    maybe_allow_updates(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec maybe_allow_updates(cb_context:context(), req_nouns(), http_method()) -> cb_context:context().
maybe_allow_updates(Context, [{?WNM_PHONE_NUMBER_DOC, _}|_], ?HTTP_GET) ->
    Context;
maybe_allow_updates(Context, [{?WNM_PHONE_NUMBER_DOC, _}|_], _Verb) ->
    try wh_services:allow_updates(cb_context:account_id(Context)) of
        'true' -> Context
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
maybe_allow_updates(Context, _Nouns, _Verb) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_accepted(Context, _Number, ?PORT_DOCS, _Name) ->
    port_types_accepted(Context, cb_context:req_verb(Context)).

-spec port_types_accepted(cb_context:context(), http_method()) -> cb_context:context().
port_types_accepted(Context, ?HTTP_PUT) ->
    cb_context:set_content_types_accepted(Context, [{'from_binary', ?MIME_TYPES}]);
port_types_accepted(Context, ?HTTP_POST) ->
    cb_context:set_content_types_accepted(Context, [{'from_binary', ?MIME_TYPES}]);
port_types_accepted(Context,  _Verb) ->
    Context.

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

validate(Context) ->
    validate_phone_numbers(Context, cb_context:req_verb(Context), cb_context:account_id(Context)).

-spec validate_phone_numbers(cb_context:context(), http_method(), api_binary()) ->
                                    cb_context:context().
validate_phone_numbers(Context, ?HTTP_GET, 'undefined') ->
    find_numbers(Context);
validate_phone_numbers(Context, ?HTTP_GET, _AccountId) ->
    summary(Context).

validate(Context, ?FIX) ->
    cb_context:set_resp_data(
        cb_context:set_resp_status(Context, 'success')
        ,wh_json:new()
    );
validate(Context, ?PREFIX) ->
    find_prefix(Context);
validate(Context, ?COLLECTION) ->
    validate_collection(Context, cb_context:req_verb(Context));
validate(Context, ?CLASSIFIERS) ->
    cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                             ,wnm_util:available_classifiers()
                            );
validate(Context, ?LOCALITY) ->
    find_locality(Context);
validate(Context, ?CHECK) ->
    check_number(Context);
validate(Context, Number) ->
    validate_number(Context, Number, cb_context:req_verb(Context)).

-spec validate_number(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_number(Context, Number, ?HTTP_GET) ->
    read(Number, Context);
validate_number(Context, _Number, ?HTTP_POST) ->
    validate_request(Context);
validate_number(Context, _Number, ?HTTP_PUT) ->
    validate_request(Context);
validate_number(Context, _Number, ?HTTP_DELETE) ->
    validate_delete(Context).

-spec validate_collection(cb_context:context(), http_method()) -> cb_context:context().
validate_collection(Context, ?HTTP_PUT) ->
    validate_request(Context);
validate_collection(Context, ?HTTP_POST) ->
    validate_request(Context);
validate_collection(Context, ?HTTP_DELETE) ->
    validate_delete(Context).

validate(Context, ?COLLECTION, ?ACTIVATE) ->
    validate_request(Context);
validate(Context, ?CLASSIFIERS, Number) ->
    classify_number(Context, Number);
validate(Context, _Number, ?ACTIVATE) ->
    case has_tokens(Context) of
        'true' -> validate_request(Context);
        'false' -> cb_context:add_system_error('too_many_requests', Context)
    end;
validate(Context, _Number, ?RESERVE) ->
    validate_request(Context);
validate(Context, _Number, ?PORT) ->
    validate_request(Context);
validate(Context, Number, ?PORT_DOCS) ->
    list_attachments(Number, Context);
validate(Context, Number, ?IDENTIFY) ->
    identify(Context, Number).

-spec classify_number(cb_context:context(), path_token()) -> cb_context:context().
classify_number(Context, Number) ->
    case wnm_util:classify_number(Number) of
        'undefined' ->
            unclassified_number(Context, Number);
        Classifier ->
            classified_number(Context, Number, Classifier)
    end.

-spec unclassified_number(cb_context:context(), path_token()) -> cb_context:context().
unclassified_number(Context, Number) ->
    RespData = base_classified_number(Context, Number),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, wh_json:from_list(RespData)}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]).

-spec base_classified_number(cb_context:context(), ne_binary()) -> wh_proplist().
base_classified_number(_Context, Number) ->
    Normalized = wnm_util:normalize_number(Number),
    [{<<"number">>, Number}
     ,{<<"e164">>, Normalized}
    ].

-spec classified_number(cb_context:context(), path_token(), api_binary()) -> cb_context:context().
classified_number(Context, Number, Classifier) ->
    ClassifierJObj = wh_json:get_value(Classifier, wnm_util:available_classifiers()),
    BaseData = base_classified_number(Context, Number),
    RespData = wh_json:set_values([{<<"name">>, Classifier}
                                   | BaseData
                                  ]
                                  ,ClassifierJObj
                                 ),
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, RespData}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]).

validate(Context, Number, ?PORT_DOCS, Name) ->
    validate_port_docs(Context, Number, Name, cb_context:req_verb(Context)).

-spec validate_port_docs(cb_context:context(), path_token(), path_token(), http_method()) ->
                                cb_context:context().
validate_port_docs(Context, Number, _Name, ?HTTP_DELETE) ->
    read(Number, Context);
validate_port_docs(Context, Number, Name, _Verb) ->
    validate_port_docs_upload(Context, Number, Name, cb_context:req_files(Context)).

-spec validate_port_docs_upload(cb_context:context(), path_token(), path_token(), req_files()) ->
                                       cb_context:context().
validate_port_docs_upload(Context, _Number, _Name, []) ->
    lager:debug("No files in request to save attachment"),
    cb_context:add_validation_error(
        <<"file">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"Please provide an port document">>}
         ])
        ,Context
    );
validate_port_docs_upload(Context, Number, Name, [{_, FileObj}]) ->
    FileName = wh_util:to_binary(http_uri:encode(wh_util:to_list(Name))),
    read(Number, cb_context:set_req_files(Context, [{FileName, FileObj}]));
validate_port_docs_upload(Context, _Name, _Number, _Files) ->
    lager:debug("Multiple files in request to save attachment"),
    cb_context:add_validation_error(
        <<"file">>
        ,<<"maxItems">>
        ,wh_json:from_list([
            {<<"message">>, <<"Please provide a single port document per request">>}
         ])
        ,Context
    ).

-spec post(cb_context:context(), path_token()) ->
                  cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) ->
                  cb_context:context().
post(Context, ?FIX) ->
    AccountId = cb_context:account_id(Context),
    _ = wh_number_fix:fix_account_numbers(AccountId),
    summary(Context);
post(Context, ?COLLECTION) ->
    post_collection(Context, cb_context:req_json(Context));
post(Context, Number) ->
    post_number(Context, Number, cb_context:req_json(Context)).

-spec post_collection(cb_context:context(), wh_json:object()) -> cb_context:context().
post_collection(Context, ReqJObj) ->
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:post(cb_context:set_req_json(Context, NewReqJObj), ?COLLECTION)
          end,
    set_response(collection_process(Context), <<>>, Context, Fun).

-spec post_number(cb_context:context(), path_token(), wh_json:object()) -> cb_context:context().
post_number(Context, Number, ReqJObj) ->
    Result = wh_number_manager:set_public_fields(Number
                                                 ,cb_context:doc(Context)
                                                 ,cb_context:auth_account_id(Context)
                                                 ,(not wh_json:is_true(<<"accept_charges">>, ReqJObj))
                                                ),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:post(cb_context:set_req_json(Context, NewReqJObj), Number)
          end,
    set_response(Result, Number, Context, Fun).

post(Context, Number, ?PORT_DOCS, _) ->
    put_attachments(Number, Context, cb_context:req_files(Context)).

-spec put(cb_context:context(), path_token()) ->
                 cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) ->
                 cb_context:context().
-spec put(cb_context:context(), path_token(), path_token(), path_token()) ->
                 cb_context:context().
put(Context, ?COLLECTION) ->
    put_collection(Context, cb_context:req_json(Context));
put(Context, Number) ->
    put_number(Context, Number, cb_context:req_json(Context)).

-spec put_collection(cb_context:context(), wh_json:object()) ->
                        cb_context:context().
put_collection(Context, ReqJObj) ->
    Results = collection_process(Context),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), ?COLLECTION)
          end,
    set_response(Results, <<>>, Context, Fun).

-spec put_number(cb_context:context(), path_token(), wh_json:object()) ->
                        cb_context:context().
put_number(Context, Number, ReqJObj) ->
    Result = wh_number_manager:create_number(Number
                                             ,cb_context:account_id(Context)
                                             ,cb_context:auth_account_id(Context)
                                             ,cb_context:doc(Context)
                                             ,(not wh_json:is_true(<<"accept_charges">>, ReqJObj))
                                            ),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), Number)
          end,
    set_response(Result, Number, Context, Fun).

put(Context, ?COLLECTION, ?ACTIVATE) ->
    activate_collection(Context, cb_context:req_json(Context));
put(Context, Number, ?ACTIVATE) ->
    activate_number(Context, Number, cb_context:req_json(Context));
put(Context, Number, ?PORT) ->
    create_port(Context, Number, cb_context:req_json(Context));
put(Context, Number, ?RESERVE) ->
    reserve_number(Context, Number, cb_context:req_json(Context));
put(Context, Number, ?PORT_DOCS) ->
    put_attachments(Number, Context, cb_context:req_files(Context)).

-spec activate_collection(cb_context:context(), wh_json:object()) -> cb_context:context().
activate_collection(Context, ReqJObj) ->
    Results = collection_process(Context, ?ACTIVATE),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, 'true', ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), ?COLLECTION, ?ACTIVATE)
          end,
    set_response(Results, <<>>, Context, Fun).

-spec activate_number(cb_context:context(), path_token(), wh_json:object()) ->
                             cb_context:context().
activate_number(Context, Number, ReqJObj) ->
    Result = wh_number_manager:assign_number_to_account(Number
                                                        ,cb_context:account_id(Context)
                                                        ,cb_context:auth_account_id(Context)
                                                        ,cb_context:doc(Context)
                                                        ,(not wh_json:is_true(<<"accept_charges">>, ReqJObj))
                                                       ),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), Number, ?ACTIVATE)
          end,
    set_response(Result, Number, Context, Fun).

-spec create_port(cb_context:context(), path_token(), wh_json:object()) -> cb_context:context().
create_port(Context, Number, ReqJObj) ->
    Result = wh_number_manager:port_in(Number
                                       ,cb_context:account_id(Context)
                                       ,cb_context:auth_account_id(Context)
                                       ,cb_context:doc(Context)
                                       ,(not wh_json:is_true(<<"accept_charges">>, ReqJObj))
                                      ),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), Number, ?PORT)
          end,
    set_response(Result, Number, Context, Fun).

-spec reserve_number(cb_context:context(), path_token(), wh_json:object()) ->
                            cb_context:context().
reserve_number(Context, Number, ReqJObj) ->
    Result = wh_number_manager:reserve_number(Number
                                              ,cb_context:account_id(Context)
                                              ,cb_context:auth_account_id(Context)
                                              ,cb_context:doc(Context)
                                              ,(not wh_json:is_true(<<"accept_charges">>, ReqJObj))
                                             ),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), Number, ?RESERVE)
          end,
    set_response(Result, Number, Context, Fun).

put(Context, Number, ?PORT_DOCS, _) ->
    put_attachments(Number, Context, cb_context:req_files(Context)).

-spec delete(cb_context:context(), path_token()) ->
                    cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token(), path_token()) ->
                    cb_context:context().
delete(Context, ?COLLECTION) ->
    Numbers = wh_json:get_value(<<"numbers">>, cb_context:req_data(Context), []),
    Results = collection_process(Context, Numbers, <<"delete">>),
    Fun = fun() -> Context end,
    set_response({'ok', Results}, <<>>, Context, Fun);
delete(Context, Number) ->
    Result = wh_number_manager:release_number(Number, cb_context:auth_account_id(Context)),
    Fun = fun() -> Context end,
    set_response(Result, Number, Context, Fun).

delete(Context, Number, ?PORT_DOCS, Name) ->
    FileName = wh_util:to_binary(http_uri:encode(wh_util:to_list(Name))),
    Result = wh_number_manager:delete_attachment(Number, FileName, cb_context:auth_account_id(Context)),
    Fun = fun() -> Context end,
    set_response(Result, Number, Context, Fun).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = crossbar_doc:load(?WNM_PHONE_NUMBER_DOC, Context),
    case cb_context:resp_error_code(Context1) of
        404 -> crossbar_util:response(wh_json:new(), Context1);
        _Code ->
            Context2 = cb_context:set_resp_data(Context1, clean_summary(Context1)),
            case cb_context:resp_status(Context2) of
                'success' ->  maybe_update_locality(Context2);
                _Status -> Context2
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec clean_summary(cb_context:context()) -> wh_json:object().
clean_summary(Context) ->
    AccountId = cb_context:account_id(Context),
    Routines = [fun(JObj) -> wh_json:delete_key(<<"id">>, JObj) end
                ,fun(JObj) -> wh_json:set_value(<<"numbers">>, JObj, wh_json:new()) end
                ,fun(JObj) ->
                         Service = wh_services:fetch(AccountId),
                         Quantity = wh_services:cascade_category_quantity(?WNM_PHONE_NUMBER_DOC, [], Service),
                         wh_json:set_value(<<"casquade_quantity">>, Quantity, JObj)
                 end
               ],
    lists:foldl(fun(F, JObj) -> F(JObj) end
                ,cb_context:resp_data(Context)
                ,Routines
               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(cb_context:context()) -> cb_context:context().
find_numbers(Context) ->
    JObj = get_find_numbers_req(Context),
    Prefix = wh_json:get_ne_value(<<"prefix">>, JObj),
    Quantity = wh_json:get_integer_value(<<"quantity">>, JObj, 1),
    OnSuccess = fun(C) ->
                        cb_context:setters(C
                                           ,[{fun cb_context:set_resp_data/2, wh_number_manager:find(Prefix, Quantity, wh_json:to_proplist(JObj))}
                                             ,{fun cb_context:set_resp_status/2, 'success'}
                                            ])
                end,

    cb_context:validate_request_data(?FIND_NUMBER_SCHEMA
                                     ,cb_context:set_req_data(Context, JObj)
                                     ,OnSuccess
                                    ).

-spec get_find_numbers_req(cb_context:context()) -> wh_json:object().
get_find_numbers_req(Context) ->
    JObj = cb_context:query_string(Context),
    AccountId = cb_context:auth_account_id(Context),
    Quantity = wh_util:to_integer(cb_context:req_value(Context, <<"quantity">>, 1)),
    wh_json:set_values([{<<"quantity">>, Quantity}
                       ,{<<"Account-ID">>, AccountId}
                       ], JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_prefix(cb_context:context()) -> cb_context:context().
find_prefix(Context) ->
    case wh_json:get_ne_value(<<"city">>, cb_context:query_string(Context)) of
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec find_locality(cb_context:context()) -> cb_context:context().
find_locality(Context) ->
    case cb_context:req_value(Context, <<"numbers">>) of
        'undefined' ->
            cb_context:add_validation_error(
                <<"numbers">>
                ,<<"required">>
                ,wh_json:from_list([
                    {<<"message">>, <<"list of numbers missing">>}
                 ])
                ,Context
            );
        [] ->
            cb_context:add_validation_error(
                <<"numbers">>
                ,<<"minimum">>
                ,wh_json:from_list([
                    {<<"message">>, <<"minimum 1 number required">>}
                 ])
                ,Context
            );
        Numbers when is_list(Numbers) ->
            Url = get_url(cb_context:req_value(Context, <<"quality">>)),
            case get_locality(Numbers, Url) of
                {'error', E} ->
                    crossbar_util:response('error', E, 500, Context);
                {'ok', Localities} ->
                    cb_context:set_resp_data(
                      cb_context:set_resp_status(Context, 'success')
                      ,Localities
                     )
            end;
        _E ->
            cb_context:add_validation_error(
                <<"numbers">>
                ,<<"type">>
                ,wh_json:from_list([
                    {<<"message">>, <<"numbers must be a list">>}
                 ])
                ,Context
            )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec check_number(cb_context:context()) -> cb_context:context().
check_number(Context) ->
    case cb_context:req_value(Context, <<"numbers">>) of
        'undefined' ->
            cb_context:add_validation_error(
                <<"numbers">>
                ,<<"required">>
                ,wh_json:from_list([
                    {<<"message">>, <<"list of numbers missing">>}
                 ])
                ,Context
            );
        [] ->
            cb_context:add_validation_error(
                <<"numbers">>
                ,<<"minimum">>
                ,wh_json:from_list([
                    {<<"message">>, <<"minimum 1 number required">>}
                 ])
                ,Context
            );
        Numbers when is_list(Numbers) ->
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,wh_number_manager:check(Numbers)
             );
        E ->
            cb_context:add_validation_error(
                <<"numbers">>
                ,<<"type">>
                ,wh_json:from_list([
                    {<<"message">>, <<"numbers must be a list">>}
                    ,{<<"cause">>, E}
                 ])
                ,Context
            )
    end.

-spec get_url(any()) -> binary().
get_url(<<"high">>) -> ?PAYED_URL;
get_url(_) -> ?FREE_URL.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec get_prefix(ne_binary()) ->
                        {'ok', wh_json:object()} |
                        {'error', any()}.
get_prefix(City) ->
    Country = whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    case whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, ?FREE_URL) of
        'undefined' ->
            {'error', <<"Unable to acquire numbers missing carrier url">>};
        Url ->
            ReqParam = wh_util:uri_encode(binary:bin_to_list(City)),
            Req = binary:bin_to_list(<<Url/binary, "/", Country/binary, "/city?pattern=">>),
            Uri = lists:append(Req, ReqParam),
            case ibrowse:send_req(Uri, [], 'get') of
                {'error', _Reason}=E -> E;
                {'ok', "200", _Headers, Body} ->
                    JObj = wh_json:decode(Body),
                    case wh_json:get_value(<<"data">>, JObj) of
                        'undefined' -> {'error ', JObj};
                        Data -> {'ok', Data}
                    end;
                {'ok', _Status, _Headers, Body} ->
                    {'error', wh_json:decode(Body)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_locality(cb_context:context()) ->
                                   cb_context:context().
maybe_update_locality(Context) ->
    Numbers = wh_json:foldl(
                fun(Key, Value, Acc) ->
                        case wh_json:get_value(<<"locality">>, Value) =:= 'undefined'
                            andalso  wnm_util:is_reconcilable(Key)
                        of
                            'true' -> [Key|Acc];
                            'false' -> Acc
                        end
                end
                ,[]
                ,wh_json:get_value(<<"numbers">>, cb_context:resp_data(Context))
               ),
    update_locality(Context, Numbers).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_locality(cb_context:context(), ne_binaries()) ->
                             cb_context:context().
update_locality(Context, []) -> Context;
update_locality(Context, Numbers) ->
    case get_locality(Numbers, ?FREE_URL) of
        {'error', <<"missing phonebook url">>} -> Context;
        {'error', _} -> Context;
        {'ok', Localities} ->
            _ = wh_util:spawn(fun() ->
                                      update_phone_numbers_locality(Context, Localities)
                              end),
            update_context_locality(Context, Localities)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_context_locality(cb_context:context(), wh_json:object()) ->
                                     cb_context:context().
update_context_locality(Context, Localities) ->
    JObj = wh_json:foldl(fun update_context_locality_fold/3, cb_context:resp_data(Context), Localities),
    cb_context:set_resp_data(Context, JObj).

-spec update_context_locality_fold(ne_binary(), wh_json:object(), wh_json:object()) -> wh_json:object().
update_context_locality_fold(Key, Value, JObj) ->
    case wh_json:get_value(<<"status">>, Value) of
        <<"success">> ->
            Locality = wh_json:delete_key(<<"status">>, Value),
            wh_json:set_value([<<"numbers">>
                               ,Key
                               ,<<"locality">>
                              ], Locality, JObj);
        _Else -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_phone_numbers_locality(cb_context:context(), wh_json:object()) ->
                                           {'ok', wh_json:object()} |
                                           {'error', _}.
update_phone_numbers_locality(Context, Localities) ->
    AccountDb = cb_context:account_db(Context),
    DocId = wh_doc:id(cb_context:doc(Context), ?WNM_PHONE_NUMBER_DOC),
    case couch_mgr:open_doc(AccountDb, DocId) of
        {'ok', JObj} ->
            J = wh_json:foldl(fun update_phone_numbers_locality_fold/3, JObj, Localities),
            couch_mgr:save_doc(AccountDb, J);
        {'error', _E}=E ->
            lager:error("failed to update locality for ~s in ~s: ~p", [DocId, AccountDb, _E]),
            E
    end.

-spec update_phone_numbers_locality_fold(ne_binary(), wh_json:object(), wh_json:object()) -> wh_json:object().
update_phone_numbers_locality_fold(Key, Value, JObj) ->
    case wh_json:get_value(<<"status">>, Value) of
        <<"success">> ->
            case wh_json:get_value(Key, JObj) of
                'undefined' -> JObj;
                _Else ->
                    Locality = wh_json:delete_key(<<"status">>, Value),
                    wh_json:set_value([Key, <<"locality">>], Locality, JObj)
            end;
        _Else -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec get_locality(ne_binaries(), ne_binary()) ->
                          {'error', ne_binary()} |
                          {'ok', wh_json:object()}.
get_locality([], _) -> {'error', <<"number missing">>};
get_locality(Numbers, UrlType) ->
    case whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, UrlType) of
        'undefined' ->
            lager:error("could not get number locality url"),
            {'error', <<"missing phonebook url">>};
        Url ->
            ReqBody = wh_json:set_value(<<"data">>, Numbers, wh_json:new()),
            Uri = <<Url/binary, "/locality/metadata">>,
            case ibrowse:send_req(binary:bin_to_list(Uri), [], 'post', wh_json:encode(ReqBody)) of
                {'error', Reason} ->
                    lager:error("number locality lookup failed: ~p", [Reason]),
                    {'error', <<"number locality lookup failed">>};
                {'ok', "200", _Headers, Body} ->
                    handle_locality_resp(wh_json:decode(Body));
                {'ok', _Status, _, _Body} ->
                    lager:error("number locality lookup failed: ~p ~p", [_Status, _Body]),
                    {'error', <<"number locality lookup failed">>}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_locality_resp(wh_json:object()) ->
                                  {'error', ne_binary()} |
                                  {'ok', wh_json:object()}.
handle_locality_resp(Resp) ->
    case wh_json:get_value(<<"status">>, Resp, <<"error">>) of
        <<"success">> ->
            {'ok', wh_json:get_value(<<"data">>, Resp, wh_json:new())};
        _E ->
            lager:error("number locality lookup failed, status: ~p", [_E]),
            {'error', <<"number locality lookup failed">>}
    end.

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
            cb_context:add_system_error(
                'bad_identifier'
                ,wh_json:from_list([{<<"cause">>, Number}])
                ,Context
            );
        {'error', E} ->
            Fun = fun() -> Context end,
            set_response({wh_util:to_binary(E), <<>>}, Number, Context, Fun);
        {'ok', AccountId, Options} ->
            JObj = wh_json:set_values([{<<"account_id">>, AccountId}
                                       ,{<<"number">>, wh_number_properties:number(Options)}
                                      ]
                                      ,wh_json:new()
                                     ),
            Fun = fun() -> Context end,
            set_response({'ok', JObj}, Number, Context, Fun)
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
    Fun = fun() -> Context end,
    set_response(Result, Number, Context, Fun).

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
    cb_context:validate_request_data(?WNM_PHONE_NUMBER_DOC, Context).

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
    Fun = fun() -> Context end,
    set_response(Result, Number, Context, Fun).

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
            Fun = fun() -> Context end,
            set_response(Result, Number, Context, Fun)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_response({'ok', operation_return()} |
                   operation_return() |
                   {'dry_run', wh_proplist()} |
                   {'dry_run', ne_binary(), wh_json:object()} |
                   {binary(), binary()}
                   ,binary()
                   ,cb_context:context()
                   ,fun(() -> cb_context:context())
                  ) ->
                          cb_context:context().
set_response({'ok', {'ok', Doc}}, _, Context, _) ->
    crossbar_util:response(Doc, Context);
set_response({'ok', Doc}, _, Context, _) ->
    crossbar_util:response(Doc, Context);
set_response({'dry_run', Props}, _, Context, Fun) ->
    RespJObj = dry_run_response(Props),
    case wh_json:is_empty(RespJObj) of
        'true' -> Fun();
        'false' -> crossbar_util:response_402(RespJObj, Context)
    end;
set_response({'dry_run', ?COLLECTION, Doc}, _, Context, Fun) ->
    RespJObj = dry_run_response(?COLLECTION, Doc),
    case wh_json:is_empty(RespJObj) of
        'true' -> Fun();
        'false' -> crossbar_util:response_402(RespJObj, Context)
    end;
set_response({'error', Data}, _, Context, _) ->
    lager:debug("error: ~p", [Data]),
    crossbar_util:response_400(<<"client error">>, Data, Context);
set_response({'invalid', Reason}, _, Context, _) ->
    lager:debug("invalid: ~p", [Reason]),
    cb_context:add_validation_error(<<"address">>, <<"invalid">>, Reason, Context);
set_response({Error, Reason}, _, Context, _) ->
    lager:debug("~p: ~p", [Error, Reason]),
    cb_context:add_system_error(Error, Reason, Context);
set_response(_Else, _, Context, _) ->
    lager:debug("unexpected response: ~p", [_Else]),
    cb_context:add_system_error('unspecified_fault', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec dry_run_response(wh_proplist()) -> wh_json:object().
-spec dry_run_response(ne_binary(), wh_json:object()) -> wh_json:object().
dry_run_response(Props) ->
    case props:get_value('services', Props) of
        'undefined' -> wh_json:new();
        Services ->
            wh_services:dry_run(Services)
    end.

dry_run_response(?COLLECTION, JObj) ->
    case wh_json:get_value(<<"error">>, JObj) of
        'undefined' -> accumulate_resp(wh_json:get_value(<<"charges">>, JObj, wh_json:new()));
        _ -> JObj
    end.

-spec accumulate_resp(wh_json:object()) -> wh_json:object().
accumulate_resp(JObj) ->
    [Resp|_] =
        wh_json:foldl(
            fun(_, Value, Acc) ->
                [dry_run_response(Value)|Acc]
            end
            ,[]
            ,JObj
        ),
    Resp.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collection_process(cb_context:context()) ->
                                operation_return() |
                                {'dry_run', ne_binary(), wh_json:object()}.
-spec collection_process(cb_context:context(), ne_binary() | ne_binaries()) ->
                                operation_return() |
                                {'dry_run', ne_binary(), wh_json:object()}.
collection_process(Context) ->
    Numbers = wh_json:get_value(<<"numbers">>, cb_context:req_data(Context), []),
    Result = collection_process(Context, Numbers, 'undefined'),
    collection_process_result(Context, Result).

collection_process(Context, ?ACTIVATE) ->
    Numbers = wh_json:get_value(<<"numbers">>, cb_context:req_data(Context), []),
    Result = collection_process(Context, Numbers, ?ACTIVATE),
    collection_process_result(Context, Result).

-spec collection_process(cb_context:context(), ne_binaries(), api_binary()) ->
                                wh_json:object().
collection_process(Context, Numbers, Action) ->
    lists:foldl(
      fun(Number, Acc) ->
              case collection_action(Context, Number, Action) of
                  {'ok', JObj} ->
                      wh_json:set_value([<<"success">>, Number], JObj, Acc);
                  {'dry_run', Data} ->
                      wh_json:set_value([<<"charges">>, Number], Data, Acc);
                  {State, Error} ->
                      JObj = wh_json:from_list([{State, Error}]),
                      wh_json:set_value([<<"error">>, Number], JObj, Acc)
              end
      end
      ,wh_json:new()
      ,Numbers
     ).

-spec collection_process_result(cb_context:context(), wh_json:object()) ->
                                       operation_return() |
                                       {'dry_run', ne_binary(), wh_json:object()}.
collection_process_result(Context, JObj) ->
    ReqJObj = cb_context:req_json(Context),
    case wh_json:get_value(<<"error">>, JObj) of
        'undefined' ->
            case (not wh_json:is_true(<<"accept_charges">>, ReqJObj, 'false')) of
                'true' -> {'dry_run', ?COLLECTION, JObj};
                'false' -> {'ok', JObj}
            end;
        Error ->
            {'error', Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec collection_action(cb_context:context(), ne_binary(), ne_binary()) ->
                               operation_return() |
                               {'dry_run', wh_json:object()}.
collection_action(Context, Number, ?ACTIVATE) ->
    DryRun = not(cb_context:accepting_charges(Context)),
    case wh_number_manager:assign_number_to_account(Number
                                                    ,cb_context:account_id(Context)
                                                    ,cb_context:auth_account_id(Context)
                                                    ,cb_context:doc(Context)
                                                    ,DryRun
                                                   )
    of
        {'ok', RJObj} ->
            {'ok', wh_json:delete_key(<<"numbers">>, RJObj)};
        {'dry_run', _Data}=Resp -> Resp;
        Else -> Else
    end;
collection_action(Context, Number, _) ->
    number_action(Context, Number, cb_context:req_verb(Context)).

-spec number_action(cb_context:context(), path_token(), http_method()) ->
                           operation_return().
number_action(Context, Number, ?HTTP_PUT) ->
    wh_number_manager:create_number(Number
                                    ,cb_context:account_id(Context)
                                    ,cb_context:auth_account_id(Context)
                                    ,wh_json:delete_key(<<"numbers">>, cb_context:doc(Context))
                                   );
number_action(Context, Number, ?HTTP_POST) ->
    case wh_number_manager:get_public_fields(Number, cb_context:auth_account_id(Context)) of
        {'ok', JObj} ->
            Doc1 = wh_json:delete_key(<<"numbers">>, cb_context:doc(Context)),
            DryRun = (not wh_json:is_true(<<"accept_charges">>, cb_context:req_json(Context), 'false')),
            wh_number_manager:set_public_fields(Number
                                                ,wh_json:merge_jobjs(JObj, Doc1)
                                                ,cb_context:auth_account_id(Context)
                                                ,DryRun
                                               );
        {State, Error} ->
            lager:error("error while fetching number ~p : ~p", [Number, Error]),
            {State, Error}
    end;
number_action(Context, Number, ?HTTP_DELETE) ->
    wh_number_manager:release_number(Number, cb_context:auth_account_id(Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec has_tokens(cb_context:context()) -> boolean().
-spec has_tokens(cb_context:context(), pos_integer()) -> boolean().
has_tokens(Context) -> has_tokens(Context, 1).
has_tokens(Context, Count) ->
    Name = <<(cb_context:account_id(Context))/binary, "/", ?PHONE_NUMBERS_CONFIG_CAT/binary>>,
    case kz_buckets:consume_tokens(?APP_NAME
                                   ,Name
                                   ,cb_modules_util:token_cost(Context, Count)
                                  )
    of
        'true' -> 'true';
        'false' ->
            lager:warning("rate limiting activation limit reached, rejecting"),
            'false'
    end.
