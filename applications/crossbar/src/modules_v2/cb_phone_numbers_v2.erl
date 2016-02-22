%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_phone_numbers_v2).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,billing/1
         ,content_types_accepted/4
         ,validate/1, validate/2, validate/3
         ,validate_request/1
         ,authorize/1
         ,authenticate/1
         ,put/2, put/3
         ,post/2
         ,delete/2
         ,summary/1
         ,populate_phone_numbers/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

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

-define(CB_LIST, <<"phone_numbers/crossbar_listing">>).
-define(KNM_NUMBER, <<"numbers">>).

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

-define(PHONE_NUMBERS_CONFIG_CAT, <<?CONFIG_CAT/binary, ".phone_numbers">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Initializes the bindings this module will respond to.
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"account.created">>, ?MODULE, 'populate_phone_numbers'),
    _ = crossbar_bindings:bind(<<"v2_resource.content_types_accepted.phone_numbers">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"v2_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v2_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v2_resource.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.phone_numbers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.phone_numbers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.phone_numbers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.post.phone_numbers">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.put.phone_numbers">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.delete.phone_numbers">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc Create & populate the ?KNM_PHONE_NUMBERS_DOC of a newly created account
%%--------------------------------------------------------------------
-spec populate_phone_numbers(cb_context:context()) -> 'ok'.
populate_phone_numbers(Context) ->
    AccountDb = cb_context:account_db(Context),
    PVTs = [{<<"_id">>, ?KNM_PHONE_NUMBERS_DOC}
            ,{<<"pvt_account_db">>, AccountDb}
            ,{<<"pvt_account_id">>, cb_context:account_id(Context)}
            ,{<<"pvt_vsn">>, <<"1">>}
            ,{<<"pvt_type">>, ?KNM_PHONE_NUMBERS_DOC}
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
-spec authenticate(http_method(), req_nouns()) -> boolean().
authenticate(Context) ->
    authenticate(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authenticate(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, []}]) ->
    'true';
authenticate(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, [?PREFIX]}]) ->
    'true';
authenticate(_Verb, _Nouns) ->
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
-spec authorize(req_verb(), req_nouns()) -> boolean().
authorize(Context) ->
    authorize(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authorize(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, []}]) ->
    'true';
authorize(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, [?PREFIX]}]) ->
    'true';
authorize(_Verb, _Nouns) ->
    'false'.

%%--------------------------------------------------------------------
%% @public
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
%% @public
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
%% @doc Ensure we will be able to bill for phone_numbers
%%--------------------------------------------------------------------
-spec billing(cb_context:context()) -> cb_context:context().
billing(Context) ->
    maybe_allow_updates(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec maybe_allow_updates(cb_context:context(), req_nouns(), http_method()) -> cb_context:context().
maybe_allow_updates(Context, [{?KNM_PHONE_NUMBERS_DOC, _}|_], _Verb)
  when _Verb /= ?HTTP_GET ->
    try wh_services:allow_updates(cb_context:account_id(Context)) of
        'true' -> Context
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
maybe_allow_updates(Context, _Nouns, _Verb) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc List content types accepted by this module
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
                             ,knm_converters:available_classifiers()
                            );
validate(Context, ?LOCALITY) ->
    validate_locality(Context, cb_context:req_value(Context, <<"numbers">>));
validate(Context, ?CHECK) ->
    validate_check(Context, cb_context:req_value(Context, <<"numbers">>));
validate(Context, Number) ->
    validate_phone_number(Context, Number, cb_context:req_verb(Context)).

validate(Context, ?COLLECTION, ?ACTIVATE) ->
    validate_request(Context);
validate(Context, ?CLASSIFIERS, Number) ->
    maybe_classify(Context, Number);
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
    list_attachments(Context, Number);
validate(Context, Number, ?IDENTIFY) ->
    identify(Context, Number).

-spec validate_phone_number(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_phone_number(Context, Number, ?HTTP_GET) ->
    read(Context, Number);
validate_phone_number(Context, _Number, ?HTTP_PUT) ->
    validate_request(Context);
validate_phone_number(Context, _Number, ?HTTP_POST) ->
    validate_request(Context);
validate_phone_number(Context, _Number, ?HTTP_DELETE) ->
    validate_delete(Context).

-spec validate_collection(cb_context:context(), http_method()) -> cb_context:context().
validate_collection(Context, ?HTTP_PUT) ->
    validate_request(Context);
validate_collection(Context, ?HTTP_POST) ->
    validate_request(Context);
validate_collection(Context, ?HTTP_DELETE) ->
    validate_delete(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?LOCALITY) ->
    case cb_context:resp_status(Context) of
        'success' ->
            locality(Context, cb_context:req_value(Context, <<"numbers">>));
        _ -> Context
    end;
post(Context, ?FIX) ->
    AccountId = cb_context:account_id(Context),
    _ = knm_maintenance:fix_by_account(AccountId),
    summary(Context);
post(Context, _Number) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ?COLLECTION) ->
    put_collection(Context, cb_context:req_json(Context));
put(Context, Num) ->
    ReqJObj = cb_context:req_json(Context),
    DryRun = not wh_json:is_true(<<"accept_charges">>, ReqJObj),
    Options = [{'assigned_to', cb_context:account_id(Context)}
               ,{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', DryRun}
               ,{'public_fields', cb_context:doc(Context)}
              ],
    case knm_number:create(Num, Options) of
        {'error', Reason} ->
            error_return(Context, Reason);
        {'dry_run', Services, _ActivationCharges} ->
            dry_run_return(Context, Services);
        {'ok', Number} ->
            success_return(Context, Number)
    end.

put(Context, Num, ?ACTIVATE) ->
    ReqJObj = cb_context:req_json(Context),
    DryRun = not wh_json:is_true(<<"accept_charges">>, ReqJObj),
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', DryRun}
              ],
    case knm_number:buy(Num, cb_context:account_id(Context), Options) of
        {'error', Reason} ->
            error_return(Context, Reason);
        {'ok', Number} ->
            success_return(Context, Number)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Num) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}],
    case knm_number:delete(Num, Options) of
        {'error', Reason} -> error_return(Context, Reason);
        {'ok', _} ->
            cb_context:set_resp_status(Context, 'success')
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clean_summary(cb_context:context()) -> wh_json:object().
clean_summary(Context) ->
    AccountId = cb_context:account_id(Context),
    Routines = [fun remove_id/1
                ,fun initialize_numbers/1
                ,fun(JObj) -> set_cascade_quantity(JObj, AccountId) end
                ,fun(JObj) -> filter_numbers(JObj, Context) end
               ],
    lists:foldl(fun(F, JObj) -> F(JObj) end
                ,cb_context:resp_data(Context)
                ,Routines
               ).

-spec remove_id(wh_json:object()) -> wh_json:object().
remove_id(JObj) ->
    wh_json:delete_key(<<"id">>, JObj).

-spec initialize_numbers(wh_json:object()) -> wh_json:object().
initialize_numbers(JObj) ->
    wh_json:set_value(<<"numbers">>, JObj, wh_json:new()).

-spec set_cascade_quantity(wh_json:object(), ne_binary()) ->
                                  wh_json:object().
set_cascade_quantity(JObj, AccountId) ->
    Service = wh_services:fetch(AccountId),
    Quantity = wh_services:cascade_category_quantity(?KNM_PHONE_NUMBERS_DOC, [], Service),
    wh_json:set_value(<<"cascade_quantity">>, Quantity, JObj).

filter_numbers(JObj, Context) ->
    QS = cb_context:query_string(Context),
    Numbers = wh_json:get_value(<<"numbers">>, JObj),
    wh_json:set_value(<<"numbers">>, apply_filters(QS, Numbers), JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec apply_filters(wh_json:object(), wh_json:object()) ->
                           wh_json:object().
apply_filters(QueryString, Numbers) ->
    wh_json:foldl(fun apply_filters_fold/3, Numbers, QueryString).

-spec apply_filters_fold(ne_binary(), ne_binary(), wh_json:object()) ->
                                wh_json:object().
apply_filters_fold(<<"filter_", Key/binary>>, Value, Numbers) ->
    apply_filter(Key, Value, Numbers);
apply_filters_fold(_Key, _Value, Numbers) ->
    lager:debug("unknown key ~s, ignoring", [_Key]),
    Numbers.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec apply_filter(ne_binary(), ne_binary(), wh_json:object()) ->
                          wh_json:object().
apply_filter(Key, Value, Numbers) ->
    wh_json:foldl(
      fun(Number, JObj, Acc) ->
              case wh_json:get_value(Key, JObj) of
                  Value -> Acc;
                  _Else -> wh_json:delete_key(Number, Acc)
              end
      end
      ,Numbers
      ,Numbers
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
    cb_context:validate_request_data(?FIND_NUMBER_SCHEMA
                                     ,cb_context:set_req_data(Context, JObj)
                                     ,fun execute_find_numbers/1
                                    ).

-spec execute_find_numbers(cb_context:context()) ->
                                  cb_context:context().
execute_find_numbers(Context) ->
    JObj = cb_context:req_data(Context),
    Prefix = wh_json:get_ne_value(<<"prefix">>, JObj),
    Quantity = wh_json:get_integer_value(<<"quantity">>, JObj, 1),

    cb_context:setters(
      Context
      ,[{fun cb_context:set_resp_data/2
         ,knm_carriers:find(Prefix, Quantity, wh_json:to_proplist(JObj))
        }
        ,{fun cb_context:set_resp_status/2, 'success'}
       ]).

-spec get_find_numbers_req(cb_context:context()) -> wh_json:object().
get_find_numbers_req(Context) ->
    JObj = cb_context:query_string(Context),
    AccountId = cb_context:auth_account_id(Context),
    Quantity = wh_util:to_integer(cb_context:req_value(Context, <<"quantity">>, 1)),
    wh_json:set_values([{<<"quantity">>, Quantity}
                        ,{<<"account_id">>, AccountId}
                       ]
                       ,JObj
                      ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_check(cb_context:context(), any()) -> cb_context:context().
validate_check(Context, 'undefined') ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"required">>
      ,wh_json:from_list([{<<"message">>, <<"list of numbers missing">>}
                         ])
      ,Context
     );
validate_check(Context, []) ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"minimum">>
      ,wh_json:from_list([{<<"message">>, <<"minimum 1 number required">>}
                         ])
      ,Context
     );
validate_check(Context, Numbers) when is_list(Numbers) ->
    Unformatted = knm_carriers:check(Numbers),
    cb_context:set_resp_data(
      cb_context:set_resp_status(Context, 'success')
      ,format_carriers_check(Unformatted)
     );
validate_check(Context, Error) ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"type">>
      ,wh_json:from_list([{<<"message">>, <<"numbers must be a list">>}
                          ,{<<"cause">>, Error}
                         ])
      ,Context
     ).

%% @private
-spec format_carriers_check(list()) -> wh_json:object().
-spec format_carriers_check(list(), wh_json:object()) -> wh_json:object().
format_carriers_check(Checked) -> format_carriers_check(Checked, wh_json:new()).
format_carriers_check([], JObj) -> JObj;
format_carriers_check([{_Module, {'ok', ModuleResults}}|Rest], JObj) ->
    JObj1 =
        lists:foldl(
          fun ({Number, Status}, Acc) ->
                  wh_json:set_value(Number, Status, Acc)
          end
          ,JObj
          ,wh_json:to_proplist(ModuleResults)
         ),
    format_carriers_check(Rest, JObj1);
format_carriers_check([_|Rest], JObj) ->
    format_carriers_check(Rest, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_locality(cb_context:context(), any()) ->
                               cb_context:context().
validate_locality(Context, 'undefined') ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"required">>
      ,wh_json:from_list(
         [{<<"message">>, <<"list of numbers missing">>}]
        )
      ,Context
     );
validate_locality(Context, []) ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"minimum">>
      ,wh_json:from_list(
         [{<<"message">>, <<"minimum 1 number required">>}]
        )
      ,Context
     );
validate_locality(Context, Numbers) when is_list(Numbers) ->
    cb_context:set_resp_status(Context, 'success');
validate_locality(Context, Numbers) ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"type">>
      ,wh_json:from_list(
         [{<<"cause">>, Numbers}
          ,{<<"message">>, <<"numbers must be a list">>}
         ])
      ,Context
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_classify(cb_context:context(), path_token()) -> cb_context:context().
maybe_classify(Context, Number) ->
    case knm_converters:classify(Number) of
        'undefined' -> unclassified(Context, Number);
        Classifier ->    classified(Context, Number, Classifier)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unclassified(cb_context:context(), path_token()) ->
                          cb_context:context().
unclassified(Context, Number) ->
    RespData = base_classified(Context, Number),
    cb_context:setters(
      Context
      ,[{fun cb_context:set_resp_data/2, wh_json:from_list(RespData)}
        ,{fun cb_context:set_resp_status/2, 'success'}
       ]
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec classified(cb_context:context(), path_token(), api_binary()) ->
                        cb_context:context().
classified(Context, Number, Classifier) ->
    ClassifierJObj = wh_json:get_value(Classifier, knm_converters:available_classifiers()),
    RespData =
        wh_json:set_values(
          [{<<"name">>, Classifier}
           | base_classified(Context, Number)]
          ,ClassifierJObj
         ),
    cb_context:setters(
      Context
      ,[{fun cb_context:set_resp_data/2, RespData}
        ,{fun cb_context:set_resp_status/2, 'success'}
       ]
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec base_classified(cb_context:context(), ne_binary()) ->
                             wh_proplist().
base_classified(_Context, Number) ->
    Normalized = knm_converters:normalize(Number),
    [{<<"number">>, Number}
     ,{<<"e164">>, Normalized}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_prefix(cb_context:context()) -> cb_context:context().
find_prefix(Context) ->
    case wh_json:get_ne_value(<<"city">>, cb_context:query_string(Context)) of
        'undefined' ->
            cb_context:add_system_error('bad_identifier', Context);
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

%% @private
-spec get_prefix(ne_binary()) -> {'ok', wh_json:object()} |
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
            case kz_http:get(Uri) of
                {'ok', 200, _Headers, Body} ->
                    JObj = wh_json:decode(Body),
                    case wh_json:get_value(<<"data">>, JObj) of
                        'undefined' -> {'error ', JObj};
                        Data -> {'ok', Data}
                    end;
                {'ok', _Status, _Headers, Body} -> {'error', wh_json:decode(Body)};
                {'error', _Reason}=E -> E
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Attempt to load a summarized listing of all instances of this resource.
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = crossbar_doc:load(?KNM_PHONE_NUMBERS_DOC, Context),
    case cb_context:resp_error_code(Context1) of
        404 -> crossbar_util:response(wh_json:new(), Context1);
        _Code ->
            Context2 = cb_context:set_resp_data(Context1, clean_summary(Context1)),
            case cb_context:resp_status(Context2) of
                'success' ->  maybe_update_locality(Context2);
                _Status -> Context2
            end
    end.

-spec maybe_update_locality(cb_context:context()) ->
                                   cb_context:context().
maybe_update_locality(Context) ->
    Numbers = wh_json:foldl(
                fun update_locality_fold/3
                ,[]
                ,wh_json:get_value(<<"numbers">>, cb_context:resp_data(Context))
               ),
    update_locality(Context, Numbers).

-spec update_locality_fold(ne_binaries(), wh_json:object(), ne_binaries()) ->
                                  ne_binaries().
update_locality_fold(Key, Value, Acc) ->
    case wh_json:get_value(<<"locality">>, Value) =:= 'undefined'
        andalso knm_converters:is_reconcilable(Key)
    of
        'true' -> [Key|Acc];
        'false' -> Acc
    end.

-spec update_locality(cb_context:context(), ne_binaries()) ->
                             cb_context:context().
update_locality(Context, []) -> Context;
update_locality(Context, Numbers) ->
    case get_locality(Numbers, ?FREE_URL) of
        {'error', <<"missing phonebook url">>} -> Context;
        {'error', _} -> Context;
        {'ok', Localities} ->
            _ = wh_util:spawn(fun update_phone_numbers_locality/2, [Context, Localities]),
            update_context_locality(Context, Localities)
    end.

-spec update_context_locality(cb_context:context(), wh_json:object()) ->
                                     cb_context:context().
update_context_locality(Context, Localities) ->
    JObj = wh_json:foldl(fun update_context_locality_fold/3
                         ,cb_context:resp_data(Context)
                         ,Localities
                        ),
    cb_context:set_resp_data(Context, JObj).

-spec update_context_locality_fold(ne_binary(), wh_json:object(), wh_json:object()) ->
                                          wh_json:object().
-spec update_context_locality_fold(ne_binary(), wh_json:object(), wh_json:object(), ne_binary()) ->
                                          wh_json:object().
update_context_locality_fold(Key, Value, JObj) ->
    update_context_locality_fold(Key, Value, JObj, wh_json:get_value(<<"status">>, Value)).

update_context_locality_fold(Key, Value, JObj, <<"success">>) ->
    Locality = wh_json:delete_key(<<"status">>, Value),
    NewKey = [<<"numbers">>, Key, <<"locality">>],
    wh_json:set_value(NewKey, Locality, JObj);
update_context_locality_fold(_Key, _Value, JObj, _Status) ->
    JObj.

-spec get_locality(ne_binaries(), ne_binary()) -> {'error', ne_binary()} |
                                                  {'ok', wh_json:object()}.
get_locality([], _) -> {'error', <<"number missing">>};
get_locality(Numbers, UrlType) ->
    case whapps_config:get(?PHONE_NUMBERS_CONFIG_CAT, UrlType) of
        'undefined' ->
            lager:error("could not get number locality url"),
            {'error', <<"missing phonebook url">>};
        Url ->
            query_locality(Numbers, Url)
    end.

-spec query_locality(ne_binaries(), ne_binary()) ->
                            {'error', ne_binary()} |
                            {'ok', wh_json:object()}.
query_locality(Numbers, Url) ->
    ReqBody = wh_json:set_value(<<"data">>, Numbers, wh_json:new()),
    Uri = <<Url/binary, "/locality/metadata">>,
    case kz_http:post(wh_util:to_list(Uri), [], wh_json:encode(ReqBody)) of
        {'ok', 200, _Headers, Body} ->
            handle_locality_resp(wh_json:decode(Body));
        {'error', Reason} ->
            lager:error("number locality lookup failed: ~p", [Reason]),
            {'error', <<"number locality lookup failed">>};
        {'ok', _Status, _, _Body} ->
            lager:error("number locality lookup failed: ~p ~p", [_Status, _Body]),
            {'error', <<"number locality lookup failed">>}
    end.

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

-spec update_phone_numbers_locality(cb_context:context(), wh_json:object()) ->
                                           {'ok', wh_json:object()} |
                                           couch_mgr:couchbeam_error().
update_phone_numbers_locality(Context, Localities) ->
    AccountDb = cb_context:account_db(Context),
    DocId = wh_doc:id(cb_context:doc(Context), ?KNM_PHONE_NUMBERS_DOC),
    case couch_mgr:open_doc(AccountDb, DocId) of
        {'ok', JObj} ->
            J = wh_json:foldl(fun update_phone_numbers_locality_fold/3, JObj, Localities),
            couch_mgr:save_doc(AccountDb, J);
        {'error', _E}=E ->
            lager:error("failed to update locality for ~s in ~s: ~p", [DocId, AccountDb, _E]),
            E
    end.

-spec update_phone_numbers_locality_fold(ne_binary(), wh_json:object(), wh_json:object()) ->
                                                wh_json:object().
-spec update_phone_numbers_locality_fold(ne_binary(), wh_json:object(), wh_json:object(), ne_binary()) ->
                                                wh_json:object().

update_phone_numbers_locality_fold(Key, Value, JObj) ->
    update_phone_numbers_locality_fold(Key, Value, JObj, wh_json:get_value(<<"status">>, Value)).

update_phone_numbers_locality_fold(Key, Value, JObj, <<"success">>) ->
    case wh_json:get_value(Key, JObj) of
        'undefined' -> JObj;
        _Else ->
            Locality = wh_json:delete_key(<<"status">>, Value),
            wh_json:set_value([Key, <<"locality">>], Locality, JObj)
    end;
update_phone_numbers_locality_fold(_Key, _Value, JObj, _Status) ->
    JObj.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, Num) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    case knm_number:get(Num, [{'auth_by', AuthAccountId}]) of
        {'error', Reason} -> error_return(Context, Reason);
        {'ok', Number} -> success_return(Context, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list_attachments(cb_context:context(), ne_binary()) -> cb_context:context().
list_attachments(Context, Num) ->
    AuthBy = cb_context:auth_account_id(Context),
    {'ok', Number} = knm_number:get(Num),
    case knm_phone_number:list_attachments(knm_number:phone_number(Number), AuthBy) of
        {'ok', JObj} ->
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,JObj
             );
        {'error', _R} ->
            cb_context:add_system_error('unauthorized', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
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
    cb_context:validate_request_data(?KNM_NUMBER, Context).

get_auth_user_email(Context) ->
    case wh_json:get_value(<<"owner_id">>, cb_context:auth_doc(Context)) of
        'undefined' -> 'undefined';
        UserId ->
            AccountId = cb_context:auth_account_id(Context),
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            case couch_mgr:open_doc(AccountDb, UserId) of
                {'ok', User} -> wh_json:get_value(<<"email">>, User);
                {'error', _} -> 'undefined'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_delete(cb_context:context()) -> cb_context:context().
validate_delete(Context) ->
    Context1 = cb_context:set_resp_status(Context, 'success'),
    cb_context:set_doc(Context1, 'undefined').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec locality(cb_context:context(), ne_binaries()) -> cb_context:context().
locality(Context, Numbers) ->
    case knm_locality:fetch(Numbers) of
        {'ok', JObj} ->
            cb_context:set_resp_data(Context, JObj);
        {'error', Reason} ->
            JObj = wh_json:from_list([{<<"message">>, wh_util:to_binary(Reason)}]),
            cb_context:add_system_error('unspecified_fault', JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec has_tokens(cb_context:context()) -> boolean().
-spec has_tokens(cb_context:context(), pos_integer()) -> boolean().
has_tokens(Context) ->
    has_tokens(Context, 1).
has_tokens(Context, Count) ->
    Name = <<(cb_context:account_id(Context))/binary, "/", ?PHONE_NUMBERS_CONFIG_CAT/binary>>,
    Cost = cb_modules_util:token_cost(Context, Count),
    case kz_buckets:consume_tokens(?APP_NAME, Name, Cost) of
        'true' -> 'true';
        'false' ->
            lager:warning("rate limiting activation limit reached, rejecting"),
            'false'
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
    case knm_number:lookup_account(Number) of
        {'error', 'not_reconcilable'} ->
            cb_context:add_system_error(
                'bad_identifier'
                ,wh_json:from_list([{<<"cause">>, Number}])
                ,Context
            );
        {'error', E} -> error_return(Context, E);
        {'ok', AccountId, Options} ->
            Public = knm_phone_number:to_public_json(knm_number:phone_number(knm_number:number(Options))),
            JObj = wh_json:set_values([{<<"account_id">>, AccountId}
                                       ,{<<"number">>, Public}
                                      ]
                                      ,wh_json:new()
                                     ),
            crossbar_util:response(JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec error_return(cb_context:context(), knm_errors:error()) ->
                          cb_context:context().
error_return(Context, Error) ->
    Code = knm_errors:code(Error),
    Msg = knm_errors:error(Error),
    cb_context:add_system_error(Code, Msg, Error, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec success_return(cb_context:context(), knm_number:knm_number()) ->
                            cb_context:context().
success_return(Context, Number) ->
    Num = knm_phone_number:to_public_json(knm_number:phone_number(Number)),
    Routines = [{fun cb_context:set_resp_data/2, Num}
                ,{fun cb_context:set_resp_status/2, 'success'}
               ],
    cb_context:setters(Context, Routines).

-spec dry_run_return(cb_context:context(), wh_services:services()) ->
                            cb_context:context().
dry_run_return(Context, Services) ->
    DryRun = wh_services:dry_run(Services),
    crossbar_util:response(DryRun, Context).
