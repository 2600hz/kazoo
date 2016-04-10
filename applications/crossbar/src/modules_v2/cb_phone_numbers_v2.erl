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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,billing/1
         ,validate/1, validate/2, validate/3
         ,validate_request/1
         ,authorize/1
         ,authenticate/1
         ,put/2, put/3
         ,post/2
         ,delete/2
         ,populate_phone_numbers/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(CB_LIST, <<"phone_numbers/crossbar_listing">>).

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
-define(KEY_PHONEBOOK_FREE_URL, <<"phonebook_url">>).
-define(KEY_PHONEBOOK_PAID_URL, <<"phonebook_url_premium">>).
-define(PREFIX, <<"prefix">>).
-define(LOCALITY, <<"locality">>).
-define(CHECK, <<"check">>).

-define(MAX_TOKENS, whapps_config:get_integer(?PHONE_NUMBERS_CONFIG_CAT, <<"activations_per_day">>, 100)).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"account.created">>, ?MODULE, 'populate_phone_numbers'),
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
    Now = wh_util:current_tstamp(),
    PVTs = [{<<"_id">>, ?KNM_PHONE_NUMBERS_DOC}
            ,{<<"pvt_account_db">>, AccountDb}
            ,{<<"pvt_account_id">>, cb_context:account_id()}
            ,{<<"pvt_vsn">>, <<"1">>}
            ,{<<"pvt_type">>, ?KNM_PHONE_NUMBERS_DOC}
            ,{<<"pvt_modified">>, Now}
            ,{<<"pvt_created">>, Now}
           ],
    _ = kz_datamgr:save_doc(AccountDb, wh_json:from_list(PVTs)),
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
maybe_authenticate(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, []}]) ->
    'true';
maybe_authenticate(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, [?PREFIX]}]) ->
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

maybe_authorize(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, []}]) ->
    'true';
maybe_authorize(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, [?PREFIX]}]) ->
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
allowed_methods(_PhoneNumber) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?COLLECTION, ?ACTIVATE) ->
    [?HTTP_PUT];
allowed_methods(?CLASSIFIERS, _PhoneNumber) ->
    [?HTTP_GET];
allowed_methods(_PhoneNumber, ?ACTIVATE) ->
    [?HTTP_PUT];
allowed_methods(_PhoneNumber, ?RESERVE) ->
    [?HTTP_PUT];
allowed_methods(_PhoneNumber, ?IDENTIFY) ->
    [?HTTP_GET].

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
resource_exists() -> 'true'.

resource_exists(?FIX) -> 'true';
resource_exists(?PREFIX) -> 'true';
resource_exists(?LOCALITY) -> 'true';
resource_exists(?CHECK) -> 'true';
resource_exists(?CLASSIFIERS) -> 'true';
resource_exists(_PhoneNumber) -> 'true'.

resource_exists(_PhoneNumber, ?ACTIVATE) -> 'true';
resource_exists(_PhoneNumber, ?RESERVE) -> 'true';
resource_exists(_PhoneNumber, ?IDENTIFY) -> 'true';
resource_exists(?CLASSIFIERS, _PhoneNumber) -> 'true';
resource_exists(_, _) -> 'false'.

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
maybe_allow_updates(Context, [{?KNM_PHONE_NUMBERS_DOC, _}|_], ?HTTP_GET) ->
    Context;
maybe_allow_updates(Context, [{?KNM_PHONE_NUMBERS_DOC, _}|_], _Verb) ->
    try wh_services:allow_updates(cb_context:account_id(Context)) of
        'true' -> Context
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
maybe_allow_updates(Context, _Nouns, _Verb) -> Context.

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
    find_locality(Context);
validate(Context, ?CHECK) ->
    check_number(Context, cb_context:req_value(Context, <<"numbers">>));
validate(Context, Number) ->
    validate_number(Context, Number, cb_context:req_verb(Context)).

-spec validate_number(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_number(Context, Number, ?HTTP_GET) ->
    read(Context, Number);
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
validate(Context, Number, ?IDENTIFY) ->
    identify(Context, Number).

-spec classify_number(cb_context:context(), path_token()) -> cb_context:context().
classify_number(Context, Number) ->
    case knm_converters:classify(Number) of
        'undefined' -> unclassified_number(Context, Number);
        Classifier ->    classified_number(Context, Number, Classifier)
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
    [{<<"number">>, Number}
     ,{<<"e164">>, knm_converters:normalize(Number)}
    ].

-spec classified_number(cb_context:context(), path_token(), api_binary()) -> cb_context:context().
classified_number(Context, Number, Classifier) ->
    ClassifierJObj = wh_json:get_value(Classifier, knm_converters:available_classifiers()),
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

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?FIX) ->
    %% TODO
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
    Options = [{'assign_to', cb_context:account_id(Context)}
               ,{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', not cb_context:accepting_charges(Context)}
               ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:create(Number, Options),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:post(cb_context:set_req_json(Context, NewReqJObj), Number)
          end,
    set_response(Result, Number, Context, Fun).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ?COLLECTION) ->
    put_collection(Context, cb_context:req_json(Context));
put(Context, Number) ->
    put_number(Context, Number, cb_context:req_json(Context)).

-spec put_collection(cb_context:context(), wh_json:object()) -> cb_context:context().
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
    Options = [{'assign_to', cb_context:account_id(Context)}
               ,{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', not cb_context:accepting_charges(Context)}
               ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:create(Number, Options),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), Number)
          end,
    set_response(Result, Number, Context, Fun).

put(Context, ?COLLECTION, ?ACTIVATE) ->
    activate_collection(Context, cb_context:req_json(Context));
put(Context, Number, ?ACTIVATE) ->
    activate_number(Context, Number, cb_context:req_json(Context));
put(Context, Number, ?RESERVE) ->
    reserve_number(Context, Number, cb_context:req_json(Context)).

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
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', not cb_context:accepting_charges(Context)}
               ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:buy(Number, cb_context:account_id(Context), Options),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), Number, ?ACTIVATE)
          end,
    set_response(Result, Number, Context, Fun).

-spec reserve_number(cb_context:context(), path_token(), wh_json:object()) ->
                            cb_context:context().
reserve_number(Context, Number, ReqJObj) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
               ,{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', cb_context:accepting_charges(Context)}
               ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:reserve(Number, Options),
    Fun = fun() ->
                  NewReqJObj = wh_json:set_value(<<"accept_charges">>, <<"true">>, ReqJObj),
                  ?MODULE:put(cb_context:set_req_json(Context, NewReqJObj), Number, ?RESERVE)
          end,
    set_response(Result, Number, Context, Fun).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ?COLLECTION) ->
    Numbers = wh_json:get_value(<<"numbers">>, cb_context:req_data(Context), []),
    Results = collection_process(Context, Numbers, <<"delete">>),
    set_response({'ok', Results}, <<>>, Context);
delete(Context, Number) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', not cb_context:accepting_charges(Context)}
              ],
    Result = knm_number:delete(Number, Options),
    set_response(Result, Number, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Lists numbers on GET /v2/accounts/{{ACCOUNT_ID}}/phone_numbers[/{{DID}}]
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = view_account_phone_numbers(Context),
    case cb_context:resp_status(Context1) of
        'success' -> maybe_update_locality(Context1);
        _Status -> Context1
    end.

%% @private
-spec view_account_phone_numbers(cb_context:context()) -> cb_context:context().
view_account_phone_numbers(Context) ->
    Context1 = crossbar_doc:load_view(?CB_LIST, [], rename_qs_filters(Context), fun normalize_view_results/2),
    ListOfNumProps = cb_context:resp_data(Context1),
    NumbersJObj = lists:foldl(fun wh_json:merge_jobjs/2, wh_json:new(), ListOfNumProps),
    Service = wh_services:fetch(cb_context:account_id(Context)),
    Quantity = wh_services:cascade_category_quantity(?KNM_PHONE_NUMBERS_DOC, [], Service),
    NewRespData = wh_json:from_list([ {<<"numbers">>, NumbersJObj}
                                    , {<<"casquade_quantity">>, Quantity}
                                    ]),
    cb_context:set_resp_data(Context1, NewRespData).

%% @private
-spec rename_qs_filters(cb_context:context()) -> cb_context:context().
rename_qs_filters(Context) ->
    Renamer = fun
                  (<<"filter_state">>, Value)       -> {<<"filter_pvt_state">>, Value};
                  (<<"filter_assigned_to">>, Value) -> {<<"filter_pvt_assigned_to">>, Value};
                  (<<"filter_locality">>, Value)    -> {<<"filter_pvt_locality">>, Value};
                  (K, V) -> {K, V}
              end,
    NewQS = wh_json:map(Renamer, cb_context:query_string(Context)),
    cb_context:set_query_string(Context, NewQS).

%% @private
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    Number = wh_json:get_value(<<"key">>, JObj),
    Properties = wh_json:get_value(<<"value">>, JObj),
    [ wh_json:set_value(Number, Properties, wh_json:new())
      | Acc
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc Search for numbers, requesting carrier module
%%--------------------------------------------------------------------
-spec find_numbers(cb_context:context()) -> cb_context:context().
find_numbers(Context) ->
    JObj = get_find_numbers_req(Context),
    Prefix = wh_json:get_ne_value(?PREFIX, JObj),
    Quantity = wh_json:get_value(<<"quantity">>, JObj, 1),
    OnSuccess =
        fun(C) ->
                Found = knm_carriers:find(Prefix, Quantity, wh_json:to_proplist(JObj)),
                cb_context:setters(C
                                  ,[{fun cb_context:set_resp_data/2, Found}
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
            case fetch_locality(Numbers, Url) of
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
%% @end
%%--------------------------------------------------------------------
-spec check_number(cb_context:context(), any()) -> cb_context:context().
check_number(Context, 'undefined') ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"required">>
      ,wh_json:from_list([{<<"message">>, <<"list of numbers missing">>}
                         ])
      ,Context
     );
check_number(Context, []) ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"minimum">>
      ,wh_json:from_list([{<<"message">>, <<"minimum 1 number required">>}
                         ])
      ,Context
     );
check_number(Context, Numbers) when is_list(Numbers) ->
    Unformatted = knm_carriers:check(Numbers),
    cb_context:set_resp_data(
      cb_context:set_resp_status(Context, 'success')
      ,format_carriers_check(Unformatted)
     );
check_number(Context, E) ->
    cb_context:add_validation_error(
      <<"numbers">>
      ,<<"type">>
      ,wh_json:from_list([{<<"message">>, <<"numbers must be a list">>}
                          ,{<<"cause">>, E}
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

-spec get_url(any()) -> ne_binary().
get_url(<<"high">>) -> ?KEY_PHONEBOOK_PAID_URL;
get_url(_) -> ?KEY_PHONEBOOK_FREE_URL.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_prefix(ne_binary()) -> {'ok', wh_json:object()} |
                                 {'error', any()}.
get_prefix(City) ->
    case whapps_config:get_string(?PHONE_NUMBERS_CONFIG_CAT, ?KEY_PHONEBOOK_FREE_URL) of
        'undefined' ->
            {'error', <<"Unable to acquire numbers missing carrier url">>};
        Url ->
            Country = whapps_config:get_string(?PHONE_NUMBERS_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
            ReqParam = wh_util:uri_encode(City),
            case kz_http:get(lists:flatten([Url, "/", Country, "/city?pattern=", ReqParam])) of
                {'ok', 200, _Headers, Body} ->
                    JObj = wh_json:decode(Body),
                    case wh_json:get_value(<<"data">>, JObj) of
                        'undefined' -> {'error', JObj};
                        Data -> {'ok', Data}
                    end;
                {'ok', _Status, _Headers, Body} ->
                    {'error', wh_json:decode(Body)};
                {'error', _Reason}=E ->
                    E
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Tries to fill [Number,locality] field with info from phonebook.
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_locality(cb_context:context()) -> cb_context:context().
maybe_update_locality(Context) ->
    Numbers = wh_json:get_value(<<"numbers">>, cb_context:resp_data(Context)),
    ToUpdate =
        [Num || {Num,NumProps} <- wh_json:to_proplist(Numbers),
                wh_json:get_value(<<"locality">>, NumProps) =:= 'undefined'
                    andalso knm_converters:is_reconcilable(Num)
        ],
    update_locality(Context, ToUpdate).

%% @private
-spec update_locality(cb_context:context(), ne_binaries()) ->
                             cb_context:context().
update_locality(Context, []) -> Context;
update_locality(Context, Numbers) ->
    case fetch_locality(Numbers, ?KEY_PHONEBOOK_FREE_URL) of
        {'ok', Localities} ->
            _ = wh_util:spawn(fun update_phone_numbers_locality/2, [Context, Localities]),
            update_context_locality(Context, Localities);
        {'error', _} -> Context
    end.

%% @private
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
            Path = [<<"numbers">>, Key, <<"locality">>],
            wh_json:set_value(Path, Locality, JObj);
        _Else -> JObj
    end.

%% @private
-spec update_phone_numbers_locality(cb_context:context(), wh_json:object()) ->
                                           {'ok', wh_json:object()} |
                                           {'error', any()}.
update_phone_numbers_locality(Context, Localities) ->
    AccountDb = cb_context:account_db(Context),
    DocId = wh_doc:id(cb_context:doc(Context), ?KNM_PHONE_NUMBERS_DOC),
    case kz_datamgr:open_doc(AccountDb, DocId) of
        {'ok', JObj} ->
            J = wh_json:foldl(fun update_phone_numbers_locality_fold/3, JObj, Localities),
            kz_datamgr:save_doc(AccountDb, J);
        {'error', _E}=E ->
            lager:error("failed to update locality for ~s in ~s: ~p", [DocId, AccountDb, _E]),
            E
    end.

%% @private
-spec update_phone_numbers_locality_fold(ne_binary(), wh_json:object(), wh_json:object()) ->
                                                wh_json:object().
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

%% @private
-spec fetch_locality(ne_binaries(), ne_binary()) -> {'ok', wh_json:object()} |
                                                    {'error', any()}.
fetch_locality(Numbers, PhonebookUrlType) ->
    case whapps_config:get_string(?PHONE_NUMBERS_CONFIG_CAT, PhonebookUrlType) of
        'undefined' ->
            lager:error("could not get locality url ~s", [PhonebookUrlType]),
            {'error', <<"could not get locality url">>};
        URL ->
            ReqBody = wh_json:set_value(<<"data">>, Numbers, wh_json:new()),
            case kz_http:post(URL++"/locality/metadata", [], wh_json:encode(ReqBody)) of
                {'ok', 200, _Headers, Body} ->
                    handle_phonebook_resp(wh_json:decode(Body));
                {'ok', _Status, _, _Body} ->
                    lager:error("number locality lookup failed: ~p ~p", [_Status, _Body]),
                    {'error', <<"number locality lookup failed">>};
                {'error', _Reason} ->
                    lager:error("number locality lookup failed: ~p", [_Reason]),
                    {'error', <<"number locality lookup failed">>}
            end
    end.

%% @private
-spec handle_phonebook_resp(wh_json:object()) -> 'ok'.
handle_phonebook_resp(Resp) ->
    case wh_json:get_value(<<"status">>, Resp) of
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
    case knm_number:lookup_account(Number) of
        {'error', 'not_reconcilable'} ->
            cb_context:add_system_error(
                'bad_identifier'
                ,wh_json:from_list([{<<"cause">>, Number}])
                ,Context
            );
        {'error', E} ->
            set_response({wh_util:to_binary(E), <<>>}, Number, Context);
        {'ok', AccountId, Options} ->
            JObj = wh_json:set_values([{<<"account_id">>, AccountId}
                                       ,{<<"number">>, knm_number:number(Options)}
                                      ]
                                      ,wh_json:new()
                                     ),
            set_response({'ok', JObj}, Number, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Load an instance from the database
%%--------------------------------------------------------------------
-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, Number) ->
    ViewOptions = [ {'keys', [knm_converters:normalize(Number)]}
                  ],
    case
        cb_context:resp_data(
          crossbar_doc:load_view(?CB_LIST, ViewOptions, Context, fun normalize_view_results/2)
         )
    of
        [NumberFound] ->
            [Id] = wh_json:get_keys(NumberFound),
            NewRespData = wh_json:set_value(<<"id">>, Id, wh_json:get_value(Id, NumberFound)),
            crossbar_util:response(NewRespData, Context);
        [] ->
            Msg = wh_json:from_list([ {<<"message">>, <<"bad identifier">>}
                                    , {<<"not_found">>, <<"The number could not be found">>}
                                    ]),
            cb_context:add_system_error('bad_identifier', Msg, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(cb_context:context()) -> cb_context:context().
validate_request(Context) ->
    cb_context:validate_request_data(?KNM_PHONE_NUMBERS_DOC, Context).

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
%%
%% @end
%%--------------------------------------------------------------------
set_response(Result, Number, Context) ->
    set_response(Result, Number, Context, fun () -> Context end).

-spec set_response({'ok', wh_json:object()} |
                   knm_number_return() |
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
    case knm_number:is_number(Doc) of
        'true' -> crossbar_util:response(knm_number:to_public_json(Doc), Context);
        'false' -> crossbar_util:response(Doc, Context)
    end;
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
set_response({'dry_run', Services, _ActivationCharges}, _, Context, _) ->
    DryRun = wh_services:dry_run(Services),
    crossbar_util:response(DryRun, Context);
set_response({'error', Data}, _, Context, _) ->
    case wh_json:is_json_object(Data) of
        'true' ->
            Code = knm_errors:code(Data),
            Msg = knm_errors:error(Data),
            lager:debug("error ~p: ~p", [Code, Msg]),
            cb_context:add_system_error(Code, Msg, Data, Context);
        'false' ->
            lager:debug("error: ~p", [Data]),
            crossbar_util:response_400(<<"client error">>, Data, Context)
    end;
set_response({'invalid', Reason}, _, Context, _) ->
    lager:debug("invalid: ~p", [Reason]),
    cb_context:add_validation_error(<<"address">>, <<"invalid">>, Reason, Context);
set_response({Error, Reason}, _, Context, _) ->
    lager:debug("error ~p: ~p", [Error, Reason]),
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
-spec collection_process(cb_context:context()) -> process_result().
-spec collection_process(cb_context:context(), ne_binary() | ne_binaries()) -> process_result().
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
                  {'ok', Thing} ->
                      JObj = case wh_json:is_json_object(Thing) of
                                 'true' -> Thing;
                                 'false' -> knm_number:to_public_json(Thing)
                             end,
                      wh_json:set_value([<<"success">>, Number], JObj, Acc);
                  {'dry_run', _Services, ActivationCharges} ->
                      wh_json:set_value([<<"charges">>, Number], ActivationCharges, Acc);
                  {'error', KNMError} ->
                      wh_json:set_value([<<"error">>, Number], KNMError, Acc)
              end
      end
      ,wh_json:new()
      ,Numbers
     ).


-type process_result() :: {'ok', wh_json:object()} |
                          {'error', any()} |
                          {'dry_run', ne_binary(), wh_json:object()}.

-spec collection_process_result(cb_context:context(), wh_json:object()) -> process_result().
collection_process_result(Context, JObj) ->
    case wh_json:get_value(<<"error">>, JObj) of
        'undefined' ->
            case not cb_context:accepting_charges(Context) of
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
                               knm_number_return() |
                               {'ok', wh_json:object()}.
collection_action(Context, Number, ?ACTIVATE) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', not cb_context:accepting_charges(Context)}
               ,{'public_fields', wh_json:delete_key(<<"numbers">>, cb_context:doc(Context))}
              ],
    case knm_number:move(Number, cb_context:account_id(Context), Options) of
        {'ok', KNum} ->
            wh_json:delete_key(<<"numbers">>, knm_number:to_public_json(KNum));
        _Else -> _Else
    end;
collection_action(Context, Number, _) ->
    number_action(Context, Number, cb_context:req_verb(Context)).

-spec number_action(cb_context:context(), path_token(), http_method()) ->
                           knm_number_return().
number_action(Context, Number, ?HTTP_PUT) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
               ,{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', not cb_context:accepting_charges(Context)}
               ,{'public_fields', wh_json:delete_key(<<"numbers">>, cb_context:doc(Context))}
              ],
    knm_number:create(Number, Options);
number_action(Context, Number, ?HTTP_POST) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', not cb_context:accepting_charges(Context)}
              ],
    ToMerge = wh_json:delete_key(<<"numbers">>, cb_context:doc(Context)),
    Routines = [{fun knm_phone_number:update_doc/2, ToMerge}
               ],
    knm_number:update(Number, Routines, Options);
number_action(Context, Number, ?HTTP_DELETE) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
               ,{'dry_run', not cb_context:accepting_charges(Context)}
              ],
    knm_number:delete(Number, Options).

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
