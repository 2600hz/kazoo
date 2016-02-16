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
         ,authenticate/1
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,post/2
         ,put/2 ,put/3
         ,delete/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

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

-define(FREE_URL, <<"phonebook_url">>).
-define(PAID_URL, <<"phonebook_url_premium">>).

-define(CLASSIFIERS, <<"classifiers">>).
-define(LOCALITY, <<"locality">>).
-define(FIX, <<"fix">>).
-define(ACTIVATE, <<"activate">>).

-define(PHONE_NUMBERS_CONFIG_CAT, <<?CONFIG_CAT/binary, ".phone_numbers">>).

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
    _ = crossbar_bindings:bind(<<"v2_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v2_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.phone_numbers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.phone_numbers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.phone_numbers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.post.phone_numbers">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.put.phone_numbers">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.delete.phone_numbers">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    maybe_authenticate(
        cb_context:req_verb(Context)
        ,cb_context:req_nouns(Context)
    ).

-spec maybe_authenticate(http_method(), req_nouns()) -> boolean().
maybe_authenticate(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC, []}]) ->
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
    maybe_authorize(
      cb_context:req_verb(Context)
      ,cb_context:req_nouns(Context)
     ).

-spec maybe_authorize(req_verb(), req_nouns()) -> boolean().
maybe_authorize(?HTTP_GET, [{?KNM_PHONE_NUMBERS_DOC,[]}]) ->
    'true';
maybe_authorize(_Verb, _Nouns) ->
    'false'.

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
allowed_methods() ->
    [?HTTP_GET].

allowed_methods(?CLASSIFIERS) ->
    [?HTTP_GET];
allowed_methods(?LOCALITY) ->
    [?HTTP_POST];
allowed_methods(?FIX) ->
    [?HTTP_POST];
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?CLASSIFIERS, _Number) ->
    [?HTTP_GET];
allowed_methods(_Number, ?ACTIVATE) ->
    [?HTTP_PUT].


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /phone_numbers => []
%%    /phone_numbers/foo => [<<"foo">>]
%%    /phone_numbers/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists() -> 'true'.

resource_exists(_) -> 'true'.

resource_exists(?CLASSIFIERS, _Number) -> 'true';
resource_exists(_Number, ?ACTIVATE) -> 'true';
resource_exists(_, _) -> 'false'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /phone_numbers mights load a list of skel objects
%% /phone_numbers/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
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
validate(Context, Id) ->
    validate_phone_number(Context, Id, cb_context:req_verb(Context)).

validate(Context, ?CLASSIFIERS, Number) ->
    maybe_classify(Context, Number);
validate(Context, _Number, ?ACTIVATE) ->
    cb_context:validate_request_data(?KNM_NUMBER, Context).


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
-spec validate_phone_number(cb_context:context(), path_token(), http_method()) ->
                                   cb_context:context().
validate_phone_number(Context, Number, ?HTTP_GET) ->
    read(Context, Number);
validate_phone_number(Context, _Number, ?HTTP_PUT) ->
    cb_context:validate_request_data(?KNM_NUMBER, Context);
validate_phone_number(Context, _Number, ?HTTP_POST) ->
    cb_context:validate_request_data(?KNM_NUMBER, Context);
validate_phone_number(Context, _Number, ?HTTP_DELETE) ->
    cb_context:set_doc(
        cb_context:set_resp_status(Context, 'success')
        ,'undefined'
    ).

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
            _ = wh_util:spawn(fun() ->
                                      update_phone_numbers_locality(Context, Localities)
                              end),
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
    wh_json:set_value([<<"numbers">>
                       ,Key
                       ,<<"locality">>
                      ]
                      ,Locality
                      ,JObj
                     );
update_context_locality_fold(_Key, _Value, JObj, _Status) ->
    JObj.

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
            query_locality(Numbers, Url)
    end.

-spec query_locality(ne_binaries(), ne_binary()) ->
                            {'error', ne_binary()} |
                            {'ok', wh_json:object()}.
query_locality(Numbers, Url) ->
    ReqBody = wh_json:set_value(<<"data">>, Numbers, wh_json:new()),
    Uri = <<Url/binary, "/locality/metadata">>,
    case ibrowse:send_req(wh_util:to_list(Uri)
                          ,[]
                          ,'post'
                          ,wh_json:encode(ReqBody)
                         )
    of
        {'error', Reason} ->
            lager:error("number locality lookup failed: ~p", [Reason]),
            {'error', <<"number locality lookup failed">>};
        {'ok', "200", _Headers, Body} ->
            handle_locality_resp(wh_json:decode(Body));
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
    case knm_number:get(Num, [{<<"auth_by">>, AuthAccountId}]) of
        {'error', Reason} -> error_return(Context, Reason);
        {'ok', Number} -> success_return(Context, Number)
    end.

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
