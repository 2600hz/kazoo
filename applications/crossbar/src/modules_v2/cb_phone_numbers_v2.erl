%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
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
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(CB_LIST, <<"phone_numbers/crossbar_listing">>).
-define(PORT_NUM_LISTING, <<"port_requests/phone_numbers_listing">>).
-define(PORT_NUMBER_KEY_INDEX, 2).

-define(ACTIVATE, <<"activate">>).
-define(RESERVE, <<"reserve">>).
-define(PORT, <<"port">>).

-define(CLASSIFIERS, <<"classifiers">>).
-define(IDENTIFY, <<"identify">>).
-define(COLLECTION, <<"collection">>).
-define(COLLECTION_NUMBERS, <<"numbers">>).
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

-define(SCHEMA_PHONE_NUMBERS, <<"phone_numbers">>).
-define(SCHEMA_FIND_NUMBERS, <<"find_numbers">>).

-define(KEY_PHONEBOOK_FREE_URL, <<"phonebook_url">>).
-define(PREFIX, <<"prefix">>).
-define(QUANTITY, <<"quantity">>).
-define(OFFSET, <<"offset">>).
-define(LOCALITY, <<"locality">>).
-define(CHECK, <<"check">>).
-define(COUNTRY, <<"country">>).
-define(KNM_CONFIG_CAT, <<"number_manager">>).

-define(MAX_TOKENS, kapps_config:get_integer(?PHONE_NUMBERS_CONFIG_CAT, <<"activations_per_day">>, 100)).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"v2_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v2_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v2_resource.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.phone_numbers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.phone_numbers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.phone_numbers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.put.phone_numbers">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.post.phone_numbers">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.delete.phone_numbers">>, ?MODULE, 'delete'),
    ok.

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
maybe_authenticate(?HTTP_GET, [{<<"phone_numbers">>, []}]) ->
    'true';
maybe_authenticate(?HTTP_GET, [{<<"phone_numbers">>, [?PREFIX]}]) ->
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

maybe_authorize(?HTTP_GET, [{<<"phone_numbers">>, []}]) ->
    'true';
maybe_authorize(?HTTP_GET, [{<<"phone_numbers">>, [?PREFIX]}]) ->
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
allowed_methods(_PhoneNumber, ?PORT) ->
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
resource_exists(_PhoneNumber, ?PORT) -> 'true';
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
maybe_allow_updates(Context, [{<<"phone_numbers">>, _}|_], ?HTTP_GET) ->
    Context;
maybe_allow_updates(Context, [{<<"phone_numbers">>, _}|_], _Verb) ->
    try kz_services:allow_updates(cb_context:account_id(Context)) of
        'true' -> Context
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_util:to_binary(Error), 500, Reason, Context)
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
    maybe_find_numbers(Context);
validate_phone_numbers(Context, ?HTTP_GET, _AccountId) ->
    case kz_json:get_ne_value(?PREFIX, cb_context:query_string(Context)) of
        'undefined' -> summary(Context);
        _Prefix -> maybe_find_numbers(Context)
    end.

validate(Context, ?FIX) ->
    cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                            ,kz_json:new()
                            );
validate(Context, ?PREFIX) ->
    find_prefix(Context);
validate(Context, ?COLLECTION) ->
    validate_collection_request(Context);
validate(Context, ?CLASSIFIERS) ->
    cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success')
                            ,knm_converters:available_classifiers()
                            );
validate(Context, ?LOCALITY) ->
    Numbers = cb_context:req_value(Context, ?COLLECTION_NUMBERS),
    validate_collection_request(Context, Numbers);
validate(Context, ?CHECK) ->
    validate_collection_request(Context);
validate(Context, Number) ->
    validate_number(Context, Number, cb_context:req_verb(Context)).

-spec validate_number(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_number(Context, Number, ?HTTP_GET) ->
    summary(Context, Number);
validate_number(Context, _Number, ?HTTP_POST) ->
    validate_request(Context);
validate_number(Context, _Number, ?HTTP_PUT) ->
    validate_request(Context);
validate_number(Context, _Number, ?HTTP_DELETE) ->
    validate_delete(Context).

validate(Context, ?COLLECTION, ?ACTIVATE) ->
    validate_collection_request(Context);
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
                      ,[{fun cb_context:set_resp_data/2, kz_json:from_list(RespData)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

-spec base_classified_number(cb_context:context(), ne_binary()) -> kz_proplist().
base_classified_number(_Context, Number) ->
    [{<<"number">>, Number}
    ,{<<"e164">>, knm_converters:normalize(Number)}
    ].

-spec classified_number(cb_context:context(), path_token(), api_binary()) -> cb_context:context().
classified_number(Context, Number, Classifier) ->
    ClassifierJObj = kz_json:get_value(Classifier, knm_converters:available_classifiers()),
    BaseData = base_classified_number(Context, Number),
    RespData = kz_json:set_values([{<<"name">>, Classifier}
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
    AccountDb = cb_context:account_db(Context),
    'ok' = kazoo_number_manager_maintenance:migrate(AccountDb),
    summary(Context);
post(Context, ?CHECK) ->
    Numbers = cb_context:req_value(Context, ?COLLECTION_NUMBERS),
    cb_context:set_resp_data(Context, knm_carriers:check(Numbers));
post(Context, ?COLLECTION) ->
    Results = collection_process(Context, ?HTTP_POST),
    CB = fun() -> post(cb_context:set_accepting_charges(Context), ?COLLECTION) end,
    set_response(Results, Context, CB);
post(Context, ?LOCALITY) ->
    fetch_locality(Context);
post(Context, Number) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ],
    JObj = cb_context:doc(Context),
    Result = knm_number:update(Number, [{fun knm_phone_number:reset_doc/2, JObj}], Options),
    set_response(Result, Context).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ?COLLECTION) ->
    Results = collection_process(Context, ?HTTP_PUT),
    CB = fun() -> ?MODULE:put(cb_context:set_accepting_charges(Context), ?COLLECTION) end,
    set_response(Results, Context, CB);
put(Context, Number) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ,{'dry_run', not cb_context:accepting_charges(Context)}
              ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:create(Number, Options),
    CB = fun() -> ?MODULE:put(cb_context:set_accepting_charges(Context), Number) end,
    set_response(Result, Context, CB).

put(Context, ?COLLECTION, ?ACTIVATE) ->
    Results = collection_process(Context, ?ACTIVATE),
    CB = fun() -> put(cb_context:set_accepting_charges(Context), ?COLLECTION, ?ACTIVATE) end,
    set_response(Results, Context, CB);
put(Context, Number, ?ACTIVATE) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ,{'dry_run', not cb_context:accepting_charges(Context)}
              ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:move(Number, cb_context:account_id(Context), Options),
    CB = fun() -> put(cb_context:set_accepting_charges(Context), Number, ?ACTIVATE) end,
    set_response(Result, Context, CB);
put(Context, Number, ?RESERVE) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ,{'dry_run', not cb_context:accepting_charges(Context)}
              ,{'public_fields', cb_context:doc(Context)}
              ],
    Result = knm_number:reserve(Number, Options),
    CB = fun() -> put(cb_context:set_accepting_charges(Context), Number, ?RESERVE) end,
    set_response(Result, Context, CB);
put(Context, Number, ?PORT) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ,{'dry_run', not cb_context:accepting_charges(Context)}
              ,{'public_fields', cb_context:doc(Context)}
              ,{'state', ?NUMBER_STATE_PORT_IN}
              ],
    Result = knm_number:create(Number, Options),
    CB = fun() -> put(cb_context:set_accepting_charges(Context), Number, ?PORT) end,
    set_response(Result, Context, CB).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ?COLLECTION) ->
    Results = collection_process(Context, ?HTTP_DELETE),
    set_response(Results, Context);
delete(Context, Number) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ],
    Releaser = pick_release_or_delete(Context, Options),
    case knm_number:Releaser(Number, Options) of
        {'error', Data}=Error ->
            case kz_json:is_json_object(Data)
                andalso knm_errors:error(Data) == <<"invalid_state_transition">>
                andalso knm_errors:cause(Data) == <<"from available to released">>
            of
                'true' -> reply_number_not_found(Context);
                'false' -> set_response(Error, Context)
            end;
        Else ->
            set_response(Else, Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Lists number on GET /v2/accounts/{{ACCOUNT_ID}}/phone_numbers/{{DID}}
%%--------------------------------------------------------------------
-spec summary(cb_context:context(), ne_binary()) -> cb_context:context().
summary(Context, Number) ->
    IncludePorts =
        kz_util:is_true(cb_context:req_value(Context, <<"include_ports">>, 'false')),
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ],
    case knm_number:get(Number, Options) of
        {'ok', KNMNumber} ->
            crossbar_util:response(knm_number:to_public_json(KNMNumber), Context);
        {'error', _JObj} ->
            maybe_find_port_number(Context, Number, IncludePorts)
    end.


-spec maybe_find_port_number(cb_context:context(), ne_binary(), boolean()) ->
                                    cb_context:context().
maybe_find_port_number(Context, _Number, 'false') ->
    reply_number_not_found(Context);
maybe_find_port_number(Context, Number, 'true') ->
    NormalizeNum = knm_converters:normalize(Number),
    ViewOption = [{'key', [cb_context:account_id(Context), NormalizeNum]}
                 ],
    case kz_datamgr:get_single_result(?KZ_PORT_REQUESTS_DB, ?PORT_NUM_LISTING, ViewOption) of
        {'error', _} -> reply_number_not_found(Context);
        {'ok', Result} ->
            Port = kz_json:get_value(<<"value">>, Result),
            {'ok', PhoneNumber} = normalize_port_number(Port, NormalizeNum, Context),
            Values = [{[<<"_read_only">>, <<"port_id">>], kz_json:get_value(<<"port_id">>, Port)}
                     ,{[<<"_read_only">>, <<"port_state">>], kz_json:get_value(<<"port_state">>, Port)}
                     ],
            JObj = kz_json:set_values(Values, knm_phone_number:to_public_json(PhoneNumber)),
            port_number_summary(JObj, Context, knm_phone_number:is_authorized(PhoneNumber))
    end.

%% @private
-spec port_number_summary(kz_json:object(), cb_context:context(), boolean()) -> cb_context:context().
port_number_summary(PhoneNumber, Context, 'true') ->
    crossbar_util:response(PhoneNumber, Context);
port_number_summary(_PhoneNumber, Context, 'false') ->
    reply_number_not_found(Context).

-spec normalize_port_number(kz_json:object(), ne_binary(), cb_context:context()) ->
                                   knm_phone_number_return().
normalize_port_number(JObj, Number, Context) ->
    Setters = [{fun knm_phone_number:set_number/2, Number}
              ,{fun knm_phone_number:set_assigned_to/2, kz_json:get_value(<<"assigned_to">>, JObj)}
              ,{fun knm_phone_number:set_used_by/2, kz_json:get_value(<<"used_by">>, JObj)}
              ,{fun knm_phone_number:set_state/2, kz_json:get_value(<<"state">>, JObj)}
              ,{fun knm_phone_number:set_auth_by/2, cb_context:auth_account_id(Context)}
              ,{fun knm_phone_number:set_modified/2, kz_json:get_value(<<"updated">>, JObj)}
              ,{fun knm_phone_number:set_created/2, kz_json:get_value(<<"created">>, JObj)}
               | [{fun knm_phone_number:set_feature/3, Feature, kz_json:new()}
                  || Feature <- kz_json:get_value(<<"features">>, JObj)
                 ]
              ],
    knm_phone_number:setters(knm_phone_number:new(), Setters).

%%--------------------------------------------------------------------
%% @private
%% @doc Lists numbers on GET /v2/accounts/{ACCOUNT_ID}/phone_numbers
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
    Context1 = crossbar_doc:load_view(?CB_LIST
                                     ,[]
                                     ,rename_qs_filters(Context)
                                     ,fun normalize_view_results/2
                                     ),
    ListOfNumProps = lists:map(fun maybe_fix_available/1, cb_context:resp_data(Context1)),
    PortNumberJObj = maybe_add_port_request_numbers(Context),
    NumbersJObj = lists:foldl(fun kz_json:merge_jobjs/2, PortNumberJObj, ListOfNumProps),
    Service = kz_services:fetch(cb_context:account_id(Context)),
    Quantity = kz_services:cascade_category_quantity(<<"phone_numbers">>, [], Service),
    NewRespData = kz_json:from_list([{<<"numbers">>, NumbersJObj}
                                    ,{<<"casquade_quantity">>, Quantity}
                                    ]),
    cb_context:set_resp_data(Context1, NewRespData).

maybe_fix_available(NumJObj) ->
    [{Num, JObj}] = kz_json:to_proplist(NumJObj),
    MaybeAccount = kz_json:get_ne_binary_value(<<"assigned_to">>, JObj),
    FAs = knm_providers:available_features(MaybeAccount),
    NewJObj = kz_json:set_value(<<"features_available">>, FAs, JObj),
    kz_json:from_list([{Num, NewJObj}]).

%% @private
-spec maybe_add_port_request_numbers(cb_context:context()) -> kz_json:object().
-spec maybe_add_port_request_numbers(cb_context:context(), boolean()) -> kz_json:object().
maybe_add_port_request_numbers(Context) ->
    IncludePorts = cb_context:req_value(Context, <<"include_ports">>, 'false'),
    maybe_add_port_request_numbers(Context, kz_util:is_true(IncludePorts)).

maybe_add_port_request_numbers(_Context, 'false') -> kz_json:new();
maybe_add_port_request_numbers(Context, 'true') ->
    AccountId = cb_context:account_id(Context),
    HasQs = crossbar_doc:has_qs_filter(Context),
    ViewOptions = [{'startkey', [cb_context:account_id(Context)]}
                  ,{'endkey', [cb_context:account_id(Context), kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, ?PORT_NUM_LISTING, ViewOptions) of
        {'error', _} -> kz_json:new();
        {'ok', Ports} ->
            PortNumberList = [normalize_port_view_result(P)
                              || P <- Ports,
                                 maybe_filter_port_number(P, Context, HasQs)
                             ],
            lists:foldl(fun kz_json:merge_jobjs/2, kz_json:new(), PortNumberList)
    end.

%% @private
-spec maybe_filter_port_number(kz_json:object(), cb_context:context(), boolean()) ->
                                      boolean().
maybe_filter_port_number(_Port, _Context, 'false') -> 'false';
maybe_filter_port_number(Port, Context, 'true') ->
    crossbar_doc:filtered_doc_by_qs(kz_json:get_value(<<"value">>, Port), Context).

%% @private
-spec rename_qs_filters(cb_context:context()) -> cb_context:context().
rename_qs_filters(Context) ->
    Renamer = fun (<<"filter_state">>, Value)       -> {<<"filter_pvt_state">>, Value};
                  (<<"filter_assigned_to">>, Value) -> {<<"filter_pvt_assigned_to">>, Value};
                  (<<"filter_locality">>, Value)    -> {<<"filter_pvt_locality">>, Value};
                  (K, V) -> {K, V}
              end,
    NewQS = kz_json:map(Renamer, cb_context:query_string(Context)),
    cb_context:set_query_string(Context, NewQS).

%% @private
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    Number = kz_json:get_value(<<"key">>, JObj),
    Properties = kz_json:get_value(<<"value">>, JObj),
    [kz_json:from_list([{Number, Properties}])
     | Acc
    ].

%% @private
-spec normalize_port_view_result(kz_json:object()) -> kz_json:object().
-spec normalize_port_view_result(kz_json:key(), kz_json:json_term()) -> kz_json:object().
normalize_port_view_result(JObj) ->
    Number = kz_json:get_value([<<"key">>, ?PORT_NUMBER_KEY_INDEX], JObj),
    Properties = kz_json:get_value(<<"value">>, JObj),

    normalize_port_view_result(Number, Properties).

normalize_port_view_result(Number, Properties) ->
    kz_json:from_list([{Number, Properties}]).

%%--------------------------------------------------------------------
%% @private
%% @doc Search for numbers, requesting carrier module
%%--------------------------------------------------------------------
-spec maybe_find_numbers(cb_context:context()) -> cb_context:context().
maybe_find_numbers(Context) ->
    case kz_json:get_value(<<"reseller_id">>, cb_context:query_string(Context)) of
        'undefined' ->
            case cb_context:account_id(Context) of
                'undefined' ->
                    AuthAccountId = cb_context:auth_account_id(Context),
                    find_numbers(Context, AuthAccountId, kz_services:find_reseller_id(AuthAccountId));
                AccountId ->
                    find_numbers(Context, AccountId, cb_context:reseller_id(Context))
            end;
        ReqResellerId ->
            maybe_reseller_id_lookup(Context, ReqResellerId)
    end.

-spec find_numbers(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
find_numbers(Context, AccountId, ResellerId) ->
    QS = cb_context:query_string(Context),
    Country = kz_json:get_ne_value(?COUNTRY, QS, ?KNM_DEFAULT_COUNTRY),
    Prefix = kz_util:remove_white_spaces(kz_json:get_ne_value(?PREFIX, QS)),
    Offset = kz_json:get_integer_value(?OFFSET, QS, 0),
    Token = cb_context:auth_token(Context),
    HashKey = <<AccountId/binary, "-", Token/binary>>,
    Hash = kz_base64url:encode(crypto:hash(sha, HashKey)),
    QueryId = list_to_binary([Country, "-", Prefix, "-", Hash]),
    Dialcode = knm_util:prefix_for_country(Country),
    NormalizedPrefix = <<Dialcode/binary, Prefix/binary>>,
    Options = props:filter_undefined(
                [{'quantity', max(1, kz_json:get_integer_value(?QUANTITY, QS, 1))}
                ,{'prefix', Prefix}
                ,{'normalized_prefix', NormalizedPrefix}
                ,{'country', Country}
                ,{'dialcode', Dialcode}
                ,{'offset', Offset}
                ,{'account_id', AccountId}
                ,{'reseller_id', ResellerId}
                ,{'query_id', QueryId}
                ]),
    OnSuccess =
        fun(C) ->
                Found = knm_search:find(Options),
                cb_context:setters(C
                                  ,[{fun cb_context:set_resp_data/2, Found}
                                   ,{fun cb_context:set_resp_status/2, 'success'}
                                   ])
        end,
    Context1 = cb_context:set_req_data(Context, kz_json:from_list(Options)),
    cb_context:validate_request_data(?SCHEMA_FIND_NUMBERS, Context1, OnSuccess).

-spec maybe_reseller_id_lookup(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_reseller_id_lookup(Context, ReqResellerId) ->
    try
        case (kz_services:is_reseller(ReqResellerId)
              orelse cb_context:is_superduper_admin(ReqResellerId)
             )
            andalso kapps_account_config:get_global(ReqResellerId
                                                   ,?KNM_CONFIG_CAT
                                                   ,<<"unauthorized_numbers_lookup">>
                                                   ,'false'
                                                   )
        of
            'true' -> find_numbers(Context, ReqResellerId, ReqResellerId);
            'false' -> crossbar_util:response('error', <<"number search restricted">>, 404, Context)
        end
    catch
        _:_ ->
            crossbar_util:response('error', <<"non-existent reseller_id">>, 404, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_prefix(cb_context:context()) -> cb_context:context().
find_prefix(Context) ->
    case kz_json:get_ne_value(<<"city">>, cb_context:query_string(Context)) of
        'undefined' -> cb_context:add_system_error('bad_identifier', Context);
        City ->
            case get_prefix(City) of
                {'ok', JObj} ->
                    cb_context:set_resp_data(
                      cb_context:set_resp_status(Context, 'success')
                                            ,JObj
                     );
                {'error', Error} ->
                    lager:error("error while prefix for city ~s: ~p", [City, Error]),
                    cb_context:set_resp_data(
                      cb_context:set_resp_status(Context, 'error')
                                            ,Error
                     )
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Validates a collection-type numbers field.
%%--------------------------------------------------------------------
-spec validate_collection_request(cb_context:context()) -> cb_context:context().
-spec validate_collection_request(cb_context:context(), any()) -> cb_context:context().
validate_collection_request(Context) ->
    Numbers = kz_json:get_value(?COLLECTION_NUMBERS, cb_context:req_data(Context)),
    validate_collection_request(Context, Numbers).
validate_collection_request(Context, 'undefined') ->
    Msg = kz_json:from_list([{<<"message">>, <<"list of numbers missing">>}
                            ]),
    cb_context:add_validation_error(?COLLECTION_NUMBERS, <<"required">>, Msg, Context);
validate_collection_request(Context, []) ->
    Msg = kz_json:from_list([{<<"message">>, <<"minimum 1 number required">>}
                            ]),
    cb_context:add_validation_error(?COLLECTION_NUMBERS, <<"minimum">>, Msg, Context);
validate_collection_request(Context, Numbers)
  when is_list(Numbers) ->
    UniqNomalised = lists:usort([knm_converters:normalize(N) || N <- Numbers]),
    case length(Numbers) =:= length(UniqNomalised) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            Msg = kz_json:from_list([{<<"message">>, <<"some numbers appear twice">>}
                                    ]),
            cb_context:add_validation_error(?COLLECTION_NUMBERS, <<"uniqueItems">>, Msg, Context)
    end;
validate_collection_request(Context, _E) ->
    Msg = kz_json:from_list([{<<"message">>, <<"numbers must be a list">>}
                            ]),
    cb_context:add_validation_error(?COLLECTION_NUMBERS, <<"type">>, Msg, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_prefix(ne_binary()) -> {'ok', kz_json:object()} |
                                 {'error', any()}.
get_prefix(City) ->
    case kapps_config:get_string(?PHONE_NUMBERS_CONFIG_CAT, ?KEY_PHONEBOOK_FREE_URL) of
        'undefined' ->
            {'error', <<"Unable to acquire numbers missing carrier url">>};
        Url ->
            Country = kapps_config:get_string(?PHONE_NUMBERS_CONFIG_CAT
                                             ,<<"default_country">>
                                             ,?KNM_DEFAULT_COUNTRY
                                             ),
            knm_locality:prefix(Url, Country, City)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Tries to fill [Number,locality] field with info from phonebook.
%% @end
%%--------------------------------------------------------------------
-spec fetch_locality(cb_context:context()) -> cb_context:context().
fetch_locality(Context) ->
    Numbers = cb_context:req_value(Context, ?COLLECTION_NUMBERS),
    case knm_locality:fetch(Numbers) of
        {'ok', Localities} ->
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
                                    ,Localities
             );
        {'error', 'lookup_failed'} ->
            Msg = <<"number locality lookup failed">>,
            crossbar_util:response('error', Msg, 500, Context);
        {'error', 'missing_url'} ->
            Msg = <<"could not get locality url">>,
            crossbar_util:response('error', Msg, 500, Context)
    end.

%% @private
-spec maybe_update_locality(cb_context:context()) -> cb_context:context().
maybe_update_locality(Context) ->
    Numbers = kz_json:get_value(<<"numbers">>, cb_context:resp_data(Context)),
    ToUpdate =
        [Num || {Num,NumProps} <- kz_json:to_proplist(Numbers),
                kz_json:get_value(<<"locality">>, NumProps) =:= 'undefined'
                    andalso knm_converters:is_reconcilable(Num)
        ],
    update_locality(Context, ToUpdate).

%% @private
-spec update_locality(cb_context:context(), ne_binaries()) ->
                             cb_context:context().
update_locality(Context, []) -> Context;
update_locality(Context, Numbers) ->
    case knm_locality:fetch(Numbers) of
        {'ok', Localities} ->
            _ = kz_util:spawn(fun update_phone_numbers_locality/2, [Context, Localities]),
            update_context_locality(Context, Localities);
        {'error', _} -> Context
    end.

%% @private
-spec update_context_locality(cb_context:context(), kz_json:object()) ->
                                     cb_context:context().
update_context_locality(Context, Localities) ->
    JObj = kz_json:foldl(fun update_context_locality_fold/3, cb_context:resp_data(Context), Localities),
    cb_context:set_resp_data(Context, JObj).

-spec update_context_locality_fold(ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
update_context_locality_fold(Key, Value, JObj) ->
    case kz_json:get_value(<<"status">>, Value) of
        <<"success">> ->
            Locality = kz_json:delete_key(<<"status">>, Value),
            Path = [<<"numbers">>, Key, <<"locality">>],
            kz_json:set_value(Path, Locality, JObj);
        _Else -> JObj
    end.

%% @private
-spec update_phone_numbers_locality(cb_context:context(), kz_json:object()) ->
                                           {'ok', kz_json:object()} |
                                           {'error', any()}.
update_phone_numbers_locality(Context, Localities) ->
    AccountDb = cb_context:account_db(Context),
    DocId = kz_doc:id(cb_context:doc(Context)),
    case kz_datamgr:open_doc(AccountDb, DocId) of
        {'ok', JObj} ->
            J = kz_json:foldl(fun update_phone_numbers_locality_fold/3, JObj, Localities),
            kz_datamgr:save_doc(AccountDb, J);
        {'error', _E}=E ->
            lager:error("failed to update locality for ~s in ~s: ~p", [DocId, AccountDb, _E]),
            E
    end.

%% @private
-spec update_phone_numbers_locality_fold(ne_binary(), kz_json:object(), kz_json:object()) ->
                                                kz_json:object().
update_phone_numbers_locality_fold(Key, Value, JObj) ->
    case kz_json:get_value(<<"status">>, Value) of
        <<"success">> ->
            case kz_json:get_value(Key, JObj) of
                'undefined' -> JObj;
                _Else ->
                    Locality = kz_json:delete_key(<<"status">>, Value),
                    kz_json:set_value([Key, <<"locality">>], Locality, JObj)
            end;
        _Else -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec identify(cb_context:context(), ne_binary()) -> cb_context:context().
identify(Context, Number) ->
    case knm_number:lookup_account(Number) of
        {'ok', AccountId, NumberOptions} ->
            JObj = kz_json:from_list(
                     [{<<"account_id">>, AccountId}
                     ,{<<"number">>, knm_number_options:number(NumberOptions)}
                     ]),
            crossbar_util:response(JObj, Context);
        {error, {R=not_in_service, _AccountId}} ->
            lager:debug("~s's account ~p ~s", [Number, _AccountId, R]),
            set_response({error, R}, Context);
        {error, {R=account_disabled, _AccountId}} ->
            lager:debug("~s's account ~p ~s", [Number, _AccountId, R]),
            set_response({error, R}, Context);
        {'error', _R}=Error ->
            set_response(Error, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_request(cb_context:context()) -> cb_context:context().
validate_request(Context) ->
    cb_context:validate_request_data(?SCHEMA_PHONE_NUMBERS, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc Always validate DELETEs
%%--------------------------------------------------------------------
-spec validate_delete(cb_context:context()) -> cb_context:context().
validate_delete(Context) ->
    cb_context:set_doc(cb_context:set_resp_status(Context, 'success')
                      ,'undefined'
                      ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
set_response(Result, Context) ->
    set_response(Result, Context, fun() -> Context end).

-type result() :: {'ok', kz_json:object()} |
                  {ne_binary(), [kz_services:services()], kz_json:object()} |
                  knm_number_return() |
                  {binary(), binary()}.
-type cb() :: fun(() -> cb_context:context()).

-spec set_response(result(), cb_context:context(), cb()) -> cb_context:context().
set_response({'ok', Thing}, Context, _) ->
    case knm_number:is_number(Thing) of
        'true' -> crossbar_util:response(knm_number:to_public_json(Thing), Context);
        'false' -> crossbar_util:response(Thing, Context)
    end;

set_response({?COLLECTION, ServicesList, ResultJObj}, Context, CB) ->
    case kz_json:get_value(<<"error">>, ResultJObj) of
        'undefined' ->
            case ServicesList of
                [] -> crossbar_util:response(ResultJObj, Context);
                _ ->
                    RespJObj = fold_dry_runs(ServicesList),
                    case kz_json:is_empty(RespJObj) of
                        'true' -> CB();
                        'false' -> crossbar_util:response_402(RespJObj, Context)
                    end
            end;
        _Errors ->
            crossbar_util:response_400(<<"client error">>, ResultJObj, Context)
    end;

set_response({'dry_run', Services, _ActivationCharges}, Context, CB) ->
    RespJObj = kz_services:dry_run(Services),
    case kz_json:is_empty(RespJObj) of
        'true' -> CB();
        'false' -> crossbar_util:response_402(RespJObj, Context)
    end;

set_response({'error', 'not_found'}, Context, _) ->
    reply_number_not_found(Context);

set_response({'error', Data}, Context, _)
  when is_atom(Data) ->
    lager:debug("error: ~p", [Data]),
    crossbar_util:response_400(<<"client error">>, Data, Context);

set_response({'error', Data}, Context, _) ->
    case kz_json:is_json_object(Data) of
        'true' ->
            Code = knm_errors:code(Data),
            Msg = knm_errors:error(Data),
            lager:debug("error ~p: ~p", [Code, Msg]),
            cb_context:add_system_error(Code, Msg, Data, Context);
        'false' ->
            lager:debug("error: ~p", [Data]),
            crossbar_util:response_400(<<"client error">>, kz_json:new(), Context)
    end;

set_response({'invalid', Reason}, Context, _) ->
    lager:debug("invalid: ~p", [Reason]),
    cb_context:add_validation_error(<<"address">>, <<"invalid">>, Reason, Context);

set_response({Error, Reason}, Context, _) ->
    lager:debug("error ~p: ~p", [Error, Reason]),
    cb_context:add_system_error(Error, Reason, Context);

set_response(_Else, Context, _) ->
    lager:debug("unexpected response: ~p", [_Else]),
    cb_context:add_system_error('unspecified_fault', Context).

-spec reply_number_not_found(cb_context:context()) -> cb_context:context().
reply_number_not_found(Context) ->
    Msg = kz_json:from_list([{<<"message">>, <<"bad identifier">>}
                            ,{<<"not_found">>, <<"The number could not be found">>}
                            ]),
    cb_context:add_system_error('bad_identifier', Msg, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type process_result() :: {kz_services:services(), kz_json:object()}.
-spec collection_process(cb_context:context(), ne_binary()) -> {ne_binary(), kz_services:services(), kz_json:object()}.
collection_process(Context, Action) ->
    ReqData = cb_context:req_data(Context),
    Numbers = kz_json:get_value(<<"numbers">>, ReqData),
    Context1 = cb_context:set_req_data(Context, kz_json:delete_key(<<"numbers">>, ReqData)),
    {ServicesList, ResultJObj} =
        collection_process(Context1, Action, Numbers),
    {?COLLECTION, ServicesList, ResultJObj}.

-spec collection_process(cb_context:context(), ne_binary(), ne_binaries()) -> process_result().
collection_process(Context, Action, Numbers) ->
    lists:foldl(fun ({Number, {'ok', KNMNumber}}, {ServicesAcc, JObjAcc}) ->
                        JObj = knm_number:to_public_json(KNMNumber),
                        {ServicesAcc
                        ,kz_json:set_value([<<"success">>, Number], JObj, JObjAcc)
                        };
                    ({Number, {'dry_run', Services, ActivationCharges}}, {ServicesAcc, JObjAcc}) ->
                        {[Services | ServicesAcc]
                        ,kz_json:set_value([<<"charges">>, Number], ActivationCharges, JObjAcc)
                        };
                    ({Number, {'error', KNMError}}, {ServicesAcc, JObjAcc}) ->
                        {ServicesAcc
                        ,kz_json:set_value([<<"error">>, Number], KNMError, JObjAcc)
                        }
                end
               ,{[], kz_json:new()}
               ,numbers_action(Context, Action, Numbers)
               ).

%% @private
-spec numbers_action(cb_context:context(), ne_binary(), ne_binaries()) ->
                            knm_numbers:numbers_return().
numbers_action(Context, ?ACTIVATE, Numbers) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ,{'dry_run', not cb_context:accepting_charges(Context)}
              ,{'public_fields', cb_context:req_data(Context)}
              ],
    knm_numbers:move(Numbers, cb_context:account_id(Context), Options);
numbers_action(Context, ?HTTP_PUT, Numbers) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ,{'dry_run', not cb_context:accepting_charges(Context)}
              ,{'public_fields', cb_context:req_data(Context)}
              ],
    knm_numbers:create(Numbers, Options);
numbers_action(Context, ?HTTP_POST, Numbers) ->
    Options = [{'assign_to', cb_context:account_id(Context)}
              ,{'auth_by', cb_context:auth_account_id(Context)}
              ],
    JObj = cb_context:req_data(Context),
    knm_numbers:update(Numbers, [{fun knm_phone_number:reset_doc/2, JObj}], Options);
numbers_action(Context, ?HTTP_DELETE, Numbers) ->
    Options = [{'auth_by', cb_context:auth_account_id(Context)}
              ],
    Releaser = pick_release_or_delete(Context, Options),
    knm_numbers:Releaser(Numbers, Options).

-spec fold_dry_runs([kz_services:services(), ...]) -> kz_json:object().
fold_dry_runs(ServicesList) ->
    F = fun(Services, _Acc) -> kz_services:dry_run(Services) end,
    lists:foldl(F, [], ServicesList).

%% @private
-spec pick_release_or_delete(cb_context:context(), knm_number_options:options()) -> 'release' | 'delete'.
pick_release_or_delete(Context, Options) ->
    AuthBy = knm_number_options:auth_by(Options),
    Pick = case kz_util:is_true(cb_context:req_param(Context, <<"hard">>, 'false'))
               andalso kz_util:is_system_admin(AuthBy)
           of
               'false' -> 'release';
               'true' -> 'delete'
           end,
    lager:debug("picked ~s", [Pick]),
    Pick.

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
