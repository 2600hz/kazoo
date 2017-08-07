%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cb_ledgers).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,authorize/1, authorize/2, authorize/3
        ,validate/1, validate/2, validate/3
        ,put/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").

-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).

-define(NOTIFY_MSG, "failed to impact reseller ~s ledger : ~p").

-define(LEDGER_VIEW, <<"ledgers/listing_by_service_legacy">>).
%%-define(LEDGER_VIEW, "ledgers/listing_by_service").
%% TODO: make this change for 4.1

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ledgers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ledgers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize.ledgers">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.ledgers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.ledgers">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.ledgers">>, ?MODULE, 'put').

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

allowed_methods(?CREDIT) ->
    [?HTTP_PUT];
allowed_methods(?DEBIT) ->
    [?HTTP_PUT];
allowed_methods(_LedgerId) ->
    [?HTTP_GET].

allowed_methods(_LedgerId, _LedgerEntryId) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /ledgers => []
%%    /ledgers/foo => [<<"foo">>]
%%    /ledgers/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) -> cb_simple_authz:authorize(Context).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, Path) ->
    authorize_request(Context, Path, cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, _Path, _Id) ->
    cb_simple_authz:authorize(Context).

-spec authorize_request(cb_context:context(), path_token(), http_method()) -> boolean().
authorize_request(Context, ?DEBIT, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, ?CREDIT, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, _, ?HTTP_PUT) ->
    {'halt', cb_context:add_system_error('forbidden', Context)};
authorize_request(Context, _, ?HTTP_GET) ->
    cb_simple_authz:authorize(Context).

-spec authorize_create(cb_context:context()) -> boolean() |
                                                {'halt', cb_context:context()}.
authorize_create(Context) ->
    IsAuthenticated = cb_context:is_authenticated(Context),
    IsSuperDuperAdmin = cb_context:is_superduper_admin(Context),
    IsReseller = cb_context:reseller_id(Context) =:= cb_context:auth_account_id(Context),
    case IsAuthenticated
        andalso (IsSuperDuperAdmin
                 orelse IsReseller
                )
    of
        'true' -> 'true';
        'false' -> {'halt', cb_context:add_system_error('forbidden', Context)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /ledgers mights load a list of ledgers objects
%% /ledgers/123 might load the ledgers object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_ledgers(Context, cb_context:req_verb(Context)).

validate(Context, ?CREDIT) ->
    ReqData = cb_context:req_data(Context),
    JObj = kz_json:set_value([<<"usage">>, <<"type">>], ?CREDIT, ReqData),
    cb_context:validate_request_data(<<"ledgers">>, cb_context:set_req_data(Context, JObj));
validate(Context, ?DEBIT) ->
    ReqData = cb_context:req_data(Context),
    JObj = kz_json:set_value([<<"usage">>, <<"type">>], ?DEBIT, ReqData),
    cb_context:validate_request_data(<<"ledgers">>, cb_context:set_req_data(Context, JObj));
validate(Context, Id) ->
    validate_ledger(Context, Id, cb_context:req_verb(Context)).

validate(Context, Ledger, Id) ->
    validate_ledger_doc(Context, Ledger, Id, cb_context:req_verb(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?CREDIT) ->
    credit_or_debit(Context, ?CREDIT);
put(Context, ?DEBIT) ->
    credit_or_debit(Context, ?DEBIT).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_ledgers(cb_context:context(), http_method()) -> cb_context:context().
validate_ledgers(Context, ?HTTP_GET) ->
    read_ledgers(Context).

-spec validate_ledger(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_ledger(Context, Id, ?HTTP_GET) ->
    read_ledger(Context, Id).

-spec validate_ledger_doc(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_ledger_doc(Context, Ledger, Id, ?HTTP_GET) ->
    read_ledger_doc(Context, Ledger, Id).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec credit_or_debit(cb_context:context(), ne_binary()) -> cb_context:context().
credit_or_debit(Context, Action) ->
    ReqData = cb_context:req_data(Context),

    AccountId = cb_context:account_id(Context),
    SrcService = kz_json:get_value([<<"source">>, <<"service">>], ReqData),
    SrcId = kz_json:get_value([<<"source">>, <<"id">>], ReqData),
    Usage = kz_json:to_proplist(kz_json:get_value(<<"usage">>, ReqData)),

    Props =
        props:filter_undefined(
          [{<<"amount">>, kz_json:get_value(<<"amount">>, ReqData)}
          ,{<<"description">>, kz_json:get_value(<<"description">>, ReqData)}
          ,{<<"period_start">>, kz_json:get_value([<<"period">>, <<"start">>], ReqData)}
          ,{<<"period_end">>, kz_json:get_value([<<"period">>, <<"end">>], ReqData)}
          ,{<<"metadata">>, kz_json:get_value(<<"metadata">>, ReqData)}
          ]),

    case process_action(Action, SrcService, SrcId, AccountId, Usage, Props) of
        {'error', Reason} ->
            crossbar_util:response('error', Reason, Context);
        {'ok', JObj} ->
            maybe_impact_reseller(Context, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_action(ne_binary(), ne_binary(), ne_binary()
                    ,ne_binary(), kz_proplist(), kz_proplist()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
process_action(?CREDIT, SrcService, SrcId, AccountId, Usage, Props) ->
    kz_ledger:credit(AccountId, SrcService, SrcId, Usage, Props);
process_action(?DEBIT, SrcService, SrcId, AccountId, Usage, Props) ->
    kz_ledger:debit(AccountId, SrcService, SrcId, Usage, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_impact_reseller(cb_context:context(), kz_json:object()) -> cb_context:context().
maybe_impact_reseller(Context, Ledger) ->
    ResellerId = cb_context:reseller_id(Context),
    ImpactReseller = kz_json:is_true(<<"impact_reseller">>, cb_context:req_json(Context))
        andalso ResellerId =/= cb_context:account_id(Context),
    maybe_impact_reseller(Context, Ledger, ImpactReseller, ResellerId).

-spec maybe_impact_reseller(cb_context:context(), kz_json:object(), boolean(), api_binary()) -> cb_context:context().
maybe_impact_reseller(Context, Ledger, 'false', _ResellerId) ->
    crossbar_util:response(kz_doc:public_fields(Ledger), Context);
maybe_impact_reseller(Context, Ledger, 'true', 'undefined') ->
    crossbar_util:response(kz_doc:public_fields(Ledger), Context);
maybe_impact_reseller(Context, Ledger, 'true', ResellerId) ->
    case kazoo_ledger:save(kz_doc:delete_revision(Ledger), ResellerId) of
        {'ok', _} -> crossbar_util:response(kz_doc:public_fields(Ledger), Context);
        {'error', Error} ->
            Props = kz_json:recursive_to_proplist(Ledger),
            kz_notify:detailed_alert(?NOTIFY_MSG, [ResellerId, Error], Props),
            crossbar_util:response(kz_doc:public_fields(Ledger), Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec read_ledgers(cb_context:context()) -> cb_context:context().
read_ledgers(Context) ->
    {From, To} = case cb_modules_util:range_view_options(Context) of
                     {_CreatedFrom, _CreatedTo}=FromTo -> FromTo;
                     _ContextWithError -> {undefined, undefined}
                 end,
    case kz_ledgers:get(cb_context:account_id(Context), From, To) of
        {'error', Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Reason), Context);
        {'ok', Ledgers} ->
            crossbar_util:response(summary_to_dollars(Ledgers), Context)
    end.

-spec summary_to_dollars(kz_json:object()) -> kz_json:object().
summary_to_dollars(LedgersJObj) ->
    kz_json:expand(
      kz_json:from_list(
        [{Path, maybe_convert_units(lists:last(Path), Value)}
         || {Path, Value} <- kz_json:to_proplist(kz_json:flatten(LedgersJObj))
        ])).

-spec maybe_convert_units(ne_binary(), kz_transaction:units() | T) -> kz_transaction:dollars() | T when T::any().
maybe_convert_units(<<"amount">>, Units) -> wht_util:units_to_dollars(Units);
maybe_convert_units(_, Value) -> Value.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec read_ledger(cb_context:context(), ne_binary()) -> cb_context:context().
read_ledger(Context, Ledger) ->
    case cb_modules_util:range_view_options(Context) of
        {CreatedFrom, CreatedTo} ->
            AccountId = cb_context:account_id(Context),
            Databases = kazoo_modb:get_range(AccountId, CreatedFrom, CreatedTo),
            ViewOptions = [{'startkey', [Ledger, CreatedTo]}
                          ,{'endkey', [Ledger, CreatedFrom]}
                          ,{'limit', pagination_page_size(Context)}
                          ,'descending'
                          ,'include_docs'
                          ,{'databases', Databases}
                          ],
            C1 = crossbar_doc:load_view(?LEDGER_VIEW, ViewOptions, Context, fun normalize_view_results/3),
            fix_start_keys(C1, cb_context:resp_status(C1));
        Context1 ->
            Context1
    end.

-spec pagination_page_size(cb_context:context()) -> api_pos_integer().
pagination_page_size(Context) ->
    case crossbar_doc:pagination_page_size(Context) of
        'undefined' -> 'undefined';
        PageSize -> PageSize + 1
    end.

-spec fix_start_keys(cb_context:context(), crossbar_status()) -> cb_context:context().
fix_start_keys(Context, 'success') ->
    cb_context:set_resp_envelope(Context
                                ,lists:foldl(fun fix_start_keys_fold/2
                                            ,cb_context:resp_envelope(Context)
                                            ,[<<"start_key">>, <<"next_start_key">>]
                                            )
                                );
fix_start_keys(Context, _) -> Context.

-spec fix_start_keys_fold(kz_json:path(), kz_json:object()) -> kz_json:object().
fix_start_keys_fold(Key, JObj) ->
    case kz_json:get_value(Key, JObj) of
        'undefined' -> JObj;
        [_Ledger, Timestamp] -> kz_json:set_value(Key, Timestamp, JObj)
    end.

-spec normalize_view_results(cb_context:context(), kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(Context, JObj, Acc) ->
    [normalize_view_result(Context, kz_json:get_value(<<"doc">>, JObj)) | Acc].

-spec normalize_view_result(cb_context:context(), kz_json:object()) -> kz_json:object().
normalize_view_result(Context, JObj) ->
    normalize_view_result(Context, kz_doc:type(JObj), JObj).

-spec normalize_view_result(cb_context:context(), ne_binary(), kz_json:object()) -> kz_json:object().
normalize_view_result(_Context, <<"ledger">>, JObj) ->
    Value = wht_util:units_to_dollars(kazoo_ledger:amount(JObj)),
    Ledger = kazoo_ledger:set_amount(JObj, Value),
    kz_doc:public_fields(maybe_set_doc_modb_prefix(Ledger));
%% Legacy, this would be debit or credit from per-minute transactions
normalize_view_result(Context, _DocType, JObj) ->
    Transaction = kz_transaction:from_json(JObj),
    kz_json:from_list(
      [{<<"source">>, kz_json:from_list([{<<"service">>, <<"per-minute-voip">>}
                                        ,{<<"id">>, kz_transaction:call_id(Transaction)}
                                        ])}
      ,{<<"account">>, kz_json:from_list(
                         case kz_transaction:code(Transaction) of
                             Code when Code =:= ?CODE_PER_MINUTE_CALL ->
                                 [{<<"id">>, kz_transaction:account_id(Transaction)}
                                 ,{<<"name">>, cb_context:account_name(Context)}
                                 ];
                             Code when Code =:= ?CODE_PER_MINUTE_CALL_SUB_ACCOUNT ->
                                 [{<<"id">>, kz_transaction:sub_account_id(Transaction)}
                                 ,{<<"name">>, kz_transaction:sub_account_name(Transaction)}
                                 ]
                         end
                        )}
      ,{<<"usage">>, kz_json:from_list([{<<"type">>, <<"voice">>}
                                       ,{<<"unit">>, <<"sec">>}
                                       ,{<<"quantity">>, kz_json:get_integer_value(<<"duration">>, kz_transaction:metadata(Transaction), 0)}
                                       ])}
      ,{<<"amount">>, wht_util:units_to_dollars(kz_transaction:amount(Transaction))}
      ,{<<"description">>, kz_transaction:description(Transaction)}
      ,{<<"period">>, kz_json:from_list([{<<"start">>, kz_transaction:created(Transaction)}])}
      ,{<<"metadata">>, kz_transaction:metadata(Transaction)}
      ,{<<"id">>, kz_doc:id(maybe_set_doc_modb_prefix(JObj))}
      ]).

-spec maybe_set_doc_modb_prefix(kz_json:object()) -> kz_json:object().
maybe_set_doc_modb_prefix(JObj) ->
    case kz_doc:id(JObj) of
        ?MATCH_MODB_PREFIX(_,_,_) -> JObj;
        _ ->
            {Year, Month, _} = kz_term:to_date(kz_doc:created(JObj)),
            Id = <<(kz_term:to_binary(Year))/binary
                   ,(kz_time:pad_month(Month))/binary
                   ,"-"
                   ,(kz_doc:id(JObj))/binary
                 >>,
            kz_doc:set_id(JObj, Id)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec read_ledger_doc(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read_ledger_doc(Context, Ledger, ?MATCH_MODB_PREFIX(YYYY, MM, SimpleId) = Id) ->
    Year  = kz_term:to_integer(YYYY),
    Month = kz_term:to_integer(MM),
    Options = ?TYPE_CHECK_OPTION([<<"ledger">>, ?DEBIT, ?CREDIT]),
    Ctx = crossbar_doc:load(Id, cb_context:set_account_modb(Context, Year, Month), Options),
    case cb_context:resp_status(Ctx) =:= 'success'
        andalso validate_returned_ledger_doc(Ledger, Ctx)
    of
        'false' ->
            read_ledger_doc(cb_context:set_account_modb(Context, Year, Month), Ledger, SimpleId);
        Ctx1 -> Ctx1
    end;
read_ledger_doc(Context, Ledger, Id) ->
    Options = ?TYPE_CHECK_OPTION([<<"ledger">>, ?DEBIT, ?CREDIT]),
    Ctx = crossbar_doc:load(Id, Context, Options),
    case cb_context:resp_status(Ctx) =:= 'success'
        andalso validate_returned_ledger_doc(Ledger, Ctx)
    of
        'false' -> Ctx;
        Ctx1 -> Ctx1
    end.

-spec validate_returned_ledger_doc(ne_binary(), cb_context:context()) -> cb_context:context().
validate_returned_ledger_doc(Ledger, Context) ->
    JObj = cb_context:doc(Context),
    TransactionTypes = [?DEBIT, ?CREDIT],
    case (kz_doc:type(JObj) =:= <<"ledger">>
              andalso kazoo_ledger:source_service(JObj) =:= Ledger
         )
        orelse (lists:member(kz_doc:type(JObj), TransactionTypes)
                andalso Ledger =:= <<"per-minute-voip">>
                    andalso kz_transaction:is_per_minute(kz_transaction:from_json(JObj))
               )
    of
        'true' -> cb_context:set_resp_data(Context, normalize_view_result(Context, JObj));
        'false' ->
            Msg = kz_json:from_list([{<<"message">>, <<"document does not belong to ledger">>}
                                    ]),
            cb_context:add_validation_error(<<"Id">>, <<"invalid">>, Msg, Context)
    end.
