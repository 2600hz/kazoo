%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
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

-define(AVAILABLE, <<"available">>).
-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).

-define(NOTIFY_MSG, "failed to impact reseller ~s ledger : ~p").

-define(LEDGER_VIEW, <<"ledgers/listing_by_service_legacy">>).
%%-define(LEDGER_VIEW, "ledgers/listing_by_service").
%% TODO: make this change for 4.1

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ledgers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ledgers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize.ledgers">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.ledgers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.ledgers">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.ledgers">>, ?MODULE, 'put').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?AVAILABLE) ->
    [?HTTP_GET];
allowed_methods(?CREDIT) ->
    [?HTTP_PUT];
allowed_methods(?DEBIT) ->
    [?HTTP_PUT];
allowed_methods(_LedgerId) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_LedgerId, _LedgerEntryId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /ledgers => []
%%    /ledgers/foo => [<<"foo">>]
%%    /ledgers/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) -> cb_simple_authz:authorize(Context).

-spec authorize(cb_context:context(), path_token()) -> boolean() | {'stop', cb_context:context()}.
authorize(Context, Path) ->
    authorize_request(Context, Path, cb_context:req_verb(Context)).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, _Path, _Id) ->
    cb_simple_authz:authorize(Context).

-spec authorize_request(cb_context:context(), path_token(), http_method()) ->
                               boolean() |
                               {'stop', cb_context:context()}.
authorize_request(Context, ?DEBIT, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, ?CREDIT, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(_Context, ?AVAILABLE, ?HTTP_GET) ->
    'true';
authorize_request(Context, _, ?HTTP_PUT) ->
    {'stop', cb_context:add_system_error('forbidden', Context)};
authorize_request(Context, _, ?HTTP_GET) ->
    cb_simple_authz:authorize(Context).

-spec authorize_create(cb_context:context()) -> boolean() |
                                                {'stop', cb_context:context()}.
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
        'false' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /ledgers mights load a list of ledgers objects
%% /ledgers/123 might load the ledgers object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_ledgers(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?CREDIT) ->
    ReqData = cb_context:req_data(Context),
    JObj = kz_json:set_value([<<"usage">>, <<"type">>], ?CREDIT, ReqData),
    cb_context:validate_request_data(<<"ledgers">>, cb_context:set_req_data(Context, JObj));
validate(Context, ?DEBIT) ->
    ReqData = cb_context:req_data(Context),
    JObj = kz_json:set_value([<<"usage">>, <<"type">>], ?DEBIT, ReqData),
    cb_context:validate_request_data(<<"ledgers">>, cb_context:set_req_data(Context, JObj));
validate(Context, ?AVAILABLE) ->
    Available = kz_ledgers:available_ledgers(cb_context:account_id(Context)),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, Available}
              ],
    cb_context:setters(Context, Setters);
validate(Context, Id) ->
    ViewOptions = [{'is_chunked', 'true'}
                  ,{'range_keymap', Id}
                  ,{'mapper', fun normalize_view_results/3}
                  ,'include_docs'
                  ],
    crossbar_view:load_modb(Context, ?LEDGER_VIEW, ViewOptions).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Ledger, Id) ->
    validate_ledger_doc(Context, Ledger, Id, cb_context:req_verb(Context)).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?CREDIT) ->
    credit_or_debit(Context, ?CREDIT);
put(Context, ?DEBIT) ->
    credit_or_debit(Context, ?DEBIT).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_ledgers(cb_context:context(), http_method()) -> cb_context:context().
validate_ledgers(Context, ?HTTP_GET) ->
    Options = [{'group', 'true'}
              ,{'group_level', 0}
              ,{'mapper', crossbar_view:map_value_fun()}
              ,{'reduce', 'true'}
              ,{'unchunkable', 'true'}
              ],
    Context1 = crossbar_view:load_modb(Context, <<"ledgers/list_by_timestamp_legacy">>, Options),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(Context1, summary_to_dollars(kz_json:sum_jobjs(cb_context:doc(Context1))));
        _ ->
            Context1
    end.

-spec validate_ledger_doc(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_ledger_doc(Context, Ledger, Id, ?HTTP_GET) ->
    read_ledger_doc(Context, Ledger, Id).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec credit_or_debit(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
credit_or_debit(Context, Action) ->
    ReqData = cb_context:req_data(Context),

    AccountId = cb_context:account_id(Context),
    SrcService = kz_json:get_value([<<"source">>, <<"service">>], ReqData),
    SrcId = kz_json:get_value([<<"source">>, <<"id">>], ReqData),
    Usage = kz_json:to_proplist(kz_json:get_value(<<"usage">>, ReqData)),

    Props =
        props:filter_undefined(
          [{<<"amount">>, kz_json:get_value(<<"amount">>, ReqData, 0)}
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_action(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()
                    ,kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
process_action(?CREDIT, SrcService, SrcId, AccountId, Usage, Props) ->
    kz_ledger:credit(AccountId, SrcService, SrcId, Usage, Props);
process_action(?DEBIT, SrcService, SrcId, AccountId, Usage, Props) ->
    kz_ledger:debit(AccountId, SrcService, SrcId, Usage, Props).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_impact_reseller(cb_context:context(), kz_json:object()) -> cb_context:context().
maybe_impact_reseller(Context, Ledger) ->
    ResellerId = cb_context:reseller_id(Context),
    ImpactReseller = kz_json:is_true(<<"impact_reseller">>, cb_context:req_json(Context))
        andalso ResellerId =/= cb_context:account_id(Context),
    maybe_impact_reseller(Context, Ledger, ImpactReseller, ResellerId).

-spec maybe_impact_reseller(cb_context:context(), kz_json:object(), boolean(), kz_term:api_binary()) -> cb_context:context().
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

-spec summary_to_dollars(kz_json:object()) -> kz_json:object().
summary_to_dollars(LedgersJObj) ->
    kz_json:expand(
      kz_json:from_list(
        [{Path, maybe_convert_units(lists:last(Path), Value)}
         || {Path, Value} <- kz_json:to_proplist(kz_json:flatten(LedgersJObj))
        ])).

-spec maybe_convert_units(kz_term:ne_binary(), kz_transaction:units() | T) -> kz_transaction:dollars() | T when T::any().
maybe_convert_units(<<"amount">>, 'undefined') -> 0;
maybe_convert_units(<<"amount">>, Units) -> wht_util:units_to_dollars(Units);
maybe_convert_units(_, Value) -> Value.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(cb_context:context(), kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(Context, JObj, Acc) ->
    [normalize_view_result(Context, kz_json:get_value(<<"doc">>, JObj)) | Acc].

-spec normalize_view_result(cb_context:context(), kz_json:object()) -> kz_json:object().
normalize_view_result(Context, JObj) ->
    normalize_view_result(Context, kz_doc:type(JObj), JObj).

-spec normalize_view_result(cb_context:context(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
normalize_view_result(_Context, <<"ledger">>, JObj) ->
    Value = wht_util:units_to_dollars(kazoo_ledger:amount(JObj)),
    Ledger = kazoo_ledger:set_amount(JObj, Value),
    Id = maybe_set_doc_modb_prefix(kz_doc:id(Ledger), kz_doc:created(Ledger)),
    kz_doc:public_fields(kz_doc:set_id(Ledger, Id));
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
      ,{<<"id">>, maybe_set_doc_modb_prefix(kz_doc:id(JObj), kz_doc:created(JObj))}
      ]).

-spec maybe_set_doc_modb_prefix(kz_term:ne_binary(), kz_term:api_integer()) -> kz_term:ne_binary().
maybe_set_doc_modb_prefix(?MATCH_MODB_PREFIX(_,_,_)=Id, _) -> Id;
maybe_set_doc_modb_prefix(Id, Created) ->
    {Year, Month, _} = kz_term:to_date(Created),
    kazoo_modb_util:modb_id(Year, Month, Id).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec read_ledger_doc(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
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

-spec validate_returned_ledger_doc(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
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
            lager:debug("document type ~s does not match the expected types", [kz_doc:type(JObj)]),
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, kz_doc:id(JObj)}]),  Context)
    end.
