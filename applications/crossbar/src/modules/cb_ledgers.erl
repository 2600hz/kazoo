%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_ledgers).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,authorize/1, authorize/2
         ,validate/1, validate/2
         ,put/2
        ]).

-include("crossbar.hrl").

-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).

-define(NOTIFY_MSG, "failed to impact reseller ~s ledger : ~p").

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
allowed_methods() ->
    [?HTTP_GET].

allowed_methods(?CREDIT) ->
    [?HTTP_PUT];
allowed_methods(?DEBIT) ->
    [?HTTP_PUT];
allowed_methods(_) ->
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
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    cb_simple_authz:authorize(Context).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, Path) ->
    authorize_request(Context, Path, cb_context:req_verb(Context)).

-spec authorize_request(cb_context:context(), path_token(), http_method()) -> boolean().
authorize_request(Context, ?DEBIT, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, ?CREDIT, ?HTTP_PUT) ->
    authorize_create(Context);
authorize_request(Context, _, ?HTTP_PUT) ->
    {'halt', cb_context:add_system_error('forbidden', Context)};
authorize_request(Context, _, ?HTTP_GET) ->
    cb_simple_authz:authorize(Context).

-spec authorize_create(cb_context:context()) -> boolean().
authorize_create(Context) ->
    IsAuthenticated = cb_context:is_authenticated(Context),
    IsSuperDuperAdmin = cb_modules_util:is_superduper_admin(Context),
    IsReseller = cb_context:reseller_id(Context) =:= cb_context:auth_account_id(Context),
    case IsAuthenticated andalso (IsSuperDuperAdmin orelse IsReseller) of
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
validate(Context) ->
    validate_ledgers(Context, cb_context:req_verb(Context)).

validate(Context, ?CREDIT) ->
    ReqData = cb_context:req_data(Context),
    JObj = kz_json:set_value([<<"usage">>, <<"type">>], ?CREDIT, ReqData),
    cb_context:validate_request_data(<<"ledgers">>, cb_context:set_req_data(Context, JObj), fun is_admin/1);
validate(Context, ?DEBIT) ->
    ReqData = cb_context:req_data(Context),
    JObj = kz_json:set_value([<<"usage">>, <<"type">>], ?DEBIT, ReqData),
    cb_context:validate_request_data(<<"ledgers">>, cb_context:set_req_data(Context, JObj), fun is_admin/1);
validate(Context, Id) ->
    validate_ledger(Context, Id, cb_context:req_verb(Context)).

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
        props:filter_undefined([
            {<<"amount">>, kz_json:get_value(<<"amount">>, ReqData)}
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
process_action(?CREDIT, SrcService, SrcId, Account, Usage, Props) ->
    kz_ledger:credit(SrcService, SrcId, Account, Usage, Props);
process_action(?DEBIT, SrcService, SrcId, Account, Usage, Props) ->
    kz_ledger:debit(SrcService, SrcId, Account, Usage, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_impact_reseller(cb_context:context(), kz_json:object()) -> cb_context:context().
maybe_impact_reseller(Context, Ledger) ->
    ResellerId = cb_context:reseller_id(Context),
    ImpactReseller = kz_json:is_true(<<"impact_reseller">>, cb_context:req_json(Context)) andalso
                         ResellerId =/= cb_context:account_id(Context),
    maybe_impact_reseller(Context, Ledger, ImpactReseller, ResellerId).

-spec maybe_impact_reseller(cb_context:context(), kz_json:object(), boolean(), api_binary()) -> cb_context:context().
maybe_impact_reseller(Context, Ledger, 'false', _ResellerId) ->
    crossbar_util:response(kz_json:public_fields(Ledger), Context);
maybe_impact_reseller(Context, Ledger, 'true', 'undefined') ->
    crossbar_util:response(kz_json:public_fields(Ledger), Context);
maybe_impact_reseller(Context, Ledger, 'true', ResellerId) ->
    case kazoo_ledger:save(kz_doc:delete_revision(Ledger), ResellerId) of
        {'ok', _} -> crossbar_util:response(kz_json:public_fields(Ledger), Context);
        {'error', Error} ->
            Props = kz_json:recursive_to_proplist(Ledger),
            kz_notify:detailed_alert(?NOTIFY_MSG, [ResellerId, Error], Props),
            crossbar_util:response(kz_json:public_fields(Ledger), Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec read_ledgers(cb_context:context()) -> cb_context:context().
read_ledgers(Context) ->
    case kz_ledgers:get(cb_context:account_id(Context)) of
        {'error', Reason} ->
            crossbar_util:response('error', Reason, Context);
        {'ok', Ledgers} ->
            crossbar_util:response(Ledgers, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec read_ledger(cb_context:context(), ne_binary()) -> cb_context:context().
read_ledger(Context, Ledger) ->
    case kz_ledger:get(cb_context:account_id(Context), Ledger) of
        {'error', Reason} ->
            crossbar_util:response('error', Reason, Context);
        {'ok', Value} ->
            crossbar_util:response(
                kz_json:from_list([
                    {Ledger, Value}
                ])
                ,Context
            )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_admin(cb_context:context()) -> cb_context:context().
is_admin(Context) ->
    case cb_modules_util:is_superduper_admin(Context) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' -> cb_context:add_system_error('forbidden', Context)
    end.
