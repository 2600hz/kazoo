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
         ,authenticate/1
         ,authorize/1
         ,validate/1, validate/2
         ,put/2
        ]).

-include("crossbar.hrl").

-define(CREDIT, <<"credit">>).
-define(DEBIT, <<"debit">>).

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
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
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
-spec authenticate(cb_context:context()) -> boolean().
authenticate(_Context) -> 'false'.
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(_Context) -> 'false'.

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
    cb_context:validate_request_data(<<"ledgers">>, Context, fun is_admin/1);
validate(Context, ?DEBIT) ->
    cb_context:validate_request_data(<<"ledgers">>, Context, fun is_admin/1);
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
    SrcService = wh_json:get_value([<<"source">>, <<"service">>], ReqData),
    SrcId = wh_json:get_value([<<"source">>, <<"id">>], ReqData),
    Usage = wh_json:to_proplist(wh_json:get_value(<<"usage">>, ReqData)),

    Props =
        props:filter_undefined([
            {<<"amount">>, wh_json:get_value(<<"amount">>, ReqData)}
            ,{<<"description">>, wh_json:get_value(<<"description">>, ReqData)}
            ,{<<"period_start">>, wh_json:get_value([<<"period">>, <<"start">>], ReqData)}
            ,{<<"period_end">>, wh_json:get_value([<<"period">>, <<"end">>], ReqData)}
        ]),

    case process_action(Action, SrcService, SrcId, AccountId, Usage, Props) of
        {'error', Reason} ->
            crossbar_util:response('error', Reason, Context);
        {'ok', JObj} ->
            JObj1 = wh_json:public_fields(JObj),
            crossbar_util:response(JObj1, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_action(ne_binary(), ne_binary(), ne_binary()
                     ,ne_binary(), wh_proplist(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
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
                wh_json:from_list([
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
