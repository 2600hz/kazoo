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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,authenticate/1
         ,authorize/1
         ,validate/1, validate/2, validate/3
         ,post/3
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"ledgers/crossbar_listing">>).
-define(LIST_BY_TYPE, <<"ledgers/listing_by_type">>).

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
    _ = crossbar_bindings:bind(<<"*.execute.post.ledgers">>, ?MODULE, 'post').

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
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(_, ?CREDIT) ->
    [?HTTP_POST];
allowed_methods(_, ?DEBIT) ->
    [?HTTP_POST].


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
resource_exists(_, ?CREDIT) -> 'true';
resource_exists(_, ?DEBIT) -> 'true'.

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
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_ledgers(Context, cb_context:req_verb(Context)).

validate(Context, Id) ->
    validate_ledger(Context, Id, cb_context:req_verb(Context)).

validate(Context, Id, _Action) ->
    validate_ledger(Context, Id, cb_context:req_verb(Context)).

-spec validate_ledgers(cb_context:context(), http_method()) -> cb_context:context().
validate_ledgers(Context, ?HTTP_GET) ->
    read_ledgers(Context).

-spec validate_ledger(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_ledger(Context, Id, ?HTTP_GET) ->
    read_ledger(Context, Id);
validate_ledger(Context, _Id, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"ledgers">>, Context, fun is_admin/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, Id, ?CREDIT) ->
    credit_or_debit(Context, Id, ?CREDIT);
post(Context, Id, ?DEBIT) ->
    credit_or_debit(Context, Id, ?DEBIT).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec credit_or_debit(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
credit_or_debit(Context, Name, Action) ->
    AccountId = cb_context:account_id(Context),
    ReqData = cb_context:req_data(Context),
    Amount = wh_json:get_integer_value(<<"amount">>, ReqData, 0),
    Desc = wh_json:get_ne_binary_value(<<"description">>, ReqData),
    case process_action(Action, Name, Amount, AccountId, Desc) of
        {'error', Reason} ->
            crossbar_util:response('error', Reason, Context);
        {'ok', Ledger} ->
            crossbar_util:response(kazoo_ledger:to_public_json(Ledger), Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_action(ne_binary(), ne_binary(), integer(), ne_binary(), ne_binary()) ->
                            {'ok', any()} |
                            {'error', any()}.
process_action(?CREDIT, Name, Amount, AccountId, Desc) ->
    kz_ledger:credit(Name, Amount, AccountId, Desc);
process_action(?DEBIT, Name, Amount, AccountId, Desc) ->
    kz_ledger:debit(Name, Amount, AccountId, Desc).

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
