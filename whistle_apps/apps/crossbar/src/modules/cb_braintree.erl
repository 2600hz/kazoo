%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%%
%%% Handle client requests for braintree documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_braintree).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").
-include_lib("braintree/include/braintree.hrl").

-define(SERVER, ?MODULE).
-define(TS_DB, <<"ts">>).
-define(IS_TOLLFREE_RE, "^(\\+?1)?(8[1-4,9]\\d{8}|80[1-9]\\d{7}|85[1-4,6-9]\\d{7}|86[1-5,7-9]\\d{7}|87[1-6,8-9]\\d{7}|88[1-7,9]\\d{7}|([1-7,9]\\d{9}))$").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    ssl:start(),
    {ok, ok, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({binding_fired, Pid, <<"v1_resource.billing">>
                 ,{RD, #cb_context{req_verb = <<"head">>}=Context}}, State) ->
     Pid ! {binding_result, true, {RD, Context}},
     {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.billing">>
                 ,{RD, #cb_context{req_nouns=[{<<"ts_accounts">>, Params}]}=Context}}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = try
                                 case authorize_trunkstore(Params, Context) of
                                     #cb_context{resp_status=success}=C ->
                                         ?LOG("billing is satisfied, allowing request"),
                                         C;
                                     Else ->
                                         Else
                                 end
                             catch
                                 _:_ ->
                                     crossbar_util:response(error, <<"error during billing validation">>, 500, Context)
                             end,
                  Pid ! {binding_result, true, {RD, Context1}}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.braintree">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.braintree">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.braintree">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.braintree">>, [RD, Context | [<<"customer">>]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Customer = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_customer:update(Customer) of
                                 {ok, #bt_customer{}=C} ->
                                     Response = braintree_customer:record_to_json(C),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_util:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.braintree">>, [RD, #cb_context{account_id=AcctID, req_data=ReqData}=Context | [<<"credits">>]=Params]}, State) ->
    spawn(fun() ->
		  _ = crossbar_util:put_reqid(Context),
                  _ = crossbar_util:binding_heartbeat(Pid),

		  Units = wapi_money:dollars_to_units(wh_json:get_float_value(<<"amount">>, ReqData)),
		  ?LOG("Putting ~p units", [Units]),

		  Transaction = wh_json:from_list([
						   {<<"amount">>, Units}
						   ,{<<"pvt_type">>, <<"credit">>}
						   ]),

		  #cb_context{resp_status=success, doc=Saved} = crossbar_doc:save(Context#cb_context{doc=Transaction, db_name=whapps_util:get_db_name(AcctID, encoded)}),

		  Id = wh_json:get_value(<<"_id">>, Saved),

		  ?LOG("publishing credit API for transaction ~s for ~p units", [Id, Units]),

		  wapi_money:publish_credit([{<<"Account-ID">>, AcctID}
					     ,{<<"Amount">>, Units}
					     ,{<<"Transaction-ID">>, Id}
					     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
					    ]),
                  Pid ! {binding_result, true, [RD, Context, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.braintree">>, [RD, Context | [<<"cards">>]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  _ = crossbar_util:binding_heartbeat(Pid),
                  Card = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_card:create(Card) of
                                 {ok, #bt_card{}=C} ->
                                     Response = braintree_card:record_to_json(C),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_util:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.braintree">>, [RD, Context | [<<"cards">>, CardId]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  _ = crossbar_util:binding_heartbeat(Pid),
                  Card = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_card:update(Card) of
                                 {ok, #bt_card{}=C} ->
                                     Response = braintree_card:record_to_json(C),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_util:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, not_found} ->
                                     crossbar_util:response_bad_identifier(CardId, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.braintree">>, [RD, Context | [<<"cards">>, CardId]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  _ = crossbar_util:binding_heartbeat(Pid),
                  Context1 = case braintree_card:delete(CardId) of
                                 {ok, #bt_card{}=C} ->
                                     Response = braintree_card:record_to_json(C),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_util:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, not_found} ->
                                     crossbar_util:response_bad_identifier(CardId, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.braintree">>, [RD, Context | [<<"addresses">>]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  _ = crossbar_util:binding_heartbeat(Pid),
                  Address = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_address:create(Address) of
                                 {ok, #bt_address{}=A} ->
                                     Response = braintree_address:record_to_json(A),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_util:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.braintree">>, [RD, Context | [<<"addresses">>, AddressId]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  _ = crossbar_util:binding_heartbeat(Pid),
                  Address = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_address:update(Address) of
                                 {ok, #bt_address{}=A} ->
                                     Response = braintree_address:record_to_json(A),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_util:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, not_found} ->
                                     crossbar_util:response_bad_identifier(AddressId, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.braintree">>, [RD, Context | [<<"addresses">>, AddressId]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  _ = crossbar_util:binding_heartbeat(Pid),
                  Context1 = case braintree_address:delete(Context#cb_context.account_id, AddressId) of
                                 {ok, #bt_address{}=A} ->
                                     Response = braintree_card:record_to_json(A),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_util:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, not_found} ->
                                     crossbar_util:response_bad_identifier(AddressId, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function binds this server to the crossbar bindings server,
%% for the keys we need to consume.
%% @end
%%--------------------------------------------------------------------
-spec bind_to_crossbar/0 :: () ->  no_return().
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.billing">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.braintree">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.braintree">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.braintree">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.braintree">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (Paths) -> tuple(boolean(), http_methods()) when
      Paths :: list().
allowed_methods([<<"customer">>]) ->
    {true, ['GET', 'POST']};

allowed_methods([<<"cards">>]) ->
    {true, ['GET', 'PUT']};
allowed_methods([<<"cards">>, _]) ->
    {true, ['GET', 'POST', 'DELETE']};

allowed_methods([<<"addresses">>]) ->
    {true, ['GET', 'PUT']};
allowed_methods([<<"addresses">>, _]) ->
    {true, ['GET', 'POST', 'DELETE']};

allowed_methods([<<"transactions">>]) ->
    {true, ['GET', 'PUT']};
allowed_methods([<<"transactions">>, _]) ->
    {true, ['GET']};

allowed_methods([<<"credits">>]) ->
    {true, ['GET', 'PUT']};

allowed_methods(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/1 :: (Paths) -> tuple(boolean(), []) when
      Paths :: list().
resource_exists([<<"customer">>]) ->
    {true, []};
resource_exists([<<"cards">>]) ->
    {true, []};
resource_exists([<<"cards">>, _]) ->
    {true, []};
resource_exists([<<"addresses">>]) ->
    {true, []};
resource_exists([<<"addresses">>, _]) ->
    {true, []};
resource_exists([<<"transactions">>]) ->
    {true, []};
resource_exists([<<"transactions">>, _]) ->
    {true, []};
resource_exists([<<"credits">>]) ->
    {true, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/2 :: (Params, Context) -> #cb_context{} when
      Params :: list(),
      Context :: #cb_context{}.
%% CUSTOMER API
validate([<<"customer">>], #cb_context{req_verb = <<"get">>, account_id=AccountId}=Context) ->
    case braintree_customer:find(AccountId) of
        {ok, #bt_customer{}=C} ->
            Response = braintree_customer:record_to_json(C),
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, not_found} ->
            create_placeholder_account(Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate([<<"customer">>], #cb_context{req_verb = <<"post">>, req_data=JObj, account_id=AccountId}=Context) ->
    Customer = braintree_customer:json_to_record(wh_json:set_value(<<"id">>, AccountId, JObj)),
    crossbar_util:response([], crossbar_util:store(braintree, Customer, Context));

%% CARD API
validate([<<"cards">>], #cb_context{req_verb = <<"get">>, account_id=AccountId}=Context) ->
    case braintree_customer:find(AccountId) of
        {ok, #bt_customer{credit_cards=Cards}} ->
            Response = [braintree_card:record_to_json(Card) || Card <- Cards],
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate([<<"cards">>], #cb_context{req_verb = <<"put">>, req_data=JObj, account_id=AccountId}=Context) ->
    Card = (braintree_card:json_to_record(JObj))#bt_card{customer_id=wh_util:to_list(AccountId)},
    crossbar_util:response([], crossbar_util:store(braintree, Card, Context));
validate([<<"cards">>, CardId], #cb_context{req_verb = <<"get">>, account_id=Account}=Context) ->
    AccountId = wh_util:to_list(Account),
    case braintree_card:find(CardId) of
        {ok, #bt_card{customer_id=AccountId}=C} ->
            Response = braintree_card:record_to_json(C),
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(CardId, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate([<<"cards">>, CardId], #cb_context{req_verb = <<"post">>, req_data=JObj, account_id=AccountId}=Context) ->
    Card = (braintree_card:json_to_record(JObj))#bt_card{customer_id=wh_util:to_list(AccountId), token=CardId},
    crossbar_util:response([], crossbar_util:store(braintree, Card, Context));
validate([<<"cards">>, _], #cb_context{req_verb = <<"delete">>}=Context) ->
    crossbar_util:response([], Context);

%% ADDRESS API
validate([<<"addresses">>], #cb_context{req_verb = <<"get">>, account_id=AccountId}=Context) ->
    case braintree_customer:find(AccountId) of
        {ok, #bt_customer{addresses=Addresses}} ->
            Response = [braintree_address:record_to_json(Address) || Address <- Addresses],
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate([<<"addresses">>], #cb_context{req_verb = <<"put">>, req_data=JObj, account_id=AccountId}=Context) ->
    Address = (braintree_address:json_to_record(JObj))#bt_address{customer_id=wh_util:to_list(AccountId)},
    crossbar_util:response([], crossbar_util:store(braintree, Address, Context));
validate([<<"addresses">>, AddressId], #cb_context{req_verb = <<"get">>, account_id=Account}=Context) ->
    AccountId = wh_util:to_list(Account),
    case braintree_address:find(AddressId) of
        {ok, #bt_address{customer_id=AccountId}=C} ->
            Response = braintree_address:record_to_json(C),
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(AddressId, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate([<<"addresses">>, AddressId], #cb_context{req_verb = <<"post">>, req_data=JObj, account_id=AccountId}=Context) ->
    Address = (braintree_address:json_to_record(JObj))#bt_address{customer_id=wh_util:to_list(AccountId), id=AddressId},
    crossbar_util:response([], crossbar_util:store(braintree, Address, Context));
validate([<<"addresses">>, _], #cb_context{req_verb = <<"delete">>}=Context) ->
    crossbar_util:response([], Context);

%% TRANSACTION API
validate([<<"transactions">>], #cb_context{req_verb = <<"get">>, account_id=AccountId}=Context) ->
    case braintree_transaction:find_by_customer(AccountId) of
        {ok, Transactions} ->
            Response = [braintree_transaction:record_to_json(Transaction) || Transaction <- Transactions],
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate([<<"transactions">>, TransactionId], #cb_context{req_verb = <<"get">>}=Context) ->
    case braintree_transaction:find(TransactionId) of
        {ok, #bt_transaction{}=T} ->
            Response = braintree_address:record_to_json(T),
            crossbar_util:response(Response, Context);
        {ok, Transactions} ->
            Response = [braintree_transaction:record_to_json(Transaction) || Transaction <- Transactions],
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;

%% CREDIT SPECIFIC API
validate([<<"credits">>], #cb_context{req_verb = <<"get">>, account_id=AccountId, doc=JObj}=Context) ->
    %% TODO: request current balance from jonny5 and put it here
    DB = whapps_util:get_db_name(AccountId, encoded),
    Units = case couch_mgr:get_results(DB, <<"transactions/credit_remaining">>, [{<<"reduce">>, true}]) of
		{ok, []} -> ?LOG("No results"), 0;
		{ok, [ViewRes|_]} -> ?LOG("Found obj ~p", [ViewRes]), wh_json:get_value(<<"value">>, ViewRes, 0);
		{error, _E} -> ?LOG("Error loading view: ~p", [_E]), 0
	    end,

    crossbar_util:response(wh_json:from_list([{<<"amount">>, wapi_money:units_to_dollars(Units)}
                                              ,{<<"billing_account_id">>, wh_json:get_value(<<"billing_account_id">>, JObj, AccountId)}
                                             ]), Context);
validate([<<"credits">>], #cb_context{req_verb = <<"put">>, account_id=AccountId, req_data=JObj}=Context) ->
    BillingId = wh_json:get_value(<<"billing_account_id">>, JObj, AccountId),
    Amount = wh_json:get_value(<<"amount">>, JObj, <<"0.0">>),
    case braintree_transaction:quick_sale(BillingId, Amount) of
        {ok, #bt_transaction{}=Transaction} ->
            crossbar_util:response(braintree_transaction:record_to_json(Transaction), Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, Error} ->
            crossbar_util:response(error, <<"braintree api error">>, 400
                                   ,wh_json:from_list([{<<"cause">>, wh_util:to_binary(Error)}])
                                   ,Context)
    end;

validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates an empty customer in braintree
%% @end
%%--------------------------------------------------------------------
-spec create_placeholder_account/1 :: (#cb_context{}) -> #cb_context{}.
create_placeholder_account(#cb_context{account_id=AccountId}=Context) ->
    case braintree_customer:create(#bt_customer{id=wh_util:to_list(AccountId)}) of
        {ok, #bt_customer{}=C} ->
            ?LOG("created new customer ~s", [AccountId]),
            Response = braintree_customer:record_to_json(C),
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{message=Msg}=ApiError} ->
            ?LOG("failed to created new customer ~s", [Msg]),
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, _}=E ->
            ?LOG("failed to created new customer ~p", [E]),
            crossbar_util:response_db_fatal(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function processes any trunkstore requests for billin
%% changes, preforms the necessary braintree updates/charges and
%% terminates the request if it fails
%% @end
%%--------------------------------------------------------------------
-spec authorize_trunkstore/2 :: ([ne_binary()], #cb_context{}) -> #cb_context{}.
authorize_trunkstore(_, #cb_context{req_verb = <<"get">>}=Context) ->
    Context#cb_context{resp_status=success};
authorize_trunkstore(_, #cb_context{req_verb = <<"put">>, doc=JObj, auth_doc=AuthDoc}=Context) ->
    Updates = [{"outbound_us", fun() -> ts_outbound_us_quantity(JObj) end()}
               ,{"did_us", fun() -> ts_did_us_quantity(JObj) end()}
               ,{"tollfree_us", fun() -> ts_tollfree_us_quantity(JObj) end()}
               ,{"e911", fun() -> ts_e911_quantity(JObj) end()}],
    AuthId = wh_json:get_value(<<"account_id">>, AuthDoc),
    BillingAccount = wh_json:get_value(<<"billing_account_id">>, JObj, AuthId),
    case ts_get_subscription(JObj, BillingAccount, Context) of
        #cb_context{}=Error ->
            Error;
        {ok, Subscription} ->
            change_subscription(Updates, Subscription, Context)
    end;
authorize_trunkstore([AccountId], #cb_context{req_verb = <<"post">>, doc=JObj}=Context) ->
    Updates = [{"outbound_us", fun() -> ts_outbound_us_quantity(JObj) end()}
               ,{"did_us", fun() -> ts_did_us_quantity(JObj) end()}
               ,{"tollfree_us", fun() -> ts_tollfree_us_quantity(JObj) end()}
               ,{"e911", fun() -> ts_e911_quantity(JObj) end()}],
    BillingAccount = wh_json:get_value(<<"billing_account_id">>, JObj, AccountId),
    case ts_get_subscription(JObj, BillingAccount, Context) of
        #cb_context{}=Error ->
            Error;
        {ok, Subscription} ->
            change_subscription(Updates, Subscription, Context)
    end;
authorize_trunkstore([AccountId], #cb_context{req_verb = <<"delete">>, doc=JObj}=Context) ->
    case ts_get_subscription(JObj, AccountId, Context) of
        #cb_context{}=Error ->
            Error;
        {ok, #bt_subscription{id=SubscriptionId}} ->
            case braintree_subscription:cancel(SubscriptionId) of
                {ok, #bt_subscription{}} ->
                    ?LOG("cancelled braintree subscription ~s", [SubscriptionId]),
                    Context#cb_context{resp_status=success};
                {error, not_found} ->
                    Context#cb_context{resp_status=success};
                {error, #bt_api_error{message=Msg}=ApiError} ->
                    ?LOG("failed to cancel braintree subscription: ~s", [Msg]),
                    Response = braintree_util:bt_api_error_to_json(ApiError),
                    crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                Error ->
                    ?LOG("failed to cancel braintree subscription: ~p", [Error]),
                    crossbar_util:response_db_fatal(Context)
            end
    end.

ts_outbound_us_quantity(JObj) ->
    wh_json:get_integer_value([<<"account">>, <<"trunks">>], JObj, 0).

ts_did_us_quantity(JObj) ->
    InUse = [wh_json:get_keys(wh_json:get_value(<<"DIDs">>, Server, []))
             || Server <- wh_json:get_value(<<"servers">>, JObj, [])],
    Unassigned = [wh_json:get_keys(wh_json:get_value(<<"DIDs_Unassigned">>, JObj, []))],
    lists:foldr(ts_fold_did_fun(false), 0, lists:flatten([InUse|Unassigned])).

ts_tollfree_us_quantity(JObj) ->
    InUse = [wh_json:get_keys(wh_json:get_value(<<"DIDs">>, Server, []))
             || Server <- wh_json:get_value(<<"servers">>, JObj, [])],
    Unassigned = [wh_json:get_keys(wh_json:get_value(<<"DIDs_Unassigned">>, JObj, []))],
    lists:foldr(ts_fold_did_fun(true), 0, lists:flatten([InUse|Unassigned])).

ts_e911_quantity(JObj) ->
    E911 = [wh_json:get_value(<<"e911_info">>, Server, [])
             || Server <- wh_json:get_value(<<"servers">>, JObj, [])],
    length(E911).

ts_fold_did_fun(true) ->
    fun(Number, Count) ->
            case re:run(Number, ?IS_TOLLFREE_RE) of
                nomatch -> Count + 1;
                _ -> Count
            end
    end;
ts_fold_did_fun(false) ->
    fun(Number, Count) ->
            case re:run(Number, ?IS_TOLLFREE_RE) of
                nomatch -> Count;
                _ -> Count + 1
            end
    end.

ts_get_subscription(_, undefined, Context) ->
    crossbar_util:response(error, <<"billing account id unspecified">>, 400, Context);
ts_get_subscription(JObj, BillingAccount, Context) ->
    SubscriptionId = wh_json:get_string_value([<<"pvt_braintree">>, <<"trunkstore_subscription_id">>], JObj),
    case SubscriptionId =/= undefined andalso braintree_subscription:find(SubscriptionId) of
        false ->
            ?LOG("no trunkstore subscription id found"),
            Token = get_payment_token(BillingAccount, Context),
            create_subscription(Token, "SIP_Services");
        {ok, #bt_subscription{status=?BT_ACTIVE}}=Ok ->
            ?LOG("found active trunkstore subscription ~s", [SubscriptionId]),
            Ok;
        {error, not_found} ->
            ?LOG("trunkstore subscription id is not valid"),
            Token = get_payment_token(BillingAccount, Context),
            create_subscription(Token, "SIP_Services");
        {ok, #bt_subscription{status=Status}} ->
            ?LOG("found trunkstore subscription ~s", [SubscriptionId]),
            crossbar_util:response(error, <<"billing account is not active">>, 400
                                   ,{struct, [{<<"current_account_status">>, wh_util:to_binary(Status)}]}
                                   ,Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, no_card} ->
            crossbar_util:response(error, <<"no credit card on file">>, 400, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end.

change_subscription(Updates, #bt_subscription{id=undefined}=Subscription, #cb_context{doc=JObj}=Context) ->
    NewSubscription =
        lists:foldr(fun({AddOn, Quantity}, Sub) ->
                            {ok, Subscription1} =
                                braintree_subscription:update_addon_quantity(Sub, AddOn, Quantity),
                            Subscription1
                    end, Subscription, Updates),
    case braintree_subscription:create(NewSubscription) of
        {ok, #bt_subscription{id=Id}} ->
            ?LOG("created braintree subscription ~s", [Id]),
            Context#cb_context{doc=wh_json:set_value([<<"pvt_braintree">>, <<"trunkstore_subscription_id">>]
                                                     ,wh_util:to_binary(Id)
                                                     ,JObj)
                               ,resp_status=success};
        {error, #bt_api_error{message=Msg}=ApiError} ->
            ?LOG("failed to create braintree subscription: ~s", [Msg]),
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        Error ->
            ?LOG("failed to create braintree subscription: ~p", [Error]),
            crossbar_util:response_db_fatal(Context)
    end;
change_subscription(Updates, Subscription, #cb_context{doc=JObj}=Context) ->
    NewSubscription =
        lists:foldr(fun({AddOn, Quantity}, Sub) ->
                            {ok, Subscription1} =
                                braintree_subscription:update_addon_quantity(Sub, AddOn, Quantity),
                            Subscription1
                    end, Subscription, Updates),
    case braintree_subscription:update(NewSubscription) of
        {ok, #bt_subscription{id=Id}} ->
            ?LOG("updated braintree subscription ~s", [Id]),
            Context#cb_context{doc=wh_json:set_value([<<"pvt_braintree">>, <<"trunkstore_subscription_id">>]
                                                     ,wh_util:to_binary(Id)
                                                     ,JObj)
                               ,resp_status=success};
        {error, #bt_api_error{message=Msg}=ApiError} ->
            ?LOG("failed to updated braintree subscription: ~s", [Msg]),
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        Error ->
            ?LOG("failed to updated braintree subscription: ~p", [Error]),
            crossbar_util:response_db_fatal(Context)
    end.

create_subscription(#cb_context{}=E, _) ->
    E;
create_subscription(Token, Plan) ->
    ?LOG("creating new subscription ~s with token ~s", [Plan, Token]),
    {ok, #bt_subscription{payment_token=Token, plan_id=Plan, do_not_inherit=true}}.

get_payment_token(BillingAccount, Context) when not is_list(BillingAccount) ->
    get_payment_token(wh_util:to_list(BillingAccount), Context);
get_payment_token(BillingAccount, Context) ->
    case braintree_customer:find(BillingAccount) of
        {ok, #bt_customer{credit_cards=Cards}} ->
            ?LOG("found braintree customer ~s", [BillingAccount]),
            case [Card || #bt_card{default=Default}=Card <- Cards, Default] of
                [#bt_card{token=Token}] ->
                    ?LOG("braintree customer ~s default credit card token ~s", [BillingAccount, Token]),
                    Token;
                _ ->
                    ?LOG("braintree customer ~s has no credit card on file", [BillingAccount]),
                    crossbar_util:response(error, <<"no credit card on file">>, 400, Context)
            end;
        {error, not_found} ->
            case  braintree_customer:create(#bt_customer{id=BillingAccount}) of
                {ok, #bt_customer{}} ->
                    ?LOG("braintree customer ~s has no credit card on file", [BillingAccount]),
                    crossbar_util:response(error, <<"no credit card on file">>, 400, Context);
                {error, #bt_api_error{}=ApiError} ->
                    Response = braintree_util:bt_api_error_to_json(ApiError),
                    crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                _Else ->
                    crossbar_util:response_db_fatal(Context)
            end;
        {error, #bt_api_error{message=Msg}=ApiError} ->
            ?LOG("failed to find braintree customer: ~s", [Msg]),
            Response = braintree_util:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        _Else ->
            ?LOG("failed to find braintree customer: ~p", [_Else]),
            crossbar_util:response_db_fatal(Context)
    end.
