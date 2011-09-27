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
                  crossbar_util:put_reqid(Context),
		  Context1 = validate(Params, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.braintree">>, [RD, Context | [<<"customer">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Customer = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_customer:update(Customer) of
                                 {ok, #bt_customer{}=C} ->
                                     Response = braintree_customer:record_to_json(C),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_utils:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.braintree">>, [RD, Context | [<<"cards">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Card = crossbar_util:fetch(braintree, Context),
                  io:format("~p~n", [Card]),
                  Context1 = case braintree_card:create(Card) of
                                 {ok, #bt_card{}=C} ->
                                     Response = braintree_card:record_to_json(C),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_utils:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.braintree">>, [RD, Context | [<<"cards">>, CardId]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Card = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_card:update(Card) of
                                 {ok, #bt_card{}=C} ->
                                     Response = braintree_card:record_to_json(C),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_utils:bt_api_error_to_json(ApiError),
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
                  crossbar_util:put_reqid(Context),
                  Context1 = case braintree_card:delete(CardId) of
                                 {ok, #bt_card{}=C} ->
                                     Response = braintree_card:record_to_json(C),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_utils:bt_api_error_to_json(ApiError),
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
                  crossbar_util:put_reqid(Context),
                  Address = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_address:create(Address) of
                                 {ok, #bt_address{}=A} ->
                                     Response = braintree_address:record_to_json(A),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_utils:bt_api_error_to_json(ApiError),
                                     crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
                                 {error, _} ->
                                     crossbar_util:response_db_fatal(Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.braintree">>, [RD, Context | [<<"addresses">>, AddressId]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Address = crossbar_util:fetch(braintree, Context),
                  Context1 = case braintree_address:update(Address) of
                                 {ok, #bt_address{}=A} ->
                                     Response = braintree_address:record_to_json(A),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_utils:bt_api_error_to_json(ApiError),
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
                  crossbar_util:put_reqid(Context),
                  Context1 = case braintree_address:delete(Context#cb_context.account_id, AddressId) of
                                 {ok, #bt_address{}=A} ->
                                     Response = braintree_card:record_to_json(A),
                                     crossbar_util:response(Response, Context);
                                 {error, #bt_api_error{}=ApiError} ->
                                     Response = braintree_utils:bt_api_error_to_json(ApiError),
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
            Response = braintree_utils:bt_api_error_to_json(ApiError),
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
            Response = braintree_utils:bt_api_error_to_json(ApiError),
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
            Response = braintree_utils:bt_api_error_to_json(ApiError),
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
            Response = braintree_utils:bt_api_error_to_json(ApiError),
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
            Response = braintree_utils:bt_api_error_to_json(ApiError),
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
            Response = braintree_utils:bt_api_error_to_json(ApiError),
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
            Response = braintree_utils:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;

validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

create_placeholder_account(#cb_context{account_id=AccountId}=Context) ->
    case braintree_customer:create(#bt_customer{id=AccountId}) of
        {ok, #bt_customer{}=C} ->
            Response = braintree_customer:record_to_json(C),
            crossbar_util:response(Response, Context);
        {error, #bt_api_error{}=ApiError} ->
            Response = braintree_utils:bt_api_error_to_json(ApiError),
            crossbar_util:response(error, <<"braintree api error">>, 400, Response, Context);
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end.
