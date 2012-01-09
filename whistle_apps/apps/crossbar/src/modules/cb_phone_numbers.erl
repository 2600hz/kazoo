%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_phone_numbers).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(FIND_NUMBER_SCHEMA, "{\"$schema\": \"http://json-schema.org/draft-03/schema#\", \"id\": \"http://json-schema.org/draft-03/schema#\", \"properties\": {\"prefix\": {\"required\": \"true\", \"type\": \"string\", \"minLength\": 3, \"maxLength\": 8}, \"quantity\": {\"default\": 1, \"type\": \"integer\", \"minimum\": 1}}}").

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
handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{req_nouns=[{<<"phone_numbers">>,[]}]
                                   ,req_id=ReqId, req_verb = <<"get">>}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"phone_numbers">>,[]}], req_verb = <<"get">>}=Context}}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  ?LOG("authenticating request"),
                  Pid ! {binding_result, true, {RD, Context}}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.phone_numbers">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.phone_numbers">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.phone_numbers">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.phone_numbers">>
                 ,[RD, #cb_context{account_id=AccountId, doc=JObj}=Context | [Number]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Result = wh_number_manager:set_public_fields(Number, AccountId, JObj),
                  Context1 = set_response(Result, Number, Context),
                  Pid ! {binding_result, true, [RD, Context1, [Number]]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.phone_numbers">>
                 ,[RD, #cb_context{account_id=AccountId, doc=JObj}=Context | [Number]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = case wh_number_manager:assign_number_to_account(Number, AccountId) of
                                 {ok, _} ->
                                     Result = wh_number_manager:set_public_fields(Number, AccountId, JObj),
                                     set_response(Result, Number, Context);
                                 Else ->
                                     set_response(Else, Number, Context)
                             end,
                  Pid ! {binding_result, true, [RD, Context1, [Number]]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.phone_numbers">>, [RD, Context | [Number]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Pid ! {binding_result, true, [RD, Context, [Number]]}
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
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.phone_numbers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.phone_numbers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.phone_numbers">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.phone_numbers">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: ([ne_binary(),...] | []) -> {boolean(), http_methods()}.
allowed_methods([]) ->
    {true, ['GET']};
allowed_methods([_]) ->
    {true, ['GET', 'PUT', 'POST']};
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
-spec resource_exists/1 :: ([ne_binary(),...] | []) -> {boolean(), []}.
resource_exists([]) ->
    {true, []};
resource_exists([_]) ->
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
-spec validate/2 :: ([ne_binary(),...] | [], #cb_context{}) -> #cb_context{}.
validate([], #cb_context{req_verb = <<"get">>, account_id=undefined}=Context) ->
    find_numbers(Context);
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate([Number], #cb_context{req_verb = <<"get">>}=Context) ->
    read(Number, Context);
validate([Number], #cb_context{req_verb = <<"put">>}=Context) ->
    create(Number, Context);
validate([Number], #cb_context{req_verb = <<"post">>}=Context) ->
    update(Number, Context);
validate([Number], #cb_context{req_verb = <<"delete">>}=Context) ->
    delete(Number, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec find_numbers/1 :: (#cb_context{}) -> #cb_context{}.
find_numbers(#cb_context{query_json=Data}=Context) ->
    Schema = wh_json:decode(?FIND_NUMBER_SCHEMA),
    case wh_json_validator:is_valid(Data, Schema) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Prefix = wh_json:get_ne_value(<<"prefix">>, JObj),
            Quantity = wh_json:get_ne_value(<<"quantity">>, JObj, 1),
            Context#cb_context{resp_status=success
                               ,resp_data=wh_number_manager:find(Prefix, Quantity)
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(#cb_context{account_id=AccountId}=Context) ->
    case crossbar_doc:load(AccountId, Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            crossbar_util:response(wh_json:get_value(<<"pvt_wnm_numbers">>, JObj, [])
                                   ,Context1);
        Else ->
            Else
    end.
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
create(_, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"phone_numbers">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Context#cb_context{resp_status=success
                               ,doc=JObj
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Number, #cb_context{account_id=AccountId}=Context) ->
    Result = wh_number_manager:get_public_fields(Number, AccountId),
    set_response(Result, Number, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(_, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"phone_numbers">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Context#cb_context{resp_status=success
                               ,doc=JObj
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
delete(_, Context) ->
    Context#cb_context{resp_status=success
                       ,doc=undefined
                      }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_response/3 :: ({ok, json_object()} | {error, term()}, ne_binary(), #cb_context{}) -> #cb_context{}.
set_response({error, reserved}, _, Context) ->
    crossbar_util:response_conflicting_docs(Context);
set_response({error, unavailable}, _, Context) ->
    crossbar_util:response_conflicting_docs(Context);
set_response({error, unathorized}, Number, Context) ->
    crossbar_util:response_bad_identifier(Number, Context);
set_response({error, unknown_carrier}, _, Context) ->
    crossbar_util:response_db_fatal(Context);
set_response({error, db_not_reachable}, _, Context) ->
    crossbar_util:response_datastore_timeout(Context);
set_response({error, not_found}, Number, Context) ->
    crossbar_util:response_bad_identifier(Number, Context);
set_response({ok, Doc}, _, Context) ->
    crossbar_util:response(Doc, Context);
set_response(_Else, _, Context) ->
    crossbar_util:response_db_fatal(Context).
