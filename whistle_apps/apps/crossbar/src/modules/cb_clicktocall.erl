%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @author Eduoard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Click to call
%%%
%%% Allow embeddable HTML forms (or other ways to POST to the URL)
%%% and create a call.
%%%
%%% @end
%%% Created : 09 May 2011 by Karl Anderson <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_clicktocall).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(CONNECT_CALL, <<"connect">>).
-define(HISTORY, <<"history">>).
-define(CB_LIST, <<"click2call/crossbar_listing">>).
-define(PVT_TYPE, <<"click2call">>).
-define(CONNECT_C2C_URL, [{<<"clicktocall">>, [_, <<"connect">>]}, {<<"accounts">>, [_]}]).

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
    {reply, ok, State}.

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.clicktocall">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.clicktocall">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  Pid ! {binding_result, true, [RD, Context, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = crossbar_doc:save(Context),
     		  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns = ?CONNECT_C2C_URL, req_verb = <<"post">>, req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{req_nouns = ?CONNECT_C2C_URL, req_verb = <<"post">>, req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, _B, Payload}, State) ->
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
-spec(bind_to_crossbar/0 :: () ->  no_return()).
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.clicktocall">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.clicktocall">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.clicktocall">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.clicktocall">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), http_methods())).
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_,?CONNECT_CALL]) ->
    {true, ['POST']};
allowed_methods([_,?HISTORY]) ->
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
-spec(resource_exists/1 :: (Paths :: list()) -> tuple(boolean(), [])).
resource_exists([]) ->
    {true, []};
resource_exists([_]) ->
    {true, []};
resource_exists([_, ?CONNECT_CALL]) ->
    {true, []};
resource_exists([_, ?HISTORY]) ->
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
-spec(validate/2 :: (Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    load_c2c_summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create_c2c(Context);

validate([C2CId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_c2c(C2CId, Context);
validate([C2CId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_c2c(C2CId, Context);
validate([C2CId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_c2c(C2CId, Context);

validate([C2CId, ?HISTORY], #cb_context{req_verb = <<"get">>}=Context) ->
    load_c2c_history(C2CId, Context);
validate([C2CId, ?CONNECT_CALL], #cb_context{req_verb = <<"post">>}=Context) ->
    establish_c2c(C2CId, Context);

validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTICE: This is very temporary, placeholder until the schema work is
%% complete!
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_doc/1 :: (JObj :: json_object()) -> tuple(boolean(), list(binary()) | [])).
is_valid_doc(JObj) ->
    case lists:any(fun(undefined) -> true; (_) -> false end, [wh_json:get_value(<<"name">>, JObj)
							      ,wh_json:get_value(<<"extension">>, JObj)]) of
	true -> {false, [<<"name">>, <<"extension">>]};
	_ -> {true, []}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results/2 :: (JObj :: json_object(), Acc :: json_objects()) -> json_objects()).
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].


load_c2c_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

load_c2c(C2CId, Context) ->
    #cb_context{doc=C2C}=Context1 = crossbar_doc:load(C2CId, Context),
    C2C1 = wh_json:set_value(<<"history_items">>, length(wh_json:get_value(<<"history">>, C2C)), wh_json:delete_key(<<"history">>, C2C)),
    Context1#cb_context{doc=C2C1}.

load_c2c_history(C2CId, Context) ->
    #cb_context{doc=C2C}=Context1 = crossbar_doc:load(C2CId, Context),
    Context1#cb_context{doc=wh_json:get_value(<<"history">>, C2C)}.

update_c2c(C2CId, #cb_context{req_data=Doc}=Context) ->
    crossbar_doc:load_merge(C2CId, Doc, Context).

establish_c2c(C2CId, #cb_context{req_data=Req, account_id=AccountId}=Context) ->
    #cb_context{doc=C2C}=Context1 = crossbar_doc:load(C2CId, Context),

    Caller = wh_util:to_e164(wh_json:get_value(<<"contact">>, Req)),
    Callee = wh_util:to_e164(wh_json:get_value(<<"extension">>, C2C)),
    C2CName = wh_json:get_value(<<"name">>, C2C),

    Status = originate_call(Caller, Callee, C2CName, AccountId),

    case Status of
	{success, [CallID, CdrID]} ->
	    History = wh_json:get_value(<<"history">>, C2C, []),
	    List = [create_c2c_history_item(Req, CallID, CdrID) | History],
	    Context1#cb_context{doc=wh_json:set_value(<<"history">>, List, C2C)};
	{error, _} -> Context1#cb_context{resp_status=error};
	{timeout, _} -> Context1#cb_context{resp_status=error}
    end.

create_c2c(#cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
	{true, _} ->
            Context#cb_context{
	      doc=wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, wh_json:set_value(<<"history">>, [], JObj))
	      ,resp_status=success
	     }
    end.

create_c2c_history_item(Req, CallID, CdrID) ->
    Now = wh_util:current_tstamp(),
    {struct, [ {<<"contact">>, wh_json:get_value(<<"contact">>, Req)},
	       {<<"timestamp">>, Now},
	       {<<"call_id">>, CallID},
	       {<<"cdr_id">>, CdrID}
	     ]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
originate_call(CallerNumber, CalleeExtension, C2CName, AccountId) ->
    ?LOG("attempting clicktocall ~s from ~s to ~s in account ~s", [C2CName, CallerNumber, CalleeExtension, AccountId]),

    %% create, bind & consume amqp queue
    Amqp = amqp_util:new_queue(),
    amqp_util:bind_q_to_targeted(Amqp),
    amqp_util:basic_consume(Amqp),

    %basic call info to display on phone's screen
    CallInfo = << ",origination_caller_id_name=", CalleeExtension/binary,
		  ",origination_caller_id_number=", CalleeExtension/binary,
		  ",origination_callee_id_name='", C2CName/binary, "'",
		  ",origination_callee_id_number=", CallerNumber/binary >>,

    OrigStringPart = <<"{ecallmgr_Account-ID=",AccountId/binary, CallInfo/binary, "}loopback/", CallerNumber/binary, "/context_2">>,

    JObjReq = [
	       {<<"Msg-ID">>, wh_util:current_tstamp()}
               ,{<<"Resource-Type">>, <<"audio">>}
               ,{<<"Invite-Format">>, <<"route">>}
	       ,{<<"Route">>, OrigStringPart}
	       ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Account-ID">>, AccountId}] } }
               ,{<<"Application-Name">>, <<"transfer">>}
	       ,{<<"Application-Data">>, {struct, [{<<"Route">>, CalleeExtension}]} }
               | wh_api:default_headers(Amqp, <<"resource">>, <<"originate_req">>, <<"clicktocall">>, <<"0.1">>)
              ],

    {ok, Json} = wh_api:resource_req({struct, JObjReq}),
    amqp_util:callmgr_publish(Json, <<"application/json">>, ?KEY_RESOURCE_REQ),

    receive
	{_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
	    JObj = mochijson2:decode(Payload),
	    case wh_json:get_value(<<"Event-Name">>, JObj) of
		<<"originate_resp">> ->
                    whapps_util:put_callid(JObj),
                    ?LOG("established clicktocall ~s from ~s to ~s in account ~s", [C2CName, CallerNumber, CalleeExtension, AccountId]),
		    {success, [wh_json:get_value(<<"Call-ID">>, JObj), null]};
		<<"resource_error">> ->
                    ?LOG("cannot establish click to call, ~s", [wh_json:get_value(<<"Failure-Message">>, JObj, <<>>)]),
		    {error, []}
	    end
    after
	15000 ->
            ?LOG("cannot establish click to call, timeout"),
	    {timeout, []}
    end.
