%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
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
-module(clicktocall).

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
-define(VIEW_FILE, <<"views/c2c.json">>).
-define(PVT_TYPE, <<"click2call">>).

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
                crossbar_util:binding_heartbeat(Pid),
                Context1 = validate(Params, Context),
                Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  case Params of
		      [_C2CID, ?CONNECT_CALL] ->
			  %% start call
			  crossbar_util:binding_heartbeat(Pid),
			  %% should return a 204
			  %% spawn off a process to start the call
			  Pid ! {binding_result, true, [RD, Context#cb_context{resp_data=undefined}, Params]};
		      _ ->
			  Pid ! {binding_result, true, [RD, Context, Params]}
		  end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  case Params of
		      [C2CID, ?CONNECT_CALL] ->
			  crossbar_util:binding_heartbeat(Pid),
			  %% log call to history
			  Context1 = update_c2c_history(C2CID, Context),
			  Pid ! {binding_result, true, [RD, Context1, Params]};
		      [C2CID] ->
			  %% update c2c 
			  Context1 = update_c2c(C2CID, Context),
			  Pid ! {binding_result, true, [RD, Context1, Params]}
		   end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.clicktocall">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"account.created">>, _Payload}, State) ->    
    Pid ! {binding_result, true, ?VIEW_FILE},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>, Payload}, State) ->
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>, Payload}, State) ->
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    whapps_util:update_all_accounts(?VIEW_FILE),
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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.clicktocall">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.clicktocall">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.clicktocall">>),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.#.clicktocall">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    crossbar_bindings:bind(<<"account.created">>).

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
    update_c2c_history(C2CId, Context);

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

load_c2c_summary(Context) ->
    crossbar_doc:load_view(<<"click2call/listing_by_id">>, [], Context).

load_c2c(C2CId, Context) ->
    #cb_context{doc=C2C}=Context1 = crossbar_doc:load(C2CId, Context),
    C2C1 = wh_json:set_value(<<"history_items">>, length(wh_json:get_value(<<"history">>, C2C)), wh_json:delete_key(<<"history">>, C2C)),
    Context1#cb_context{doc=C2C1}.

load_c2c_history(C2CId, Context) ->
    #cb_context{doc=C2C}=Context1 = crossbar_doc:load(C2CId, Context),
    Context1#cb_context{doc=wh_json:get_value(<<"history">>, C2C)}.

update_c2c(C2CId, #cb_context{req_data=Doc}=Context) ->
    crossbar_doc:load_merge(C2CId, Doc, Context).

update_c2c_history(C2CId, #cb_context{req_data=Req}=Context) ->
    #cb_context{doc=C2C}=Context1 = crossbar_doc:load(C2CId, Context),
    History = wh_json:get_value(<<"history">>, C2C, []),
    Context1#cb_context{doc=wh_json:set_value(<<"history">>, [create_c2c_history_item(Req) | History], C2C)}.

create_c2c(#cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
	{true, []} ->
            Context#cb_context{
	      doc=wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, wh_json:set_value(<<"history">>, [], JObj))
	      ,resp_status=success
	     }
    end.

create_c2c_history_item(Req) ->
    Req.
