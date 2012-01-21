%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Registration viewer / creator
%%% @end
%%% Created : 15 Apr 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_registrations).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(REG_DB, <<"registrations">>).

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
    _ = setup_couch(),
    _ = bind_to_crossbar(),
    {ok, ok}.

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.registrations">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.registrations">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.registrations">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, RD, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.registrations">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
		  Pid ! {binding_result, true, [RD, Context, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.registrations">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  {Context1, Resp} = case Context#cb_context.resp_status =:= success of
					 true -> {crossbar_doc:save(Context), true};
					 false -> {Context, false}
				     end,
		  Pid ! {binding_result, Resp, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.registrations">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  Pid ! {binding_result, true, [RD, Context, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.registrations">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  Pid ! {binding_result, true, [RD, Context, Params]}
	  end),
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
-spec setup_couch/0 :: () -> 'ok'.
setup_couch() ->
    ok.

-spec bind_to_crossbar/0 :: () -> 'ok' | {'error', 'exists'}.
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.registrations">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.registrations">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.registrations">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.registrations">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Paths contains the tokenized portions of the URI after the module
%% /account/{AID}/registrations => Paths == []
%% /account/{AID}/registrations/{RegistrationsID} => Paths = [<<RegistrationsID>>]
%% /account/{AID}/registrations/{RegistrationsID}/raw => Paths = [<<"RegistrationsID">>, <<"raw">>]
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_tokens()) -> {boolean(), http_methods()}.
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_RegID]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Paths contains the tokenized portions of the URI after the module
%% /account/{AID}/registrations => Paths == []
%% /account/{AID}/registrations/{RegistrationsID} => Paths = [<<<RegistrationsID>>]
%% /account/{AID}/registrations/{RegistrationsID}/raw => Paths = [<<"RegistrationsID">>, <<"raw">>]
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/1 :: (path_tokens()) -> {boolean(), []}.
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
-spec validate/3 :: (path_tokens(), #wm_reqdata{}, #cb_context{}) -> #cb_context{}.
validate([], _, #cb_context{req_verb = <<"get">>, db_name=DbName}=Context) ->
    {ok, JObjs} = couch_mgr:get_all_results(DbName, <<"devices/sip_credentials">>),
    AccountUsers = [ list_to_tuple(wh_json:get_value(<<"key">>, JObj)) || JObj <- JObjs],

    ?LOG("CurrentRegs:"),
    CurrentRegs = [ begin ?LOG("~p", [JObj]), wh_json:normalize(JObj) end || {_, JObj} <- cb_modules_util:lookup_regs(AccountUsers)],

    crossbar_util:response(CurrentRegs, Context);

validate([], _, #cb_context{req_verb = <<"put">>, req_data=_Data}=Context) ->
    Context#cb_context{db_name=?REG_DB};

validate([RegID], _, #cb_context{req_verb = <<"get">>}=Context) ->
    crossbar_doc:load(RegID, Context#cb_context{db_name=?REG_DB});

validate([RegID], _, #cb_context{req_verb = <<"post">>, req_data=Data}=Context) ->
    crossbar_doc:load_merge(RegID, Data, Context#cb_context{db_name=?REG_DB});

validate([RegID], _, #cb_context{req_verb = <<"delete">>}=Context) ->
    crossbar_doc:delete(crossbar_doc:load(RegID, Context#cb_context{db_name=?REG_DB}));

validate(_Params, _, Context) ->
    crossbar_util:response_faulty_request(Context).
