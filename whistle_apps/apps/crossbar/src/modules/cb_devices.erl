%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Devices module
%%%
%%% Handle client requests for device documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_devices).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(SERVER, ?MODULE).

-define(DEVICES_LIST, <<"devices/listing_by_id">>).
-define(FIXTURE_LIST, [<<"611.device.json">>]). %% fixtures to load into each account DB

-define(CB_LIST, <<"devices/crossbar_listing">>).
-define(SIP_LIST, <<"devices/sip_credentials">>).

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.devices">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.devices">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.devices">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, RD, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.devices">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:save(Context) of
                      #cb_context{resp_status=success, doc=Doc1}=Context1 ->
                          DeviceId = wh_json:get_value(<<"_id">>, Doc1),
                          provision(Doc1, whapps_config:get_list(<<"crossbar.devices">>, <<"provisioning_url">>)),
                          case couch_mgr:lookup_doc_rev(?SIP_AGG_DB, DeviceId) of
                              {ok, Rev} ->
                                  couch_mgr:ensure_saved(?SIP_AGG_DB, wh_json:set_value(<<"_rev">>, Rev, Doc1)),
                                  Pid ! {binding_result, true, [RD, Context1, Params]};
                              {error, not_found} ->
                                  couch_mgr:ensure_saved(?SIP_AGG_DB, wh_json:delete_key(<<"_rev">>, Doc1)),
                                  Pid ! {binding_result, true, [RD, Context1, Params]}
                          end;
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.devices">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:save(Context) of
                      #cb_context{resp_status=success, doc=Doc1}=Context1 ->
                          provision(Doc1, whapps_config:get_list(<<"crossbar.devices">>, <<"provisioning_url">>)),
                          couch_mgr:ensure_saved(?SIP_AGG_DB, wh_json:delete_key(<<"_rev">>, Doc1)),
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.devices">>, [RD, #cb_context{doc=Doc}=Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:delete(Context) of
                      #cb_context{resp_status=success}=Context1 ->
                          DeviceId = wh_json:get_value(<<"_id">>, Doc),
                          case couch_mgr:lookup_doc_rev(?SIP_AGG_DB, DeviceId) of
                              {ok, Rev} ->
                                  couch_mgr:del_doc(?SIP_AGG_DB, wh_json:set_value(<<"_rev">>, Rev, Doc));
                              {error, not_found} ->
                                  ok
                          end,
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
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
-spec(bind_to_crossbar/0 :: () -> no_return()).
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.devices">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.devices">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.devices">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.devices">>).

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
allowed_methods([<<"status">>]) ->
    {true, ['GET']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
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
-spec(validate/3 :: (Params :: list(), RD :: #wm_reqdata{}, Context :: #cb_context{}) -> #cb_context{}).
validate([], _, #cb_context{req_verb = <<"get">>, req_json=RJ}=Context) ->
    load_device_summary(Context, RJ);
validate([], _, #cb_context{req_verb = <<"put">>}=Context) ->
    create_device(Context);
validate([<<"status">>], _, #cb_context{req_verb = <<"get">>}=Context) ->
    load_device_status(Context);
validate([DocId], _, #cb_context{req_verb = <<"get">>}=Context) ->
    load_device(DocId, Context);
validate([DocId], _, #cb_context{req_verb = <<"post">>}=Context) ->
    update_device(DocId, Context);
validate([DocId], _, #cb_context{req_verb = <<"delete">>}=Context) ->
    load_device(DocId, Context);
validate(_, _, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_device_summary/2 :: (#cb_context{}, json_object()) -> #cb_context{}.
load_device_summary(Context, ?EMPTY_JSON_OBJECT) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
load_device_summary(#cb_context{db_name=DbName}=Context, QueryParams) ->
    Result = crossbar_filter:filter_on_query_string(DbName, ?CB_LIST, wh_json:to_proplist(QueryParams)),
    Context#cb_context{resp_data=Result, resp_status=success}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new device document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_device/1 :: (#cb_context{}) -> #cb_context{}.
create_device(#cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {error, Fields} ->
	    crossbar_util:response_invalid_data(Fields, Context);
        {ok, _} ->
            Context#cb_context{
                 doc=wh_json:set_value(<<"pvt_type">>, <<"device">>, JObj)
                ,resp_status=success
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a device document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_device/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_device(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing device document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec(update_device/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_device(DocId, #cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {error, Fields} ->
	    crossbar_util:response_invalid_data(Fields, Context);
        {ok, _} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve the status of the devices linked to the account
%% Reads registered devices in registrations, then map to devices of the account
%% @end
%%--------------------------------------------------------------------
-spec load_device_status/1 :: (Context) -> #cb_context{} when
      Context :: #cb_context{}.
load_device_status(#cb_context{db_name=Db}=Context) ->
    %% RegDevices = [[realm1, user1], [realmN, userN], ...], those are owners of currently  registered devices.
    %% RegDevices is reinjected as keys for devices/sip_credentials
    {ok, JObjs} = couch_mgr:get_results(Db, ?SIP_LIST, []),
    AccountCreds = [ list_to_tuple(wh_json:get_value(<<"key">>, JObj)) || JObj <- JObjs],

    Result = case cb_modules_util:lookup_regs(AccountCreds) of
		 [] -> [];
		 RegDevices ->
		     lists:foldl(fun(JObj, Acc) ->
					 Realm = wh_json:get_value([<<"value">>, <<"realm">>], JObj),
					 User = wh_json:get_value([<<"value">>, <<"username">>], JObj),

					 case lists:keyfind({Realm, User}, 1, RegDevices) =/= false of
					     true ->
						 [ wh_json:set_values([{<<"registered">>, true}
								       ,{<<"device_id">>, wh_json:get_value(<<"id">>, JObj)}
								      ], wh_json:get_value(<<"value">>, JObj)) | Acc];
					     false -> Acc
					 end
				 end, [], JObjs)
	     end,
    crossbar_util:response(Result, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results/2 :: (JObj :: json_object(), Acc :: json_objects()) -> json_objects()).
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates JObj against their schema
%% @end
%%--------------------------------------------------------------------
-spec is_valid_doc/1 :: (JObj :: json_object()) -> tuple(error, json_objects());
			(JObj :: json_object()) -> tuple(ok, []).
is_valid_doc(JObj) ->
     crossbar_schema:do_validate(JObj, device).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Spawn the function to post data to a provisioning server, if
%% provided with URL
%% @end
%%--------------------------------------------------------------------
-spec provision/2 :: (JObj, Url) -> ok | pid() when
      JObj :: json_object(),
      Url :: undefined | binary().
provision(_, undefined) ->
    ok;
provision(JObj, Url) ->
    spawn(fun() -> do_provision(JObj, Url) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% post data to a provisiong server
%% @end
%%--------------------------------------------------------------------

-spec do_provision/2 :: (JObj, Url) -> ok when
      JObj :: json_object(),
      Url :: binary().
do_provision(JObj, Url) ->
    Headers = [{K, V}
               || {K, V} <- [{"Host", whapps_config:get_list(<<"crossbar.devices">>, <<"provisioning_host">>)}
                             ,{"Referer", whapps_config:get_list(<<"crossbar.devices">>, <<"provisioning_referer">>)}
                             ,{"User-Agent", wh_util:to_list(erlang:node())}
                             ,{"Content-Type", "application/x-www-form-urlencoded"}]
                      ,V =/= undefined],
    HTTPOptions = [],
    Body = [{"api[realm]", wh_json:get_list_value([<<"sip">>, <<"realm">>], JObj)}
            ,{"mac", re:replace(wh_json:get_list_value(<<"mac_address">>, JObj, ""), "[^0-9a-fA-F]", "", [{return, list}, global])}
            ,{"label", wh_json:get_list_value(<<"name">>, JObj)}
            ,{"sip[username]", wh_json:get_list_value([<<"sip">>, <<"username">>], JObj)}
            ,{"sip[password]", wh_json:get_list_value([<<"sip">>, <<"password">>], JObj)}
            ,{"submit", "true"}],
    Encoded = mochiweb_util:urlencode(Body),
    ?LOG("posting to ~s with ~s", [Url, Encoded]),
    ibrowse:send_req(Url, Headers, post, Encoded, HTTPOptions),
    ok.
