%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow gen server for CRUD
%%%
%%% @end
%%% Created :  3 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
-module (callflows).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(VIEW_FILE, <<"views/callflows.json">>).

-define(CALLFLOWS_LIST, <<"callflows/listing_by_id">>).
-define(FIXTURE_LIST, [<<"611.callflow.json">>]).

-define(AGG_DB, <<"callflows">>).
-define(AGG_FILTER, <<"callflows/export">>).

-define(CB_LIST, {<<"callflows">>, <<"crossbar_listing">>}).


%%-----------------------------------------------------------------------------
%% PUBLIC API
%%-----------------------------------------------------------------------------
%%

%------------------------------------------------------------------------------
% @public
% @doc
% Starts the server
%
% @end
%------------------------------------------------------------------------------
-spec ( start_link/0 :: ( ) -> tuple(ok, Pid :: pid()) | ignore | tuple(error, Error :: term()) ).
start_link ( ) ->
    gen_server:start_link( {local, ?SERVER}, ?MODULE, [], [] ).

%%-----------------------------------------------------------------------------
%% GEN SERVER CALLBACKS
%%-----------------------------------------------------------------------------
%%

%------------------------------------------------------------------------------
% @private
% @doc
% Initializes the server
%
% @end
%------------------------------------------------------------------------------
init(_) ->
    {ok, ok, 0}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles call messages
%
% @end
%------------------------------------------------------------------------------
handle_call (_Request, _From, State) ->
   {reply, ok, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles cast messages
%
% @end
%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
   {noreply, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles all non call/cast messages
%
% @end
%------------------------------------------------------------------------------
handle_info ({binding_fired, Pid, <<"v1_resource.allowed_methods.callflows">>, Payload}, State) ->
    spawn(fun ( ) ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! { binding_result, Result, Payload1 }
          end),
    {noreply, State};

handle_info ({binding_fired, Pid, <<"v1_resource.resource_exists.callflows">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info ({binding_fired, Pid, <<"v1_resource.validate.callflows">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                crossbar_util:binding_heartbeat(Pid),
                Context1 = validate(Params, Context),
                Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.callflows">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.callflows">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.callflows">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"account.created">>, DBName}, State) ->
    spawn(fun() -> import_fixtures(?FIXTURE_LIST, DBName) end),
    Pid ! {binding_result, true, ?VIEW_FILE},
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    whapps_util:update_all_accounts(?VIEW_FILE),
    {noreply, State};

handle_info (_Info, State) ->
   {noreply, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Is called by a gen_server when it is about to terminate. It should be the
% opposite of Module:init/1 and do any necessary cleaning up. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
%
% @end
%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%------------------------------------------------------------------------------
% @private
% @doc
% Converts process state when code is changed
%
% @end
%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% INTERNAL API
%%-----------------------------------------------------------------------------
%%

%------------------------------------------------------------------------------
% @private
% @doc
% binds this server to the crossbar bindings server for the keys we need to
% consume.
%
% @end
%------------------------------------------------------------------------------
-spec(bind_to_crossbar/0 :: () -> no_return()).
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.callflows">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.callflows">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.callflows">>),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.#.callflows">>),
    crossbar_bindings:bind(<<"account.created">>).

%------------------------------------------------------------------------------
% @private
% @doc
% This function determines the verbs that are appropriate for the given nouns.
%
% @end
%------------------------------------------------------------------------------
-spec (allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), list(atom()) | [])).
allowed_methods([]) ->
   { true, ['PUT', 'GET'] };                    % PUT - create new callflow
                                                % GET - call flow collection
allowed_methods([_]) ->
   { true, ['GET', 'POST', 'DELETE'] };         % GET    - retrieve callflow
                                                % POST   - update callflow
                                                % DELETE - delete callflow
allowed_methods(_) ->
    { false, [] }.

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
-spec(validate/2 :: (Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    load_callflow_summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create_callflow(Context);
validate([DocId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_callflow(DocId, Context);
validate([DocId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_callflow(DocId, Context);
validate([DocId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_callflow(DocId, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec(load_callflow_summary/1 :: (Context :: #cb_context{}) -> #cb_context{}).
load_callflow_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new callflow document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec(create_callflow/1 :: (Context :: #cb_context{}) -> #cb_context{}).
create_callflow(#cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        %% {false, Fields} ->
        %%     crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            Context#cb_context{
                 doc=wh_json:set_value(<<"pvt_type">>, <<"callflow">>, JObj)
                ,resp_status=success
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a callflow document from the database
%% @end
%%--------------------------------------------------------------------
-spec(load_callflow/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_callflow(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing callflow document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec(update_callflow/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_callflow(DocId, #cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        %% {false, Fields} ->
        %%     crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_doc/1 :: (JObj :: json_object()) -> tuple(true, [])). %% tuple(boolean(), json_objects())).
is_valid_doc(_JObj) ->
    {true, []}.

-spec(import_fixtures/2 :: (Fixtures :: list(binary()), DBName :: binary()) -> list(#cb_context{} | tuple(error, no_file))).
import_fixtures(Fixtures, DBName) ->
    ?LOG_SYS("Importing fixtures into ~s", [DBName]),
    Context = #cb_context{db_name=DBName},
    [ import_fixture(Fixture, Context) || Fixture <- Fixtures].

-spec(import_fixture/2 :: (Fixture :: binary(), Context :: #cb_context{}) -> #cb_context{} | tuple(error, no_file)).
import_fixture(Fixture, #cb_context{db_name=DBName}=Context) ->
    Path = [code:priv_dir(crossbar), <<"/couchdb/fixtures/">>],
    ?LOG_SYS("Read from ~s", [[Path, Fixture]]),
    FixFile = list_to_binary([Path, Fixture]),
    case filelib:is_regular(FixFile) of
	true ->
	    {ok, FixStr} = file:read_file(FixFile),
	    ?LOG_SYS("Loaded fixture ~s", [FixStr]),
	    JObj = mochijson2:decode(FixStr),

	    DeviceFile = list_to_binary([Path, binary:replace(Fixture, <<"callflow">>, <<"device">>)]),
	    ?LOG_SYS("Trying device file ~s", [DeviceFile]),
	    JObj1 = case file:read_file(DeviceFile) of
			{ok, Device} ->
			    DevJObj = mochijson2:decode(Device),
			    ?LOG_SYS("Set device id to ~s", [wh_json:get_value(<<"_id">>, DevJObj)]),
			    wh_json:set_value([<<"flow">>, <<"data">>, <<"id">>], wh_json:get_value(<<"_id">>, DevJObj), JObj);
			{error, _E} ->
			    ?LOG_SYS("No corresponding device(~p), just update the realms", [_E]),
			    JObj
		    end,

	    {ok, Realm} = accounts:get_realm_from_db(DBName),
	    JObj2 = wh_json:set_value([<<"realms">>], [Realm], JObj1),
	    ?LOG_SYS("Set realms to [~s]", [Realm]),

	    crossbar_doc:save(Context#cb_context{doc=JObj2});
	false ->
	    ?LOG_SYS("File path doesn't point to a file"),
	    {error, no_file}
    end.
