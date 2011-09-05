%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Subscribe to AMQP events on behalf of user
%%%
%%% Subscribing
%%% 1. Request comes in from an account's user to start/stop subscribing to events
%%%    a. spawn process to save/delete the subscription from the user's event sub doc in Couch
%%% 2. cb_events asks cb_events_sup for the user's event server
%%%    a. if no server exists (and a sub is requested, start the server)
%%% 3. cb_events uses the returned PID to call cb_events_srv:sub/unsub with requested subs
%%%    a. if unsubbing the last subscription, cb_events_srv stops running
%%%
%%% Polling
%%% 1. Same as (1) above
%%% 2. Same as (2), except an error is returned if no server exists
%%% 3. cb_events uses the returned PID to call cb_events_srv:fetch to retrieve all events
%%%
%%% REST API
%%%
%%% /v1/accounts/{AID}/events
%%% - GET => fetches all events and overflow flag: RESP: {"events": [{},...], "overflow":boolean()}
%%% - POST => {"max_events": integer()} sets the max events to store
%%% - DELETE => removes all subscriptions
%%%
%%% /v1/accounts/{AID}/events/available
%%% - GET => returns all known subscriptions: RESP: {"subscriptions": ["authentication", "cdrs",...]}
%%%
%%% /v1/accounts/{AID}/events/subscriptions
%%% - GET => returns the list of subscriptions currently subscribed
%%%
%%% /v1/accounts/{AID}/events/{SUBSCRIPTION}
%%% - PUT => adds the subscription
%%% - DELETE => removes the subscription
%%%
%%% @end
%%% Created : 24 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_events).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_USER, <<"events_user">>).
-define(EVENT_DOC_ID(User), <<"event_sub_", User/binary>>).

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.events">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.events">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.events">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  Context1 = validate(Params, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.events">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:ensure_saved(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.events">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:ensure_saved(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.events">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:delete(Context),
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
-spec bind_to_crossbar/0 :: () ->  'ok'.
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.events">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.events">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.events">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.events">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (Paths) -> {boolean(), http_methods()} when
      Paths :: list().
allowed_methods([]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([<<"available">>]) ->
    {true, ['GET']};
allowed_methods([<<"subscriptions">>]) ->
    {true, ['GET']};
allowed_methods([_]) ->
    {true, ['PUT', 'DELETE']};
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
-spec resource_exists/1 :: (Paths) -> {boolean(), []} when
      Paths :: list().
resource_exists([]) ->
    {true, []};
resource_exists([<<"available">>]) ->
    {true, []};
resource_exists([<<"subscriptions">>]) ->
    {true, []};
resource_exists([Sub]) ->
    {lists:member(wh_util:to_atom(Sub), queue_bindings:known_bind_types()), []};
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
validate([], #cb_context{req_verb = <<"get">>, account_id=AcctId, auth_doc=AuthDoc}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
	{ok, Pid} when is_pid(Pid) ->
	    load_events(Context, Pid);
	_ ->
	    crossbar_util:response_faulty_request(Context)
    end;

validate([], #cb_context{req_verb = <<"post">>, account_id=AcctId, auth_doc=AuthDoc}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
	{ok, Pid} when is_pid(Pid) ->
	    update_srv(Context, Pid, UserId);
	_ ->
	    crossbar_util:response_faulty_request(Context)
    end;

validate([], #cb_context{req_verb = <<"delete">>, account_id=AcctId, auth_doc=AuthDoc}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
	{ok, Pid} when is_pid(Pid) ->
	    stop_srv(Context, Pid, UserId);
	_ ->
	    crossbar_util:response_faulty_request(Context)
    end;

validate([<<"available">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_available_bindings(Context);

validate([<<"subscriptions">>], #cb_context{req_verb = <<"get">>, account_id=AcctId, auth_doc=AuthDoc}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
	{ok, Pid} when is_pid(Pid) ->
	    load_current_subscriptions(Context, Pid);
	_ ->
	    crossbar_util:response_faulty_request(Context)
    end;

validate([Subscription], #cb_context{req_verb = <<"put">>, auth_doc=AuthDoc, account_id=AcctId}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
	{ok, Pid} when is_pid(Pid) ->
	    add_subscription(Context, Pid, Subscription, UserId);
	_ ->
	    crossbar_util:response_faulty_request(Context)
    end;

validate([Subscription], #cb_context{req_verb = <<"delete">>, auth_doc=AuthDoc, account_id=AcctId}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
	{ok, Pid} when is_pid(Pid) ->
	    rm_subscription(Context, Pid, Subscription, UserId);
	_ ->
	    crossbar_util:response_faulty_request(Context)
    end.

load_events(Context, Srv) ->
    {Events, Overflow} = cb_events_srv:fetch(Srv),
    RespJObj = {struct, [{<<"events">>, Events}, {<<"overflow">>, Overflow}]},

    crossbar_util:response(RespJObj, Context).

update_srv(#cb_context{req_data=ReqJObj}=Context, Srv, User) ->
    MaxEvents = wh_json:get_integer_value(<<"max_events">>, ReqJObj),
    {ok, EventsDropped} = cb_events_srv:set_maxevents(Srv, MaxEvents),

    #cb_context{resp_status=success} = crossbar_doc:load(?EVENT_DOC_ID(User), Context),

    RespJObj = wh_json:set_value(<<"events_dropped">>, EventsDropped, ?EMPTY_JSON_OBJECT),

    crossbar_util:response(RespJObj, Context).

stop_srv(Context, Srv, User) ->
    ok = cb_events_srv:stop(Srv),
    crossbar_doc:load(?EVENT_DOC_ID(User), Context).

load_available_bindings(Context) ->
    RespJObj = {struct, [{<<"available_bindings">>, queue_bindings:known_bind_types()}]},
    crossbar_util:response(RespJObj, Context).

load_current_subscriptions(Context, Srv) ->
    {ok, Subs} = cb_events_srv:subscriptions(Srv),
    RespJObj = wh_json:set_value(<<"current_subscriptions">>, Subs, ?EMPTY_JSON_OBJECT),
    crossbar_util:response(RespJObj, Context).

add_subscription(#cb_context{req_data=ReqJObj}=Context, Srv, Sub, User) ->
    case cb_events_srv:subscribe(Srv, Sub, wh_json:get_value(<<"options">>, ReqJObj, [])) of
	{error, unknown} ->
	    crossbar_util:response_faulty_request(Context);
	{error, already_present} ->
	    crossbar_util:response(?EMPTY_JSON_OBJECT, Context);
	ok ->
	    save_latest(Context, Srv, User)
    end.

rm_subscription(Context, Srv, Sub, User) ->
    cb_events_srv:unsubscribe(Srv, Sub),
    save_latest(Context, Srv, User).

save_latest(Context, Srv, User) ->
    {ok, Subs} = cb_events_srv:subscriptions(Srv),
    Doc = wh_json:set_value(<<"subscriptions">>, Subs, ?EMPTY_JSON_OBJECT),
    crossbar_doc:load_merge(?EVENT_DOC_ID(User), Doc, Context).
