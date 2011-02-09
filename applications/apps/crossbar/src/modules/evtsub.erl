%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% Account module
%%%
%%% Handle client requests for account documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(evtsub).

-behaviour(gen_server).

%% API
-export([start_link/0, get_subscriber_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).

%% {SessionId, PidToQueueHandlingProc}
-type evtsub_subscriber() :: tuple( binary(), pid()).
-type evtsub_subscriber_list() :: list(evtsub_subscriber()) | [].

-record(state, {
	  subscriber_list = [] :: evtsub_subscriber_list()
	 }).

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

-spec(get_subscriber_pid/1 :: (SessionID :: binary()) -> tuple(ok, pid()) | tuple(error, undefined)).
get_subscriber_pid(SessionID) ->
    gen_server:call(?SERVER, {get_subscriber_pid, SessionID}).

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
-spec(init/1 :: (_) -> tuple(ok, #state{})).
init([]) ->
    bind_to_crossbar(),
    {ok, #state{}}.

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
handle_call({get_subscriber_pid, SessionId}, _, #state{subscriber_list=Ss}=State) ->
    Resp = case props:get_value(SessionId, Ss) of
	       undefined -> {error, undefined};
	       Pid when is_pid(Pid) -> {ok, Pid}
	   end,
    {reply, Resp, State};
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
    io:format("Unhandled ~p", [_Msg]),
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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.evtsub">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.evtsub">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.evtsub">>, [RD, Context | Params]}, State) ->
    format_log(info, "Context for Validate: ~p~n~p~n", [Context, Params]),
    spawn(fun() ->
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.evtsub">>, [RD, Context | Params]}, State) ->
    format_log(info, "GET evtsub: Context ~p Params: ~p~n", [Context, Params]),
    spawn(fun() ->
		  Pid ! {binding_result, true, [RD, Context, Params]}
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.evtsub">>, [RD, Context | Params]}, State) ->
    format_log(info, "POST evtsub: Context ~p Params: ~p~n", [Context, Params]),
    spawn(fun() ->
		  Pid ! {binding_result, [RD, Context, Params]}
     	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.evtsub">>, [RD, Context | Params]}, State) ->
    format_log(info, "PUT evtsub: Context ~p Params: ~p~n", [Context, Params]),
    spawn(fun() ->
		  Pid ! {binding_result, true, [RD, Context, Params]}
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.evtsub">>, [RD, Context | Params]}, State) ->
    format_log(info, "DELETE evtsub: Context ~p Params: ~p~n", [Context, Params]),
    spawn(fun() ->
                  Pid ! {binding_result, true, [RD, Context, Params]}
    	  end),
    {noreply, State};

handle_info({binding_fired, Pid, _Route, Payload}, State) ->
    %%format_log(info, "ACCOUNTS(~p): unhandled binding: ~p~n", [self(), Route]),
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info(_Info, State) ->
    format_log(info, "ACCOUNTS(~p): unhandled info ~p~n", [self(), _Info]),
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
-spec(bind_to_crossbar/0 :: () ->  ok | tuple(error, exists)).
bind_to_crossbar() ->
    crossbar_bindings:bind(<<"v1_resource.allowed_methods.evtsub">>),
    crossbar_bindings:bind(<<"v1_resource.resource_exists.evtsub">>),
    crossbar_bindings:bind(<<"v1_resource.validate.evtsub">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.evtsub">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Paths contains the tokenized portions of the URI after the module
%% /account/{AID}/evtsub => Paths == []
%% /account/{AID}/evtsub/{CallID} => Paths = [<<CallID>>]
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), http_methods())).
allowed_methods([]) ->
    {true, ['GET', 'PUT', 'POST', 'DELETE']};
allowed_methods([_CallID]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods(_Paths) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Paths contains the tokenized portions of the URI after the module
%% /account/{AID}/evtsub => Paths == []
%% /account/{AID}/evtsub/{CallID} => Paths = [<<CallID>>]
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec(resource_exists/1 :: (Paths :: list()) -> tuple(boolean(), [])).
resource_exists([]) ->
    {true, []};
resource_exists([_CallID]) ->
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
validate([], #cb_context{req_verb = <<"get">>, session=Session}=Context) ->
    %% get list of subscriptions and events, if any
    {ok, _SubPid} = ?MODULE:get_subscriber_pid(Session#session.'_id'),
    Context;
validate([], #cb_context{req_verb = <<"put">>, session=Session}=Context) ->
    %% create a queue bound to a call-id's event stream
    Context;
validate([], #cb_context{req_verb = <<"delete">>, session=Session}=Context) ->
    %% remove all subscriptions
    Context;
validate([CallID], #cb_context{req_verb = <<"get">>}=Context) ->
    %% get events associated with CallID, if any
    Context;
validate([CallID], #cb_context{req_verb = <<"post">>}=Context) ->
    %% get events associated with CallID, if any
    Context;
validate([CallID], #cb_context{req_verb = <<"delete">>}=Context) ->
    %% get events associated with CallID, if any
    Context;
validate(Params, #cb_context{req_verb=Verb, req_nouns=Nouns}=Context) ->
    format_log(info, "EvtSub.validate: P: ~p~nV: ~s Ns: ~p~n", [Params, Verb, Nouns]),
    crossbar_util:response_faulty_request(Context).
