%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 02 Nov 2011 James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_limits).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(SERVER, ?MODULE).
-define(CB_LIST, <<"limits/crossbar_listing">>).
-define(PVT_TYPE, <<"limits">>).

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.limits">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.limits">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.limits">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  _BPid = crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, RD, Context),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.limits">>, [RD, Context | Params]}, State) ->
    Pid ! {binding_result, true, [RD, Context, Params]},
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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.limits">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.limits">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.limits">>),
    crossbar_bindings:bind(<<"v1_resource.execute.get.limits">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/limit/' can only accept GET
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), http_methods())).
allowed_methods([]) ->
    {true, ['GET']};
allowed_methods([_]) ->
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
-spec validate/3 :: ([binary(),...] | [], #wm_reqdata{}, #cb_context{}) -> #cb_context{}.
validate([], _RD, #cb_context{req_verb = <<"get">>}=Context) ->
    try
        load_limit_summary(Context)
    catch
        _T:_R ->
	    ST = erlang:get_stacktrace(),
	    ?LOG("Loading summary crashed: ~p: ~p", [_T, _R]),
	    [?LOG("~p", [S]) || S <- ST],
            crossbar_util:response_db_fatal(Context)
    end;
validate([LimitId], _, #cb_context{req_verb = <<"get">>}=Context) ->
    try
        load_limit(LimitId, Context)
    catch
        _T:_R ->
	    ?LOG("Loading limit crashed: ~p: ~p", [_T, _R]),
            crossbar_util:response_db_fatal(Context)
    end;
validate(_, _, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of Limit, each summarized.
%% @end
%%--------------------------------------------------------------------
-spec load_limit_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_limit_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [{<<"include_docs">>, true}], Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a Limit document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_limit/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_limit(LimitId, Context) ->
    crossbar_doc:load(LimitId, Context).
