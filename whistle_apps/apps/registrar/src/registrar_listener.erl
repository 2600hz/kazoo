%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Listener for authn_req, reg_success, and reg_query AMQP requests
%%% @end
%%% Created : 13 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(registrar_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0, lookup/3, lookup/2, stop/1]).

%% gen_listener callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("reg.hrl").

-define(RESPONDERS, [
		     {authn_req, [{<<"directory">>, <<"authn_req">>}]}
		     ,{reg_success, [{<<"directory">>, <<"reg_success">>}]}
		     ,{reg_query, [{<<"directory">>, <<"reg_query">>}]}
		    ]).
-define(BINDINGS, [
		   {authentication, []}
		   ,{registrations, []}
		  ]).

-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<"registrar.queue">>).
-define(REG_QUEUE_OPTIONS, [{exclusive, false}]).
-define(REG_CONSUME_OPTIONS, [{exclusive, false}]).

-record(state, {
	   amqp_q = <<>> :: binary()
	  ,cache = undefined :: undefined | pid()
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
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
				      ,{bindings, ?BINDINGS}
				      ,{queue_name, ?REG_QUEUE_NAME}
				      ,{queue_options, ?REG_QUEUE_OPTIONS}
				      ,{consume_options, ?REG_CONSUME_OPTIONS}
				     ], []).

-spec lookup/2 :: (Srv, Realm) -> {'ok', json_objects()} | {'error', 'not_found'} when
      Srv :: pid() | atom(),
      Realm :: binary().
-spec lookup/3 :: (Srv, Realm, Username) -> {'ok', json_object()} | {'error', 'not_found'} when
      Srv :: pid() | atom(),
      Realm :: binary(),
      Username :: binary().
lookup(Srv, Realm) when is_pid(Srv) orelse is_atom(Srv) ->
    gen_listener:call(Srv, {lookup, Realm}).
lookup(Srv, Realm, Username) when is_pid(Srv) orelse is_atom(Srv) ->
    gen_listener:call(Srv, {lookup, Realm, Username}).

stop(Srv) ->
    gen_listener:stop(Srv).

%%%===================================================================
%%% gen_listener callbacks
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
init([]) ->
    process_flag(trap_exit, true),
    ?LOG_SYS("starting new registrar server"),
    ?LOG_SYS("ensuring database ~s exists", [?REG_DB]),
    couch_mgr:db_create(?REG_DB),
    ?LOG_SYS("ensuring database ~s exists", [?AUTH_DB]),
    couch_mgr:db_create(?AUTH_DB),
    lists:foreach(fun({DB, File}) ->
                          ?LOG_SYS("ensuring database ~s has view ~s", [DB, File]),
			  try
			      {ok, _} = couch_mgr:update_doc_from_file(DB, registrar, File)
			  catch
			      _:_ ->
				  couch_mgr:load_doc_from_file(DB, registrar, File)
			  end
		  end, ?JSON_FILES),
    {ok, #state{}, 0}.

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
handle_call({lookup, Realm}, From, #state{cache=Cache}=State) ->
    spawn(fun() -> gen_listener:reply(From, reg_util:lookup_registrations(Realm, Cache)) end),
    {noreply, State};
handle_call({lookup, Realm, Username}, From, #state{cache=Cache}=State) ->
    spawn(fun() -> gen_listener:reply(From, reg_util:lookup_registration(Realm, Username, Cache)) end),
    {noreply, State}.

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
handle_info(timeout, #state{cache=undefined}=State) ->
    ?LOG("Looking up reg_cache"),
    case whereis(reg_cache) of
        Pid when is_pid(Pid) ->
            _ = prime_cache(Pid),
            erlang:monitor(process, Pid),
            {noreply, State#state{cache=Pid}};
        _ ->
            ?LOG_SYS("could not locate cache, trying again in 1000 msec"),
            {noreply, State, 1000}
    end;

handle_info({'DOWN', MRef, process, Cache, _Reason}, #state{cache=Cache}=State) ->
    ?LOG_SYS("registrar cache process went down, attempting to reconnect"),
    erlang:demonitor(MRef),
    {noreply, State#state{cache=undefined}, 50};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{cache=Cache}) ->
    {reply, [{cache, Cache}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate/2 :: (term(), #state{}) -> ok.
terminate(_Reason, _) ->
    ?LOG_SYS("registrar server ~p termination", [_Reason]).

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

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% load the registrar cache with the contents from the registrar db
%% @end
%%-----------------------------------------------------------------------------
-spec prime_cache/1 :: (Pid) -> ok when
      Pid :: pid().
prime_cache(Pid) when is_pid(Pid) ->
    ?LOG_SYS("priming registrar cache"),
    {ok, Docs} = couch_mgr:all_docs(?REG_DB),
    _ = [ reg_util:prime_cache(Pid, View) || View <- Docs, binary:match(wh_json:get_value(<<"id">>, View), <<"_design/">>) =:= nomatch ],
    ok.
