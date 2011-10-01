%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Directory lookups from FS
%%% @end
%%% Created : 29 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_auth).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, lookup_user/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FS_TIMEOUT, 5000).

-include("ecallmgr.hrl").

-record(state, {
	  node = undefined :: atom()
          ,stats = #handler_stats{} :: #handler_stats{}
          ,lookups = [] :: [{pid(), binary(), {integer(), integer(), integer()}},...] | []
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
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

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
init([Node, _Options]) ->
    ?LOG_SYS("starting new fs auth listener for ~s", [Node]),
    process_flag(trap_exit, true),
    Stats = #handler_stats{started=erlang:now()},
    {ok, #state{node=Node, stats=Stats}, 0}.

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
handle_info({fetch, directory, <<"domain">>, <<"name">>, _Value, ID, [undefined | Data]}, #state{node=Node, stats=Stats, lookups=LUs}=State) ->
    ?LOG_START(ID, "received fetch request for domain parameters (user creds) from ~s", [Node]),
    case {props:get_value(<<"Event-Name">>, Data), props:get_value(<<"action">>, Data)} of
	{<<"REQUEST_PARAMS">>, <<"sip_auth">>} ->
	    {ok, LookupPid} = ecallmgr_fs_auth_sup:start_req(Node, ID, Data),
	    erlang:monitor(process, LookupPid),

	    LookupsReq = Stats#handler_stats.lookups_requested + 1,
	    {noreply, State#state{lookups=[{LookupPid, ID, erlang:now()} | LUs], stats=Stats#handler_stats{lookups_requested=LookupsReq}}, hibernate};
	_Other ->
	    _ = freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	    ?LOG_END("ignoring request for ~p", [Node, _Other]),
	    {noreply, State}
    end;

handle_info({fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]}, #state{node=Node}=State) ->
    ?LOG_START(ID, "received fetch request for ~s ~s from ~s", [ _Section, _Something, Node]),
    _ = freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
    ?LOG_END("ignoring request from for ~s", [Node, props:get_value(<<"Event-Name">>, _Data)]),
    {noreply, State};

handle_info({nodedown, Node}, #state{node=Node}=State) ->
    ?LOG_SYS("lost connection to node ~s, waiting for reconnection", [Node]),
    freeswitch:close(Node),
    _Ref = erlang:send_after(0, self(), {is_node_up, 100}),
    {noreply, State};

handle_info({is_node_up, Timeout}, State) when Timeout > ?FS_TIMEOUT ->
    handle_info({is_node_up, ?FS_TIMEOUT}, State);
handle_info({is_node_up, Timeout}, #state{node=Node}=State) ->
    case ecallmgr_fs_handler:is_node_up(Node) of
	true ->
	    ?LOG_SYS("node ~s recovered, rebinding", [Node]),
	    {noreply, State, 0};
	false ->
	    ?LOG_SYS("node ~s still down, retrying in ~b ms", [Node, Timeout]),
	    _Ref = erlang:send_after(Timeout, self(), {is_node_up, Timeout*2}),
	    {noreply, State}
    end;

handle_info(shutdown, #state{node=Node, lookups=LUs}=State) ->
    lists:foreach(fun({Pid, _CallID, _StartTime}) ->
			  case erlang:is_process_alive(Pid) of
			      true -> Pid ! shutdown;
			      false -> ok
			  end
		  end, LUs),
    freeswitch:close(Node),
    ?LOG_SYS("asked to shut down for node ~s", [Node]),
    {stop, normal, State};

handle_info({diagnostics, Pid}, #state{lookups=LUs, stats=Stats}=State) ->
    ActiveLUs = [ [{fs_auth_id, ID}, {started, Started}] || {_, ID, Started} <- LUs ],
    Resp = [{active_lookups, ActiveLUs}
	    | ecallmgr_diagnostics:get_diagnostics(Stats)
	   ],
    Pid ! Resp,
    {noreply, State};

handle_info({'DOWN', _Ref, process, LU, _Reason}, #state{lookups=LUs}=State) ->
    {noreply, State#state{lookups=lists:keydelete(LU, 1, LUs)}, hibernate};

handle_info({'EXIT', LU, _Reason}, #state{node=Node, lookups=LUs}=State) ->
    ?LOG_SYS("lookup ~w for node ~s exited unexpectedly", [LU, Node]),
    {noreply, State#state{lookups=lists:keydelete(LU, 1, LUs)}, hibernate};

handle_info(timeout, #state{node=Node}=State) ->
    Type = {bind, directory},
    erlang:monitor_node(Node, true),
    {foo, Node} ! Type,
    receive
	ok ->
	    ?LOG_SYS("bound to directory request on ~s", [Node]),
	    {noreply, State};
	{error, Reason} ->
	    ?LOG_SYS("failed to bind to directory requests on ~s, ~p", [Node, Reason]),
	    {stop, Reason, State}
    after ?FS_TIMEOUT ->
	    ?LOG_SYS("timed out binding to directory requests on ~s", [Node]),
	    {stop, timeout, State}
    end;

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
    ?LOG_SYS("fs auth ~p termination", [_Reason]),
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
-spec lookup_user/3 :: (Node, ID, Data) -> {ok, pid()} when
      Node :: atom(),
      ID :: binary(),
      Data :: proplist().
lookup_user(Node, ID, Data) ->
    Pid = spawn_link(fun() ->
                             put(callid, ID),

			     %% build req for rabbit
			     AuthRealm = props:get_value(<<"domain">>, Data, props:get_value(<<"Auth-Realm">>, Data)),
			     AuthUser = props:get_value(<<"user">>, Data, props:get_value(<<"Auth-User">>, Data)),

			     AuthReq = [{<<"Msg-ID">>, ID}
					,{<<"To">>, ecallmgr_util:get_sip_to(Data)}
					,{<<"From">>, ecallmgr_util:get_sip_from(Data)}
					,{<<"Orig-IP">>, ecallmgr_util:get_orig_ip(Data)}
                                        ,{<<"Method">>, props:get_value(<<"sip_auth_method">>, Data)}
					,{<<"Auth-User">>, AuthUser}
					,{<<"Auth-Realm">>, AuthRealm}
					| wh_api:default_headers(<<>>, <<"directory">>, <<"authn_req">>, ?APP_NAME, ?APP_VERSION)],

			     ?LOG(ID, "looking up credentials of ~s@~s for a ~s", [AuthUser, AuthRealm, props:get_value(<<"Method">>, AuthReq)]),

                             try
                                 {ok, AuthResp} = ecallmgr_amqp_pool:authn_req(AuthReq),

                                 true = wh_api:authn_resp_v(AuthResp),
                                 ?LOG(ID, "received authn_resp", []),
                                 {ok, Xml} = ecallmgr_fs_xml:authn_resp_xml(
					       wh_json:set_value(<<"Auth-Realm">>, AuthRealm
								 ,wh_json:set_value(<<"Auth-User">>, AuthUser, AuthResp))
					      ),
                                 ?LOG_END(ID, "sending XML to ~w: ~s", [Node, Xml]),
                                 freeswitch:fetch_reply(Node, ID, Xml)
                             catch
                                 throw:_T ->
                                     ?LOG("auth request lookup failed: thrown ~w", [_T]);
				 error:_E ->
				     ?LOG("auth request lookup failed: error ~w", [_E])
                             end
		     end),
    {ok, Pid}.
