%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Directory lookups from FS
%%% @end
%%% Created : 29 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_auth).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(FS_TIMEOUT, 5000).
-define(VSN, <<"0.5.0">>).

-include("ecallmgr.hrl").

-record(state, {
	  node = undefined :: atom()
          ,stats = #handler_stats{} :: #handler_stats{}
          ,lookups = [] :: list(tuple(pid(), binary(), tuple(integer(), integer(), integer())))
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
handle_info(timeout, #state{node=Node}=State) ->
    Type = {bind, directory},
    erlang:monitor_node(Node, true),
    {foo, Node} ! Type,
    receive
	ok ->
	    logger:format_log(info, "Bound ~p to ~p~n", [Type, Node]),
	    {noreply, State};
	{error, Reason} ->
	    logger:format_log(info, "Failed to bind: ~p~n", [Reason]),
	    {stop, Reason, State}
    after ?FS_TIMEOUT ->
	    logger:format_log(info, "Failed to bind: TIMEOUT~n", []),
	    {stop, timeout, State}
    end;

handle_info({fetch, directory, <<"domain">>, <<"name">>, _Value, ID, [undefined | Data]}, #state{node=Node, stats=Stats, lookups=LUs}=State) ->
    case props:get_value(<<"Event-Name">>, Data) of
	<<"REQUEST_PARAMS">> ->
	    LookupPid = spawn_link(fun() -> lookup_user(Node, ID, Data) end),
	    LookupsReq = Stats#handler_stats.lookups_requested + 1,
	    logger:format_log(info, "FETCH_USER(~p): fetch directory: Id: ~p Lookup ~p (Number ~p)~n", [self(), ID, LookupPid, LookupsReq]),
	    {noreply, State#state{lookups=[{LookupPid, ID, erlang:now()} | LUs], stats=Stats#handler_stats{lookups_requested=LookupsReq}}};
	_Other ->
	    logger:format_log(info, "FETCH_USER(~p): Ignoring event ~p~n~p~n", [self(), _Other, Data]),
	    _ = freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	    {noreply, State}
    end;

handle_info({fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]}, #state{node=Node}=State) ->
    logger:format_log(info, "FETCH_USER(~p): fetch unknown: Se: ~p So: ~p, K: ~p V: ~p ID: ~p~n~p~n"
	       ,[self(), _Section, _Something, _Key, _Value, ID, _Data]),
    _ = freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
    {noreply, State};

handle_info({nodedown, Node}, #state{node=Node}=State) ->
    logger:format_log(error, "FETCH_USER(~p): Node ~p exited", [self(), Node]),
    freeswitch:close(Node),
    {ok, _} = timer:send_after(0, self(), {is_node_up, 100}),
    {noreply, State};

handle_info({is_node_up, Timeout}, State) when Timeout > ?FS_TIMEOUT ->
    handle_info({is_node_up, ?FS_TIMEOUT}, State);
handle_info({is_node_up, Timeout}, #state{node=Node}=State) ->
    case ecallmgr_fs_handler:is_node_up(Node) of
	true ->
	    logger:format_log(info, "FS_ROUTE(~p): Node ~p recovered, restarting~n", [self(), Node]),
	    {noreply, State, 0};
	false ->
	    logger:format_log(error, "FS_ROUTE(~p): Node ~p down, retrying in ~p ms~n", [self(), Node, Timeout]),
	    {ok, _} = timer:send_after(Timeout, self(), {is_node_up, Timeout*2}),
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
    logger:format_log(error, "FETCH_USER(~p): shutting down~n", [self()]),
    {stop, normal, State};

handle_info({diagnostics, Pid}, #state{lookups=LUs, stats=Stats}=State) ->
    ActiveLUs = [ [{fs_auth_id, ID}, {started, Started}] || {_, ID, Started} <- LUs ],
    Resp = [{active_lookups, ActiveLUs}
	    | ecallmgr_diagnostics:get_diagnostics(Stats)
	   ],
    Pid ! Resp,
    {noreply, State};

handle_info({'EXIT', LU, _Reason}, #state{lookups=LUs}=State) ->
    logger:format_log(info, "FS_AUTH(~p): Lookup ~p exited~n", [self(), LU]),
    {noreply, State#state{lookups=lists:keydelete(LU, 1, LUs)}};

handle_info(_Info, State) ->
    logger:format_log(info, "FS_AUTH(~p): Other recv: ~p~n", [self(), _Info]),
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
lookup_user(Node, ID, Data) ->
    %% build req for rabbit
    AuthReq = [{<<"Msg-ID">>, ID}
	       ,{<<"To">>, ecallmgr_util:get_sip_to(Data)}
	       ,{<<"From">>, ecallmgr_util:get_sip_from(Data)}
	       ,{<<"Orig-IP">>, ecallmgr_util:get_orig_ip(Data)}
	       ,{<<"Auth-User">>, props:get_value(<<"user">>, Data, props:get_value(<<"Auth-User">>, Data))}
	       ,{<<"Auth-Domain">>, props:get_value(<<"domain">>, Data, props:get_value(<<"Auth-Domain">>, Data))}
	       | whistle_api:default_headers(<<>>, <<"directory">>, <<"auth_req">>, <<"ecallmgr.auth">>, ?VSN)],
    {ok, {struct, AuthResp}} = ecallmgr_amqp_pool:auth_req(AuthReq),

    true = whistle_api:auth_resp_v(AuthResp),

    {ok, Xml} = ecallmgr_fs_xml:auth_resp_xml([{<<"Auth-User">>, props:get_value(<<"user">>, Data, props:get_value(<<"Auth-User">>, Data))}
					       ,{<<"Auth-Domain">>, props:get_value(<<"domain">>, Data, props:get_value(<<"Auth-Domain">>, Data))}
					       | AuthResp]),
    freeswitch:fetch_reply(Node, ID, Xml).
