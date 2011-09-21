%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handles authorizing flat-rate and per-minute calls for Crossbar accounts
%%% TODO: Convert to gen_listener
%%% @end
%%% Created :  7 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(jonny5_acct).

-behaviour(gen_server).

%% API
-export([start_link/0, get_cache_pid/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("jonny5.hrl").

-record(state, {
	  is_amqp_up = true :: boolean()
         ,cache = undefined :: undefined | pid()
         ,cache_ref = make_ref() :: reference()
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

-spec get_cache_pid/0 :: () -> pid().
get_cache_pid() ->
    gen_server:call(?SERVER, get_cache_pid).

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
init([]) ->
    Q = start_amqp(),
    CPid = whereis(j5_cache),
    Ref = erlang:monitor(process, CPid),

    spawn(fun() -> preload_accounts() end),

    {ok, #state{is_amqp_up=is_binary(Q), cache=CPid, cache_ref=Ref}}.

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
handle_call(get_cache_pid, _, #state{cache=undefined}=State) ->
    {noreply, #state{cache=CPid}=State1} = handle_info(timeout, State),
    {reply, {ok, CPid}, State1};
handle_call(get_cache_pid, _, #state{cache=CPid}=State) ->
    {reply, {ok, CPid}, State}.

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
handle_info(timeout, #state{is_amqp_up=false}=State) ->
    {noreply, State#state{is_amqp_up=is_binary(start_amqp())}, 1000};
handle_info(timeout, #state{cache=undefined}=State) ->
    CPid = whereis(j5_cache),
    Ref = erlang:monitor(process, CPid),
    {noreply, State#state{cache=CPid, cache_ref=Ref}};

handle_info({_, #amqp_msg{payload=Payload}}, #state{cache=CPid}=State) ->
    ?LOG_START("Recv amqp payload: ~s", [Payload]),
    spawn(fun() -> handle_authz(mochijson2:decode(Payload), CPid) end),
    {noreply, State};

handle_info({'DOWN', Ref, process, CPid, _Reason}, #state{cache=CPid, cache_ref=Ref}=State) ->
    ?LOG_SYS("Cache went down: ~p", [_Reason]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{cache=undefined}};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
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
    ?LOG_END("Terminating: ~p", [_Reason]).

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
start_amqp() ->
    try
	Q = amqp_util:new_queue(),
	_ = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHZ_REQ),
	amqp_util:basic_consume(Q),
	?LOG_SYS("Listening on ~s for authz events", [Q]),
	Q
    catch
	_:_E ->
	    ?LOG_SYS("Error starting amqp: ~p", [_E]),
	    amqp_error
    end.

-spec handle_authz/2 :: (JObj, CPid) -> ok when
      JObj :: json_object(),
      CPid :: pid().
handle_authz(JObj, CPid) ->
    true = wh_api:authz_req_v(JObj),
    put(callid, wh_json:get_value(<<"Call-ID">>, JObj)),

    ?LOG("Authorize ~s can make the call to ~s", [wh_json:get_value(<<"From">>, JObj), wh_json:get_value(<<"To">>, JObj)]),

    AuthZResp = case {wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj)
		      ,wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj)
		     } of
		    {AcctID, undefined} when is_binary(AcctID) ->
			%% Coming from carrier (off-net)
			?LOG("Authorize inbound call"),
			j5_acctmgr:authz_trunk(AcctID, JObj, inbound, CPid);
		    {AcctID, AuthID} when is_binary(AcctID) andalso is_binary(AuthID) ->
			%% Coming from PBX (on-net); authed by Registrar
			?LOG("Authorize outbound call"),
			j5_acctmgr:authz_trunk(AcctID, JObj, outbound, CPid);
		    {_AcctID, _AuthID} ->
			?LOG("Error in authorization: AcctID: ~s AuthID: ~s", [_AcctID, _AuthID]),
			undefined
		end,
    send_resp(JObj, AuthZResp).

send_resp(_JObj, undefined) ->
    ?LOG_END("No response for authz");
send_resp(JObj, {AuthzResp, CCV}) ->
    ?LOG_SYS("AuthzResp: ~s", [AuthzResp]),
    ?LOG_SYS("CCVs: ~p", [CCV]),

    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),

    Prop = [{<<"Is-Authorized">>, wh_util:to_binary(AuthzResp)}
	     ,{<<"Custom-Channel-Vars">>, {struct, CCV}}
	     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
	     ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
	     | wh_api:default_headers(<<>>, <<"dialplan">>, <<"authz_resp">>, ?APP_NAME, ?APP_VSN)
	    ],

    {ok, JSON} = wh_api:authz_resp(Prop),
    ?LOG_END("Sending authz resp: ~s", [JSON]),
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>).

preload_accounts() ->
    {ok, Accts} = couch_mgr:get_results(<<"accounts">>, <<"accounts/listing_by_id">>, []),
    ?LOG_SYS("Preloading ~b accounts", [length(Accts)]),
    _ = [ jonny5_acct_sup:start_proc(wh_json:get_value(<<"id">>, AcctJObj, <<"no_id">>)) || AcctJObj <- Accts],
    {ok, TSAccts} = couch_mgr:get_results(<<"ts">>, <<"LookUpDID/DIDsByAcct">>, []), %% crappy way, make new view
    ?LOG_SYS("Preloading ~b trunkstore accounts", [length(TSAccts)]),
    [ jonny5_acct_sup:start_proc(wh_json:get_value(<<"id">>, TSAcctJObj)) || TSAcctJObj <- TSAccts].
