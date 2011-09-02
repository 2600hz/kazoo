%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Query DTH whapps for their blacklist
%%% @end
%%% Created : 30 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth_blacklist).

-behaviour(gen_listener).

%% API
-export([start_link/0, is_blacklisted/2, handle_req/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("jonny5.hrl").
-include_lib("dth/include/dth_amqp.hrl").

-define(RESPONDERS, [
		     {?MODULE, [{<<"dth">>, <<"blacklist_resp">>}]}
		    ]).
-define(BINDINGS, [
		   {self, []}
		  ]).

-define(SERVER, ?MODULE).
-define(BLACKLIST_UPDATE_TIMER, 5000).

-record(state, {
	  blacklist = dict:new() :: dict() %% {AccountID, Reason}
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
				     ], []).

stop(Srv) ->
    gen_listener:stop(Srv).

-spec is_blacklisted/2 :: (Srv, AccountID) -> 'false' | 'true' | {'true', binary()} when
      Srv :: pid(),
      AccountID :: binary().
is_blacklisted(Srv, AccountID) ->
    gen_listener:call(Srv, {is_blacklisted, AccountID}).

-spec handle_req/2 :: (JObj, Props) -> 'ok' when
      JObj :: json_object(),
      Props :: proplist().
handle_req(JObj, Props) ->
    true = dth_api:blacklist_resp_v(JObj),
    Srv = props:get_value(server, Props),
    ?LOG_SYS("Sending blacklist to ~p", [Srv]),
    Accounts = wh_json:get_value(<<"Accounts">>, JObj, []),
    gen_listener:cast(Srv, {blacklist, Accounts}).

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
    ?LOG_SYS("DTH blacklist server started"),
    true = is_reference(erlang:send_after(?BLACKLIST_UPDATE_TIMER, self(), update_blacklist)),
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
handle_call({is_blacklisted, AccountID}, _From, #state{blacklist=BL}=State) ->
    try
	Reason = dict:fetch(AccountID, BL),
	{reply, {true, Reason}, State}
    catch
	_:_ ->
	    {reply, false, State}
    end.

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
handle_cast({blacklist, {struct, Accounts}}, State) ->
    ?LOG_SYS("Updating blacklist with ~p", [Accounts]),
    {noreply, State#state{blacklist=dict:from_list(Accounts)}}.

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
handle_info(update_blacklist, State) ->
    true = is_reference(erlang:send_after(?BLACKLIST_UPDATE_TIMER, self(), update_blacklist)),
    Self = self(),
    spawn(fun() -> request_blacklist(Self) end),
    {noreply, State};
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
handle_event(_, _) ->
    {reply, [{server, self()}]}.

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
request_blacklist(Srv) ->
    Queue = gen_listener:queue_name(Srv),
    Prop = wh_api:default_headers(Queue, <<"dth">>, <<"blacklist_req">>, ?APP_NAME, ?APP_VSN),
    {ok, JSON} = dth_api:blacklist_req(Prop),
    ?LOG_SYS("Sending request for blacklist: ~s", [JSON]),
    amqp_util:callmgr_publish(JSON, <<"application/json">>, ?KEY_DTH_BLACKLIST_REQ).
