%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 4 Sep 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_events_srv).

-behaviour(gen_listener).

%% API
-export([start_link/2, subscribe/3, unsubscribe/2, fetch/1, stop/1]).

%% gen_listener callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

%% gen_listener handler callback
-export([handle_req/2]).

-include("crossbar.hrl").

-define(RESPONDERS, [{?MODULE, [{<<"*">>, <<"*">>}]}]). % all matching event cat/name combos go to here
-define(BINDINGS, []).

-define(SERVER, ?MODULE).

-record(state, {
	  account_id = <<>> :: binary()
	  ,user_id = <<>> :: binary()
          ,subscriptions = [] :: [atom(),...] | []
	  ,events = [] :: json_objects()
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
start_link(AccountID, UserID) ->
    gen_listener:start_link(?MODULE
			    ,[{responders, ?RESPONDERS}, {bindings, ?BINDINGS}]
			    ,[AccountID, UserID]
			   ).

-spec subscribe/3 :: (Srv, Sub, Options) -> 'ok' | {'error', 'unknown' | 'already_present'} when
      Srv :: pid(),
      Sub :: atom(),
      Options :: proplist().
subscribe(Srv, Sub, Options) ->
    gen_listener:call(Srv, {subscribe, Sub, Options}).

-spec unsubscribe/2 :: (Srv, Sub) -> 'ok' when
      Srv :: pid(),
      Sub :: atom().
unsubscribe(Srv, Sub) ->
    gen_listener:cast(Srv, {unsubscribe, Sub}).

-spec fetch/1 :: (Srv) -> json_objects() when
      Srv :: pid().
fetch(Srv) ->
    gen_listener:call(Srv, fetch).

-spec stop/1 :: (Srv) -> 'ok' when
      Srv :: pid().
stop(Srv) ->
    gen_listener:stop(Srv).

-spec handle_req/2 :: (JObj, Props) -> 'ok' when
      JObj :: json_object(),
      Props :: proplist().
handle_req(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {add_event, JObj}).

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
init([AccountID, UserID]) ->
    {ok, #state{account_id=AccountID, user_id=UserID}}.

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
handle_call(fetch, _, #state{events=Events}=State) ->
    {reply, lists:reverse(Events), State#state{events=[]}};
handle_call({subscribe, Sub, Options}, _, #state{subscriptions=Subs}=State) ->
    case lists:member(Sub, queue_bindings:known_bind_types()) of
	false -> {reply, {error, unknown}, State};
	true ->
	    case lists:member(Sub, Subs) of
		false -> {reply, {error, already_present}, State};
		true ->
		    gen_listener:add_binding(self(), Sub, Options),
		    {reply, ok, State#state{subscriptions=[Sub|Subs]}}
	    end
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
handle_cast({unsubscribe, Sub}, #state{subscriptions=Subs}=State) ->
    gen_listener:rm_binding(self(), Sub),
    {noreply, State#state{subscriptions=lists:delete(Sub, Subs)}};
handle_cast({add_event, JObj}, #state{events=Events}=State) ->
    {noreply, State#state{events=[JObj|Events]}}.

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
handle_event(_JObj, _State) ->
    {reply, [{server, self()}]}.

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
