%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Manage the account documents and provide specific API access to
%%% their contents for Trunkstore components (ts_route, etc).
%%% @end
%%% Created :  3 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_acctmgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Data Access API
-export([has_credit/1, deduct_credit/2, reserve_trunk/2, reserve_trunk/3, release_trunk/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE).

%% { {AccountID, Reference}, CallID, flat_rate | per_min}
-type active_call() :: tuple(tuple(binary(), reference()), binary(), flat_rate | per_min).

-record(acct, {
	  account_id = <<>> :: binary()
         ,active_calls = [] :: list(active_call())
	 }).

-record(state, {
	  accounts = [] :: list(#acct{})
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

%%%===================================================================
%%% Data Access API
%%%===================================================================
-spec(has_credit/1 :: (Acct :: binary()) -> boolean()).
has_credit(Acct) ->
    has_credit(Acct, 0).

%% Does the account have enough credit to cover Amt
-spec(has_credit/2 :: (Acct :: binary(), Amt :: integer()) -> boolean()).
has_credit(Acct, Amt) ->
    gen_server:call(?SERVER, {has_credit, Acct, Amt}, infinity).

%% Deduct the cost of the call from the account, returning the remaining balance; or return an error
-spec(deduct_credit/2 :: (Acct :: binary(), Amt :: integer()) -> tuple(ok, integer()) | tuple(error, term())).
deduct_credit(Acct, Amt) ->
    gen_server:call(?SERVER, {deduct_credit, Acct, Amt}, infinity).

%% try to reserve a trunk
%% first try to reserve a flat_rate trunk; if none are available, try a per_min trunk;
%% if the Amt is more than available credit, return error
-spec(reserve_trunk/2 :: (Acct :: binary(), CallID :: binary()) -> tuple(ok, reference(), flat_rate | per_min) | tuple(error, term())).
reserve_trunk(Acct, CallID) ->
    reserve_trunk(Acct, CallID, 0).

-spec(reserve_trunk/3 :: (Acct :: binary(), CallID :: binary(), Amt :: integer()) -> tuple(ok, reference(), flat_rate | per_min) | tuple(error, term())).
reserve_trunk(Acct, CallID, Amt) ->
    gen_server:call(?SERVER, {reserve_trunk, Acct, CallID, Amt}, infinity).

%% release a reserved trunk
%% pass the account and the reference from the reserve_trunk/2 call to release the trunk back to the account
-spec(release_trunk/2 :: (Acct :: binary(), Ref :: reference()) -> no_return()).
release_trunk(Acct, Ref) ->
    gen_server:cast(?SERVER, {release_trunk, Acct, Ref}).

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
handle_call(
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
