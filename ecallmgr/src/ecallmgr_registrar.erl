%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Serve up registration information
%%% @end
%%% Created : 25 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_registrar).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(CLEANUP_RATE, 1000).
-define(NEW_REF, erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok)).

-include("ecallmgr.hrl").

-record(state, {
	  cached_registrations = dict:new() :: dict() % { {Realm, User}, Fields }
	  ,timer_ref = undefined :: undefined | reference()
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

lookup(Realm, User, Fields) ->
    gen_server:call(?SERVER, {lookup, Realm, User, Fields}).

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
    Ref = ?NEW_REF,
    {ok, #state{timer_ref=Ref}}.

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
handle_call({lookup, Realm, User, Fields}, From, State) ->
    spawn(fun() -> gen_server:reply(From, lookup_reg(Realm, User, Fields, State)) end),
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
handle_info({timeout, Ref, _}, #state{cached_registrations=Regs, timer_ref=Ref}=State) ->
    NewRef = ?NEW_REF, % clean out every 60 seconds
    {noreply, State#state{cached_registrations=remove_regs(Regs), timer_ref=NewRef}};

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
-spec(lookup_reg/4 :: (Realm :: binary(), User :: binary(), Fields :: list(binary()), State :: #state{}) -> proplist()).
lookup_reg(Realm, User, Fields, #state{cached_registrations=CRegs}) ->
    case dict:find({Realm, User}, CRegs) of
	error ->
	    RegProp = [{<<"Username">>, User}
		       ,{<<"Realm">>, Realm}
		       ,{<<"Fields">>, []}
		       | whistle_api:default_headers(<<>>, <<"directory">>, <<"reg_query">>, <<"ecallmgr">>, <<>>) ],
	    {struct, RegResp} = ecallmgr_amqp_pool:reg_query(RegProp, 1500),
	    true = whistle_api:reg_query_resp_v(RegResp),
	    
	    ?SERVER ! {cache_registrations, Realm, User, RegResp},
	    lists:foldr(fun({K, _}=V, Acc) -> case lists:member(K, Fields) of true -> [V | Acc]; false -> Acc end end, [], props:get_value(<<"Fields">>, RegResp, []));
	{ok, RegResp} ->
	    lists:foldr(fun({K, _}=V, Acc) -> case lists:member(K, Fields) of true -> [V | Acc]; false -> Acc end end, [], props:get_value(<<"Fields">>, RegResp, []))
    end.

-spec(remove_regs/1 :: (Regs :: dict()) -> dict()).
remove_regs(Regs) ->
    TStamp = calendar:datetime_to_gregorian_seconds(erlang:datetime()),
    dict:filter(fun(_, RegData) ->
			RegTstamp = whistle_util:to_integer(props:get_value(<<"Timestamp">>, RegData)) +
			    whistle_util:to_integer(props:get_value(<<"Expires">>, RegData)),
			RegTstamp > TStamp
		end, Regs).
			       
